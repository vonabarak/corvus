{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Daemon-side Cap'n Proto client for `corvus-netd`.
--
-- The daemon dials the agent at startup, captures a @Session@ cap
-- scoped to the daemon's uid, and stashes the cap on
-- 'Corvus.Types.ServerState'. Every Phase 3 network handler that
-- used to call @nsExec@ / @nsSpawn@ now calls one of the typed
-- wrappers in this module.
--
-- All wrappers run in @IO@ and surface failures as a 'NetAgentError'
-- — either a connection-level failure (the agent went away) or a
-- remote exception (the agent's cap method threw via
-- @throwFailed@). The handler layer converts these to Cap'n
-- Proto error envelopes for the daemon's own clients.
--
-- The Session cap is thread-safe: 'Capnp.Repr.Methods.callP' goes
-- through STM, so concurrent daemon handlers can issue calls
-- without locking. The underlying TCP connection is held by a
-- single 'withConn' bracket running on the daemon's main thread
-- — see 'withNetAgentClient'.
module Corvus.NetAgentClient
  ( -- * Client handle
    NetAgentClient (..)
  , NetAgentError (..)

    -- * Lifecycle
  , withNetAgentClient
  , defaultNetAgentAddress

    -- * Liveness / negotiation
  , ping
  , agentVersion

    -- * Bridge ops
  , createBridge
  , claimBridge
  , destroyBridge

    -- * TAP ops
  , createTap
  , claimTap
  , destroyTap
  , attachTapToBridge
  , detachTapFromBridge

    -- * NAT
  , installNat
  , destroyNatRule

    -- * dnsmasq
  , startDnsmasq
  , stopDnsmasq

    -- * Kernel knobs
  , setIpForwarding

    -- * Re-exported cap types for handler signatures
  , BridgeCap
  , TapCap
  , NatRuleCap
  , DnsmasqHandleCap
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Netagent as CGN
import Capnp.Rpc
  ( ConnConfig (..)
  , fromClient
  , requestBootstrap
  , socketTransport
  , withConn
  )
import qualified Control.Exception as E
import qualified Data.Default as Def
import Data.Function ((&))
import qualified Data.Text as T
import Data.Word (Word32)
import qualified Network.Socket as NS
import Supervisors (Supervisor, withSupervisor)

-- | Aliases so handler signatures don't have to drag the
-- generated module path through their type. Each is the wire
-- shape of a cap returned by the agent.
type BridgeCap = C.Client CGN.Bridge

type TapCap = C.Client CGN.Tap

type NatRuleCap = C.Client CGN.NatRule

type DnsmasqHandleCap = C.Client CGN.DnsmasqHandle

-- | The daemon-side handle for the agent. Holds the bootstrap
-- @NetAgent@ cap (for ping/version/session) and the
-- already-opened @Session@ cap (used by every privileged op).
data NetAgentClient = NetAgentClient
  { nacAgent :: !(C.Client CGN.NetAgent)
  , nacSession :: !(C.Client CGN.Session)
  , nacSupervisor :: !Supervisor
  , nacOwner :: !T.Text
  }

-- | Failures observable by handlers.
data NetAgentError
  = NetAgentConnectFailed !T.Text
  | -- | Agent-side error; the agent threw, the wire round-tripped
    -- the message back.
    NetAgentRemoteError !T.Text
  deriving (Show)

instance E.Exception NetAgentError

-- | Default endpoint matches the agent's defaults
-- ('Corvus.Netd.Server.defaultNetdHost' /
-- 'Corvus.Netd.Server.defaultNetdPort'). Override via the daemon's
-- @--netd-host@ / @--netd-port@ flags.
defaultNetAgentAddress :: (String, Int)
defaultNetAgentAddress = ("127.0.0.1", 9877)

-- | Dial the agent, open a 'Session', and run @body@ with the
-- resulting handle. The TCP connection stays open for the
-- entire @body@; closing happens via the bracket inside
-- 'withConn'.
--
-- The owner tag is the daemon's identity; the agent uses it as
-- a partition key in its ledger. We pass the daemon's effective
-- uid as a stringified number.
withNetAgentClient
  :: String
  -- ^ host
  -> Int
  -- ^ port
  -> T.Text
  -- ^ owner tag (typically the daemon uid)
  -> (Either NetAgentError NetAgentClient -> IO a)
  -> IO a
withNetAgentClient host port owner body = do
  sockResult <- E.try @E.SomeException (openTcp host port)
  case sockResult of
    Left e ->
      body (Left (NetAgentConnectFailed (T.pack (show e))))
    Right sock ->
      E.bracket (pure sock) NS.close $ \_ -> runOnSocket sock
  where
    runOnSocket sock = do
      let transport = socketTransport sock C.defaultLimit
          cfg = Def.def {debugMode = False}
      r <-
        E.try @E.SomeException $
          withSupervisor $ \sup ->
            withConn transport cfg $ \conn -> do
              rawAgent <- requestBootstrap conn
              let agent :: C.Client CGN.NetAgent
                  agent = fromClient rawAgent
              -- Open a session in the same connection.
              CGN.NetAgent'session'results {CGN.session = sess} <-
                callOn
                  #session
                  CGN.NetAgent'session'params {CGN.owner = owner}
                  agent
              body $
                Right
                  NetAgentClient
                    { nacAgent = agent
                    , nacSession = sess
                    , nacSupervisor = sup
                    , nacOwner = owner
                    }
      case r of
        Left (e :: E.SomeException) -> body (Left (NetAgentConnectFailed (T.pack (show e))))
        Right out -> pure out

-- | Liveness probe. Returns @Right ()@ on success.
ping :: NetAgentClient -> IO (Either NetAgentError ())
ping nac = remote $ do
  _ :: C.Parsed CGN.NetAgent'ping'results <-
    callOn #ping CGN.NetAgent'ping'params (nacAgent nac)
  pure ()

-- | @version()@. Returns the agent's semver + capability list.
agentVersion :: NetAgentClient -> IO (Either NetAgentError (T.Text, [T.Text]))
agentVersion nac = remote $ do
  CGN.NetAgent'version'results {CGN.info = info_} <-
    callOn #version CGN.NetAgent'version'params (nacAgent nac)
  let CGN.AgentInfo {CGN.semver = sv, CGN.capabilities = caps} = info_
  pure (sv, caps)

-- ---------------------------------------------------------------------------
-- Bridge

createBridge
  :: NetAgentClient
  -> T.Text
  -- ^ name
  -> T.Text
  -- ^ CIDR (or "" for L2-only)
  -> Word32
  -- ^ MTU
  -> IO (Either NetAgentError BridgeCap)
createBridge nac name cidr mtu = remote $ do
  let bp =
        CGN.BridgeParams
          { CGN.name = name
          , CGN.cidr = cidr
          , CGN.mtu = mtu
          }
  CGN.Session'createBridge'results {CGN.bridge = br} <-
    callOn
      #createBridge
      CGN.Session'createBridge'params {CGN.params = bp}
      (nacSession nac)
  pure br

claimBridge :: NetAgentClient -> T.Text -> IO (Either NetAgentError BridgeCap)
claimBridge nac name = remote $ do
  CGN.Session'claimBridge'results {CGN.bridge = br} <-
    callOn
      #claimBridge
      CGN.Session'claimBridge'params {CGN.name = name}
      (nacSession nac)
  pure br

destroyBridge :: BridgeCap -> IO (Either NetAgentError ())
destroyBridge bridge = remote $ do
  _ :: C.Parsed CGN.Bridge'destroy'results <-
    callOn #destroy CGN.Bridge'destroy'params bridge
  pure ()

-- ---------------------------------------------------------------------------
-- TAP

createTap
  :: NetAgentClient
  -> T.Text
  -- ^ tap ifname
  -> BridgeCap
  -- ^ bridge to attach to
  -> Word32
  -- ^ owner uid (TUNSETOWNER)
  -> Word32
  -- ^ owner gid
  -> IO (Either NetAgentError TapCap)
createTap nac name bridge uid gid = remote $ do
  let tp =
        CGN.TapParams
          { CGN.name = name
          , CGN.bridge = bridge
          , CGN.uid = uid
          , CGN.gid = gid
          }
  CGN.Session'createTap'results {CGN.tap = t} <-
    callOn
      #createTap
      CGN.Session'createTap'params {CGN.params = tp}
      (nacSession nac)
  pure t

claimTap :: NetAgentClient -> T.Text -> IO (Either NetAgentError TapCap)
claimTap nac name = remote $ do
  CGN.Session'claimTap'results {CGN.tap = t} <-
    callOn
      #claimTap
      CGN.Session'claimTap'params {CGN.name = name}
      (nacSession nac)
  pure t

destroyTap :: TapCap -> IO (Either NetAgentError ())
destroyTap tap = remote $ do
  _ :: C.Parsed CGN.Tap'destroy'results <-
    callOn #destroy CGN.Tap'destroy'params tap
  pure ()

attachTapToBridge :: BridgeCap -> TapCap -> IO (Either NetAgentError ())
attachTapToBridge bridge tap = remote $ do
  _ :: C.Parsed CGN.Bridge'attachTap'results <-
    callOn #attachTap CGN.Bridge'attachTap'params {CGN.tap = tap} bridge
  pure ()

detachTapFromBridge :: BridgeCap -> TapCap -> IO (Either NetAgentError ())
detachTapFromBridge bridge tap = remote $ do
  _ :: C.Parsed CGN.Bridge'detachTap'results <-
    callOn #detachTap CGN.Bridge'detachTap'params {CGN.tap = tap} bridge
  pure ()

-- ---------------------------------------------------------------------------
-- NAT

installNat
  :: NetAgentClient
  -> BridgeCap
  -> T.Text
  -- ^ uplink interface ("" = match any)
  -> T.Text
  -- ^ subnet (CIDR)
  -> IO (Either NetAgentError NatRuleCap)
installNat nac bridge uplinkIf subnet = remote $ do
  let np =
        CGN.NatParams
          { CGN.bridge = bridge
          , CGN.uplinkIf = uplinkIf
          , CGN.subnet = subnet
          }
  CGN.Session'installNat'results {CGN.nat = n} <-
    callOn
      #installNat
      CGN.Session'installNat'params {CGN.params = np}
      (nacSession nac)
  pure n

destroyNatRule :: NatRuleCap -> IO (Either NetAgentError ())
destroyNatRule nat = remote $ do
  _ :: C.Parsed CGN.NatRule'destroy'results <-
    callOn #destroy CGN.NatRule'destroy'params nat
  pure ()

-- ---------------------------------------------------------------------------
-- dnsmasq

startDnsmasq
  :: NetAgentClient
  -> BridgeCap
  -> T.Text
  -- ^ listen address (the bridge gateway IP)
  -> T.Text
  -- ^ DHCP range, or "" to disable DHCP
  -> T.Text
  -- ^ domain, or "" for default
  -> [T.Text]
  -- ^ extra args
  -> IO (Either NetAgentError DnsmasqHandleCap)
startDnsmasq nac bridge listenAddr dhcpRange domain extraArgs = remote $ do
  let dp =
        CGN.DnsmasqParams
          { CGN.bridge = bridge
          , CGN.listenAddr = listenAddr
          , CGN.dhcpRange = dhcpRange
          , CGN.domain = domain
          , CGN.extraArgs = extraArgs
          }
  CGN.Session'startDnsmasq'results {CGN.server = srv} <-
    callOn
      #startDnsmasq
      CGN.Session'startDnsmasq'params {CGN.params = dp}
      (nacSession nac)
  pure srv

stopDnsmasq :: DnsmasqHandleCap -> IO (Either NetAgentError ())
stopDnsmasq handle_ = remote $ do
  _ :: C.Parsed CGN.DnsmasqHandle'stop'results <-
    callOn #stop CGN.DnsmasqHandle'stop'params handle_
  pure ()

-- ---------------------------------------------------------------------------
-- Kernel knobs

-- | Enable/disable IPv4 forwarding on the host. The agent writes
-- @/proc/sys/net/ipv4/ip_forward@ directly.
setIpForwarding :: NetAgentClient -> Bool -> IO (Either NetAgentError ())
setIpForwarding nac enabled = remote $ do
  _ :: C.Parsed CGN.Session'setIpForwarding'results <-
    callOn
      #setIpForwarding
      CGN.Session'setIpForwarding'params
        { CGN.enabled = enabled
        , CGN.family_ = CGN.NetFamily'v4
        }
      (nacSession nac)
  pure ()

-- ---------------------------------------------------------------------------
-- Internals

-- | Open a TCP socket to the agent. Helper for 'withNetAgentClient'.
openTcp :: String -> Int -> IO NS.Socket
openTcp host port = do
  ais <- NS.getAddrInfo Nothing (Just host) (Just (show port))
  case ais of
    (ai : _) -> do
      sock <- NS.socket (NS.addrFamily ai) NS.Stream NS.defaultProtocol
      NS.connect sock (NS.addrAddress ai)
      pure sock
    [] -> E.throwIO (userError ("no addrinfo for " <> host))

-- | Wrap a remote call so exceptions become 'NetAgentRemoteError'.
remote :: IO a -> IO (Either NetAgentError a)
remote action = do
  r <- E.try @E.SomeException action
  case r of
    Right a -> pure (Right a)
    Left e -> pure (Left (NetAgentRemoteError (T.pack (show e))))

-- | Call a method, wait for the reply, and parse the raw struct
-- into its 'Parsed' form. Mirrors
-- 'Corvus.Client.Capnp.Rpc.callOn' but lives here so the daemon's
-- network handlers don't have to pull in the @crv@ client layer.
callOn
  :: forall iface params results
   . ( C.IsCap iface
     , C.IsStruct params
     , C.IsStruct results
     , C.Parse params (C.Parsed params)
     , C.Parse results (C.Parsed results)
     )
  => C.Method iface params results
  -> C.Parsed params
  -> C.Client iface
  -> IO (C.Parsed results)
callOn method p client = do
  raw <- (client & C.callP method p) >>= C.waitPipeline
  C.evalLimitT C.defaultLimit (C.parse raw)
