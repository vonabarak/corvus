{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Daemon-side Cap'n Proto client for `corvus-netd`.
--
-- Phase 2.5: declarative API. The daemon expresses intent
-- ('applyNetwork', 'applyTap') and the agent reconciles. No
-- per-resource caps to hold; the daemon's DB is the only intent
-- store.
--
-- The Session cap is thread-safe ('callP' goes through STM), so
-- concurrent daemon handlers can issue calls without extra
-- locking. The underlying TCP connection is held by a single
-- 'withConn' bracket on the daemon's main thread — see
-- 'withNetAgentClient'.
module Corvus.NetAgentClient
  ( -- * Client handle
    NetAgentClient (..)
  , NetAgentError (..)

    -- * Lifecycle
  , withNetAgentClient
  , defaultNetAgentAddress

    -- * Spec types (re-exported convenience)
  , NetworkSpec (..)
  , NatSpec (..)
  , DhcpSpec (..)
  , NetworkInfo (..)
  , TapSpec (..)
  , TapInfo (..)

    -- * Liveness / negotiation
  , ping
  , agentVersion

    -- * Networks
  , applyNetwork
  , listNetworks
  , deleteNetwork

    -- * TAPs
  , applyTap
  , listTaps
  , deleteTap

    -- * Kernel knobs
  , setIpForwarding
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

-- ---------------------------------------------------------------------------
-- Spec types (mirror agent-side Network / Tap modules + Cap'n Proto)

data NetworkSpec = NetworkSpec
  { nsName :: !T.Text
  , nsCidr :: !T.Text
  , nsMtu :: !Word32
  , nsNat :: !NatSpec
  , nsDhcp :: !DhcpSpec
  }
  deriving (Eq, Show)

data NatSpec = NatSpec
  { natEnabled :: !Bool
  , natUplinkIf :: !T.Text
  }
  deriving (Eq, Show)

data DhcpSpec = DhcpSpec
  { dhcpEnabled :: !Bool
  , dhcpRangeStart :: !T.Text
  , dhcpRangeEnd :: !T.Text
  , dhcpLeaseTime :: !T.Text
  , dhcpDomain :: !T.Text
  , dhcpExtraArgs :: ![T.Text]
  }
  deriving (Eq, Show)

data NetworkInfo = NetworkInfo
  { niSpec :: !NetworkSpec
  , niUpState :: !T.Text
  , niDnsmasqPid :: !Word32
  }
  deriving (Show)

data TapSpec = TapSpec
  { tsName :: !T.Text
  , tsBridge :: !T.Text
  , tsUid :: !Word32
  , tsGid :: !Word32
  }
  deriving (Eq, Show)

data TapInfo = TapInfo
  { tiSpec :: !TapSpec
  , tiUpState :: !T.Text
  }
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Client handle + lifecycle

-- | The daemon-side handle for the agent. Holds the bootstrap
-- @NetAgent@ cap (for ping/version/session) and the
-- already-opened @Session@ cap (used by every privileged op).
data NetAgentClient = NetAgentClient
  { nacAgent :: !(C.Client CGN.NetAgent)
  , nacSession :: !(C.Client CGN.Session)
  , nacSupervisor :: !Supervisor
  , nacOwner :: !T.Text
  }

data NetAgentError
  = NetAgentConnectFailed !T.Text
  | NetAgentRemoteError !T.Text
  deriving (Show)

instance E.Exception NetAgentError

defaultNetAgentAddress :: (String, Int)
defaultNetAgentAddress = ("127.0.0.1", 9877)

withNetAgentClient
  :: String
  -> Int
  -> T.Text
  -- ^ owner tag (typically the daemon uid as text)
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
        Left (e :: E.SomeException) ->
          body (Left (NetAgentConnectFailed (T.pack (show e))))
        Right out -> pure out

-- ---------------------------------------------------------------------------
-- Liveness

ping :: NetAgentClient -> IO (Either NetAgentError ())
ping nac = remote $ do
  _ :: C.Parsed CGN.NetAgent'ping'results <-
    callOn #ping CGN.NetAgent'ping'params (nacAgent nac)
  pure ()

agentVersion :: NetAgentClient -> IO (Either NetAgentError (T.Text, [T.Text]))
agentVersion nac = remote $ do
  CGN.NetAgent'version'results {CGN.info = info_} <-
    callOn #version CGN.NetAgent'version'params (nacAgent nac)
  let CGN.AgentInfo {CGN.semver = sv, CGN.capabilities = caps} = info_
  pure (sv, caps)

-- ---------------------------------------------------------------------------
-- Networks

applyNetwork
  :: NetAgentClient -> NetworkSpec -> IO (Either NetAgentError NetworkInfo)
applyNetwork nac spec = remote $ do
  CGN.Session'applyNetwork'results {CGN.info = info_} <-
    callOn
      #applyNetwork
      CGN.Session'applyNetwork'params {CGN.spec = encodeNetworkSpec spec}
      (nacSession nac)
  pure (decodeNetworkInfo info_)

listNetworks :: NetAgentClient -> IO (Either NetAgentError [NetworkInfo])
listNetworks nac = remote $ do
  CGN.Session'listNetworks'results {CGN.networks = ns} <-
    callOn
      #listNetworks
      CGN.Session'listNetworks'params
      (nacSession nac)
  pure (map decodeNetworkInfo ns)

deleteNetwork :: NetAgentClient -> T.Text -> IO (Either NetAgentError ())
deleteNetwork nac name = remote $ do
  _ :: C.Parsed CGN.Session'deleteNetwork'results <-
    callOn
      #deleteNetwork
      CGN.Session'deleteNetwork'params {CGN.name = name}
      (nacSession nac)
  pure ()

-- ---------------------------------------------------------------------------
-- TAPs

applyTap :: NetAgentClient -> TapSpec -> IO (Either NetAgentError TapInfo)
applyTap nac spec = remote $ do
  CGN.Session'applyTap'results {CGN.info = info_} <-
    callOn
      #applyTap
      CGN.Session'applyTap'params {CGN.spec = encodeTapSpec spec}
      (nacSession nac)
  pure (decodeTapInfo info_)

listTaps :: NetAgentClient -> IO (Either NetAgentError [TapInfo])
listTaps nac = remote $ do
  CGN.Session'listTaps'results {CGN.taps = ts} <-
    callOn
      #listTaps
      CGN.Session'listTaps'params
      (nacSession nac)
  pure (map decodeTapInfo ts)

deleteTap :: NetAgentClient -> T.Text -> IO (Either NetAgentError ())
deleteTap nac name = remote $ do
  _ :: C.Parsed CGN.Session'deleteTap'results <-
    callOn
      #deleteTap
      CGN.Session'deleteTap'params {CGN.name = name}
      (nacSession nac)
  pure ()

-- ---------------------------------------------------------------------------
-- Kernel knobs

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
-- Encoders / decoders

encodeNetworkSpec :: NetworkSpec -> CGN.Parsed CGN.NetworkSpec
encodeNetworkSpec s =
  CGN.NetworkSpec
    { CGN.name = nsName s
    , CGN.cidr = nsCidr s
    , CGN.mtu = nsMtu s
    , CGN.nat = encodeNatSpec (nsNat s)
    , CGN.dhcp = encodeDhcpSpec (nsDhcp s)
    }

encodeNatSpec :: NatSpec -> CGN.Parsed CGN.NatSpec
encodeNatSpec n =
  CGN.NatSpec
    { CGN.enabled = natEnabled n
    , CGN.uplinkIf = natUplinkIf n
    }

encodeDhcpSpec :: DhcpSpec -> CGN.Parsed CGN.DhcpSpec
encodeDhcpSpec d =
  CGN.DhcpSpec
    { CGN.enabled = dhcpEnabled d
    , CGN.rangeStart = dhcpRangeStart d
    , CGN.rangeEnd = dhcpRangeEnd d
    , CGN.leaseTime = dhcpLeaseTime d
    , CGN.domain = dhcpDomain d
    , CGN.extraArgs = dhcpExtraArgs d
    }

encodeTapSpec :: TapSpec -> CGN.Parsed CGN.TapSpec
encodeTapSpec s =
  CGN.TapSpec
    { CGN.name = tsName s
    , CGN.bridge = tsBridge s
    , CGN.uid = tsUid s
    , CGN.gid = tsGid s
    }

decodeNetworkInfo :: CGN.Parsed CGN.NetworkInfo -> NetworkInfo
decodeNetworkInfo
  CGN.NetworkInfo
    { CGN.spec = spec
    , CGN.upState = up_
    , CGN.dnsmasqPid = pid
    } =
    NetworkInfo
      { niSpec = decodeNetworkSpec spec
      , niUpState = up_
      , niDnsmasqPid = pid
      }

decodeNetworkSpec :: CGN.Parsed CGN.NetworkSpec -> NetworkSpec
decodeNetworkSpec
  CGN.NetworkSpec
    { CGN.name = name
    , CGN.cidr = cidr
    , CGN.mtu = mtu
    , CGN.nat = nat
    , CGN.dhcp = dhcp
    } =
    NetworkSpec
      { nsName = name
      , nsCidr = cidr
      , nsMtu = mtu
      , nsNat = decodeNatSpec nat
      , nsDhcp = decodeDhcpSpec dhcp
      }

decodeNatSpec :: CGN.Parsed CGN.NatSpec -> NatSpec
decodeNatSpec CGN.NatSpec {CGN.enabled = e, CGN.uplinkIf = u} =
  NatSpec {natEnabled = e, natUplinkIf = u}

decodeDhcpSpec :: CGN.Parsed CGN.DhcpSpec -> DhcpSpec
decodeDhcpSpec
  CGN.DhcpSpec
    { CGN.enabled = e
    , CGN.rangeStart = rs
    , CGN.rangeEnd = re
    , CGN.leaseTime = lt
    , CGN.domain = dom
    , CGN.extraArgs = extra
    } =
    DhcpSpec
      { dhcpEnabled = e
      , dhcpRangeStart = rs
      , dhcpRangeEnd = re
      , dhcpLeaseTime = lt
      , dhcpDomain = dom
      , dhcpExtraArgs = extra
      }

decodeTapInfo :: CGN.Parsed CGN.TapInfo -> TapInfo
decodeTapInfo
  CGN.TapInfo
    { CGN.spec = spec
    , CGN.upState = up_
    } =
    TapInfo {tiSpec = decodeTapSpec spec, tiUpState = up_}

decodeTapSpec :: CGN.Parsed CGN.TapSpec -> TapSpec
decodeTapSpec
  CGN.TapSpec
    { CGN.name = name
    , CGN.bridge = bridge
    , CGN.uid = uid
    , CGN.gid = gid
    } =
    TapSpec
      { tsName = name
      , tsBridge = bridge
      , tsUid = uid
      , tsGid = gid
      }

-- ---------------------------------------------------------------------------
-- Internals

openTcp :: String -> Int -> IO NS.Socket
openTcp host port = do
  ais <- NS.getAddrInfo Nothing (Just host) (Just (show port))
  case ais of
    (ai : _) -> do
      sock <- NS.socket (NS.addrFamily ai) NS.Stream NS.defaultProtocol
      NS.connect sock (NS.addrAddress ai)
      pure sock
    [] -> E.throwIO (userError ("no addrinfo for " <> host))

remote :: IO a -> IO (Either NetAgentError a)
remote action = do
  r <- E.try @E.SomeException action
  case r of
    Right a -> pure (Right a)
    Left e -> pure (Left (NetAgentRemoteError (T.pack (show e))))

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
