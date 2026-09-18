{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Connection setup, liveness, and shared RPC helpers for the daemon-side
-- nodeagent client.
module Corvus.NodeAgentClient.Core
  ( NodeAgentClient (..)
  , NodeAgentError (..)
  , withNodeAgentClient
  , defaultNodeAgentAddress
  , ping
  , sessionPing
  , agentVersion
  , agentDefaultBasePath
  , remote
  , remoteWithin
  , callOn
  )
where

import Capnp (export)
import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
import Capnp.Rpc
  ( ConnConfig (..)
  , Transport
  , fromClient
  , requestBootstrap
  , socketTransport
  , withConn
  )
import qualified Control.Exception as E
import qualified Corvus.Tls as Tls
import qualified Data.Default as Def
import Data.Function ((&))
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified Network.Socket as NS
import Supervisors (Supervisor, withSupervisor)
import qualified System.Timeout as Timeout

-- ---------------------------------------------------------------------------
-- Client handle + lifecycle

-- | The daemon-side handle for the node agent. Holds the
-- bootstrap @NodeAgent@ cap (for ping/version/session) and the
-- already-opened @Session@ cap (used by every operation once
-- session-level methods land in later phases).
data NodeAgentClient = NodeAgentClient
  { nacAgent :: !(C.Client CGNA.NodeAgent)
  , nacSession :: !(C.Client CGNA.Session)
  , nacSupervisor :: !Supervisor
  , nacOwner :: !T.Text
  }

data NodeAgentError
  = NodeAgentConnectFailed !T.Text
  | NodeAgentRemoteError !T.Text
  | NodeAgentTimeout !T.Text
  deriving (Show)

instance E.Exception NodeAgentError

defaultNodeAgentAddress :: (String, Int)
defaultNodeAgentAddress = ("127.0.0.1", 9878)

withNodeAgentClient
  :: String
  -> Int
  -> T.Text
  -- ^ owner tag (typically the daemon uid as text)
  -> Maybe Tls.TlsConfig
  -- ^ When 'Just', wrap the TCP socket with mTLS and validate
  -- the peer's CN before issuing the bootstrap call. The
  -- caller is expected to have already specialised the config
  -- with the peer node's expected CN suffix via
  -- 'Tls.withPeerExpectation'.
  -> (Either NodeAgentError NodeAgentClient -> IO a)
  -> IO a
withNodeAgentClient host port owner mTlsCfg body = do
  sockResult <- E.try @E.SomeException (openTcp host port)
  case sockResult of
    Left e ->
      body (Left (NodeAgentConnectFailed (T.pack (show e))))
    Right sock ->
      E.bracket (pure sock) NS.close $ \_ -> runOnSocket sock
  where
    runOnSocket sock = do
      eTransport <- buildTransport mTlsCfg sock
      case eTransport of
        Left err -> body (Left (NodeAgentConnectFailed err))
        Right (transport, cleanup) ->
          (`E.finally` cleanup) $ do
            -- A streaming build provisioner can hold one
            -- outstanding 'vmGuestExecStream' for tens of minutes
            -- AND the agent is pushing tens of writes/sec on its
            -- LineBufferSink during that window. The default
            -- 128-question / 32 MiB-word budget is comfortable
            -- but small under burst loads (emerge spewing MB/s
            -- of output): exhaustion triggers STM retries inside
            -- haskell-capnp and, in pathological cases,
            -- 'BlockedIndefinitelyOnSTM'. Quadruple the budget
            -- for headroom; the memory cost is negligible vs the
            -- failure mode.
            let cfg =
                  Def.def
                    { debugMode = False
                    , maxQuestions = 4096
                    , maxCallWords = 128 * 1024 * 1024 `div` 8
                    }
            -- We need to distinguish two failure regimes inside
            -- the try below: an exception *during connection
            -- setup* (transport handshake, bootstrap, session
            -- call) should land in @body@ as
            -- @Left (NodeAgentConnectFailed ...)@ so the caller
            -- can react to a dead agent. But once setup
            -- completes and we hand @body@ a live client, any
            -- exception it throws is the caller's own (a test
            -- assertion failure, a downstream RPC error, …) and
            -- must propagate unchanged — re-invoking @body@ with
            -- a synthetic @Left@ would double-call it AND
            -- swallow the original exception.
            bodyEntered <- newIORef False
            r <-
              E.try @E.SomeException $
                withSupervisor $ \sup ->
                  withConn transport cfg $ \conn -> do
                    rawAgent <- requestBootstrap conn
                    let agent :: C.Client CGNA.NodeAgent
                        agent = fromClient rawAgent
                    CGNA.NodeAgent'session'results {CGNA.session = sess} <-
                      callOn
                        #session
                        CGNA.NodeAgent'session'params {CGNA.owner = owner}
                        agent
                    writeIORef bodyEntered True
                    body $
                      Right
                        NodeAgentClient
                          { nacAgent = agent
                          , nacSession = sess
                          , nacSupervisor = sup
                          , nacOwner = owner
                          }
            case r of
              Left (e :: E.SomeException) -> do
                entered <- readIORef bodyEntered
                if entered
                  then E.throwIO e
                  else body (Left (NodeAgentConnectFailed (T.pack (show e))))
              Right out -> pure out

-- | Build a Cap'n Proto 'Transport' over the connected socket,
-- TLS-wrapped when 'Just' was passed. Returns a teardown action
-- the caller runs when the transport is no longer in use.
buildTransport
  :: Maybe Tls.TlsConfig
  -> NS.Socket
  -> IO (Either T.Text (Transport, IO ()))
buildTransport Nothing sock =
  pure (Right (socketTransport sock C.defaultLimit, pure ()))
buildTransport (Just cfg) sock = do
  r <- E.try @E.SomeException (Tls.wrapClientSocket cfg sock)
  case r of
    Left e -> pure (Left (T.pack ("TLS handshake failed: " <> show e)))
    Right (ctx, ref) -> do
      v <- Tls.validatePeerCN cfg ref
      case v of
        Left msg -> do
          Tls.closeTlsContext ctx
          pure (Left ("TLS peer rejected: " <> msg))
        Right () -> do
          transport <- Tls.tlsTransport ctx C.defaultLimit
          pure (Right (transport, Tls.closeTlsContext ctx))

-- ---------------------------------------------------------------------------
-- Liveness

ping :: NodeAgentClient -> IO (Either NodeAgentError ())
ping nac = remote $ do
  _ :: C.Parsed CGNA.NodeAgent'ping'results <-
    callOn #ping CGNA.NodeAgent'ping'params (nacAgent nac)
  pure ()

sessionPing :: NodeAgentClient -> IO (Either NodeAgentError ())
sessionPing nac = remote $ do
  _ :: C.Parsed CGNA.Session'ping'results <-
    callOn #ping CGNA.Session'ping'params (nacSession nac)
  pure ()

agentVersion :: NodeAgentClient -> IO (Either NodeAgentError (T.Text, [T.Text]))
agentVersion nac = remote $ do
  CGNA.NodeAgent'version'results {CGNA.info = info_} <-
    callOn #version CGNA.NodeAgent'version'params (nacAgent nac)
  let CGNA.AgentInfo {CGNA.semver = sv, CGNA.capabilities = caps} = info_
  pure (sv, caps)

-- | Ask the remote nodeagent for its preferred @basePath@. The
-- agent resolves @$HOME/VMs@ against its own process environment,
-- so a heterogeneous cluster (different users running the agent
-- on each node) gets per-node-correct defaults without the
-- operator needing to know each user's home.
agentDefaultBasePath :: NodeAgentClient -> IO (Either NodeAgentError T.Text)
agentDefaultBasePath nac = remote $ do
  CGNA.NodeAgent'defaultBasePath'results {CGNA.path = p} <-
    callOn #defaultBasePath CGNA.NodeAgent'defaultBasePath'params (nacAgent nac)
  pure p

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

remote :: IO a -> IO (Either NodeAgentError a)
remote action = do
  r <- E.try @E.SomeException action
  case r of
    Right a -> pure (Right a)
    Left e -> pure (Left (NodeAgentRemoteError (T.pack (show e))))

-- | Like 'remote', but bounds the call with a wall-clock deadline.
-- On expiry the daemon thread is freed with a 'NodeAgentTimeout';
-- because the long agent-side handlers run via 'handleParsedAsync'
-- (forked off the session dispatcher), abandoning the call here no
-- longer wedges the rest of that node's RPCs — only the orphaned
-- forked handler keeps running until it finishes. Used for the
-- calls that can legitimately block for a long time
-- (vmStopGraceful, vmStart, vmSave, snapshotCreateLive) and for
-- probeVsockCid (which sits behind those in the allocator loop).
remoteWithin :: Int -> IO a -> IO (Either NodeAgentError a)
remoteWithin deadlineSec action = do
  r <- E.try @E.SomeException (Timeout.timeout (deadlineSec * 1000000) action)
  case r of
    Right (Just a) -> pure (Right a)
    Right Nothing ->
      pure $
        Left $
          NodeAgentTimeout $
            "node-agent RPC exceeded " <> T.pack (show deadlineSec) <> "s deadline"
    Left e -> pure (Left (NodeAgentRemoteError (T.pack (show e))))

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
