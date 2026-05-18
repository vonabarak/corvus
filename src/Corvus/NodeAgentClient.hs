{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Daemon-side Cap'n Proto client for `corvus-nodeagent`.
--
-- Phase 1: bootstrap only. The session cap is opened at connect
-- time so handlers can route session-level RPC through it once
-- later phases add operations. The Session cap is thread-safe
-- ('callP' goes through STM), so concurrent daemon handlers can
-- issue calls without extra locking.
--
-- The underlying TCP connection is held by a single 'withConn'
-- bracket on the daemon's main thread — see
-- 'withNodeAgentClient' and the 'runNodeAgentConnection' async
-- in @app/daemon/Main.hs@.
module Corvus.NodeAgentClient
  ( -- * Client handle
    NodeAgentClient (..)
  , NodeAgentError (..)

    -- * Lifecycle
  , withNodeAgentClient
  , defaultNodeAgentAddress

    -- * Liveness / negotiation
  , ping
  , sessionPing
  , agentVersion
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
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
import qualified Network.Socket as NS
import Supervisors (Supervisor, withSupervisor)

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
  deriving (Show)

instance E.Exception NodeAgentError

defaultNodeAgentAddress :: (String, Int)
defaultNodeAgentAddress = ("127.0.0.1", 9878)

withNodeAgentClient
  :: String
  -> Int
  -> T.Text
  -- ^ owner tag (typically the daemon uid as text)
  -> (Either NodeAgentError NodeAgentClient -> IO a)
  -> IO a
withNodeAgentClient host port owner body = do
  sockResult <- E.try @E.SomeException (openTcp host port)
  case sockResult of
    Left e ->
      body (Left (NodeAgentConnectFailed (T.pack (show e))))
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
              let agent :: C.Client CGNA.NodeAgent
                  agent = fromClient rawAgent
              CGNA.NodeAgent'session'results {CGNA.session = sess} <-
                callOn
                  #session
                  CGNA.NodeAgent'session'params {CGNA.owner = owner}
                  agent
              body $
                Right
                  NodeAgentClient
                    { nacAgent = agent
                    , nacSession = sess
                    , nacSupervisor = sup
                    , nacOwner = owner
                    }
      case r of
        Left (e :: E.SomeException) ->
          body (Left (NodeAgentConnectFailed (T.pack (show e))))
        Right out -> pure out

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
