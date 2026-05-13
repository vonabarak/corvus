{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cap'n Proto-side client connection management.
--
-- This module is the Phase 4 / Phase 5 replacement for
-- "Corvus.Client.Connection". Each connection holds an open
-- Cap'n Proto vat plus a 'Daemon' client cap that lets the
-- caller walk to every subsystem manager and resource cap.
--
-- The two modules live side by side during the staged migration:
-- Phase 4d (this commit) adds this one; Phase 4e wires the new
-- 'Corvus.Client.Capnp.Rpc' to use it; Phase 4f flips the test
-- DSL over; Phase 5 deletes the legacy
-- "Corvus.Client.Connection".
module Corvus.Client.Capnp.Connection
  ( -- * Connection type
    CapnpConnection (..)
  , CapnpConnectionError (..)

    -- * Bracketed connecting
  , withCapnpConnection

    -- * Non-bracketed connecting (for long-lived handles)
  , openCapnpConnection
  , closeCapnpConnection

    -- * Default address helpers
  , defaultCapnpAddress
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Corvus as CGCorvus
import Capnp.Rpc
  ( ConnConfig (..)
  , fromClient
  , requestBootstrap
  , socketTransport
  , withConn
  )
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (Exception, SomeException, bracket, try)
import Corvus.Types (ListenAddress (..), getDefaultSocketPath)
import qualified Data.Default as Def
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.Simple.TCP as TCP
import Network.Socket
  ( Family (..)
  , SockAddr (..)
  , Socket
  , SocketType (..)
  , close
  , connect
  , socket
  )
import qualified Network.Socket as NS
import Supervisors (Supervisor, withSupervisor)

-- | A live Cap'n Proto client session. Holds the open socket plus
-- the bootstrap 'Daemon' client cap; closes both via
-- 'closeCapnpConnection'.
data CapnpConnection = CapnpConnection
  { ccDaemon :: !(C.Client CGCorvus.Daemon)
  -- ^ Bootstrap cap. Use as the entry point for everything else
  -- (e.g. @C.callP \#vms def ccDaemon@).
  , ccSupervisor :: !Supervisor
  -- ^ Supervisor that owns any client-side caps the caller exports
  -- (e.g. 'BuildEventSink' for streaming builds, 'ByteSink' for
  -- the serial console). Lives for the lifetime of the connection.
  , ccClose :: !(IO ())
  -- ^ Tear-down action. Closes the underlying socket, cancels the
  -- connection handler thread, and releases the supervisor.
  , ccSocket :: !Socket
  -- ^ Raw socket. Retained for transitional code paths during the
  -- Phase 6 streaming rollout; replaced once every protocol upgrade
  -- has a Cap'n Proto sink.
  }

-- | Connection-level errors. Maps roughly onto the legacy
-- 'Corvus.Client.Connection.ConnectionError' so callers can
-- translate cleanly.
data CapnpConnectionError
  = CapnpConnectFailed !Text
  | CapnpRpcError !Text
  deriving (Eq, Show)

instance Exception CapnpConnectionError

-- | Default Cap'n Proto Unix socket path: the canonical Corvus
-- socket in the XDG runtime dir. The daemon owns this socket
-- end-to-end now that the legacy 'Data.Binary' listener is gone.
defaultCapnpAddress :: IO ListenAddress
defaultCapnpAddress = UnixAddress <$> getDefaultSocketPath

-- ---------------------------------------------------------------------
-- Bracketed connect
-- ---------------------------------------------------------------------

-- | Open a connection, run an action, and close it on exit.
withCapnpConnection
  :: ListenAddress
  -> (CapnpConnection -> IO a)
  -> IO (Either CapnpConnectionError a)
withCapnpConnection addr action = do
  r <- try (openCapnpConnection addr)
  case r of
    Left (e :: SomeException) ->
      pure (Left (CapnpConnectFailed (T.pack (show e))))
    Right (Left err) -> pure (Left err)
    Right (Right conn) -> do
      a <- try (action conn)
      ccClose conn
      case a of
        Left (e :: SomeException) ->
          pure (Left (CapnpRpcError (T.pack (show e))))
        Right ok -> pure (Right ok)

-- ---------------------------------------------------------------------
-- Non-bracketed connect / close
-- ---------------------------------------------------------------------

-- | Open a long-lived connection. The caller is responsible for
-- invoking 'closeCapnpConnection' (or 'ccClose') when done.
openCapnpConnection :: ListenAddress -> IO (Either CapnpConnectionError CapnpConnection)
openCapnpConnection addr = do
  result <- try $ case addr of
    TcpAddress host port -> do
      -- Network.Simple.TCP.connect returns the socket through a
      -- continuation that closes it on exit; we want the socket
      -- to outlive the call, so use Network.Socket directly.
      addrInfo <- NS.getAddrInfo Nothing (Just host) (Just (show port))
      case addrInfo of
        (ai : _) -> do
          sock <- NS.socket (NS.addrFamily ai) NS.Stream NS.defaultProtocol
          NS.connect sock (NS.addrAddress ai)
          pure sock
        [] -> error ("no addrinfo for " <> host)
    UnixAddress path -> do
      sock <- socket AF_UNIX Stream 0
      connect sock (SockAddrUnix path)
      pure sock
  case result of
    Left (e :: SomeException) ->
      pure (Left (CapnpConnectFailed (T.pack (show e))))
    Right sock -> attachVat sock

-- | Attach a Cap'n Proto vat to the open socket and request the
-- bootstrap cap. Runs the vat's I/O loop on one background async,
-- and a long-lived 'Supervisor' (used to export client-side caps
-- like 'BuildEventSink' / 'ByteSink' for the streaming flows) on
-- another. Both are torn down via 'ccClose'.
attachVat :: Socket -> IO (Either CapnpConnectionError CapnpConnection)
attachVat sock = do
  let transport = socketTransport sock C.defaultLimit
      cfg = Def.def {debugMode = False}
  -- We need the bootstrap cap and the supervisor synchronously
  -- after spawning their async loops. Hand them out via one-shot
  -- MVars and use a paired stop MVar to release each.
  bootBox <- newEmptyMVar :: IO (MVar (Either CapnpConnectionError (C.Client CGCorvus.Daemon)))
  stopBox <- newEmptyMVar :: IO (MVar ())
  supBox <- newEmptyMVar :: IO (MVar Supervisor)
  supStopBox <- newEmptyMVar :: IO (MVar ())
  supThread <- async $ withSupervisor $ \sup -> do
    putMVar supBox sup
    takeMVar supStopBox
  sup <- takeMVar supBox
  hConn <- async $ do
    r <- try $ withConn transport cfg $ \conn -> do
      bootResult <- try $ do
        client <- requestBootstrap conn
        let daemon :: C.Client CGCorvus.Daemon
            daemon = fromClient client
        pure daemon
      case bootResult of
        Left (e :: SomeException) ->
          putMVar bootBox (Left (CapnpRpcError (T.pack (show e))))
        Right daemon -> do
          putMVar bootBox (Right daemon)
          -- Block until the connection is torn down externally.
          takeMVar stopBox
    case r of
      Left (e :: SomeException) -> do
        -- If the connection died before bootstrap, surface the
        -- failure via bootBox if nobody's read it yet.
        let err = CapnpRpcError (T.pack (show e))
        _ <- try (putMVar bootBox (Left err)) :: IO (Either SomeException ())
        pure ()
      Right () -> pure ()
  bootResult <- takeMVar bootBox
  case bootResult of
    Left err -> do
      cancel hConn
      _ <- try (putMVar supStopBox ()) :: IO (Either SomeException ())
      cancel supThread
      close sock
      pure (Left err)
    Right daemon ->
      pure
        ( Right
            CapnpConnection
              { ccDaemon = daemon
              , ccSupervisor = sup
              , ccClose = do
                  -- Signal both handlers to release their blocks,
                  -- then cancel everything and close the socket.
                  _ <- try (putMVar stopBox ()) :: IO (Either SomeException ())
                  _ <- try (putMVar supStopBox ()) :: IO (Either SomeException ())
                  cancel hConn
                  cancel supThread
                  close sock
              , ccSocket = sock
              }
        )

-- | Close a 'CapnpConnection'. Idempotent.
closeCapnpConnection :: CapnpConnection -> IO ()
closeCapnpConnection = ccClose
