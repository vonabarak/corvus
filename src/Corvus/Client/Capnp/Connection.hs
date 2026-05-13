{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cap'n Proto-side client connection management.
--
-- A 'CapnpConnection' is a thin handle the CLI passes to every
-- 'Corvus.Client.Capnp.Rpc' wrapper. The implementation follows
-- the Cap'n Proto tutorial pattern: @withConn@ runs synchronously
-- on the calling thread, and the caller's action lives inside it
-- so the message pump's STM references stay reachable from the
-- thread that's actually issuing calls. Wrapping @withConn@ in
-- an 'async' (an earlier draft of this module did) trips GHC's
-- 'BlockedIndefinitelyOnSTM' detector the moment the caller
-- waits on a 'C.waitPipeline' result.
module Corvus.Client.Capnp.Connection
  ( -- * Connection handle
    CapnpConnection (..)
  , CapnpConnectionError (..)

    -- * Bracketed connect
  , withCapnpConnection

    -- * Default address helper
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
import Control.Exception (Exception, SomeException, bracket, try)
import Corvus.Types (ListenAddress (..), getDefaultSocketPath)
import qualified Data.Default as Def
import Data.Text (Text)
import qualified Data.Text as T
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

-- | A live Cap'n Proto client session, scoped to the
-- 'withCapnpConnection' callback. Holds the bootstrap 'Daemon'
-- cap and a 'Supervisor' callers can use to export client-side
-- caps (e.g. 'BuildEventSink' / 'ByteSink' for streaming flows).
-- The socket itself stays inside the bracket; callers should
-- not retain references to fields of this record beyond the
-- callback.
data CapnpConnection = CapnpConnection
  { ccDaemon :: !(C.Client CGCorvus.Daemon)
  , ccSupervisor :: !Supervisor
  , ccSocket :: !Socket
  -- ^ Raw socket, retained for diagnostics. Closing it is the
  -- enclosing 'withCapnpConnection''s responsibility.
  }

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

-- | Open a connection, run an action with the resulting
-- 'CapnpConnection', and tear everything down cleanly when the
-- action returns (or throws).
withCapnpConnection
  :: ListenAddress
  -> (CapnpConnection -> IO a)
  -> IO (Either CapnpConnectionError a)
withCapnpConnection addr action = do
  sockResult <- try (openSocket addr) :: IO (Either SomeException Socket)
  case sockResult of
    Left e ->
      pure (Left (CapnpConnectFailed (T.pack (show e))))
    Right sock ->
      bracket (pure sock) close $ \_ -> runOnSocket sock
  where
    runOnSocket sock = do
      let transport = socketTransport sock C.defaultLimit
          cfg = Def.def {debugMode = False}
      r <- try $
        withSupervisor $ \sup ->
          withConn transport cfg $ \conn -> do
            rawClient <- requestBootstrap conn
            let daemon :: C.Client CGCorvus.Daemon
                daemon = fromClient rawClient
            action
              CapnpConnection
                { ccDaemon = daemon
                , ccSupervisor = sup
                , ccSocket = sock
                }
      case r of
        Left (e :: SomeException) ->
          pure (Left (CapnpRpcError (T.pack (show e))))
        Right a -> pure (Right a)

-- | Open the underlying socket for the listen address (TCP or
-- Unix). The socket is closed by the enclosing
-- 'withCapnpConnection' bracket.
openSocket :: ListenAddress -> IO Socket
openSocket (TcpAddress host port) = do
  ais <- NS.getAddrInfo Nothing (Just host) (Just (show port))
  case ais of
    (ai : _) -> do
      sock <- NS.socket (NS.addrFamily ai) NS.Stream NS.defaultProtocol
      NS.connect sock (NS.addrAddress ai)
      pure sock
    [] -> error ("no addrinfo for " <> host)
openSocket (UnixAddress path) = do
  sock <- socket AF_UNIX Stream 0
  connect sock (SockAddrUnix path)
  pure sock
