{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Cap'n Proto RPC listener — one listener per 'ListenAddress'.
--
-- The daemon spawns one of these per enabled endpoint (Unix
-- socket and/or TCP) so a single daemon can serve local Unix
-- clients alongside remote TCP/mTLS clients. The Unix socket
-- path is @\$XDG_RUNTIME_DIR\/corvus\/corvus.sock@ by default;
-- a host\/port can be passed for TCP. Each accepted connection
-- gets its own 'handleConn' loop with the 'Daemon' cap as the
-- bootstrap interface.
module Corvus.Rpc.Server
  ( runCapnpServer
  )
where

import Capnp (export)
import qualified Capnp.Gen.Corvus as CGCorvus
import Capnp.Rpc
  ( ConnConfig (..)
  , Transport
  , handleConn
  , socketTransport
  , toClient
  )
import Capnp.TraversalLimit (defaultLimit)
import Control.Concurrent (forkFinally)
import Control.Exception (bracket, catch)
import Control.Monad (forever, void)
import Corvus.Rpc.Daemon (newDaemonCap)
import qualified Corvus.Tls as Tls
import Corvus.Types (ListenAddress (..), ServerState (..), getDefaultSocketPath)
import qualified Data.Default as Def
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.Simple.TCP as TCP
import Network.Socket
  ( Family (..)
  , SockAddr (..)
  , Socket
  , SocketType (..)
  , accept
  , bind
  , close
  , listen
  , maxListenQueue
  , socket
  )
import Supervisors (Supervisor, withSupervisor)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (IOError)

-- | Run the Cap'n Proto RPC server on the given address.
-- Blocks the calling thread; spawns a fresh handler per connection.
-- Unix-socket connections always go plain; TCP connections wrap
-- with mutual TLS iff 'ssTlsConfig' is 'Just'. After the TLS
-- handshake the peer's CN is validated against the daemon's
-- 'tcExpectedPeerPrefix' (set to @corvus-client:@ at boot);
-- connections from any other prefix are dropped before any RPC
-- frame is dispatched.
runCapnpServer :: ServerState -> ListenAddress -> IO ()
runCapnpServer state addr = withSupervisor $ \sup ->
  case addr of
    TcpAddress host port ->
      TCP.serve (TCP.Host host) (show port) $
        uncurry (runOneTcpConn state sup)
    UnixAddress path -> do
      createDirectoryIfMissing True (takeDirectory path)
      removeIgnore path
      bracket
        (socket AF_UNIX Stream 0)
        ( \s -> do
            close s
            removeIgnore path
        )
        $ \listenSock -> do
          bind listenSock (SockAddrUnix path)
          listen listenSock maxListenQueue
          forever $ do
            (clientSock, _addr) <- accept listenSock
            void $
              forkFinally
                (runOneUnixConn state sup clientSock)
                (\_ -> close clientSock)
  where
    removeIgnore p = removeFile p `catch` \(_ :: IOError) -> pure ()

-- | Plain (Unix-socket) connection: no TLS regardless of
-- 'ssTlsConfig'. Filesystem permissions on the socket file
-- remain authoritative for local-only access. Task records made
-- by this connection carry the @clientName@ @"local"@.
runOneUnixConn :: ServerState -> Supervisor -> Socket -> IO ()
runOneUnixConn state sup sock =
  runHandler state sup "local" (socketTransport sock defaultLimit)

-- | TCP connection: wrap with mTLS if 'ssTlsConfig' is set,
-- validate the peer CN, then hand the transport to 'handleConn'.
-- The @clientName@ stamped on this connection's task records is:
-- the @<name>@ suffix of the peer CN over TLS, or @"local"@ when
-- TLS is disabled or the CN can't be read.
runOneTcpConn :: ServerState -> Supervisor -> Socket -> SockAddr -> IO ()
runOneTcpConn state sup sock peerAddr =
  case ssTlsConfig state of
    Nothing ->
      runHandler state sup "local" (socketTransport sock defaultLimit)
    Just cfg -> do
      r <- tryWrap cfg
      case r of
        Left e -> do
          logTlsRejection "TLS handshake failed" peerAddr e
          close sock
        Right (ctx, ref) -> do
          v <- Tls.validatePeerCN cfg ref
          case v of
            Left msg -> do
              logTlsRejection "TLS peer rejected" peerAddr (T.unpack msg)
              Tls.closeTlsContext ctx
              close sock
            Right () -> do
              mCN <- Tls.readPeerCNRef ref
              let clientName = case mCN of
                    Just cn -> Tls.peerNameFromCN (Tls.tcExpectedPeerPrefix cfg) cn
                    Nothing -> "local"
              transport <- Tls.tlsTransport ctx defaultLimit
              runHandler state sup clientName transport
                `catch` \(e :: IOError) ->
                  hPutStrLn stderr $
                    "TCP/TLS RPC session error from "
                      <> show peerAddr
                      <> " (client="
                      <> T.unpack clientName
                      <> "): "
                      <> show e
              Tls.closeTlsContext ctx
  where
    tryWrap cfg =
      (Right <$> Tls.wrapServerSocket cfg sock)
        `catch` \(e :: IOError) -> pure (Left (show e))

logTlsRejection :: String -> SockAddr -> String -> IO ()
logTlsRejection what peerAddr why =
  TIO.hPutStrLn stderr $
    T.pack (what <> " from " <> show peerAddr <> ": " <> why)

-- | Hand a Cap'n Proto 'Transport' (TLS-wrapped or plain) to the
-- daemon's RPC machinery. Allocates a fresh 'Daemon' cap so each
-- client session has its own cap-graph rooted at the bootstrap.
-- The 'clientName' is propagated into every cap created off the
-- daemon cap so 'runAction' calls can stamp it on the task row.
runHandler :: ServerState -> Supervisor -> T.Text -> Transport -> IO ()
runHandler state sup clientName transport = do
  daemonCap <- newDaemonCap state sup clientName
  bootClient <- export @CGCorvus.Daemon sup daemonCap
  handleConn
    transport
    Def.def
      { debugMode = False
      , bootstrap = Just (toClient bootClient)
      }
