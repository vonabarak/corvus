{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Cap'n Proto RPC listener — the daemon's sole wire endpoint.
--
-- The Unix socket path is @\$XDG_RUNTIME_DIR\/corvus\/corvus.sock@
-- by default; a host\/port can be passed for TCP. Each accepted
-- connection gets its own 'handleConn' loop with the 'Daemon' cap
-- as the bootstrap interface.
module Corvus.Rpc.Server
  ( runCapnpServer
  , defaultCapnpSocketPath
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

-- | Default path for the Cap'n Proto Unix socket: the canonical
-- daemon socket in the XDG runtime dir.
defaultCapnpSocketPath :: IO FilePath
defaultCapnpSocketPath = getDefaultSocketPath

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
      TCP.serve (TCP.Host host) (show port) $ \(sock, _) ->
        runOneTcpConn state sup sock
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
-- remain authoritative for local-only access.
runOneUnixConn :: ServerState -> Supervisor -> Socket -> IO ()
runOneUnixConn state sup sock =
  runHandler state sup (socketTransport sock defaultLimit)

-- | TCP connection: wrap with mTLS if 'ssTlsConfig' is set,
-- validate the peer CN, then hand the transport to 'handleConn'.
runOneTcpConn :: ServerState -> Supervisor -> Socket -> IO ()
runOneTcpConn state sup sock =
  case ssTlsConfig state of
    Nothing ->
      runHandler state sup (socketTransport sock defaultLimit)
    Just cfg -> do
      r <- tryWrap cfg
      case r of
        Left e -> do
          logTlsRejection "TLS handshake failed" e
          close sock
        Right (ctx, ref) -> do
          v <- Tls.validatePeerCN cfg ref
          case v of
            Left msg -> do
              logTlsRejection "TLS peer rejected" (T.unpack msg)
              Tls.closeTlsContext ctx
              close sock
            Right () -> do
              transport <- Tls.tlsTransport ctx defaultLimit
              runHandler state sup transport
                `catch` \(e :: IOError) ->
                  hPutStrLn stderr ("TCP/TLS connection error: " <> show e)
              Tls.closeTlsContext ctx
  where
    tryWrap cfg =
      (Right <$> Tls.wrapServerSocket cfg sock)
        `catch` \(e :: IOError) -> pure (Left (show e))

logTlsRejection :: String -> String -> IO ()
logTlsRejection what why =
  TIO.hPutStrLn stderr $ T.pack (what <> ": " <> why)

-- | Hand a Cap'n Proto 'Transport' (TLS-wrapped or plain) to the
-- daemon's RPC machinery. Allocates a fresh 'Daemon' cap so each
-- client session has its own cap-graph rooted at the bootstrap.
runHandler :: ServerState -> Supervisor -> Transport -> IO ()
runHandler state sup transport = do
  daemonCap <- newDaemonCap state sup
  bootClient <- export @CGCorvus.Daemon sup daemonCap
  handleConn
    transport
    Def.def
      { debugMode = False
      , bootstrap = Just (toClient bootClient)
      }
