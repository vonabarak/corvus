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
  , handleConn
  , socketTransport
  , toClient
  )
import Capnp.TraversalLimit (defaultLimit)
import Control.Concurrent (forkFinally)
import Control.Exception (bracket, catch)
import Control.Monad (forever, void)
import Corvus.Rpc.Daemon (newDaemonCap)
import Corvus.Types (ListenAddress (..), ServerState, getDefaultSocketPath)
import qualified Data.Default as Def
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
import System.IO.Error (IOError)

-- | Default path for the Cap'n Proto Unix socket: the canonical
-- daemon socket in the XDG runtime dir.
defaultCapnpSocketPath :: IO FilePath
defaultCapnpSocketPath = getDefaultSocketPath

-- | Run the Cap'n Proto RPC server on the given address.
-- Blocks the calling thread; spawns a fresh handler per connection.
runCapnpServer :: ServerState -> ListenAddress -> IO ()
runCapnpServer state addr = withSupervisor $ \sup ->
  case addr of
    TcpAddress host port ->
      TCP.serve (TCP.Host host) (show port) $ \(sock, _) ->
        runOneConn state sup sock
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
                (runOneConn state sup clientSock)
                (\_ -> close clientSock)
  where
    removeIgnore p = removeFile p `catch` \(_ :: IOError) -> pure ()

-- | Handle a single accepted connection. Allocates a fresh 'Daemon'
-- cap so each client session has its own cap-graph rooted at the
-- bootstrap.
runOneConn :: ServerState -> Supervisor -> Socket -> IO ()
runOneConn state sup sock = do
  daemonCap <- newDaemonCap state sup
  bootClient <- export @CGCorvus.Daemon sup daemonCap
  handleConn
    (socketTransport sock defaultLimit)
    Def.def
      { debugMode = False
      , bootstrap = Just (toClient bootClient)
      }
