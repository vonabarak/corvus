{-# LANGUAGE TupleSections #-}

-- | Thread-safe, idempotently-closable wrapper around
-- "Corvus.Client.Connection"'s 'Connection' for the Python extension.
--
-- The underlying wire-format and socket code lives in the main library
-- (via 'openUnixConnection' / 'openTcpConnection'); this module only
-- adds:
--
--   * an 'MVar' so two Python threads sharing a 'Client' don't
--     interleave bytes on the socket, and
--   * an 'IORef' tracking close-state so repeated 'closeConn' calls
--     (from Python's explicit close + the 'PyCapsule' destructor) are
--     a no-op instead of a double-free on the underlying socket.
module Corvus.Python.Connection
  ( ConnHandle
  , openUnix
  , openTcp
  , closeConn
  , withConn
  )
where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Corvus.Client.Connection
  ( Connection (..)
  , ConnectionError
  , openTcpConnection
  , openUnixConnection
  )
import Data.IORef (IORef, atomicModifyIORef', newIORef)

-- | A persistent connection wrapped in an 'MVar' so concurrent callers
-- from Python don't interleave their bytes on the socket. The 'IORef'
-- tracks close-state so 'closeConn' is idempotent — defence against
-- Python double-closing (via both explicit close() and GC).
data ConnHandle = ConnHandle
  { chConn :: !Connection
  , chLock :: !(MVar ())
  , chClosed :: !(IORef Bool)
  }

-- | Open a Unix-socket connection. Left on failure.
openUnix :: FilePath -> IO (Either ConnectionError ConnHandle)
openUnix path = openUnixConnection path >>= wrap

-- | Open a TCP connection to @host:port@.
openTcp :: String -> Int -> IO (Either ConnectionError ConnHandle)
openTcp host port = openTcpConnection host port >>= wrap

-- | Wrap a freshly-opened 'Connection' with the MVar lock + close flag.
wrap :: Either ConnectionError Connection -> IO (Either ConnectionError ConnHandle)
wrap (Left e) = pure (Left e)
wrap (Right c) = do
  lock <- newMVar ()
  closed <- newIORef False
  pure
    ( Right
        ConnHandle
          { chConn = c
          , chLock = lock
          , chClosed = closed
          }
    )

-- | Close the connection. Idempotent — repeated calls are no-ops.
closeConn :: ConnHandle -> IO ()
closeConn h = do
  alreadyClosed <- atomicModifyIORef' (chClosed h) (True,)
  if alreadyClosed
    then pure ()
    else connClose (chConn h)

-- | Run an action against the connection under the lock.
withConn :: ConnHandle -> (Connection -> IO a) -> IO a
withConn h f = withMVar (chLock h) (\() -> f (chConn h))
