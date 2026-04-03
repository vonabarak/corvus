{-# LANGUAGE OverloadedStrings #-}

-- | FFI bindings for unprivileged network namespace management.
-- Uses Linux user namespaces — no root or doas required.
--
-- The C code forks internally to create a single-threaded child process
-- that is safe for unshare(CLONE_NEWUSER). The child creates a
-- user+network+UTS namespace and keeps it alive.
--
-- Commands are executed inside the namespace via 'nsExec', which
-- enters the namespace via setns(), sets ambient capabilities
-- (CAP_NET_ADMIN, etc.), then exec's the command.
module Corvus.Qemu.Netns
  ( startNamespace
  , nsExec
  , nsSpawn
  , nsCreateTap
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.String (CString, newCString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (withArray0)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import System.Posix.Types (CPid (..), ProcessID)

foreign import ccall safe "corvus_netns_start"
  c_corvus_netns_start
    :: Ptr CPid
    -> IO CInt

foreign import ccall safe "corvus_netns_exec"
  c_corvus_netns_exec
    :: CPid
    -> Ptr CString
    -> IO CInt

foreign import ccall safe "corvus_netns_spawn"
  c_corvus_netns_spawn
    :: CPid
    -> Ptr CString
    -> Ptr CPid
    -> IO CInt

foreign import ccall safe "corvus_netns_create_tap"
  c_corvus_netns_create_tap
    :: CPid
    -> CString
    -> Ptr CInt
    -> IO CInt

-- | Start a namespace manager process that creates an unprivileged
-- user+network+UTS namespace and keeps it alive.
--
-- Returns the PID of the namespace manager process.
-- Send SIGTERM to the returned PID to tear down the namespace.
startNamespace :: IO (Either Text ProcessID)
startNamespace =
  alloca $ \pidPtr -> do
    rv <- c_corvus_netns_start pidPtr
    if rv == 0
      then do
        (CPid pid) <- peek pidPtr
        pure $ Right (fromIntegral pid)
      else pure $ Left "Namespace manager child failed to start"

-- | Execute a command inside the namespace with network capabilities.
-- Forks a child that enters the namespace, sets ambient capabilities
-- (CAP_NET_ADMIN, etc.), then exec's the command.
-- Returns Right () on success (exit code 0), Left errorMsg on failure.
nsExec :: Int -> [String] -> IO (Either Text ())
nsExec nsPid args = do
  let cPid = fromIntegral nsPid :: CPid
  -- Allocate C strings (must stay alive during the FFI call)
  cArgs <- mapM newCString args
  rv <- withArray0 nullPtr cArgs $ \argsPtr ->
    c_corvus_netns_exec cPid argsPtr
  -- Free allocated C strings
  mapM_ free cArgs
  if rv == 0
    then pure $ Right ()
    else pure $ Left $ "Command failed (exit " <> T.pack (show rv) <> "): " <> T.pack (unwords args)

-- | Spawn a long-running process inside the namespace with capabilities.
-- Like 'nsExec' but does not wait for the child — returns its PID immediately.
-- Used for dnsmasq and QEMU.
nsSpawn :: Int -> [String] -> IO (Either Text Int)
nsSpawn nsPid args = do
  let cPid = fromIntegral nsPid :: CPid
  cArgs <- mapM newCString args
  result <- alloca $ \outPidPtr ->
    withArray0 nullPtr cArgs $ \argsPtr -> do
      rv <- c_corvus_netns_spawn cPid argsPtr outPidPtr
      if rv == 0
        then do
          (CPid childPid) <- peek outPidPtr
          pure $ Right (fromIntegral childPid)
        else pure $ Left $ "Failed to spawn: " <> T.pack (unwords args)
  mapM_ free cArgs
  pure result

-- | Create a TAP device inside the namespace and add it to a bridge.
-- Returns the TAP file descriptor. The fd works across namespace boundaries —
-- QEMU can use it from the host namespace with @-netdev tap,fd=\<N\>@.
-- The caller is responsible for closing the fd when done.
nsCreateTap :: Int -> String -> IO (Either Text Int)
nsCreateTap nsPid bridgeName =
  withCString bridgeName $ \cBridge ->
    alloca $ \fdPtr -> do
      rv <- c_corvus_netns_create_tap (fromIntegral nsPid) cBridge fdPtr
      if rv == 0
        then do
          fd <- peek fdPtr
          pure $ Right (fromIntegral fd)
        else pure $ Left $ "Failed to create TAP on bridge " <> T.pack bridgeName
