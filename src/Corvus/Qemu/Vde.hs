{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | VDE switch process management.
-- Handles starting and stopping vde_switch processes for virtual networks.
module Corvus.Qemu.Vde
  ( startVdeSwitch
  , stopVdeSwitch
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Corvus.Model (Network (..))
import qualified Corvus.Model as M
import Corvus.Qemu.Config (QemuConfig (..))
import Corvus.Qemu.Runtime (createNetworkRuntimeDir, getVdeSwitchSocket)
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend, toSqlKey)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.IO (hClose)
import System.Posix.Signals (sigTERM, signalProcess)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getPid, proc, waitForProcess)

-- | Start a vde_switch process for a network.
-- Stores PID in database and forks a background thread to monitor process exit.
startVdeSwitch :: QemuConfig -> Pool SqlBackend -> Int64 -> IO (Either Text ())
startVdeSwitch config pool networkId = do
  let key = toSqlKey networkId :: M.NetworkId

  -- Create runtime directory
  _ <- createNetworkRuntimeDir networkId
  socketPath <- getVdeSwitchSocket networkId

  -- Spawn vde_switch
  let binary = qcVdeSwitchBinary config
      args = ["--sock", socketPath]
      cp =
        (proc binary args)
          { std_in = CreatePipe -- Keep stdin open so vde_switch doesn't exit on EOF
          , std_out = CreatePipe
          , std_err = CreatePipe
          , create_group = True
          }

  result <- try $ createProcess cp
  case result of
    Left (err :: SomeException) ->
      pure $ Left $ "Failed to start vde_switch: " <> T.pack (show err)
    Right (mStdin, _, _, ph) -> do
      mPid <- getPid ph
      case mPid of
        Nothing -> pure $ Left "Failed to get PID of vde_switch process"
        Just pid -> do
          -- Store PID in database
          runSqlPool (update key [M.NetworkPid =. Just (fromIntegral pid)]) pool

          -- Fork background thread to monitor process exit.
          -- This thread also holds a reference to the stdin handle,
          -- preventing it from being GC'd and closed (which would cause
          -- vde_switch to exit with "EOF on stdin").
          _ <- forkIO $ do
            _ <- waitForProcess ph
            -- Close stdin handle now that the process has exited
            forM_ mStdin hClose
            -- Clear PID when process exits
            runSqlPool (update key [M.NetworkPid =. Nothing]) pool

          -- Wait for socket to become available
          waitForSocket socketPath

          pure $ Right ()

-- | Stop a vde_switch process for a network.
-- Sends SIGTERM and clears PID from database.
stopVdeSwitch :: Pool SqlBackend -> Int64 -> IO (Either Text ())
stopVdeSwitch pool networkId = do
  let key = toSqlKey networkId :: M.NetworkId
  mNetwork <- runSqlPool (get key) pool
  case mNetwork of
    Nothing -> pure $ Left "Network not found"
    Just network -> case networkPid network of
      Nothing -> pure $ Left "Network is not running"
      Just pid -> do
        -- Send SIGTERM
        result <- try $ signalProcess sigTERM (fromIntegral pid)
        case result of
          Left (err :: SomeException) -> do
            -- Process might already be dead — clear PID anyway
            runSqlPool (update key [M.NetworkPid =. Nothing]) pool
            pure $ Left $ "Error stopping vde_switch: " <> T.pack (show err)
          Right () -> do
            -- Clear PID
            runSqlPool (update key [M.NetworkPid =. Nothing]) pool
            -- Clean up runtime directory
            cleanupNetworkRuntime networkId
            pure $ Right ()

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Wait for the vde_switch control directory to appear (up to 5 seconds).
-- vde_switch creates a directory at the socket path containing ctl and data sockets.
waitForSocket :: FilePath -> IO ()
waitForSocket path = go 50
  where
    go :: Int -> IO ()
    go 0 = pure () -- Give up silently
    go n = do
      exists <- doesDirectoryExist path
      if exists
        then pure ()
        else do
          threadDelay 100000 -- 100ms
          go (n - 1)

-- | Clean up network runtime directory
cleanupNetworkRuntime :: Int64 -> IO ()
cleanupNetworkRuntime networkId = do
  socketPath <- getVdeSwitchSocket networkId
  -- vde_switch creates a directory at the socket path
  result <- try $ removeDirectoryRecursive socketPath
  case result of
    Left (_ :: SomeException) -> pure ()
    Right () -> pure ()
