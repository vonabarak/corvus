{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Virtiofsd process management.
-- Handles starting and killing virtiofsd processes for shared directories.
module Corvus.Qemu.Virtiofsd
  ( -- * Starting virtiofsd
    startVirtiofsdProcesses
  , VirtiofsdResult (..)

    -- * Socket paths
  , getVirtiofsdSocket

    -- * Killing virtiofsd
  , killVirtiofsdProcesses
  )
where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugN, logInfoN, logWarnN)
import Corvus.Model
import Corvus.Process (StopResult (..), stopProcess, waitForSocketFile)
import Corvus.Qemu.Config (QemuConfig (..))
import Corvus.Qemu.Runtime (createVmRuntimeDir, getVmRuntimeDir)
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend, SqlPersistT, toSqlKey)
import System.FilePath ((</>))
import System.Process (StdStream (..), createProcess, getPid, proc, std_err, std_out, waitForProcess)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Result of starting virtiofsd processes
data VirtiofsdResult
  = -- | All processes started successfully
    VirtiofsdAllStarted
  | -- | Some processes failed to start
    VirtiofsdSomeFailed
  | -- | No shared directories configured
    VirtiofsdNoSharedDirs
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Socket Paths
--------------------------------------------------------------------------------

-- | Get path to virtiofsd socket for a shared directory
getVirtiofsdSocket :: QemuConfig -> Int64 -> Text -> IO FilePath
getVirtiofsdSocket config vmId tag = do
  vmDir <- getVmRuntimeDir config vmId
  pure $ vmDir </> "virtiofsd-" ++ T.unpack tag ++ ".sock"

--------------------------------------------------------------------------------
-- Starting Virtiofsd
--------------------------------------------------------------------------------

-- | Start virtiofsd processes for all shared directories of a VM
-- Only starts processes that don't already have a PID in the database
startVirtiofsdProcesses
  :: (MonadIO m, MonadLogger m)
  => Pool SqlBackend
  -> QemuConfig
  -> Int64
  -> m VirtiofsdResult
startVirtiofsdProcesses pool config vmId = do
  -- Ensure runtime directory exists
  _ <- liftIO $ createVmRuntimeDir config vmId

  -- Get shared directories for this VM
  sharedDirs <- liftIO $ runSqlPool (getSharedDirsForVm vmId) pool

  if null sharedDirs
    then do
      logDebugN $ "VM " <> T.pack (show vmId) <> " has no shared directories"
      pure VirtiofsdNoSharedDirs
    else do
      logInfoN $
        "Starting virtiofsd for "
          <> T.pack (show $ length sharedDirs)
          <> " shared directories for VM "
          <> T.pack (show vmId)

      -- Start a process for each shared directory without a PID
      results <- mapM (startVirtiofsdForDir pool config vmId) sharedDirs

      if and results
        then pure VirtiofsdAllStarted
        else pure VirtiofsdSomeFailed

-- | Start virtiofsd for a single shared directory
startVirtiofsdForDir
  :: (MonadIO m, MonadLogger m)
  => Pool SqlBackend
  -> QemuConfig
  -> Int64
  -> Entity SharedDir
  -> m Bool
startVirtiofsdForDir pool config vmId (Entity dirKey dir) = do
  -- Check if already running
  case sharedDirPid dir of
    Just pid -> do
      logDebugN $
        "Virtiofsd for tag '"
          <> sharedDirTag dir
          <> "' already running with PID "
          <> T.pack (show pid)
      pure True
    Nothing -> do
      -- Get socket path
      socketPath <- liftIO $ getVirtiofsdSocket config vmId (sharedDirTag dir)

      -- Build command
      let binary = qcVirtiofsdBinary config
          cacheArg = T.unpack $ enumToText (sharedDirCache dir)
          args =
            [ "--socket-path=" ++ socketPath
            , "--shared-dir=" ++ T.unpack (sharedDirPath dir)
            , "--cache=" ++ cacheArg
            , "--sandbox=none"
            ]

      logDebugN $
        "Starting virtiofsd for tag '"
          <> sharedDirTag dir
          <> "': "
          <> T.pack (unwords (binary : args))

      -- Fork a thread to run the process and wait for it to exit
      _ <- liftIO $ forkIO $ do
        result <-
          try $
            createProcess
              (proc binary args)
                { std_out = CreatePipe
                , std_err = CreatePipe
                }
        case result of
          Left (_ :: SomeException) -> do
            -- Process failed to start
            pure ()
          Right (_, _, _, ph) -> do
            mPid <- getPid ph
            case mPid of
              Just pid -> do
                -- Save PID to database
                runSqlPool (saveDirPid dirKey (fromIntegral pid)) pool
                -- Wait for process to exit (prevents zombie)
                _ <- waitForProcess ph
                -- Clear PID from database when process exits
                runSqlPool (clearDirPid dirKey) pool
              Nothing -> pure ()

      -- Wait for virtiofsd to create its socket (up to 5 seconds)
      socketReady <- liftIO $ waitForSocketFile socketPath 5000
      if socketReady
        then pure True
        else do
          logWarnN $ "Virtiofsd socket did not appear for tag '" <> sharedDirTag dir <> "'"
          pure False

-- | Save virtiofsd PID to database
saveDirPid :: Key SharedDir -> Int -> SqlPersistT IO ()
saveDirPid key pid = update key [SharedDirPid =. Just pid]

-- | Clear virtiofsd PID from database
clearDirPid :: Key SharedDir -> SqlPersistT IO ()
clearDirPid key = update key [SharedDirPid =. Nothing]

-- | Get shared directories for a VM
getSharedDirsForVm :: Int64 -> SqlPersistT IO [Entity SharedDir]
getSharedDirsForVm vmId = do
  let key = toSqlKey vmId :: VmId
  selectList [SharedDirVmId ==. key] []

--------------------------------------------------------------------------------
-- Killing Virtiofsd
--------------------------------------------------------------------------------

-- | Stop all virtiofsd processes for a VM.
--
-- Virtiofsd has no control channel, so we go straight to @SIGTERM@ (which
-- it handles cleanly) and fall back to @SIGKILL@ if it's still alive after
-- 3 seconds.
killVirtiofsdProcesses
  :: (MonadIO m, MonadLogger m)
  => Pool SqlBackend
  -> Int64
  -> m ()
killVirtiofsdProcesses pool vmId = do
  sharedDirs <- liftIO $ runSqlPool (getSharedDirsForVm vmId) pool

  forM_ sharedDirs $ \(Entity dirKey dir) -> do
    case sharedDirPid dir of
      Nothing -> pure ()
      Just pid -> do
        let name = "virtiofsd(tag=" <> sharedDirTag dir <> ")"
        _ <- stopProcess name (fromIntegral pid) Nothing 0 3
        -- Clear PID from database regardless of StopResult: even a failed
        -- stop leaves nothing useful to track, and on next spawn we'd
        -- create a new process anyway.
        liftIO $ runSqlPool (clearDirPid dirKey) pool
