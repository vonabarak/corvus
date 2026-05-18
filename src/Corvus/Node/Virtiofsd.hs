{-# LANGUAGE OverloadedStrings #-}

-- | Virtiofsd process management — daemon-side facade.
--
-- Phase 3: virtiofsd subprocesses are spawned and reaped by
-- @corvus-nodeagent@. This module marshals each shared-dir
-- intent (which binary, which argv, where the socket should land)
-- and routes through 'Corvus.NodeAgentClient.processSpawnVirtiofsd'
-- / 'Corvus.NodeAgentClient.processStop'. The daemon never spawns
-- or signals virtiofsd directly.
module Corvus.Node.Virtiofsd
  ( -- * Starting virtiofsd
    startVirtiofsdProcesses
  , VirtiofsdResult (..)

    -- * Socket paths
  , getVirtiofsdSocket

    -- * Killing virtiofsd
  , killVirtiofsdProcesses
  )
where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugN, logInfoN, logWarnN)
import Corvus.Model
import Corvus.Node.Runtime (getVmRuntimeDir)
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Qemu.Config (QemuConfig (..))
import Corvus.Types (ServerState (..))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlPersistT, toSqlKey)
import System.FilePath ((</>))

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

-- | Spawn virtiofsd via the agent for every shared directory of
-- the VM that doesn't already have a PID recorded.
--
-- Returns 'VirtiofsdSomeFailed' (rather than throwing) if the
-- agent is currently unreachable; callers treat this the same as
-- "a process failed to start" and roll back the VM start.
startVirtiofsdProcesses
  :: (MonadIO m, MonadLogger m)
  => ServerState
  -> Int64
  -> m VirtiofsdResult
startVirtiofsdProcesses state vmId = do
  let pool = ssDbPool state
      config = ssQemuConfig state

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

      mAgent <- liftIO $ readTVarIO (ssNodeAgent state)
      case mAgent of
        Nothing -> do
          logWarnN "nodeagent unavailable; cannot start virtiofsd"
          pure VirtiofsdSomeFailed
        Just nac -> do
          results <- mapM (startVirtiofsdForDir state nac vmId) sharedDirs
          if and results
            then pure VirtiofsdAllStarted
            else pure VirtiofsdSomeFailed

-- | Start virtiofsd for a single shared directory via the agent.
startVirtiofsdForDir
  :: (MonadIO m, MonadLogger m)
  => ServerState
  -> NOA.NodeAgentClient
  -> Int64
  -> Entity SharedDir
  -> m Bool
startVirtiofsdForDir state nac vmId (Entity dirKey dir) =
  case sharedDirPid dir of
    Just pid -> do
      logDebugN $
        "Virtiofsd for tag '"
          <> sharedDirTag dir
          <> "' already running with PID "
          <> T.pack (show pid)
      pure True
    Nothing -> do
      let config = ssQemuConfig state
          pool = ssDbPool state
      socketPath <- liftIO $ getVirtiofsdSocket config vmId (sharedDirTag dir)

      -- Build command. The `--readonly` flag (modern Rust
      -- virtiofsd) tells the daemon to refuse every write op at the
      -- FUSE layer regardless of guest mount flags.
      let binary = qcVirtiofsdBinary config
          cacheArg = T.unpack $ enumToText (sharedDirCache dir)
          baseArgs =
            [ "--socket-path=" ++ socketPath
            , "--shared-dir=" ++ T.unpack (sharedDirPath dir)
            , "--cache=" ++ cacheArg
            , "--sandbox=none"
            ]
          args
            | sharedDirReadOnly dir = baseArgs ++ ["--readonly"]
            | otherwise = baseArgs

      logDebugN $
        "Starting virtiofsd for tag '"
          <> sharedDirTag dir
          <> "' via nodeagent: "
          <> T.pack (unwords (binary : args))

      r <-
        liftIO $
          NOA.processSpawnVirtiofsd
            nac
            (T.pack binary)
            (map T.pack args)
            (T.pack socketPath)
            5000
      case r of
        Left e -> do
          logWarnN $
            "virtiofsd spawn RPC failed for tag '"
              <> sharedDirTag dir
              <> "': "
              <> T.pack (show e)
          pure False
        Right (NOA.VirtiofsdSpawned pidW) -> do
          liftIO $
            runSqlPool
              (saveDirPid dirKey (fromIntegral (pidW :: Word32)))
              pool
          pure True
        Right (NOA.VirtiofsdSpawnFailed err) -> do
          logWarnN $
            "virtiofsd spawn failed for tag '"
              <> sharedDirTag dir
              <> "': "
              <> err
          pure False
        Right (NOA.VirtiofsdSocketTimeout _) -> do
          logWarnN $
            "Virtiofsd socket did not appear for tag '"
              <> sharedDirTag dir
              <> "'"
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

-- | Stop all virtiofsd processes for a VM by asking the agent to
-- SIGTERM-then-SIGKILL each recorded PID.
--
-- If the agent is currently unreachable we log a warning and
-- still clear the DB PIDs so the next start spawns fresh
-- processes — the agent's own startup cleanup will eventually
-- reap any genuinely-orphaned ones.
killVirtiofsdProcesses
  :: (MonadIO m, MonadLogger m)
  => ServerState
  -> Int64
  -> m ()
killVirtiofsdProcesses state vmId = do
  let pool = ssDbPool state
  sharedDirs <- liftIO $ runSqlPool (getSharedDirsForVm vmId) pool
  mAgent <- liftIO $ readTVarIO (ssNodeAgent state)
  forM_ sharedDirs $ \(Entity dirKey dir) ->
    case sharedDirPid dir of
      Nothing -> pure ()
      Just pid -> do
        case mAgent of
          Nothing ->
            logWarnN $
              "nodeagent unavailable; not stopping virtiofsd tag '"
                <> sharedDirTag dir
                <> "' pid="
                <> T.pack (show pid)
          Just nac -> do
            r <- liftIO $ NOA.processStop nac (fromIntegral pid) 3
            case r of
              Left e ->
                logWarnN $
                  "processStop RPC failed for virtiofsd tag '"
                    <> sharedDirTag dir
                    <> "': "
                    <> T.pack (show e)
              Right _ -> pure ()
        -- Clear PID from database regardless of stop outcome.
        liftIO $ runSqlPool (clearDirPid dirKey) pool
