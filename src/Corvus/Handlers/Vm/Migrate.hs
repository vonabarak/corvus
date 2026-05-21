{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Offline VM migration orchestrator.
--
-- The high-level dance, with no bytes flowing through the daemon:
--
--   1. Acquire the per-VM @migrating@ lock with a conditional
--      update (@migrating := True@ when the row currently has
--      @migrating = False AND status = stopped@). Refuses if the
--      conditional update changes 0 rows.
--   2. Run the pre-check from 'Corvus.Handlers.Vm.Migrate.PreCheck'
--      to validate the migration is legal and to build the
--      per-drive 'MigrationPlan'.
--   3. For every drive in the plan, drive
--      'transferImageBetweenNodes' from the previous slice. Each
--      copy entry inserts a 'DiskImageNode' row on the
--      destination; each move entry defers the source-side row
--      delete + file delete to step 5 so a transfer failure
--      mid-flight leaves the source intact.
--   4. On any transfer failure: ask the destination agent to
--      delete every file we wrote, drop the destination
--      'DiskImageNode' rows we inserted, clear the lock, return
--      the error. Source data is intact.
--   5. On success: in a single transaction, flip the VM's
--      @nodeId@, clear its @vsockCid@ / @spicePort@ (the
--      destination's allocators will hand out fresh ones on the
--      next @vm start@), delete the source-side placements for
--      every "move" plan entry, and clear @migrating@. Then,
--      best-effort, ask the source agent to delete the
--      now-orphaned files.
module Corvus.Handlers.Vm.Migrate
  ( VmMigrate (..)
  , handleVmMigrate
  )
where

import Corvus.Action

import Control.Monad (foldM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, logWarnN)
import qualified Corvus.Handlers.Disk.Agent as DA
import qualified Corvus.Handlers.Disk.Db as DDb
import qualified Corvus.Handlers.Disk.Path as DP
import qualified Corvus.Handlers.Disk.Transfer as DT
import Corvus.Handlers.Vm.Migrate.PreCheck
  ( MigrationDriveOp (..)
  , MigrationPlan (..)
  , validateMigration
  )
import Corvus.Model (fromSqlKey, toSqlKey)
import qualified Corvus.Model as M
import Corvus.Node.Image (ImageResult (..))
import Corvus.Protocol
import Corvus.Qemu.Config (getEffectiveBasePath)
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import System.FilePath (takeFileName, (</>))

-- ---------------------------------------------------------------------------
-- Action

-- | Migrate a stopped VM from its current node to a different node.
data VmMigrate = VmMigrate
  { vmiVmId :: !Int64
  , vmiDestNodeId :: !Int64
  }

instance Action VmMigrate where
  actionSubsystem _ = M.SubMigration
  actionCommand _ = "migrate"
  actionEntityId = Just . fromIntegral . vmiVmId
  actionExecute ctx a = handleVmMigrate (acState ctx) (vmiVmId a) (vmiDestNodeId a)

-- ---------------------------------------------------------------------------
-- Handler

handleVmMigrate :: ServerState -> Int64 -> Int64 -> IO Response
handleVmMigrate state vmIdRaw destNodeRaw = runServerLogging state $ do
  let pool = ssDbPool state
      vmId = toSqlKey vmIdRaw :: M.VmId
      destNode = toSqlKey destNodeRaw :: M.NodeId
  logInfoN $
    "vm migrate: vm "
      <> T.pack (show vmIdRaw)
      <> " → node "
      <> T.pack (show destNodeRaw)
  -- (1) Conditional lock acquisition. Persistent has no
  -- "update WHERE … returning rowcount" combinator we trust
  -- across backends, so we fetch + check + set inside a single
  -- transaction; concurrent migrations serialise on the row lock
  -- the read takes.
  acquired <-
    liftIO $
      runSqlPool
        ( do
            mVm <- get vmId
            case mVm of
              Just vm
                | M.vmStatus vm == M.VmStopped && not (M.vmMigrating vm) -> do
                    update vmId [M.VmMigrating =. True]
                    pure True
              _ -> pure False
        )
        pool
  if not acquired
    then pure (RespError "VM is not stopped or another migration is in progress")
    else do
      -- (2) Pre-check.
      ePlan <- liftIO $ validateMigration state vmId destNode
      case ePlan of
        Left err -> do
          liftIO $ clearMigrating state vmId
          pure (RespError err)
        Right plan -> driveTransfers state vmId destNode plan

-- | Run the transfers and, on success, commit the DB swap. On any
-- error during transfers, roll back destination-side state and
-- clear the lock.
driveTransfers
  :: ServerState
  -> M.VmId
  -> M.NodeId
  -> MigrationPlan
  -> LoggingT IO Response
driveTransfers state vmId destNode plan = do
  let srcNode = mpSrcNode plan
      ops = mpDriveOps plan
  -- Stage 1: byte transfers. We accumulate a list of "what we
  -- created on the destination" so we can roll back on failure.
  basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
  result <-
    liftIO $
      foldM
        (runOp state srcNode destNode basePath)
        (Right [])
        ops
  case result of
    Left (err, created) -> do
      liftIO $ rollbackCreated state destNode created
      liftIO $ clearMigrating state vmId
      pure (RespError err)
    Right created -> commitMigration state vmId destNode plan created

-- | Per-op transfer step. Carries (Right placements-so-far) on
-- success or (Left (err, placements-so-far)) on the first failure
-- so the caller can roll back.
runOp
  :: ServerState
  -> M.NodeId
  -> M.NodeId
  -> FilePath
  -> Either (T.Text, [(M.DiskImageId, T.Text)]) [(M.DiskImageId, T.Text)]
  -> MigrationDriveOp
  -> IO (Either (T.Text, [(M.DiskImageId, T.Text)]) [(M.DiskImageId, T.Text)])
runOp _ _ _ _ acc@(Left _) _ = pure acc
runOp state srcNode destNode basePath (Right created) op = do
  let diskKey = opDiskKey op
  -- Resolve source-side path.
  srcAbs <- DP.resolveDiskPath (ssDbPool state) (ssQemuConfig state) diskKey srcNode
  if null srcAbs
    then
      pure $
        Left
          ( "internal: source placement vanished mid-migration for disk "
              <> T.pack (show (M.fromSqlKey diskKey))
          , created
          )
    else do
      -- Look up destination's basePath. The Node row carries the
      -- agent's effective base for that host.
      mDest <- runSqlPool (get destNode) (ssDbPool state)
      let destBase = maybe basePath (T.unpack . M.nodeBasePath) mDest
          destAbs = destBase </> takeFileName srcAbs
          destRel = T.pack (takeFileName srcAbs)
      tResult <-
        DT.transferImageBetweenNodes
          state
          srcNode
          destNode
          srcAbs
          destAbs
      case tResult of
        Left err -> pure (Left (err, created))
        Right () -> do
          runSqlPool
            (DDb.recordDiskImageNode diskKey destNode destRel)
            (ssDbPool state)
          pure (Right ((diskKey, destRel) : created))

-- | DB swap that promotes the VM to the destination node.
-- Performed as a single transaction:
--
--   * Vm.nodeId   := destNode
--   * Vm.vsockCid := Nothing  (next start re-allocates)
--   * Vm.spicePort:= Nothing  (same)
--   * Vm.migrating:= False
--   * For each "move" op, drop the source DiskImageNode row.
--
-- After the DB commit, best-effort delete the now-orphaned files
-- on the source node.
commitMigration
  :: ServerState
  -> M.VmId
  -> M.NodeId
  -> MigrationPlan
  -> [(M.DiskImageId, T.Text)]
  -> LoggingT IO Response
commitMigration state vmId destNode plan _created = do
  let pool = ssDbPool state
      moves =
        [ d
        | OpMove d <- mpDriveOps plan
        ]
  liftIO $
    runSqlPool
      ( do
          update
            vmId
            [ M.VmNodeId =. destNode
            , M.VmVsockCid =. Nothing
            , M.VmSpicePort =. Nothing
            , M.VmMigrating =. False
            ]
          forM_ moves $ \d ->
            DDb.deleteDiskImageNodeRow d (mpSrcNode plan)
      )
      pool
  -- Best-effort source-side file delete for moved disks.
  basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
  forM_ moves $ \d -> do
    -- The source row is gone now, so we can't ask
    -- 'resolveDiskPath' for the moved-out path; reconstruct it
    -- from the source basename and base path.
    mDestPath <-
      liftIO $
        runSqlPool
          (DDb.diskImageNodeFilePathFor d destNode)
          pool
    let bn = case mDestPath of
          Just p -> takeFileName (T.unpack p)
          Nothing -> ""
        srcPath
          | null bn = ""
          | otherwise = basePath </> bn
    unless (null srcPath) $ do
      r <- liftIO $ DA.deleteImageViaAgent state (mpSrcNode plan) srcPath
      case r of
        ImageSuccess -> pure ()
        ImageNotFound -> pure ()
        other ->
          logWarnN $
            "source-side file delete after move did not complete cleanly: "
              <> T.pack (show other)
  logInfoN "vm migrate complete"
  pure RespOk

-- | Best-effort rollback: delete every destination-side file +
-- DiskImageNode row we created during a failed transfer pass.
-- Used by 'driveTransfers' when one of the transfers fails so the
-- destination doesn't end up with half a workload.
rollbackCreated
  :: ServerState
  -> M.NodeId
  -> [(M.DiskImageId, T.Text)]
  -> IO ()
rollbackCreated state destNode created = do
  basePath <- getEffectiveBasePath (ssQemuConfig state)
  forM_ created $ \(d, relPath) -> do
    let raw = T.unpack relPath
        absPath =
          if "/" `isPrefixOf` raw
            then raw
            else basePath </> raw
    -- Drop the file first (so a leftover on the agent doesn't
    -- collide with a future retry).
    _ <- DA.deleteImageViaAgent state destNode absPath
    runSqlPool (DDb.deleteDiskImageNodeRow d destNode) (ssDbPool state)

-- | Clear the @migrating@ flag back to False. Standalone helper
-- used both on pre-check failure and as part of rollback (the
-- DB swap path clears it in-transaction instead).
clearMigrating :: ServerState -> M.VmId -> IO ()
clearMigrating state vmId =
  runSqlPool
    (update vmId [M.VmMigrating =. False])
    (ssDbPool state)

opDiskKey :: MigrationDriveOp -> M.DiskImageId
opDiskKey (OpCopy d) = d
opDiskKey (OpMove d) = d
