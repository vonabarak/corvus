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

import Control.Monad (foldM, forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, logWarnN)
import qualified Corvus.Handlers.Disk.Agent as DA
import qualified Corvus.Handlers.Disk.Db as DDb
import qualified Corvus.Handlers.Disk.Path as DP
import qualified Corvus.Handlers.Disk.Transfer as DT
import qualified Corvus.Handlers.Vm as HVm
import Corvus.Handlers.Vm.Migrate.PreCheck
  ( MigrationDriveOp (..)
  , MigrationPlan (..)
  , validateMigration
  )
import Corvus.Model (fromSqlKey, toSqlKey)
import qualified Corvus.Model as M
import Corvus.Node.Image (ImageResult (..))
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Protocol
import Corvus.Qemu.Config (getEffectiveBasePath)
import Corvus.Types (ServerState (..), runServerLogging, withNodeAgent)
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
  actionExecute ctx a = handleVmMigrate ctx (vmiVmId a) (vmiDestNodeId a)

-- ---------------------------------------------------------------------------
-- Handler

handleVmMigrate :: ActionContext -> Int64 -> Int64 -> IO Response
handleVmMigrate ctx vmIdRaw destNodeRaw = runServerLogging state $ do
  let vmId = toSqlKey vmIdRaw :: M.VmId
      destNode = toSqlKey destNodeRaw :: M.NodeId
  logInfoN $
    "vm migrate: vm "
      <> T.pack (show vmIdRaw)
      <> " → node "
      <> T.pack (show destNodeRaw)
  -- (1) Auto-save running/paused VMs as a child task. The save
  -- runs through the same VmSave Action 'crv vm save' uses, so
  -- the FSM gate + node-side migrate-quit-reap flow is shared.
  -- 'runActionAsSubtask' records the save as a child of the
  -- migrate task so 'crv task history' shows the full causality.
  saveOutcome <- liftIO (autoSaveIfLive ctx vmId)
  case saveOutcome of
    Left err -> pure (RespError err)
    Right () -> do
      -- (2) Conditional lock acquisition. Persistent has no
      -- "update WHERE … returning rowcount" combinator we trust
      -- across backends, so we fetch + check + set inside a single
      -- transaction; concurrent migrations serialise on the row
      -- lock the read takes. By this point the row's status is
      -- either 'VmStopped' (cold migrate) or 'VmSaved' (either
      -- pre-existing or just produced by the auto-save above).
      acquired <-
        liftIO $
          runSqlPool
            ( do
                mVm <- get vmId
                case mVm of
                  Just vm
                    | M.vmStatus vm `elem` [M.VmStopped, M.VmSaved]
                    , not (M.vmMigrating vm) -> do
                        update vmId [M.VmMigrating =. True]
                        pure True
                  _ -> pure False
            )
            pool
      if not acquired
        then
          pure
            ( RespError
                "VM must be stopped or saved (running/paused are auto-saved); another migration may be in progress"
            )
        else do
          -- (3) Pre-check.
          ePlan <- liftIO $ validateMigration state vmId destNode
          case ePlan of
            Left err -> do
              liftIO $ clearMigrating state vmId
              pure (RespError err)
            Right plan -> driveTransfers state vmId destNode plan
  where
    state = acState ctx
    pool = ssDbPool state

-- | If the VM row is currently 'VmRunning' or 'VmPaused', invoke
-- 'VmSave' as a child task so the existing save path (FSM gate,
-- node-side migrate + quit + reap, DB pre-commit + race fix) does
-- the work. Returns 'Right ()' when the row is now in a
-- migrate-eligible state ('VmStopped' or 'VmSaved'), or 'Left'
-- with a diagnostic when the save itself failed.
--
-- Stopped and saved rows fall through without touching anything.
-- A row in @starting@ / @stopping@ / @error@ is left alone here —
-- the subsequent lock-acquire produces the proper "not in an
-- eligible status" rejection.
autoSaveIfLive :: ActionContext -> M.VmId -> IO (Either T.Text ())
autoSaveIfLive ctx vmId = do
  let state = acState ctx
  mVm <- runSqlPool (get vmId) (ssDbPool state)
  case mVm of
    Just vm | M.vmStatus vm `elem` [M.VmRunning, M.VmPaused] -> do
      resp <- runActionAsSubtask ctx (HVm.VmSave (fromSqlKey vmId))
      pure $ case resp of
        RespVmStateChanged M.VmSaved -> Right ()
        RespError msg -> Left ("auto-save before migrate failed: " <> msg)
        RespInvalidTransition curr msg ->
          Left
            ( "auto-save before migrate refused (current status "
                <> M.enumToText curr
                <> "): "
                <> msg
            )
        other -> Left ("auto-save before migrate returned unexpected response: " <> T.pack (show other))
    _ -> pure (Right ())

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
  -- Stage 1: per-drive byte transfers. We accumulate a list of
  -- "what we created on the destination" so we can roll back on
  -- failure.
  basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
  result <-
    liftIO $
      foldM
        (runOp state srcNode destNode basePath)
        (Right [])
        ops
  case result of
    Left (err, created) -> do
      liftIO $ rollbackCreated state plan destNode created
      liftIO $ clearMigrating state vmId
      pure (RespError err)
    Right created -> do
      -- Stage 2: state-file transfer for saved VMs. The PreCheck
      -- set 'mpStateFile' iff the source row was 'VmSaved' (either
      -- pre-existing or produced by the auto-save in
      -- 'handleVmMigrate'). Disks are already on the destination
      -- at this point — a failure here rolls back everything via
      -- the same path the disk-transfer failure uses.
      if mpStateFile plan
        then do
          stateRes <- liftIO $ transferStateFile state plan basePath
          case stateRes of
            Left err -> do
              liftIO $ rollbackCreated state plan destNode created
              liftIO $ clearMigrating state vmId
              pure (RespError err)
            Right () -> commitMigration state vmId destNode plan created
        else commitMigration state vmId destNode plan created

-- | Move @\<basePath\>\/\<vmName\>\/state.qemu.zst@ from the source
-- node to the destination, reusing the same agent-to-agent byte
-- path the disk transfers use. The destination's
-- @diskImportFromPeer@ @mkdir -p@s the parent before writing.
--
-- Filename must stay in lockstep with the agent's
-- 'Corvus.Node.Runtime.getSavedStateFile' — the daemon recomputes
-- the path here rather than asking the agent, so a rename has to
-- touch both sites.
transferStateFile :: ServerState -> MigrationPlan -> FilePath -> IO (Either T.Text ())
transferStateFile state plan daemonBase = do
  let pool = ssDbPool state
      srcNode = mpSrcNode plan
      destNode = mpDestNode plan
  mVm <- runSqlPool (get (mpVmId plan)) pool
  case mVm of
    Nothing -> pure (Left "internal: VM row vanished mid-state-transfer")
    Just vm -> do
      mSrc <- runSqlPool (get srcNode) pool
      mDest <- runSqlPool (get destNode) pool
      let srcBase = maybe daemonBase (T.unpack . M.nodeBasePath) mSrc
          destBase = maybe daemonBase (T.unpack . M.nodeBasePath) mDest
          name = T.unpack (M.vmName vm)
          srcPath = srcBase </> name </> "state.qemu.zst"
          destPath = destBase </> name </> "state.qemu.zst"
      tResult <-
        DT.transferImageBetweenNodes
          state
          srcNode
          destNode
          srcPath
          destPath
      pure $ case tResult of
        Left err -> Left ("state-file transfer: " <> err)
        Right () -> Right ()

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
  -- Resolve source-side absolute path AND the source's stored
  -- (relative-or-absolute) DiskImageNode.filePath. The latter is
  -- what carries any subdirectory layout (e.g. baked artifacts
  -- placed under a target-specific @templates/<name>/@ subdir);
  -- using @takeFileName srcAbs@ would flatten those paths and
  -- QEMU on the destination would fail to find the images.
  srcAbs <- DP.resolveDiskPath (ssDbPool state) (ssQemuConfig state) diskKey srcNode
  mSrcStored <-
    runSqlPool (DDb.diskImageNodeFilePathFor diskKey srcNode) (ssDbPool state)
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
          -- Preserve the source's relative subdirectory. If the
          -- source's stored path is absolute (off-base imported
          -- disk), an arbitrary absolute path is rarely writable
          -- on a different node, so fall back to placing it under
          -- destBase with the source basename.
          (destAbs, destRel) = case mSrcStored of
            Just stored
              | not ("/" `T.isPrefixOf` stored) ->
                  (destBase </> T.unpack stored, stored)
            _ ->
              let bn = takeFileName srcAbs
               in (destBase </> bn, T.pack bn)
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
commitMigration state vmId destNode plan created = do
  let pool = ssDbPool state
      moves =
        [ d
        | OpMove d <- mpDriveOps plan
        ]
      -- Stored (relative-or-absolute) paths captured by 'runOp'
      -- mirror what was on the source's DiskImageNode row. Use
      -- them to construct the source-side delete paths; the
      -- source row itself is deleted in the same transaction
      -- below, so we can't go back and read it.
      storedByDisk = created
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
  daemonBase <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
  mSrcRow <- liftIO $ runSqlPool (get (mpSrcNode plan)) pool
  let srcBase = maybe daemonBase (T.unpack . M.nodeBasePath) mSrcRow
  forM_ moves $ \d -> do
    let srcPath = case lookup d storedByDisk of
          Just stored ->
            let raw = T.unpack stored
             in if "/" `isPrefixOf` raw
                  then raw
                  else srcBase </> raw
          Nothing -> ""
    unless (null srcPath) $ do
      r <- liftIO $ DA.deleteImageViaAgent state (mpSrcNode plan) srcPath
      case r of
        ImageSuccess -> pure ()
        ImageNotFound -> pure ()
        other ->
          logWarnN $
            "source-side file delete after move did not complete cleanly: "
              <> T.pack (show other)
  -- Best-effort source-side state-file delete for saved VMs. The
  -- destination already has its own copy from the state-file
  -- transfer step; leaving the source's around would be a long-
  -- lived leak across migrations of the same VM. Idempotent on
  -- the agent, so unreachable source is harmless (a leftover file
  -- gets reaped on the next migrate or on agent cleanup).
  when (mpStateFile plan) $ do
    mVm <- liftIO $ runSqlPool (get vmId) pool
    forM_ mVm $ \vm -> do
      delR <-
        liftIO $
          deleteSavedStateOnNode state (mpSrcNode plan) (M.vmName vm)
      case delR of
        Right () -> pure ()
        Left err ->
          logWarnN $
            "source-side state-file delete after migrate did not complete: "
              <> err
  logInfoN "vm migrate complete"
  pure RespOk

-- | Best-effort rollback: delete every destination-side file +
-- DiskImageNode row we created during a failed transfer pass, and
-- (for saved VMs) drop the destination state file if it landed
-- before the failure. Used by 'driveTransfers' when one of the
-- transfers fails so the destination doesn't end up with half a
-- workload.
rollbackCreated
  :: ServerState
  -> MigrationPlan
  -> M.NodeId
  -> [(M.DiskImageId, T.Text)]
  -> IO ()
rollbackCreated state plan destNode created = do
  daemonBase <- getEffectiveBasePath (ssQemuConfig state)
  mDestRow <- runSqlPool (get destNode) (ssDbPool state)
  let destBase = maybe daemonBase (T.unpack . M.nodeBasePath) mDestRow
  forM_ created $ \(d, relPath) -> do
    let raw = T.unpack relPath
        absPath =
          if "/" `isPrefixOf` raw
            then raw
            else destBase </> raw
    -- Drop the file first (so a leftover on the agent doesn't
    -- collide with a future retry).
    _ <- DA.deleteImageViaAgent state destNode absPath
    runSqlPool (DDb.deleteDiskImageNodeRow d destNode) (ssDbPool state)
  -- For saved-VM migrations, the state-file transfer step may have
  -- already written the destination's state.qemu before another
  -- step failed. 'deleteSavedState' is idempotent on the agent
  -- (a missing file is success), so unconditional invocation when
  -- 'mpStateFile' is set is safe even if the state-file step never
  -- ran.
  when (mpStateFile plan) $ do
    mVm <- runSqlPool (get (mpVmId plan)) (ssDbPool state)
    forM_ mVm $ \vm ->
      void $ deleteSavedStateOnNode state destNode (M.vmName vm)

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

-- | Ask the named node's agent to unlink the saved-state file for
-- @vmName@. Idempotent on the agent (missing file is success); the
-- daemon-side caller can ignore the result. Returns 'Left' iff the
-- agent itself is unreachable, so callers that care can log it.
deleteSavedStateOnNode
  :: ServerState
  -> M.NodeId
  -> T.Text
  -> IO (Either T.Text ())
deleteSavedStateOnNode state nid vmName = do
  r <- withNodeAgent state nid (`NOA.deleteSavedState` vmName)
  pure $ case r of
    Left err -> Left err
    Right (Left e) -> Left (T.pack (show e))
    Right (Right ()) -> Right ()
