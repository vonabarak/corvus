{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cache resume lifecycle for the build pipeline.
--
-- Handles taking a bake VM left behind by a prior @--build-cache@ run,
-- stopping it, rolling back to a cached step (disk-mode) or resuming
-- from vmstate (memory-mode), and pushing the cleanup destructor.
module Corvus.Handlers.Build.CacheResume
  ( runFromCachedBakeVm
  , resumeMemoryCacheBakeVm
  , resolveCachedArtifactDiskId
  , strategyCacheRoles
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, logWarnN)
import Corvus.Action (mkActionContext, runActionAsSubtask)
import qualified Corvus.Build.Cache.Hash as H
import qualified Corvus.Build.Cache.Store as CStore
import Corvus.Handlers.Build.Cache (CacheDisk (..), pruneCacheTail, rollbackToCachedStep, writableCacheDisks)
import Corvus.Handlers.Build.Cleanup (CleanupStack, push)
import Corvus.Handlers.Build.CleanupBakeVm (cleanupBakeVm)
import Corvus.Handlers.Build.Installer (runBootKeys, runInstallerPhase, waitForBakeVmShutdown)
import Corvus.Handlers.Build.Run (runBakeAndPublish, runProvisionersStopAndPublish)
import Corvus.Handlers.Build.Template (instantiateBakeVm, resolveTemplateAndValidate, sanitizeNameFragment, setupTargetDisk)
import Corvus.Handlers.Disk.Agent
  ( guestSetTimeViaAgent
  , loadSnapshotViaAgentWithVmstate
  )
import Corvus.Handlers.Disk.Db (listDiskImageNodes)
import Corvus.Handlers.Disk.Path (resolveDiskPath)
import Corvus.Handlers.Vm
  ( VmStart (..)
  , VmStop (..)
  , hasNetdMediatedNetIf
  )
import Corvus.Handlers.Vm.Db (claimVmStart, setVmErrorIfCurrent, setVmStartedIfCurrent)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Node.Image (ImageResult (..))
import Corvus.Node.Qmp (QmpResult (..), qmpSendKey)
import qualified Corvus.Node.VmSpec as VS
import qualified Corvus.NodeAgentClient as NOA
import Corvus.NodeAgentClient.Spec (assembleVmSpec)
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Protocol (Response (..))
import Corvus.Protocol.Build (BuildEvent (BuildLogLine, StepCacheHit, StepCacheRestore, StepCacheStore), BuildSink (..))
import Corvus.Schema.Build (Build (..), BuildCacheMode (..), BuildStrategy (..), buildCacheMode, buildName, buildNode, buildStrategy, buildTarget, buildTemplate)
import Corvus.Types (BuildOptions (..), ServerState (..), lookupNetAgentMaybe)
import Data.Int (Int64)
import qualified Data.List
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist (get, selectList, (==.))
import Database.Persist.Sql (SelectOpt (LimitTo), SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)

-- | The cache-resumed path: take the bake VM the prior @--build-cache@
-- run left behind, stop it (offline rollback needs an unlocked qcow2),
-- roll its writable disks back to step @K@'s snapshot, push a
-- 'cleanupBakeVm' destructor on the stack (which will skip the
-- @VmDelete@ thanks to the surviving cache rows), then hand off to
-- 'runBakeAndPublish' with @startStep = K + 1@. The cached VM
-- already has its target disk attached in place from the priming run;
-- we don't re-do that setup.
runFromCachedBakeVm
  :: ServerState
  -> TaskId
  -> BuildSink
  -> CleanupStack
  -> UTCTime
  -> BuildOptions
  -> Build
  -> Int
  -- ^ matched prefix length @K@
  -> [Text]
  -- ^ chain hashes (left-to-right)
  -> Int64
  -- ^ cached bake VM id
  -> Text
  -- ^ chain hash at step K (the snapshot to roll back to)
  -> LoggingT IO (Either Text Int64)
runFromCachedBakeVm state parentTaskId sink stack startTime opts b k chains cachedVmId prefixHash = do
  setupRes <- prepareCacheReuse
  case setupRes of
    Right (artifactDiskId, alreadyRunning) -> do
      liftIO $ sink (StepCacheRestore k prefixHash)
      mapM_
        (\(i, h) -> liftIO $ sink (StepCacheHit i h))
        (zip [1 .. k] (take k chains))
      if alreadyRunning
        then
          -- Memory-mode resume: the bake VM is already up and
          -- running with vmstate restored. Skip the VmStart in
          -- 'runBakeAndPublish' and continue directly with the
          -- provisioner tail.
          case buildStrategy b of
            BuildStrategyInstaller ->
              -- Installer strategy doesn't cache (strategyCacheRoles
              -- returns []), so we should never reach here. Defensive:
              -- fall back to the standard path which will detect the
              -- inconsistency.
              runBakeAndPublish
                state
                parentTaskId
                sink
                cachedVmId
                (buildStrategy b)
                artifactDiskId
                (buildTarget b)
                False
                startTime
                opts
                (k + 1)
                b
            _ ->
              runProvisionersStopAndPublish
                state
                parentTaskId
                sink
                cachedVmId
                artifactDiskId
                (buildTarget b)
                False
                startTime
                opts
                (k + 1)
                b
        else
          runBakeAndPublish
            state
            parentTaskId
            sink
            cachedVmId
            (buildStrategy b)
            artifactDiskId
            (buildTarget b)
            False
            startTime
            opts
            (k + 1)
            b
    Left reason -> do
      -- Cache reuse setup failed (cached VM gone, snapshot gone,
      -- rollback error, no writable disks left, …). Purge the stale
      -- cache rows that pointed at this bake VM so the next build
      -- doesn't repeat the same mistake, then fall through to a
      -- fresh bake. The user-visible event stream notes the fallback
      -- so the operator understands why their cached prefix wasn't
      -- used. Only SETUP failures fall back; once we hand off to
      -- runBakeAndPublish, any downstream failure is reported as-is.
      logWarnN $ "cache: reuse failed — falling back to a fresh bake: " <> reason
      liftIO $ sink (BuildLogLine ("cache: reuse failed (" <> reason <> "); rebuilding from scratch"))
      liftIO $ CStore.purgeCacheRowsForVm state cachedVmId
      runFreshBakeFallback state parentTaskId sink stack startTime opts b
  where
    -- Returns @Right (artifactDiskId, alreadyRunning)@ where
    -- @alreadyRunning = True@ for the memory-mode path (the
    -- snapshot-load lifecycle already brought the bake VM up and
    -- the caller must skip its own VmStart). @Left reason@ means
    -- cache setup failed.
    prepareCacheReuse = do
      -- Verify the cached VM still exists. The Vm row could have
      -- been deleted by an operator's `crv vm delete` between the
      -- previous bake and now; the FK cascade in 'deleteVm' should
      -- have dropped the cache rows too, but we double-check.
      mVm <-
        liftIO $
          runSqlPool
            (get (toSqlKey cachedVmId :: VmId))
            (ssDbPool state)
      case mVm of
        Nothing -> pure (Left "cached bake VM has been deleted")
        Just vm -> stopAndPrepare vm

    stopAndPrepare vm = do
      -- Both modes need the bake VM stopped at this point: disk
      -- mode does an offline qcow2 rollback that takes the file
      -- lock; memory mode launches a fresh QEMU process with
      -- `-S`. VmStop on an already-stopped VM returns
      -- RespInvalidTransition, which classifyStopResp surfaces as
      -- Left, so skip the call when state is already VmStopped.
      stopR <-
        if vmStatus vm == VmStopped
          then pure (Right ())
          else do
            resp <-
              liftIO $
                runActionAsSubtask
                  (mkActionContext state parentTaskId "system")
                  (VmStop cachedVmId 300)
            pure (classifyStopResp resp)
      case stopR of
        Left err -> pure (Left ("stop cached bake VM: " <> err))
        Right () -> do
          -- Stale-tail cleanup: drop any cache rows + qcow2 internal
          -- snapshots for stepIndex > k left over from a previous
          -- run that diverged at or before this point. The bake VM
          -- is stopped above, so offline qemu-img can take the
          -- qcow2 lock cleanly. See 'runFreshBake' for the reason
          -- the gate uses the merged 'boBuildCache' opt rather
          -- than the YAML-only 'buildBuildCache b'.
          when (boBuildCache opts) $
            pruneCacheTail
              state
              (H.envelopeHash b <> ":" <> buildName b)
              k
          dispatchOnMode

    dispatchOnMode = do
      artifactRes <- liftIO $ resolveCachedArtifactDiskId state cachedVmId prefixHash
      case artifactRes of
        Left err -> pure (Left err)
        Right artifactDiskId -> do
          dr <- writableCacheDisks state cachedVmId artifactDiskId
          case dr of
            Left err -> pure (Left ("enumerate disks: " <> err))
            Right disks
              | null disks ->
                  pure (Left "cached bake VM has no writable disks left")
              | otherwise -> do
                  -- Push the destructor BEFORE the mode-specific
                  -- work so a partial failure still gets the bake VM
                  -- reaped by the standard cleanup pass.
                  liftIO $
                    push
                      stack
                      "bake-vm-cached"
                      (cleanupBakeVm state parentTaskId cachedVmId)
                  case buildCacheMode b of
                    CacheModeDisk -> do
                      rbR <-
                        rollbackToCachedStep
                          state
                          disks
                          prefixHash
                      case rbR of
                        Left err -> pure (Left err)
                        Right () -> pure (Right (artifactDiskId, False))
                    CacheModeMemory -> do
                      lifecycleR <-
                        resumeMemoryCacheBakeVm
                          state
                          cachedVmId
                          disks
                          prefixHash
                      case lifecycleR of
                        Left err -> pure (Left err)
                        Right () -> pure (Right (artifactDiskId, True))

-- | Memory-mode cache resume lifecycle: launch the cached bake VM
-- paused (QEMU @-S@), restore vmstate via QMP @snapshot-load@,
-- @cont@ to unfreeze, resync the wall clock via QGA
-- @guest-set-time@, then mark the DB row 'VmRunning'.
--
-- Bypasses the @VmStart@ Action — that action commits @VmStarting@
-- and then waits for the first QGA ping, which can't land before
-- @snapshot-load@ runs. We talk to the agent's vmStart RPC
-- directly with @vsStartPaused = True@ so the agent skips its
-- own GA-wait fork.
--
-- The pre-bake artifact disk is the vmstate carrier; the
-- @CacheDisk@ enumeration already filters to writable qcow2
-- drives, and the disk tagged @"artifact"@ holds the RAM dump.
resumeMemoryCacheBakeVm
  :: ServerState
  -> Int64
  -- ^ cached bake VM id (must be stopped)
  -> [CacheDisk]
  -- ^ writable cache-worthy disks; must include role="artifact"
  -> Text
  -- ^ chainHash of step K (the snapshot tag to load)
  -> LoggingT IO (Either Text ())
resumeMemoryCacheBakeVm state cachedVmId disks chain = do
  let pool = ssDbPool state
  case Data.List.find (\d -> cdRole d == "artifact") disks of
    Nothing ->
      pure $
        Left "memory-mode resume: no artifact-role disk in the cache set"
    Just carrier -> do
      mVm <- liftIO $ runSqlPool (get (toSqlKey cachedVmId :: VmId)) pool
      case mVm of
        Nothing -> pure $ Left "cached bake VM disappeared mid-resume"
        Just _ -> do
          mClaimed <-
            liftIO $
              runSqlPool
                (claimVmStart cachedVmId VmStopped VmStarting)
                pool
          case mClaimed of
            Nothing ->
              pure $ Left "memory-mode resume: cached bake VM lifecycle was superseded"
            Just claimedVm ->
              resumeMemoryCacheBakeVmClaimed state cachedVmId disks chain carrier claimedVm

resumeMemoryCacheBakeVmClaimed
  :: ServerState
  -> Int64
  -> [CacheDisk]
  -> Text
  -> CacheDisk
  -> Vm
  -> LoggingT IO (Either Text ())
resumeMemoryCacheBakeVmClaimed state cachedVmId disks chain carrier vm = do
  let pool = ssDbPool state
      cfg = ssQemuConfig state
      snapName = H.cacheSnapshotName chain
  needsNetd <- liftIO $ runSqlPool (hasNetdMediatedNetIf cachedVmId) pool
  mNetAgent <- liftIO $ lookupNetAgentMaybe state (M.vmNodeId vm)
  when (needsNetd && isNothing mNetAgent) $
    logWarnN $
      "memory-mode resume: VM "
        <> T.pack (show cachedVmId)
        <> " has a managed NIC but netd is unavailable; the lifecycle will probably fail"
  let netAgentForSpec = if needsNetd then mNetAgent else Nothing
      -- Cache resume re-uses the bake VM. The QGA wait happens AFTER our
      -- snapshot-load + cont, so we inherit the 90-second steady-state
      -- budget the non-cloud-init path uses. The bake VM has QGA on by
      -- construction (overlay + from-scratch strategies both require it).
      waitMs = if vmGuestAgent vm then 90000 else 0
  mSpec <-
    liftIO $
      assembleVmSpec pool cfg netAgentForSpec cachedVmId (M.vmLifecycleRevision vm) (fromMaybe (M.vmLifecycleRevision vm) (M.vmRuntimeGeneration vm)) waitMs
  case mSpec of
    Left err -> pure $ Left $ "memory-mode resume: assembleVmSpec: " <> err
    Right baseSpec -> do
      let spec = baseSpec {VS.vsStartPaused = True}
          nodeId = M.vmNodeId vm
          paths = map cdFilePath disks
          carrierPath = cdFilePath carrier
      logInfoN $
        "memory-mode resume: starting bake VM "
          <> T.pack (show cachedVmId)
          <> " paused for snapshot-load tag="
          <> snapName
      startR <-
        liftIO $ withVmNodeAgent state cachedVmId $ \nac -> NOA.vmStart nac spec
      case startR of
        Left err -> do
          _ <- liftIO $ runSqlPool (setVmErrorIfCurrent cachedVmId (M.vmLifecycleRevision vm) (fromMaybe (M.vmLifecycleRevision vm) (M.vmRuntimeGeneration vm)) err) pool
          pure $ Left ("memory-mode resume: vmStart paused: " <> err)
        Right (Left e) -> do
          let msg = "vmStart paused: " <> T.pack (show e)
          _ <- liftIO $ runSqlPool (setVmErrorIfCurrent cachedVmId (M.vmLifecycleRevision vm) (fromMaybe (M.vmLifecycleRevision vm) (M.vmRuntimeGeneration vm)) msg) pool
          pure $ Left ("memory-mode resume: " <> msg)
        Right (Right NOA.VmStartVsockCidBusy) -> do
          let msg = "vmStart paused: nodeagent reported a VSOCK CID collision"
          _ <- liftIO $ runSqlPool (setVmErrorIfCurrent cachedVmId (M.vmLifecycleRevision vm) (fromMaybe (M.vmLifecycleRevision vm) (M.vmRuntimeGeneration vm)) msg) pool
          pure $ Left ("memory-mode resume: " <> msg)
        Right (Right (NOA.VmStartStarted _runtime)) -> do
          -- QEMU is up and paused. Drive snapshot-load.
          logInfoN $
            "memory-mode resume: loading vmstate tag="
              <> snapName
              <> " into VM "
              <> T.pack (show cachedVmId)
          loadRes <-
            liftIO $
              loadSnapshotViaAgentWithVmstate
                state
                nodeId
                carrierPath
                paths
                snapName
                cachedVmId
          case loadRes of
            ImageSuccess -> do
              logInfoN "memory-mode resume: snapshot-load OK, issuing cont"
              resumeR <-
                liftIO $ withVmNodeAgent state cachedVmId $ \nac ->
                  NOA.vmResume nac cachedVmId
              case resumeR of
                Left e -> do
                  let msg = "memory-mode resume: cont (outer): " <> e
                  _ <- liftIO $ runSqlPool (setVmErrorIfCurrent cachedVmId (M.vmLifecycleRevision vm) (fromMaybe (M.vmLifecycleRevision vm) (M.vmRuntimeGeneration vm)) msg) pool
                  pure $ Left msg
                Right (Left e) -> do
                  let msg =
                        "memory-mode resume: cont: " <> T.pack (show e)
                  _ <- liftIO $ runSqlPool (setVmErrorIfCurrent cachedVmId (M.vmLifecycleRevision vm) (fromMaybe (M.vmLifecycleRevision vm) (M.vmRuntimeGeneration vm)) msg) pool
                  pure $ Left msg
                Right (Right ()) -> do
                  -- Best-effort clock resync.
                  _ <-
                    liftIO $ guestSetTimeViaAgent state nodeId cachedVmId
                  completed <-
                    liftIO $
                      runSqlPool (setVmStartedIfCurrent cachedVmId (M.vmLifecycleRevision vm) (fromMaybe (M.vmLifecycleRevision vm) (M.vmRuntimeGeneration vm)) VmRunning) pool
                  if completed
                    then do
                      logInfoN $
                        "memory-mode resume: VM "
                          <> T.pack (show cachedVmId)
                          <> " resumed at snapshot state"
                      pure (Right ())
                    else pure $ Left "memory-mode resume: cached bake VM lifecycle was superseded"
            ImageFormatNotSupported msg ->
              finishWithError pool ("snapshot-load: " <> msg)
            ImageError err ->
              finishWithError pool ("snapshot-load: " <> err)
            ImageNotFound ->
              finishWithError pool "snapshot-load: snapshot not found"
  where
    finishWithError pool msg = do
      _ <- liftIO $ runSqlPool (setVmErrorIfCurrent cachedVmId (M.vmLifecycleRevision vm) (fromMaybe (M.vmLifecycleRevision vm) (M.vmRuntimeGeneration vm)) msg) pool
      pure $ Left ("memory-mode resume: " <> msg)

-- | Classify a VmStop response as success or failure.
classifyStopResp :: Response -> Either Text ()
classifyStopResp r = case r of
  RespVmStateChanged _ -> Right ()
  RespError err -> Left err
  RespInvalidTransition _ msg -> Left msg
  _ -> Left $ "unexpected response: " <> T.pack (show r)

-- | Disk roles a strategy expects to find in a complete cache step.
strategyCacheRoles :: BuildStrategy -> [Text]
strategyCacheRoles BuildStrategyOverlay = ["artifact"]
strategyCacheRoles BuildStrategyFromScratch = ["artifact", "system"]
strategyCacheRoles BuildStrategyInstaller = []

-- | Walk the cache rows for @(bakeVmId, chainHash)@ and return the
-- 'DiskImage' id behind the row tagged \"artifact\".
resolveCachedArtifactDiskId :: ServerState -> Int64 -> Text -> IO (Either Text Int64)
resolveCachedArtifactDiskId state vmId chain = do
  rows <-
    runSqlPool
      ( selectList
          [ M.BuildCacheEntryVmId ==. toSqlKey vmId
          , M.BuildCacheEntryChainHash ==. chain
          , M.BuildCacheEntryDiskRole ==. ("artifact" :: Text)
          ]
          [LimitTo 1]
      )
      (ssDbPool state)
  case rows of
    (Entity _ row : _) -> do
      mSnap <- runSqlPool (get (M.buildCacheEntrySnapshotId row)) (ssDbPool state)
      case mSnap of
        Just snap -> pure (Right (fromSqlKey (M.snapshotDiskImageId snap)))
        Nothing -> pure (Left "cache row refers to a missing snapshot")
    [] -> pure (Left "no 'artifact' cache row found for the matched prefix")

-- | Inlined version of 'runFreshBake' from Build.hs.
-- Lifted here to avoid a module cycle: Build.hs → CacheResume.hs → Build.hs.
runFreshBakeFallback
  :: ServerState
  -> TaskId
  -> BuildSink
  -> CleanupStack
  -> UTCTime
  -> BuildOptions
  -> Build
  -> LoggingT IO (Either Text Int64)
runFreshBakeFallback state parentTaskId sink stack startTime opts b = do
  -- No prefix matched (or --use-cache was off). Any rows still on
  -- file under this pipeline key are stale orphans from a previous
  -- run that no longer shares a chain — drop them before we start
  -- writing new ones.
  when (boBuildCache opts) $
    pruneCacheTail
      state
      (H.envelopeHash b <> ":" <> buildName b)
      0
  let prefix = "__build_" <> T.pack (show (fromSqlKey parentTaskId)) <> "_"
      bakeVmName = prefix <> sanitizeNameFragment (buildName b) <> "-vm"
      targetTmpName = prefix <> sanitizeNameFragment (buildName b) <> "-target"
      target = buildTarget b
      strategy = buildStrategy b
  tplR <- resolveTemplateAndValidate state strategy (buildTemplate b)
  case tplR of
    Left err -> pure $ Left err
    Right templateId -> do
      vmR <- instantiateBakeVm state parentTaskId stack templateId bakeVmName (buildNode b)
      case vmR of
        Left err -> pure $ Left err
        Right vmIdLong -> do
          tgtR <- setupTargetDisk state parentTaskId stack vmIdLong strategy target targetTmpName (buildNode b)
          case tgtR of
            Left err -> pure $ Left err
            Right (artifactDiskId, needFlatten) ->
              runBakeAndPublish
                state
                parentTaskId
                sink
                vmIdLong
                strategy
                artifactDiskId
                target
                needFlatten
                startTime
                opts
                1
                b
