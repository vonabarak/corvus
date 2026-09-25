{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Placement disk-image handlers.
module Corvus.Handlers.Disk.Placement
  ( DiskCopy (..)
  , DiskMove (..)
  , computeDestPaths
  , nodeBasePathFor
  , withSelectedDiskNode
  )
where

import Corvus.Action

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, logWarnN)
import Corvus.Handlers.Disk.Agent
  ( cloneImageViaAgent
  , createImageViaAgent
  , createOverlayViaAgent
  , deleteImageViaAgent
  , getImageInfoViaAgent
  , getImageSizeMbViaAgent
  , resizeImageViaAgent
  )
import Corvus.Handlers.Disk.Attach (DiskAttach (..), DiskDetachByDisk (..), handleDiskAttach, handleDiskDetach)
import Corvus.Handlers.Disk.Db (deleteDiskAndSnapshots, deleteDiskImageNodeRow, diskImageNodeFilePathFor, getAttachedVms, getBackingChainIds, getDiskImageInfo, getOverlayIds, getReadWriteAttachedVms, getRunningAttachedVms, hasPlacementOnNode, listDiskImageNodes, listDiskImages, recordDiskImageNode)
import Corvus.Handlers.Disk.Import (DiskImportAction (..), handleDiskImportCopy)
import Corvus.Handlers.Disk.Path (makeRelativeToBase, resolveDiskFilePath, resolveDiskFilePathPure, resolveDiskPath, sanitizeDiskName)
import Corvus.Handlers.Disk.Rebase (DiskRebase (..), handleDiskRebase)
import Corvus.Handlers.Disk.Snapshot (SnapshotCreate (..), SnapshotDelete (..), SnapshotMerge (..), SnapshotRollback (..), handleSnapshotCreate, handleSnapshotDelete, handleSnapshotList, handleSnapshotMerge, handleSnapshotRollback)
import Corvus.Handlers.Disk.Transfer (stageBackingChain, transferImageBetweenNodes)
import Corvus.Handlers.Resolve (ResolveError (..), resolveErrorMessage, resolveNode, validateName)
import Corvus.Handlers.Scheduler (pickNodeForDisk, pickNodeForExistingDisk)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Node.Image (ImageInfo (..), ImageResult (..), detectFormatFromPath)
import Corvus.Protocol
import Corvus.Qemu.Config (getEffectiveBasePath)
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Sql (runSqlPool)
import System.FilePath (takeExtension, takeFileName, (</>))

-- | Every relative disk path is rooted at the placement node's base path,
-- never at the daemon process's local configuration.
nodeBasePathFor :: ServerState -> M.NodeId -> IO FilePath
nodeBasePathFor state nid = do
  mNode <- runSqlPool (get nid) (ssDbPool state)
  case mNode of
    Just node -> pure $ T.unpack (M.nodeBasePath node)
    Nothing -> getEffectiveBasePath (ssQemuConfig state)

-- | Resolve an explicit disk-placement reference, or let the scheduler choose
-- when the request omitted it. Both disk creation and registration use the
-- same convention for Cap'n Proto's unset @byId 0@ reference.
withSelectedDiskNode
  :: ServerState
  -> Text
  -> (M.NodeId -> LoggingT IO Response)
  -> LoggingT IO Response
withSelectedDiskNode state nodeRefText action
  | T.null nodeRefText || nodeRefText == "0" = do
      eNid <- liftIO $ pickNodeForDisk state
      case eNid of
        Left err -> pure $ RespError err
        Right nid -> action nid
  | otherwise = do
      result <- liftIO $ resolveNode (Ref nodeRefText) (ssDbPool state)
      case result of
        Left (RefNotFound _ _) -> pure RespNodeNotFound
        Left resolveErr -> pure $ RespAmbiguousRef (resolveErrorMessage resolveErr)
        Right nidRaw -> action (M.toSqlKey nidRaw)

--------------------------------------------------------------------------------
-- Disk copy / move (inter-node)
--------------------------------------------------------------------------------

-- | Helper: planning step shared by copy and move.
--
-- Returns a 'Left' diagnostic when the operation must be
-- refused for non-data reasons (target equal to source, target
-- not online, attachment-state conflicts, backing chain missing,
-- path collision, …). Returns a 'Right' on success with everything
-- the transfer needs:
--
--   * source 'NodeId' (the placement we transfer from)
--   * source absolute file path
--   * destination absolute file path
--   * destination relative path to record in the DB row
data TransferPlan = TransferPlan
  { tpSrcNode :: !M.NodeId
  , tpSrcAbsPath :: !FilePath
  , tpDestAbsPath :: !FilePath
  , tpDestRelPath :: !Text
  }

-- | Build the plan for a copy/move. The 'allowAttachedRO' flag
-- toggles whether read-only attachment is allowed:
--
--   * Copy: 'True' (copying an attached r/o image is legal).
--   * Move: 'False' (moving an attached image of either flavour
--     is rejected; r/w go through @vm migrate@, r/o can only be
--     copied per the user spec).
--
-- @mToPath@ is the operator-supplied destination path on the
-- target node. 'Nothing' (no override) means:
--
--   * source path is relative → preserve the same relative
--     path on the destination (so @templates/ubuntu.qcow2@
--     stays at @templates/ubuntu.qcow2@ rather than collapsing
--     to @ubuntu.qcow2@);
--   * source path is absolute → refuse, because the same
--     absolute path on a different node is rarely writable and
--     almost never the intent; the operator must pick.
--
-- 'Just' follows the same rules as @--path@ on @disk create@
-- via 'resolveDiskFilePathPure': relative is anchored at the
-- destination node's @basePath@, absolute is honoured verbatim,
-- trailing @/@ means "this is a directory; append the source
-- basename".
planTransfer
  :: ServerState
  -> Int64
  -- ^ disk id
  -> M.NodeId
  -- ^ destination node
  -> Bool
  -- ^ allowAttachedRO (copy=True, move=False)
  -> Maybe Text
  -- ^ operator-supplied destination path (--to-path)
  -> IO (Either Text TransferPlan)
planTransfer state diskId destNode allowAttachedRO mToPath = do
  let pool = ssDbPool state
      diskKey = toSqlKey diskId :: DiskImageId
  mDisk <- runSqlPool (get diskKey) pool
  case mDisk of
    Nothing -> pure (Left "disk image not found")
    Just _disk -> do
      mDestNode <- runSqlPool (get destNode) pool
      case mDestNode of
        Nothing -> pure (Left "destination node not found")
        Just destRow ->
          if M.nodeAdminState destRow /= M.NodeOnline
            then pure (Left "destination node is not online")
            else do
              -- Refuse if the disk is attached read-write to any VM
              -- (must use @vm migrate@). Read-only attachments are
              -- copy-only.
              rwVms <- runSqlPool (getReadWriteAttachedVms diskId) pool
              case rwVms of
                ((_, n) : _) ->
                  pure $
                    Left $
                      "disk is attached read-write to VM '"
                        <> n
                        <> "'; use `crv vm migrate` instead"
                [] -> do
                  attached <- runSqlPool (getAttachedVms diskId) pool
                  if not (null attached) && not allowAttachedRO
                    then case attached of
                      (NamedRef {nrName = n} : _) ->
                        pure $
                          Left $
                            "disk is attached to VM '"
                              <> n
                              <> "' read-only; only copy is allowed"
                      [] -> pure (Left "internal: empty attached list")
                    else do
                      -- Resolve source placement: pick any existing placement
                      -- that isn't the destination.
                      placements <- runSqlPool (listDiskImageNodes diskKey) pool
                      case [p | p@(Entity _ row) <- placements, M.diskImageNodeNodeId row /= destNode] of
                        [] ->
                          pure (Left "no source placement available (target is the only node hosting this disk)")
                        (Entity _ srcRow : _) -> do
                          -- Reject if destination already has a placement.
                          alreadyOnDest <-
                            runSqlPool (hasPlacementOnNode diskKey destNode) pool
                          if alreadyOnDest
                            then pure (Left "destination node already has a placement for this disk")
                            else do
                              -- Walk the backing chain — every ancestor must
                              -- be on the destination (per the user decision:
                              -- copy/move refuses if the chain is missing).
                              chain <-
                                runSqlPool
                                  (getBackingChainIds diskId)
                                  pool
                              missing <- runSqlPool (filterMissing chain destNode) pool
                              case missing of
                                (parentKey : _) -> do
                                  mParent <- runSqlPool (get parentKey) pool
                                  let nm = maybe "(deleted)" diskImageName mParent
                                  pure $
                                    Left $
                                      "backing image '"
                                        <> nm
                                        <> "' is not present on the destination; "
                                        <> "copy it first with `crv disk copy "
                                        <> nm
                                        <> " --to-node <NAME>`"
                                [] -> do
                                  -- Compute paths according to the
                                  -- absolute-source / relative-source
                                  -- rules described in the docstring.
                                  let srcStored = M.diskImageNodeFilePath srcRow
                                      srcRel = T.unpack srcStored
                                      isSrcAbs = "/" `isPrefixOf` srcRel
                                      destBase = T.unpack (M.nodeBasePath destRow)
                                  srcBase <- nodeBasePathFor state (M.diskImageNodeNodeId srcRow)
                                  let srcAbs =
                                        if isSrcAbs
                                          then srcRel
                                          else srcBase </> srcRel
                                  case computeDestPaths isSrcAbs srcStored srcAbs destBase mToPath of
                                    Left err -> pure (Left err)
                                    Right (destAbs, destStored) -> do
                                      -- Reject path collision: another
                                      -- DiskImageNode on the destination
                                      -- already claims this stored path.
                                      -- Key the lookup on the stored form
                                      -- (relative when inside basePath,
                                      -- absolute otherwise) — that's how
                                      -- UniqueDiskImagePathPerNode is keyed.
                                      collision <-
                                        runSqlPool
                                          (getBy (M.UniqueDiskImagePathPerNode destNode destStored))
                                          pool
                                      case collision of
                                        Just (Entity _ row) ->
                                          pure $
                                            Left $
                                              "destination path '"
                                                <> destStored
                                                <> "' already in use by disk id "
                                                <> T.pack (show (M.fromSqlKey (M.diskImageNodeDiskImageId row)))
                                                <> " on the target node"
                                        Nothing ->
                                          pure $
                                            Right
                                              TransferPlan
                                                { tpSrcNode = M.diskImageNodeNodeId srcRow
                                                , tpSrcAbsPath = srcAbs
                                                , tpDestAbsPath = destAbs
                                                , tpDestRelPath = destStored
                                                }
  where
    filterMissing chain target = filterM (fmap not . (`hasPlacementOnNode` target)) chain

-- | Pure path-resolution for 'planTransfer'. Split out so unit
-- tests can exercise the decision matrix without an agent or DB.
--
-- Returns @(destAbsPath, destStoredText)@ where the storage form
-- is relative-inside-basePath when applicable, absolute otherwise
-- — matching every other call site of 'recordDiskImageNode'.
computeDestPaths
  :: Bool
  -- ^ source path is absolute
  -> Text
  -- ^ source's stored path (verbatim from DiskImageNode.filePath)
  -> FilePath
  -- ^ source's resolved absolute path (for basename extraction)
  -> FilePath
  -- ^ destination node's basePath
  -> Maybe Text
  -- ^ --to-path override (Nothing == no override)
  -> Either Text (FilePath, Text)
computeDestPaths isSrcAbs srcStored srcAbs destBase0 mToPath =
  -- Normalise the destination basePath up-front so a node row
  -- registered with a trailing slash (e.g. @/home/kvm/VMs/@)
  -- doesn't poison the @basePath </> rel@ join with stray
  -- slashes that would later defeat 'makeRelativeToBase'.
  let destBase = stripTrailingSlashes destBase0
   in case (isSrcAbs, mToPath) of
        (True, Nothing) ->
          Left $
            "source path '"
              <> srcStored
              <> "' is absolute; --to-path is required for copy/move "
              <> "(the same absolute path on the destination node is rarely "
              <> "writable, and silently retargeting under basePath is unsafe)"
        (False, Nothing) ->
          -- Preserve the relative path verbatim. The destination
          -- agent's importFromPeer call will mkdir -p the parent.
          let destAbs = destBase </> T.unpack srcStored
           in Right (destAbs, srcStored)
        (_, Just rawToPath) ->
          let srcBase = takeFileName srcAbs
              destAbs = resolveDiskFilePathPure destBase (Just rawToPath) srcBase
              destStored = makeRelativeToBase destBase destAbs
           in Right (destAbs, destStored)
  where
    stripTrailingSlashes :: FilePath -> FilePath
    stripTrailingSlashes [] = []
    stripTrailingSlashes s
      | last s == '/' = stripTrailingSlashes (init s)
      | otherwise = s

-- | Helper: lift "monadic filter" into the SqlPersistT context.
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = pure []
filterM p (x : xs) = do
  b <- p x
  rest <- filterM p xs
  pure $ if b then x : rest else rest

-- | Copy a disk image's bytes to another node, leaving the source
-- placement intact. Records a new 'DiskImageNode' row on the
-- destination after the transfer succeeds.
--
-- When @withBackingChain@ is 'True', every backing ancestor that
-- isn't yet on the destination is staged first via
-- 'stageBackingChain'. The historical refuse-if-chain-missing
-- behaviour of 'planTransfer' (its check at the "backing image …
-- is not present on the destination" line) then passes naturally
-- because the chain is now in place.
handleDiskCopy :: ServerState -> Int64 -> Int64 -> Maybe Text -> Bool -> IO Response
handleDiskCopy state diskId destNodeRaw mToPath withBackingChain = runServerLogging state $ do
  let destNode = toSqlKey destNodeRaw :: M.NodeId
      diskKey = toSqlKey diskId :: DiskImageId
  logInfoN $
    "disk copy: image "
      <> T.pack (show diskId)
      <> " → node "
      <> T.pack (show destNodeRaw)
      <> if withBackingChain then " (with backing chain)" else ""
  eStaged <- liftIO $ stageChainIfRequested state diskKey destNode withBackingChain
  case eStaged of
    Left err -> pure (RespError err)
    Right () -> do
      ePlan <- liftIO $ planTransfer state diskId destNode True mToPath
      case ePlan of
        Left err -> pure (RespError err)
        Right plan -> do
          tResult <-
            liftIO $
              transferImageBetweenNodes
                state
                (tpSrcNode plan)
                destNode
                (tpSrcAbsPath plan)
                (tpDestAbsPath plan)
          case tResult of
            Left err -> pure (RespError err)
            Right () -> do
              liftIO $
                runSqlPool
                  (recordDiskImageNode diskKey destNode (tpDestRelPath plan))
                  (ssDbPool state)
              logInfoN "disk copy complete"
              pure RespDiskOk

-- | Resolve the source node for the chain walk (any existing
-- placement that isn't the destination), then call
-- 'stageBackingChain'. A no-op when @withBackingChain@ is 'False'
-- or when the chain is empty. Errors from 'stageBackingChain'
-- (including the "source placement vanished" case) propagate to
-- the caller verbatim.
stageChainIfRequested
  :: ServerState
  -> DiskImageId
  -> M.NodeId
  -> Bool
  -> IO (Either Text ())
stageChainIfRequested _ _ _ False = pure (Right ())
stageChainIfRequested state diskKey destNode True = do
  placements <- runSqlPool (listDiskImageNodes diskKey) (ssDbPool state)
  case [M.diskImageNodeNodeId row | Entity _ row <- placements, M.diskImageNodeNodeId row /= destNode] of
    [] ->
      pure $
        Left "no source placement available (target is the only node hosting this disk)"
    srcNode : _ -> do
      r <- stageBackingChain state diskKey srcNode destNode
      pure $ case r of
        Left err -> Left err
        Right _ -> Right ()

-- | Move a disk image's bytes to another node and delete the
-- source-side placement + file on success. Refused for any disk
-- still attached to a VM (the user must go through @vm migrate@
-- for attached images).
--
-- See 'handleDiskCopy' for the @withBackingChain@ semantics; the
-- staged ancestors land as separate 'DiskImageNode' rows on the
-- destination (not moved — backing images may still have other
-- consumers on the source).
handleDiskMove :: ServerState -> Int64 -> Int64 -> Maybe Text -> Bool -> IO Response
handleDiskMove state diskId destNodeRaw mToPath withBackingChain = runServerLogging state $ do
  let destNode = toSqlKey destNodeRaw :: M.NodeId
      diskKey = toSqlKey diskId :: DiskImageId
  logInfoN $
    "disk move: image "
      <> T.pack (show diskId)
      <> " → node "
      <> T.pack (show destNodeRaw)
      <> if withBackingChain then " (with backing chain)" else ""
  eStaged <- liftIO $ stageChainIfRequested state diskKey destNode withBackingChain
  case eStaged of
    Left err -> pure (RespError err)
    Right () -> do
      ePlan <- liftIO $ planTransfer state diskId destNode False mToPath
      case ePlan of
        Left err -> pure (RespError err)
        Right plan -> do
          tResult <-
            liftIO $
              transferImageBetweenNodes
                state
                (tpSrcNode plan)
                destNode
                (tpSrcAbsPath plan)
                (tpDestAbsPath plan)
          case tResult of
            Left err -> pure (RespError err)
            Right () -> do
              -- Insert destination row, drop source row.
              liftIO $
                runSqlPool
                  ( do
                      recordDiskImageNode diskKey destNode (tpDestRelPath plan)
                      deleteDiskImageNodeRow diskKey (tpSrcNode plan)
                  )
                  (ssDbPool state)
              -- Best-effort: delete the source file. Failure here is
              -- logged but doesn't roll back the DB swap — the move
              -- is logically already done.
              delResult <-
                liftIO $
                  deleteImageViaAgent state (tpSrcNode plan) (tpSrcAbsPath plan)
              case delResult of
                ImageSuccess -> pure ()
                ImageNotFound -> pure ()
                other ->
                  logWarnN $
                    "source file delete after move did not complete cleanly: "
                      <> T.pack (show other)
              logInfoN "disk move complete"
              pure RespDiskOk

data DiskCopy = DiskCopy
  { dcpDiskId :: Int64
  , dcpDestNodeId :: Int64
  , dcpToPath :: Maybe Text
  -- ^ Operator-supplied destination path. 'Nothing' (or an empty
  -- wire string the dispatcher converted) means "preserve the
  -- source's relative path; refuse if the source path is
  -- absolute".
  , dcpWithBackingChain :: Bool
  -- ^ When 'True', stage every missing backing ancestor on the
  -- destination before transferring the primary disk; see
  -- 'stageBackingChain'.
  }

instance Action DiskCopy where
  actionSubsystem _ = SubDisk
  actionCommand _ = "copy"
  actionEntityId = Just . fromIntegral . dcpDiskId
  actionExecute ctx a =
    handleDiskCopy
      (acState ctx)
      (dcpDiskId a)
      (dcpDestNodeId a)
      (dcpToPath a)
      (dcpWithBackingChain a)

data DiskMove = DiskMove
  { dmvDiskId :: Int64
  , dmvDestNodeId :: Int64
  , dmvToPath :: Maybe Text
  -- ^ Same semantics as 'dcpToPath'.
  , dmvWithBackingChain :: Bool
  -- ^ Same semantics as 'dcpWithBackingChain'.
  }

instance Action DiskMove where
  actionSubsystem _ = SubDisk
  actionCommand _ = "move"
  actionEntityId = Just . fromIntegral . dmvDiskId
  actionExecute ctx a =
    handleDiskMove
      (acState ctx)
      (dmvDiskId a)
      (dmvDestNodeId a)
      (dmvToPath a)
      (dmvWithBackingChain a)
