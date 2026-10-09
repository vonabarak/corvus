{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Explicit pruning of historical image versions and their node placements.
module Corvus.Handlers.Disk.Cleanup (DiskCleanup (..), previewDiskCleanup) where

import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Monad (foldM)
import Corvus.Action
import Corvus.DiskSelector (validateImageName)
import Corvus.Handlers.Disk.Agent (deleteImageViaAgent)
import Corvus.Handlers.Disk.CleanupGraph
import Corvus.Handlers.Disk.Db (deleteDiskAndSnapshots, deleteDiskImageNodeRow)
import Corvus.Handlers.Disk.Path (resolveDiskPath)
import Corvus.ImageOperationGuard (withImageOperationGuard)
import Corvus.Model
import Corvus.Node.Image (ImageResult (..))
import Corvus.Protocol
import Corvus.Types
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (fromSqlKey, runSqlPool)

-- | Nothing selects every registered family; node Nothing selects every node.
data DiskCleanup = DiskCleanup
  { dcName :: Maybe Text
  , dcNode :: Maybe NodeId
  , dcIncludeTagged :: Bool
  }
  deriving (Show)

instance Action DiskCleanup where
  actionSubsystem _ = SubDisk
  actionCommand _ = "cleanup"
  actionEntityName = dcName
  actionExclusiveImages _ = True
  actionValidate = validateCleanup
  actionExecute ctx options = do
    graph <- loadGraph (acState ctx)
    case validateGraph options graph of
      Just err -> pure err
      Nothing -> do
        versions <- mapM (executeVersion ctx options) (cleanupOrder graph (dcName options))
        pure $ RespDiskCleanup (makeReport False versions)

-- Each version has its own audit record, including partial placement failure.
data CleanupVersion = CleanupVersion DiskCleanup DiskImageId
instance Action CleanupVersion where
  actionSubsystem _ = SubDisk
  actionCommand _ = "cleanup-version"
  actionEntityId (CleanupVersion _ key) = Just (fromIntegral (fromSqlKey key))
  actionExecute ctx (CleanupVersion options key) = do
    graph <- loadGraph (acState ctx)
    (_, report) <- processVersion (Just ctx) options graph key
    pure $ RespDiskCleanup (makeReport False [report])

executeVersion :: ActionContext -> DiskCleanup -> DiskImageId -> IO DiskCleanupVersion
executeVersion ctx options key = do
  throwIfCancelled ctx
  response <- runActionAsSubtask ctx (CleanupVersion options key)
  case response of
    RespDiskCleanup report -> case dcrVersions report of
      version : _ -> pure version
      [] -> failed "Empty cleanup subtask report"
    RespError err -> failed err
    _ -> failed "Cleanup subtask failed"
  where
    failed err = do
      throwIfCancelled ctx
      pure $ DiskCleanupVersion (NamedRef (fromSqlKey key) "") [] "failed" err False []

previewDiskCleanup :: ServerState -> DiskCleanup -> IO Response
previewDiskCleanup state options = withImageOperationGuard (ssImageOperations state) False $ do
  graph <- loadGraph state
  case validateGraph options graph of
    Just err -> pure err
    Nothing -> do
      (_, versions) <- foldM step (graph, []) (cleanupOrder graph (dcName options))
      pure $ RespDiskCleanup (makeReport True (reverse versions))
  where
    step (graph, reports) key = do
      (remaining, report) <- processVersion Nothing options graph key
      pure (remaining, report : reports)

loadGraph :: ServerState -> IO CleanupGraph
loadGraph state = runSqlPool loadCleanupGraph (ssDbPool state)

validateCleanup :: ServerState -> DiskCleanup -> IO (Maybe Response)
validateCleanup state options = validateGraph options <$> loadGraph state

validateGraph :: DiskCleanup -> CleanupGraph -> Maybe Response
validateGraph options graph = case dcName options of
  Just name -> case validateImageName name of
    Left err -> Just (RespError err)
    Right () | not (any ((== name) . diskImageName) (Map.elems (cgImages graph))) -> Just (RespError "Image family not found")
    Right () -> checkNode
  Nothing -> checkNode
  where
    checkNode = case dcNode options of
      Just node | Map.notMember node (cgNodes graph) -> Just (RespError "Node not found")
      _ -> Nothing

processVersion :: Maybe ActionContext -> DiskCleanup -> CleanupGraph -> DiskImageId -> IO (CleanupGraph, DiskCleanupVersion)
processVersion context options graph key = do
  let imageName = maybe "" diskImageName (Map.lookup key (cgImages graph))
      report = DiskCleanupVersion (NamedRef (fromSqlKey key) imageName) (cleanupTags graph key) "retained" "" False []
      reason = versionReason graph (dcIncludeTagged options) key
      placements = [p | p <- cgPlacements graph, diskImageNodeDiskImageId p == key, maybe True (== diskImageNodeNodeId p) (dcNode options)]
  if not (T.null reason)
    then pure (graph, report {dcvReason = reason})
    else do
      (remaining, results) <- foldM (processPlacement context key) (graph, []) placements
      let unplaced = not (any ((== key) . diskImageNodeDiskImageId) (cgPlacements remaining))
          globalRefs =
            any ((== key) . fst) (cgAttachments remaining)
              || any ((== Just key) . diskImageBackingImageId) (Map.elems (cgImages remaining))
          deleteVersion = unplaced && not globalRefs && (isNothing (dcNode options) || not (null placements))
          failed = any ((== "failed") . dcpStatus) results
          changed = any (\p -> dcpStatus p `elem` ["removed", "planned"]) results
      case context of
        Just ctx | deleteVersion -> runSqlPool (deleteDiskAndSnapshots (fromSqlKey key)) (ssDbPool (acState ctx))
        _ -> pure ()
      let status
            | failed = "failed"
            | deleteVersion || changed = maybe "planned" (const "removed") context
            | otherwise = "retained"
          why
            | deleteVersion || changed || failed = ""
            | globalRefs = "VM or overlay references"
            | null placements = "no placement in scope"
            | otherwise = "required placements"
      pure
        ( if deleteVersion then forgetVersion key remaining else remaining
        , report {dcvStatus = status, dcvReason = why, dcvVersionDeleted = deleteVersion && contextPresent context, dcvPlacements = reverse results}
        )
  where
    contextPresent Nothing = False
    contextPresent (Just _) = True

processPlacement :: Maybe ActionContext -> DiskImageId -> (CleanupGraph, [DiskCleanupPlacement]) -> DiskImageNode -> IO (CleanupGraph, [DiskCleanupPlacement])
processPlacement context key (graph, reports) placement = do
  let node = diskImageNodeNodeId placement
      nodeName' = maybe "" nodeName (Map.lookup node (cgNodes graph))
      report = DiskCleanupPlacement (NamedRef (fromSqlKey node) nodeName') (diskImageNodeFilePath placement) "retained" ""
      reason = placementReason graph key node
  for_ context throwIfCancelled
  if not (T.null reason)
    then pure (graph, report {dcpReason = reason} : reports)
    else case context of
      Nothing -> pure (forgetPlacement key node graph, report {dcpStatus = "planned"} : reports)
      Just ctx -> do
        outcome <- deletePlacement ctx key node
        case outcome of
          Just err -> pure (graph, report {dcpStatus = "failed", dcpReason = err} : reports)
          Nothing -> pure (forgetPlacement key node graph, report {dcpStatus = "removed"} : reports)

deletePlacement :: ActionContext -> DiskImageId -> NodeId -> IO (Maybe Text)
deletePlacement ctx key node = do
  let state = acState ctx
  result <- try $ do
    path <- resolveDiskPath (ssDbPool state) (ssQemuConfig state) key node
    outcome <- deleteImageViaAgent state node path
    case outcome of
      ImageSuccess -> runSqlPool (deleteDiskImageNodeRow key node) (ssDbPool state)
      ImageNotFound -> runSqlPool (deleteDiskImageNodeRow key node) (ssDbPool state)
      _ -> pure ()
    pure outcome
  case result of
    Left (err :: SomeException) -> case fromException err :: Maybe TaskCancelledException of
      Just cancelled -> throwIO cancelled
      Nothing -> pure (Just (T.pack (show err)))
    Right ImageSuccess -> pure Nothing
    Right ImageNotFound -> pure Nothing
    Right (ImageError err) -> pure (Just err)
    Right (ImageFormatNotSupported err) -> pure (Just err)

makeReport :: Bool -> [DiskCleanupVersion] -> DiskCleanupReport
makeReport dry versions =
  DiskCleanupReport
    dry
    versions
    (fromIntegral (length (filter dcvVersionDeleted versions)))
    (fromIntegral (length [p | v <- versions, p <- dcvPlacements v, dcpStatus p == "removed"]))
    (fromIntegral (sum [max (if dcvStatus v == "failed" then 1 else 0) (length (filter ((== "failed") . dcpStatus) (dcvPlacements v))) | v <- versions]))
