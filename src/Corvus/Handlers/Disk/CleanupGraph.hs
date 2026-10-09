{-# LANGUAGE OverloadedStrings #-}

-- | Snapshot and simulation of the references that protect image placements.
module Corvus.Handlers.Disk.CleanupGraph
  ( CleanupGraph (..)
  , loadCleanupGraph
  , cleanupOrder
  , versionReason
  , placementReason
  , forgetPlacement
  , forgetVersion
  , cleanupTags
  ) where

import Corvus.Model
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql (SqlPersistT)

data CleanupGraph = CleanupGraph
  { cgImages :: Map.Map DiskImageId DiskImage
  , cgPlacements :: [DiskImageNode]
  , cgTags :: [DiskImageTag]
  , cgTemplates :: Set.Set DiskImageId
  , cgAttachments :: [(DiskImageId, NodeId)]
  , cgNodes :: Map.Map NodeId Node
  }

loadCleanupGraph :: SqlPersistT IO CleanupGraph
loadCleanupGraph = do
  images <- selectList [] []
  placements <- map entityVal <$> selectList [] []
  tags <- map entityVal <$> selectList [] []
  templates <- map entityVal <$> selectList [] []
  drives <- map entityVal <$> selectList [] []
  vms <- Map.fromList . map (\(Entity key vm) -> (key, vmNodeId vm)) <$> selectList [] []
  nodes <- Map.fromList . map (\(Entity key node) -> (key, node)) <$> selectList [] []
  let resolveTemplate td = case templateDriveDiskImageId td of
        Just key -> Just key
        Nothing | templateDriveCloneStrategy td == StrategyCreate -> Nothing
        Nothing -> do
          name <- templateDriveDiskName td
          tag <- templateDriveDiskTag td
          diskImageTagDiskImageId <$> findTag name tag tags
      attached d = (,) <$> driveDiskImageId d <*> Map.lookup (driveVmId d) vms
  pure $
    CleanupGraph
      (Map.fromList [(key, image) | Entity key image <- images])
      placements
      tags
      (Set.fromList (mapMaybe resolveTemplate templates))
      (mapMaybe attached drives)
      nodes
  where
    findTag name tag = foldr (\t rest -> if diskImageTagName t == name && diskImageTagTag t == tag then Just t else rest) Nothing

cleanupTags :: CleanupGraph -> DiskImageId -> [Text]
cleanupTags graph key = sortOn id [diskImageTagTag t | t <- cgTags graph, diskImageTagDiskImageId t == key]

-- | Descendants first, even when a backing chain crosses family boundaries.
-- Cycles are retained by the reference checks rather than traversed forever.
cleanupOrder :: CleanupGraph -> Maybe Text -> [DiskImageId]
cleanupOrder graph name =
  map fst $
    sortOn
      (\(key, _) -> (negate (depth Set.empty key), key))
      [(key, image) | (key, image) <- Map.toList (cgImages graph), maybe True (== diskImageName image) name]
  where
    depth seen key
      | key `Set.member` seen = 0 :: Int
      | otherwise = case Map.lookup key (cgImages graph) >>= diskImageBackingImageId of
          Nothing -> 0
          Just base -> 1 + depth (Set.insert key seen) base

versionReason :: CleanupGraph -> Bool -> DiskImageId -> Text
versionReason graph includeTagged key
  | "latest" `elem` tags = "latest"
  | not includeTagged && not (null tags) = "tagged"
  | key `Set.member` cgTemplates graph = "template reference"
  | otherwise = ""
  where
    tags = cleanupTags graph key

placementReason :: CleanupGraph -> DiskImageId -> NodeId -> Text
placementReason graph key node
  | (key, node) `elem` cgAttachments graph = "attached VM on node"
  | any (`hasPlacement` node) overlays = "backing image required on node"
  | final && any ((== key) . fst) (cgAttachments graph) = "final placement has VM references"
  | final && not (null overlays) = "final placement has overlay references"
  | otherwise = ""
  where
    overlays = [overlay | (overlay, image) <- Map.toList (cgImages graph), diskImageBackingImageId image == Just key]
    hasPlacement image nid = any (\p -> diskImageNodeDiskImageId p == image && diskImageNodeNodeId p == nid) (cgPlacements graph)
    final = length [p | p <- cgPlacements graph, diskImageNodeDiskImageId p == key] <= 1

forgetPlacement :: DiskImageId -> NodeId -> CleanupGraph -> CleanupGraph
forgetPlacement key node graph =
  graph
    { cgPlacements = filter (\p -> diskImageNodeDiskImageId p /= key || diskImageNodeNodeId p /= node) (cgPlacements graph)
    }

forgetVersion :: DiskImageId -> CleanupGraph -> CleanupGraph
forgetVersion key graph = graph {cgImages = Map.delete key (cgImages graph)}
