{-# LANGUAGE OverloadedStrings #-}

-- | Node placement for VM / disk / network creation when the
-- operator did not pass an explicit @--node@.
--
-- Three pickers live here so handlers and the apply pipeline
-- have a single place to consult:
--
--   * 'pickNodeForVm'      — used by @vm.create@ and template
--                            instantiation.
--   * 'pickNodeForDisk'    — used by every disk handler that
--                            still needs a node placement
--                            (Phase 3 will replace these with
--                            proper 'DiskImageNode' lookups).
--   * 'pickNodeForNetwork' — used by @network.create@.
--
-- Selection policies (Phase 1):
--
--   * 'pickNodeForVm' filters to @adminState = NodeOnline@ and,
--     when the agent push has populated stats, requires
--     @ramMbFree >= requestedRamMb + 'ramSafetyMb'@. It scores
--     by free RAM, free storage (in GiB), and a small load
--     penalty, breaking ties by node name. When stats are still
--     'Nothing' (e.g. before the first nodeagent push), the
--     filter degrades to "any online, non-draining node" and
--     ties break by name.
--   * 'pickNodeForDisk' and 'pickNodeForNetwork' have no
--     objective criteria yet — they pick the first online
--     non-draining node ordered by id. Phase 3 (disks) and a
--     follow-up (networks) will plug richer policies in.
module Corvus.Handlers.Scheduler
  ( pickNodeForVm
  , pickNodeForDisk
  , pickNodeForNetwork
  , ramSafetyMb
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Corvus.Model as M
import Corvus.Types (ServerState (..))
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (Entity (..), selectList, (==.))
import Database.Persist.Postgresql (runSqlPool)

tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- | Safety margin on top of the VM's @ramMb@ request when
-- filtering candidate nodes. Picked deliberately small in
-- Phase 1 (no over-commit) — Phase 2's scheduler may tune this
-- once we have real observations.
ramSafetyMb :: Int
ramSafetyMb = 512

-- | Pick a node to place a new VM on.
--
-- 'Right' result is suitable to pass to the rest of the create
-- path (handlers persist it into 'Vm.nodeId'). 'Left' carries a
-- diagnostic message describing why nothing qualified.
pickNodeForVm
  :: (MonadIO m)
  => ServerState
  -> Int
  -- ^ requested RAM (MiB)
  -> m (Either Text M.NodeId)
pickNodeForVm state requestedRamMb = do
  nodes <- liftIO $ runSqlPool (selectList [M.NodeAdminState ==. M.NodeOnline] []) (ssDbPool state)
  case nodes of
    [] -> pure $ Left "no online node available to place this VM (register one with `crv node add`)"
    candidates -> do
      let withStats = filter (hasEnoughRam requestedRamMb . entityVal) candidates
          chosen =
            if null withStats
              then -- Fall back to "first online node" while node stats are
              -- still NULL (pre-first-push). Order by id for stability.
                sortBy (comparing entityKey) candidates
              else sortBy scoreOrder withStats
      case chosen of
        [] ->
          pure $
            Left $
              "no online node has capacity for "
                <> tshow requestedRamMb
                <> " MiB RAM"
        (Entity k _ : _) -> pure $ Right k
  where
    -- Either no stats yet (Nothing — accept) or free RAM clears
    -- the request plus the safety margin.
    hasEnoughRam reqRam n = case M.nodeRamMbFree n of
      Nothing -> True
      Just free -> free >= reqRam + ramSafetyMb

    -- Higher score = better placement. Tie-break by node name
    -- so two equally good nodes resolve deterministically.
    scoreOrder = comparing (Down . scoreNode) <> comparing (M.nodeName . entityVal)

    scoreNode :: Entity M.Node -> Double
    scoreNode (Entity _ n) =
      let ram = fromIntegral (fromMaybe 0 (M.nodeRamMbFree n) - requestedRamMb) :: Double
          gib = case M.nodeStorageBytesFree n of
            Nothing -> 0
            Just b -> fromIntegral (b `div` (1024 * 1024 * 1024)) :: Double
          load = fromMaybe 0 (M.nodeLoadAvg1 n) :: Double
       in ram + gib - 100 * load

-- | Pick a node to host a new disk image. No objective criteria
-- yet (Phase 3 will replace this with proper 'DiskImageNode'
-- lookups across the cluster); for now we pick the lowest-id
-- online non-draining node.
pickNodeForDisk :: (MonadIO m) => ServerState -> m (Either Text M.NodeId)
pickNodeForDisk = firstOnlineNode "disk"

-- | Pick a node to host a new virtual network. Same simple
-- "first online node by id" policy as 'pickNodeForDisk'.
pickNodeForNetwork :: (MonadIO m) => ServerState -> m (Either Text M.NodeId)
pickNodeForNetwork = firstOnlineNode "network"

-- | Shared helper: pick the lowest-id online node, or fail with
-- a "register a node first" message tailored to the resource
-- being created.
firstOnlineNode :: (MonadIO m) => Text -> ServerState -> m (Either Text M.NodeId)
firstOnlineNode resourceLabel state = do
  nodes <-
    liftIO $
      runSqlPool
        (selectList [M.NodeAdminState ==. M.NodeOnline] [])
        (ssDbPool state)
  pure $ case sortBy (comparing entityKey) nodes of
    [] ->
      Left $
        "no online node available to host this "
          <> resourceLabel
          <> " (register one with `crv node add`)"
    (Entity k _ : _) -> Right k
