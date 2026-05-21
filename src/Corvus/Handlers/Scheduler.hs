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
  , hasCapacityFor
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Corvus.Model as M
import Corvus.Types (ServerState (..), reservedRamFor)
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
      -- Annotate every candidate with the daemon's in-memory
      -- reservation for that node, then run the filter + score
      -- against the *effective* free RAM (raw stat minus the
      -- pending reservation). Two back-to-back picks within the
      -- same daemon won't pile onto the same node before the
      -- agent's next 'NodeStats' push refreshes the raw value.
      annotated <- liftIO $ traverse withReservation candidates
      let withStats = filter (hasEnoughRam requestedRamMb) annotated
          chosen =
            if null withStats
              then -- Fall back to "first online node" while node stats are
              -- still NULL (pre-first-push). Order by id for stability.
                sortBy (comparing (entityKey . fst)) annotated
              else sortBy scoreOrder withStats
      case chosen of
        [] ->
          pure $
            Left $
              "no online node has capacity for "
                <> tshow requestedRamMb
                <> " MiB RAM"
        ((Entity k _, _) : _) -> pure $ Right k
  where
    withReservation :: Entity M.Node -> IO (Entity M.Node, Int)
    withReservation e@(Entity k _) = do
      r <- reservedRamFor state k
      pure (e, r)

    -- Effective free RAM = reported free minus already-reserved.
    -- 'Nothing' (no stats yet) is treated as "accept" — degrades
    -- to the first-online-node fallback at the call site.
    hasEnoughRam reqRam (Entity _ n, reserved) = case M.nodeRamMbFree n of
      Nothing -> True
      Just free -> (free - reserved) >= reqRam + ramSafetyMb

    -- Higher score = better placement. Tie-break by node name
    -- so two equally good nodes resolve deterministically.
    scoreOrder =
      comparing (Down . scoreNode)
        <> comparing (M.nodeName . entityVal . fst)

    scoreNode :: (Entity M.Node, Int) -> Double
    scoreNode (Entity _ n, reserved) =
      let effectiveFree = fromMaybe 0 (M.nodeRamMbFree n) - reserved
          ram = fromIntegral (effectiveFree - requestedRamMb) :: Double
          gib = case M.nodeStorageBytesFree n of
            Nothing -> 0
            Just b -> fromIntegral (b `div` (1024 * 1024 * 1024)) :: Double
          load = fromMaybe 0 (M.nodeLoadAvg1 n) :: Double
       in ram + gib - 100 * load

-- | Predicate: does the named node currently have enough free
-- RAM to host a new VM that needs @requestedRamMb@ MiB?
--
-- 'Left' carries a human-readable diagnostic ("not online",
-- "insufficient RAM") that callers (e.g. migration pre-check)
-- can surface directly to the operator. Reservations and the
-- 'ramSafetyMb' margin are applied exactly as in
-- 'pickNodeForVm'.
hasCapacityFor
  :: (MonadIO m)
  => ServerState
  -> M.NodeId
  -> Int
  -- ^ requested RAM (MiB)
  -> m (Either Text ())
hasCapacityFor state nodeId requestedRamMb = do
  mRow <- liftIO $ runSqlPool (selectList [M.NodeId ==. nodeId] []) (ssDbPool state)
  case mRow of
    [] -> pure (Left ("node " <> tshow (M.fromSqlKey nodeId) <> " not found"))
    (Entity _ node : _)
      | M.nodeAdminState node /= M.NodeOnline ->
          pure (Left ("node " <> M.nodeName node <> " is not online"))
      | otherwise -> do
          reserved <- liftIO $ reservedRamFor state nodeId
          case M.nodeRamMbFree node of
            Nothing -> pure (Right ()) -- no stats yet → optimistic, like the scheduler's pre-push fallback
            Just free ->
              if free - reserved >= requestedRamMb + ramSafetyMb
                then pure (Right ())
                else
                  pure $
                    Left $
                      "node "
                        <> M.nodeName node
                        <> " has insufficient free RAM ("
                        <> tshow (free - reserved)
                        <> " MiB available, need "
                        <> tshow (requestedRamMb + ramSafetyMb)
                        <> " MiB with safety margin)"

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
