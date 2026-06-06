{-# LANGUAGE BangPatterns #-}

-- | Database I/O for the build-step cache. The table itself
-- ('BuildCacheEntry') lives in 'Corvus.Model'; this module owns the
-- lookup pattern, the insert pattern, and the @bakeVmHasCache@
-- query used by the cleanup stack to decide whether to reap a bake
-- VM.
--
-- One row per (pipelineKey, chainHash, diskRole) is the table's
-- unique invariant. Concurrent builds of the same YAML may race on
-- the same chain hash; the @UniqueBuildCacheChainDisk@ index turns
-- the conflict into a 'PersistError' which 'recordCacheStep' surfaces
-- to the caller. We treat that as best-effort — the cache is a
-- correctness-neutral optimisation, not source of truth.
module Corvus.Build.Cache.Store
  ( -- * Lookup
    CacheLookup (..)
  , lookupCachePrefix

    -- * Recording
  , recordCacheStep

    -- * Bake-VM gating
  , bakeVmHasCache

    -- * Idempotency
  , cacheStepRoles

    -- * Stale-row cleanup
  , purgeCacheRowsForVm
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Types (ServerState (..))
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Sql (SqlPersistT, runSqlPool, toSqlKey)

--------------------------------------------------------------------------------
-- Lookup
--------------------------------------------------------------------------------

-- | Result of resolving a chain against the cache table.
data CacheLookup = CacheLookup
  { clPrefix :: !Int
  -- ^ Length of the matched prefix (0 == no usable cache).
  , clVmId :: !(Maybe VmId)
  -- ^ The bake VM that owns the cached chain. 'Nothing' when @clPrefix@ is 0.
  , clChainHashOfPrefix :: !(Maybe Text)
  -- ^ The chain hash of step @clPrefix@ — i.e. the snapshot we roll
  -- back to before resuming. 'Nothing' when @clPrefix@ is 0.
  }
  deriving (Eq, Show)

-- | Find the longest prefix of @chainHashes@ for which the cache
-- holds a row for every required disk role. The chain's VM id must
-- agree across the whole match (a single bake VM per pipeline); if
-- it doesn't, treat the inconsistency as \"no cache\" and fall back
-- to a fresh bake.
lookupCachePrefix
  :: ServerState
  -> Text
  -- ^ pipelineKey
  -> [Text]
  -- ^ chain hashes, left-to-right (one per provisioner step)
  -> [Text]
  -- ^ required disk roles for this strategy
  -> IO CacheLookup
lookupCachePrefix _ _ [] _ = pure (CacheLookup 0 Nothing Nothing)
lookupCachePrefix _ _ _ [] = pure (CacheLookup 0 Nothing Nothing)
lookupCachePrefix state pipelineKey chains required = do
  rows <-
    runSqlPool
      ( selectList
          [ M.BuildCacheEntryPipelineKey ==. pipelineKey
          , M.BuildCacheEntryChainHash <-. chains
          ]
          []
      )
      (ssDbPool state)
  let byChain :: Map.Map Text [Entity M.BuildCacheEntry]
      byChain =
        Map.fromListWith
          (++)
          [(M.buildCacheEntryChainHash (entityVal e), [e]) | e <- rows]
  pure (walk byChain chains 0 Nothing Nothing)
  where
    walk _ [] !k vmOpt hashOpt = CacheLookup k vmOpt hashOpt
    walk m (h : rest) !k vmOpt hashOpt =
      case Map.lookup h m of
        Nothing -> CacheLookup k vmOpt hashOpt
        Just entries
          | not (allRolesPresent entries) -> CacheLookup k vmOpt hashOpt
          | otherwise ->
              case sharedVmId entries of
                Nothing -> CacheLookup k vmOpt hashOpt
                Just vmId
                  | consistent vmOpt vmId ->
                      walk m rest (k + 1) (Just vmId) (Just h)
                  | otherwise -> CacheLookup k vmOpt hashOpt

    allRolesPresent entries =
      let roles = [M.buildCacheEntryDiskRole (entityVal e) | e <- entries]
       in all (`elem` roles) required

    sharedVmId :: [Entity M.BuildCacheEntry] -> Maybe VmId
    sharedVmId entries =
      case map (M.buildCacheEntryVmId . entityVal) entries of
        [] -> Nothing
        (v : vs)
          | all (== v) vs -> Just v
          | otherwise -> Nothing

    consistent :: Maybe VmId -> VmId -> Bool
    consistent Nothing _ = True
    consistent (Just prev) v = prev == v

--------------------------------------------------------------------------------
-- Recording
--------------------------------------------------------------------------------

-- | Insert one 'BuildCacheEntry' row per (diskRole, snapshotId) pair
-- in a single transaction. Caller must have already created the
-- 'Snapshot' rows (one per writable disk on the bake VM) and the
-- qcow2-side snapshots via the multi-disk QMP transaction; this
-- function only writes the cache index that ties the chain hash to
-- the snapshots.
recordCacheStep
  :: ServerState
  -> Text
  -- ^ pipelineKey
  -> Int
  -- ^ 1-based step index
  -> Text
  -- ^ chainHash through this step inclusive
  -> Int64
  -- ^ bake VM id (the row owner)
  -> [(Text, Int64)]
  -- ^ @(diskRole, snapshotId)@ pairs
  -> IO ()
recordCacheStep state pipelineKey stepIdx chain vmId pairs =
  runSqlPool (recordCacheStepSql pipelineKey stepIdx chain vmId pairs) (ssDbPool state)

recordCacheStepSql
  :: Text
  -> Int
  -> Text
  -> Int64
  -> [(Text, Int64)]
  -> SqlPersistT IO ()
recordCacheStepSql pipelineKey stepIdx chain vmId pairs = do
  now <- liftIO getCurrentTime
  forM_ pairs $ \(role, snapId) ->
    insert_
      M.BuildCacheEntry
        { M.buildCacheEntryPipelineKey = pipelineKey
        , M.buildCacheEntryStepIndex = stepIdx
        , M.buildCacheEntryChainHash = chain
        , M.buildCacheEntryDiskRole = role
        , M.buildCacheEntrySnapshotId = toSqlKey snapId
        , M.buildCacheEntryVmId = toSqlKey vmId
        , M.buildCacheEntryCreatedAt = now
        }

--------------------------------------------------------------------------------
-- Bake-VM gating
--------------------------------------------------------------------------------

-- | Return every @diskRole@ already present in the cache for the
-- given @(pipelineKey, chainHash)@ pair. Used by the per-step write
-- path to make snapshot creation idempotent: if a concurrent runner
-- has already written rows for every required role, this run skips
-- the snapshot and the insert rather than racing on the unique
-- constraint (which would leave a partial qcow2 snapshot orphaned).
cacheStepRoles :: ServerState -> Text -> Text -> IO [Text]
cacheStepRoles state pipelineKey chain = do
  rows <-
    runSqlPool
      ( selectList
          [ M.BuildCacheEntryPipelineKey ==. pipelineKey
          , M.BuildCacheEntryChainHash ==. chain
          ]
          []
      )
      (ssDbPool state)
  pure [M.buildCacheEntryDiskRole (entityVal e) | e <- rows]

-- | Drop every 'BuildCacheEntry' row that pins the given bake VM.
-- Used by the cache-resume path when reuse failed (cached VM gone,
-- snapshot deleted out-of-band, rollback error): the rows are now
-- known-stale and would only trip up a future @--use-cache@ run.
-- The function does NOT touch the 'Vm' row or any 'Snapshot' rows;
-- those have their own lifecycles ('Snapshot' rows are cascaded
-- away by 'deleteDiskAndSnapshots' when a disk is dropped).
purgeCacheRowsForVm :: ServerState -> Int64 -> IO ()
purgeCacheRowsForVm state vmId =
  runSqlPool
    (deleteWhere [M.BuildCacheEntryVmId ==. (toSqlKey vmId :: VmId)])
    (ssDbPool state)

-- | True iff at least one cache row pins the given bake VM. Used by
-- the cleanup stack to skip 'VmDelete' for a bake VM that the cache
-- still references.
bakeVmHasCache :: ServerState -> Int64 -> IO Bool
bakeVmHasCache state vmId = do
  rows <-
    runSqlPool
      ( selectList
          [M.BuildCacheEntryVmId ==. (toSqlKey vmId :: VmId)]
          [LimitTo 1]
      )
      (ssDbPool state)
  pure (not (null rows))
