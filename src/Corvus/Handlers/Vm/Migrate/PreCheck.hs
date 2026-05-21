{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Pre-flight checks for @crv vm migrate@.
--
-- All daemon-side validation that must hold before the migration
-- orchestrator starts moving any bytes. Each failure returns a
-- single human-readable 'Text' suitable to surface directly to
-- the operator.
--
-- The check builds a 'MigrationPlan' along the way so the
-- orchestrator can reuse the work — there's no point computing
-- per-drive "copy or move" decisions twice.
module Corvus.Handlers.Vm.Migrate.PreCheck
  ( MigrationPlan (..)
  , MigrationDriveOp (..)
  , validateMigration
  , storageSafetyMb
  )
where

import qualified Corvus.Handlers.Disk.Db as DDb
import qualified Corvus.Handlers.Scheduler as Sched
import qualified Corvus.Model as M
import Corvus.Types (ServerState (..))
import Data.Foldable (foldl')
import Data.Int (Int64)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (Entity (..), get, selectList, (==.))
import Database.Persist.Postgresql (SqlPersistT, runSqlPool)

-- ---------------------------------------------------------------------------
-- Plan

-- | What needs to happen to one disk image during a VM migration.
data MigrationDriveOp
  = -- | Read-only drive image: copy the bytes, leave source row
    -- in place. Backing chain entries hit this branch too.
    OpCopy !M.DiskImageId
  | -- | Read-write drive image: move (copy bytes, then delete
    -- source-side placement + file).
    OpMove !M.DiskImageId
  deriving (Eq, Show)

-- | Result of a successful pre-check. The orchestrator consumes
-- this to drive the actual transfers.
data MigrationPlan = MigrationPlan
  { mpVmId :: !M.VmId
  , mpSrcNode :: !M.NodeId
  , mpDestNode :: !M.NodeId
  , mpDriveOps :: ![MigrationDriveOp]
  }
  deriving (Eq, Show)

-- | Safety margin on top of the bytes a migration is about to
-- write to the target node's storage. 1 GiB.
storageSafetyMb :: Int64
storageSafetyMb = 1024

-- ---------------------------------------------------------------------------
-- Top-level entry

-- | Run all pre-checks on the (vm, target) pair. Returns either a
-- diagnostic suitable to surface to the operator, or a
-- 'MigrationPlan' the orchestrator can drive.
validateMigration
  :: ServerState
  -> M.VmId
  -> M.NodeId
  -> IO (Either Text MigrationPlan)
validateMigration state vmId destNode = do
  let pool = ssDbPool state
  mVm <- runSqlPool (get vmId) pool
  case mVm of
    Nothing -> pure (Left "VM not found")
    Just vm
      -- Note: the @migrating@ flag is intentionally NOT checked
      -- here. The orchestrator's conditional update has already
      -- acquired the lock by the time it calls this function;
      -- @vm.migrating@ is now @True@. The lock acquisition is
      -- the single source of truth for "another migration is in
      -- progress" — see Corvus.Handlers.Vm.Migrate.handleVmMigrate.
      | M.vmStatus vm /= M.VmStopped ->
          pure (Left "VM must be stopped before migrating")
      | M.vmNodeId vm == destNode ->
          pure (Left "destination is the VM's current node")
      | otherwise -> do
          mDest <- runSqlPool (get destNode) pool
          case mDest of
            Nothing -> pure (Left "destination node not found")
            Just dest
              | M.nodeAdminState dest /= M.NodeOnline ->
                  pure $
                    Left
                      ( "destination node "
                          <> M.nodeName dest
                          <> " is not online"
                      )
              | otherwise -> checkSharedDirsAndNet state vmId vm destNode dest

checkSharedDirsAndNet
  :: ServerState
  -> M.VmId
  -> M.Vm
  -> M.NodeId
  -> M.Node
  -> IO (Either Text MigrationPlan)
checkSharedDirsAndNet state vmId vm destNode dest = do
  let pool = ssDbPool state
  sds <- runSqlPool (selectList [M.SharedDirVmId ==. vmId] []) pool
  case sds of
    (_ : _) ->
      pure $
        Left "VM has shared directories; remove them before migrating (host paths are node-local)"
    [] -> do
      nis <-
        runSqlPool
          (selectList [M.NetworkInterfaceVmId ==. vmId] [])
          pool
      let badIf =
            [ M.networkInterfaceInterfaceType (entityVal e)
            | e <- nis
            , M.networkInterfaceInterfaceType (entityVal e) /= M.NetUser
            ]
      case badIf of
        (t : _) ->
          pure $
            Left $
              "VM has a non-user network interface ("
                <> M.enumToText t
                <> "); only user (SLIRP) or no network is supported"
        [] -> buildPlanFromDrives state vmId vm destNode dest

buildPlanFromDrives
  :: ServerState
  -> M.VmId
  -> M.Vm
  -> M.NodeId
  -> M.Node
  -> IO (Either Text MigrationPlan)
buildPlanFromDrives state vmId vm destNode destRow = do
  let pool = ssDbPool state
      vmRamMb = M.vmRamMb vm
  drives <- runSqlPool (selectList [M.DriveVmId ==. vmId] []) pool
  let primaryOps =
        [ if M.driveReadOnly (entityVal e)
          then OpCopy (M.driveDiskImageId (entityVal e))
          else OpMove (M.driveDiskImageId (entityVal e))
        | e <- drives
        ]
      primaryDiskKeys = map opDiskKey primaryOps
  chains <-
    runSqlPool
      ( mapM
          ( \dkey ->
              fmap (dkey,) (DDb.getBackingChainIds (M.fromSqlKey dkey))
          )
          primaryDiskKeys
      )
      pool
  let chainAncestors = L.nub [a | (_, cs) <- chains, a <- cs]
  ancestorsOnDest <-
    runSqlPool
      ( mapM
          (\dkey -> fmap (dkey,) (DDb.hasPlacementOnNode dkey destNode))
          chainAncestors
      )
      pool
  let missingChain = [d | (d, False) <- ancestorsOnDest]
      chainOps = map OpCopy missingChain
      allOps = primaryOps ++ chainOps
  totalMb <- runSqlPool (sumPlanSizeMb allOps) pool
  let needMb = totalMb + storageSafetyMb
  case M.nodeStorageBytesFree destRow of
    Just bytesFree
      | mbOf bytesFree < needMb ->
          pure $
            Left $
              "destination node has insufficient free storage ("
                <> T.pack (show (mbOf bytesFree))
                <> " MiB available, need "
                <> T.pack (show needMb)
                <> " MiB with safety margin)"
    _ -> do
      rc <- Sched.hasCapacityFor state destNode vmRamMb
      case rc of
        Left err -> pure (Left err)
        Right () ->
          pure $
            Right
              MigrationPlan
                { mpVmId = vmId
                , mpSrcNode = M.vmNodeId vm
                , mpDestNode = destNode
                , mpDriveOps = allOps
                }

-- ---------------------------------------------------------------------------
-- Helpers

opDiskKey :: MigrationDriveOp -> M.DiskImageId
opDiskKey (OpCopy d) = d
opDiskKey (OpMove d) = d

-- | Convert raw byte-count from the Node row into MiB.
mbOf :: Int -> Int64
mbOf bytes = fromIntegral (bytes `div` (1024 * 1024))

-- | Sum the virtual sizes (MiB) of every disk in a plan. Disks
-- without a recorded size contribute 0; that's safest given
-- @qemu-img@'s post-create refresh isn't always synchronous.
sumPlanSizeMb :: [MigrationDriveOp] -> SqlPersistT IO Int64
sumPlanSizeMb ops = do
  sizes <- mapM lookupOne ops
  pure $ foldl' (+) 0 sizes
  where
    lookupOne op = do
      mDisk <- get (opDiskKey op)
      pure $ case mDisk of
        Just d -> maybe 0 fromIntegral (M.diskImageSizeMb d)
        Nothing -> 0

-- Suppress unused-import warning for 'fromMaybe' (kept for
-- future error-message refinements).
_unusedFromMaybe :: Maybe a -> a -> a
_unusedFromMaybe = flip fromMaybe
