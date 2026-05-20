{-# LANGUAGE OverloadedStrings #-}

-- | Allocate AF_VSOCK CIDs for QEMU vhost-vsock-pci devices.
--
-- Iterates the configured @qcVsockCidMin..qcVsockCidMax@ range and
-- skips:
--
--   * CIDs already recorded against a VM on the target node, and
--   * CIDs currently held by some other process on that node's
--     host kernel (probed via the nodeagent's @probeVsockCid@ RPC,
--     which ioctls @VHOST_VSOCK_SET_GUEST_CID@ against
--     @/dev/vhost-vsock@).
--
-- The first CID that passes both checks wins.
--
-- Phase 6 made this per-node: the daemon doesn't probe its own
-- kernel (the agent may be on a different host), and the DB
-- filter is keyed on the target node's id rather than scanning
-- every VM cluster-wide.
module Corvus.Node.VsockCid
  ( allocateVsockCid
  , withAllocatedVsockCid
  , isHostFree
  , hostHasVhostVsock
  )
where

import Control.Concurrent.MVar (withMVar)
import Data.Maybe (mapMaybe)
import Data.Pool (Pool)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (Entity (..), selectList, (==.))
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend)
import Foreign.C.Types (CInt (..), CULLong (..))
import System.Directory (doesPathExist)

import qualified Corvus.Model as M
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Qemu.Config (QemuConfig (..))
import Corvus.Types
  ( ServerState (..)
  , lookupNodeAgent
  , vsockCidLockFor
  )

foreign import ccall unsafe "corvus_vsock_cid_available"
  c_vsock_cid_available :: CULLong -> IO CInt

-- | Allocate a free AF_VSOCK CID on the given node. Returns
-- 'Left' with an operator-facing message when the configured
-- range is exhausted or the node's nodeagent is unreachable
-- (so the kernel probe can't run).
--
-- Callers that persist the result should prefer
-- 'withAllocatedVsockCid' — bare 'allocateVsockCid' is racey
-- because the read-filter-probe step is not atomic with the
-- caller's later persist step.
allocateVsockCid :: ServerState -> M.NodeId -> IO (Either Text Int)
allocateVsockCid state nid = do
  let cfg = ssQemuConfig state
      lo = qcVsockCidMin cfg
      hi = qcVsockCidMax cfg
  reserved <- dbAllocatedCids (ssDbPool state) nid
  mAgent <- lookupNodeAgent state nid
  case mAgent of
    Left err -> pure (Left err)
    Right nac -> do
      let candidates = filter (`Set.notMember` reserved) [lo .. hi]
      tryCids nac candidates
  where
    tryCids _ [] = pure $ Left "no free vsock CID in configured range"
    tryCids nac (c : cs) = do
      r <- NOA.probeVsockCid nac c
      case r of
        -- Agent probe failed (RPC error, no kernel module, etc.):
        -- bubble up so the caller decides whether to fall back to
        -- "no CID at all" (hosts without vhost-vsock).
        Left _ -> pure $ Left ("vsock probe failed on node " <> T.pack (show c))
        Right True -> pure (Right c)
        Right False -> tryCids nac cs

-- | Allocate a CID and persist it atomically.
--
-- Holds the per-node 'MVar' lock across the allocator (DB read
-- + agent probe) AND the caller's @persist@ action, so
-- concurrent callers targeting the same node always see each
-- other's writes when choosing candidates. Calls against
-- different nodes don't contend.
withAllocatedVsockCid
  :: ServerState
  -> M.NodeId
  -> (Int -> IO a)
  -- ^ Persist the freshly allocated CID. Runs while the lock is
  -- held; its return value flows back to the caller.
  -> IO (Either Text a)
withAllocatedVsockCid state nid persist = do
  lk <- vsockCidLockFor state nid
  withMVar lk $ \_ -> do
    eCid <- allocateVsockCid state nid
    case eCid of
      Left err -> pure (Left err)
      Right cid -> Right <$> persist cid

-- | Does this host even have @/dev/vhost-vsock@?
--
-- Some kernels (notably nested-virt test images) ship without
-- @vhost_vsock@. Only used by the *agent* now — the daemon
-- routes through 'NOA.probeVsockCid' which already returns
-- @true@ when the device is missing.
hostHasVhostVsock :: IO Bool
hostHasVhostVsock = doesPathExist "/dev/vhost-vsock"

-- | Probe the host kernel for an unused guest CID.
--
-- Used by the agent's @probeVsockCid@ RPC implementation;
-- the daemon never calls this directly. Returns @True@ when
-- the CID is free or when the probe itself failed (e.g.
-- @/dev/vhost-vsock@ is missing).
isHostFree :: Int -> IO Bool
isHostFree cid = do
  rc <- c_vsock_cid_available (fromIntegral cid)
  pure $ rc /= 0 -- 1 = available, -1 = probe failed → assume free

-- | Collect CIDs already assigned to other VMs on the same
-- node. The CID stays bound to the VM row for its lifetime —
-- unlike SPICE ports, the daemon does not clear it on stop,
-- because re-allocating the same CID across restarts is exactly
-- what users want when pasting the @ssh user\@vsock\/CID@
-- command from @crv vm show@.
--
-- Phase 6 scopes the filter to a single 'NodeId': CIDs are
-- per-kernel-unique, and the kernel only sees VMs on the
-- agent's host.
dbAllocatedCids :: Pool SqlBackend -> M.NodeId -> IO (Set.Set Int)
dbAllocatedCids pool nid = do
  rows <- runSqlPool (selectList [M.VmNodeId ==. nid] []) pool
  pure $ Set.fromList $ mapMaybe (M.vmVsockCid . entityVal) (rows :: [Entity M.Vm])
