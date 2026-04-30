{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Allocate AF_VSOCK CIDs for QEMU vhost-vsock-pci devices.
--
-- Iterates the configured @qcVsockCidMin..qcVsockCidMax@ range and
-- skips:
--
--   * CIDs already recorded against a VM in the database, and
--   * CIDs currently held by some other process on the host kernel
--     (probed via @/dev/vhost-vsock@ + @VHOST_VSOCK_SET_GUEST_CID@).
--
-- The first CID that passes both checks wins.
--
-- The host probe matters because the kernel enforces CID uniqueness
-- across all @vhost-vsock@ users on the machine, not just within one
-- daemon's database. Without it, two daemons (or two parallel test
-- processes) sharing a host can independently allocate the same CID
-- from their own DBs and then race at QEMU start time, with the
-- losing VM dropping into 'VmError'.
module Corvus.Qemu.VsockCid
  ( allocateVsockCid
  , isHostFree
  , hostHasVhostVsock
  )
where

import Data.Maybe (mapMaybe)
import Data.Pool (Pool)
import qualified Data.Set as Set
import Data.Text (Text)
import Database.Persist (Entity (..), selectList)
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend)
import Foreign.C.Types (CInt (..), CULLong (..))
import System.Directory (doesPathExist)

import qualified Corvus.Model as M
import Corvus.Qemu.Config (QemuConfig (..))
import Corvus.Types (ServerState (..))

foreign import ccall unsafe "corvus_vsock_cid_available"
  c_vsock_cid_available :: CULLong -> IO CInt

-- | Allocate a free AF_VSOCK CID. Returns 'Left' with an
-- operator-facing message when the configured range is exhausted.
allocateVsockCid :: ServerState -> IO (Either Text Int)
allocateVsockCid state = do
  let cfg = ssQemuConfig state
      lo = qcVsockCidMin cfg
      hi = qcVsockCidMax cfg
  reserved <- dbAllocatedCids (ssDbPool state)
  let candidates = filter (`Set.notMember` reserved) [lo .. hi]
  tryCids candidates
  where
    tryCids [] = pure $ Left "no free vsock CID in configured range"
    tryCids (c : cs) = do
      ok <- isHostFree c
      if ok then pure (Right c) else tryCids cs

-- | Does this host even have @\/dev\/vhost-vsock@?
--
-- Some kernels (notably nested-virt test images) ship without
-- @vhost_vsock@. We don't want to attach a vhost-vsock-pci device to a
-- VM that QEMU then can't open — so callers check this before
-- allocating a CID and store @vsockCid = Nothing@ on hosts where the
-- kernel module is unavailable.
hostHasVhostVsock :: IO Bool
hostHasVhostVsock = doesPathExist "/dev/vhost-vsock"

-- | Probe the host kernel for an unused guest CID.
--
-- Returns @True@ when the CID is free or when the probe itself
-- failed (e.g. @/dev/vhost-vsock@ is missing or unreadable). The
-- "probe-failed → assume free" fallback keeps the allocator working
-- on hosts where the device is unavailable; in that case, QEMU's own
-- start-time check is the backstop.
isHostFree :: Int -> IO Bool
isHostFree cid = do
  rc <- c_vsock_cid_available (fromIntegral cid)
  pure $ rc /= 0 -- 1 = available, -1 = probe failed → assume free

-- | Collect CIDs already assigned to other VMs. The CID stays bound
-- to the VM row for its lifetime — unlike SPICE ports, the daemon
-- does not clear it on stop, because re-allocating the same CID
-- across restarts is exactly what users want when pasting the
-- @ssh user\@vsock\/CID@ command from @crv vm show@.
dbAllocatedCids :: Pool SqlBackend -> IO (Set.Set Int)
dbAllocatedCids pool = do
  rows <- runSqlPool (selectList [] []) pool
  pure $ Set.fromList $ mapMaybe (M.vmVsockCid . entityVal) (rows :: [Entity M.Vm])
