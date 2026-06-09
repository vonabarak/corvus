{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Node-agent orchestration for LIVE (online) qcow2 internal snapshots.
--
-- Offline snapshots live in "Corvus.Node.Image" and shell out to
-- @qemu-img snapshot -c/-d/-a@ against the disk file directly,
-- which requires the VM to be stopped (qemu-img grabs an exclusive
-- file lock that collides with the running QEMU's own lock).
--
-- The live path here goes through QMP + QGA on the running VM:
--
--   1. (optional, per 'QuiesceMode') QGA @guest-fsfreeze-freeze@
--      to pause guest filesystem writes — QEMU flushes in-flight
--      writes through to the block layer before the snapshot is
--      stamped, but the guest's own page cache is opaque to QEMU,
--      so without an fsfreeze the snapshot is hard-reset-equivalent
--      for any unflushed in-guest writes.
--   2. QMP @blockdev-snapshot-internal-sync@. Produces an on-disk
--      record bit-identical to @qemu-img snapshot -c@.
--   3. (always, if step 1 ran) QGA @guest-fsfreeze-thaw@. MUST run
--      on every path including snapshot failure — leaving the
--      guest frozen indefinitely wedges in-guest I/O.
--
-- There is NO online equivalent of @qemu-img snapshot -a@
-- (rollback). The daemon's @autoStop@ knob wraps the offline
-- rollback in a stop + revert + start cycle; the runner here does
-- not expose a live rollback function because none exists.
module Corvus.Node.SnapshotLive
  ( QuiesceMode (..)
  , createSnapshotLive
  , createSnapshotLiveMany
  , deleteSnapshotLive
  , createSnapshotWithVmstate
  , loadSnapshotWithVmstate
  , deleteSnapshotWithVmstate
  )
where

import Control.Exception (SomeException, onException, try)
import Corvus.Node.GuestAgent
  ( GuestAgentConns
  , guestFsFreeze
  , guestFsThaw
  , guestPing
  )
import qualified Corvus.Node.Image as NI
import qualified Corvus.Node.Qmp as Qmp
import Corvus.Qemu.Config (QemuConfig)
import Data.Int (Int64)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | Agent-internal mirror of the wire 'Capnp.Gen.Enums.QuiesceMode'.
-- Kept here so this module stays independent of the generated
-- Cap'n Proto bindings — the Session handler in
-- "Corvus.Node.Caps.Session" translates between the two.
data QuiesceMode
  = QuiesceAuto
  | QuiesceRequire
  | QuiesceSkip
  deriving (Eq, Show)

-- | Create a snapshot of a running VM's disk via QMP, bracketing
-- the QMP call with QGA fsfreeze+thaw per the supplied
-- 'QuiesceMode'. Returns the snapshot result plus a flag
-- indicating whether the snapshot was actually quiesced.
--
-- The @path@ argument names the qcow2 file; the agent uses it to
-- resolve the matching QEMU BlockBackend via @query-block@. Pass
-- the same absolute path the daemon would supply to
-- @qemu-img snapshot -c@.
createSnapshotLive
  :: GuestAgentConns
  -> QemuConfig
  -> Int64
  -- ^ VM ID
  -> Text
  -- ^ snapshot name
  -> FilePath
  -- ^ qcow2 file path on this node
  -> QuiesceMode
  -- ^ whether to bracket with QGA fsfreeze
  -> IO (NI.ImageResult, Bool)
createSnapshotLive qgaConns qcfg vmId snapName path mode = do
  eDevice <- Qmp.qmpFindBlockDeviceByPath qcfg vmId path
  case eDevice of
    Left err -> pure (NI.ImageError err, False)
    Right device -> do
      willFreeze <- decideQuiesce qgaConns qcfg vmId mode
      case willFreeze of
        Left err -> pure (NI.ImageError err, False)
        Right shouldFreeze -> do
          if shouldFreeze
            then do
              eFreeze <- guestFsFreeze qgaConns qcfg vmId
              case eFreeze of
                Left err -> case mode of
                  QuiesceRequire ->
                    pure
                      ( NI.ImageError ("guest-fsfreeze-freeze failed: " <> err)
                      , False
                      )
                  -- 'auto' tolerates a freeze error; snapshot
                  -- without quiesce. (And there's nothing to thaw
                  -- because freeze didn't actually succeed.)
                  _ -> do
                    res <- snapAndClassify qcfg vmId device snapName
                    pure (res, False)
                Right _frozenCount ->
                  -- Freeze succeeded; the thaw MUST run before we
                  -- return. 'onException' runs the thaw on async
                  -- exception, then we run it explicitly on the
                  -- normal path too.
                  runWithThaw qgaConns qcfg vmId $ do
                    res <- snapAndClassify qcfg vmId device snapName
                    pure (res, True)
            else do
              res <- snapAndClassify qcfg vmId device snapName
              pure (res, False)

-- | Atomic multi-disk live snapshot. Wraps N
-- @blockdev-snapshot-internal-sync@ actions in a single QMP
-- @transaction@; QGA fsfreeze (per 'QuiesceMode') brackets the
-- whole transaction so all disks see the same on-disk-stable
-- guest filesystem state. Returns the result plus a flag indicating
-- whether the snapshot was actually quiesced.
--
-- An empty path list is treated as a no-op success; the caller
-- has already decided no writable disks are eligible.
createSnapshotLiveMany
  :: GuestAgentConns
  -> QemuConfig
  -> Int64
  -- ^ VM ID
  -> Text
  -- ^ snapshot name (same for every disk)
  -> [FilePath]
  -- ^ qcow2 file paths on this node
  -> QuiesceMode
  -> IO (NI.ImageResult, Bool)
createSnapshotLiveMany _ _ _ _ [] _ = pure (NI.ImageSuccess, False)
createSnapshotLiveMany qgaConns qcfg vmId snapName paths mode = do
  eDevs <- resolveDevices qcfg vmId paths
  case eDevs of
    Left err -> pure (NI.ImageError err, False)
    Right devices -> do
      let pairs = map (,snapName) devices
      willFreeze <- decideQuiesce qgaConns qcfg vmId mode
      case willFreeze of
        Left err -> pure (NI.ImageError err, False)
        Right shouldFreeze ->
          if shouldFreeze
            then do
              eFreeze <- guestFsFreeze qgaConns qcfg vmId
              case eFreeze of
                Left err -> case mode of
                  QuiesceRequire ->
                    pure
                      ( NI.ImageError ("guest-fsfreeze-freeze failed: " <> err)
                      , False
                      )
                  _ -> do
                    res <- snapManyAndClassify qcfg vmId pairs
                    pure (res, False)
                Right _frozenCount ->
                  runWithThaw qgaConns qcfg vmId $ do
                    res <- snapManyAndClassify qcfg vmId pairs
                    pure (res, True)
            else do
              res <- snapManyAndClassify qcfg vmId pairs
              pure (res, False)

resolveDevices :: QemuConfig -> Int64 -> [FilePath] -> IO (Either Text [Text])
resolveDevices _ _ [] = pure (Right [])
resolveDevices cfg vmId (p : rest) = do
  e <- Qmp.qmpFindBlockDeviceByPath cfg vmId p
  case e of
    Left err -> pure (Left err)
    Right dev -> do
      r <- resolveDevices cfg vmId rest
      case r of
        Left err -> pure (Left err)
        Right ds -> pure (Right (dev : ds))

snapManyAndClassify
  :: QemuConfig -> Int64 -> [(Text, Text)] -> IO NI.ImageResult
snapManyAndClassify cfg vmId pairs = do
  r <- Qmp.qmpBlockSnapshotCreateMany cfg vmId pairs
  pure (classifyQmpResult r)

-- | Delete a snapshot from a running VM via QMP. No QGA freeze
-- needed — snapshot deletion only edits the qcow2 snapshot table,
-- it doesn't interact with in-flight guest I/O.
deleteSnapshotLive
  :: QemuConfig
  -> Int64
  -- ^ VM ID
  -> Text
  -- ^ snapshot name
  -> FilePath
  -- ^ qcow2 file path on this node
  -> IO NI.ImageResult
deleteSnapshotLive qcfg vmId snapName path = do
  eDevice <- Qmp.qmpFindBlockDeviceByPath qcfg vmId path
  case eDevice of
    Left err -> pure (NI.ImageError err)
    Right device -> do
      r <- Qmp.qmpBlockSnapshotDelete qcfg vmId device snapName
      pure (classifyQmpResult r)

-- ---------------------------------------------------------------------------
-- Full-machine snapshots (vmstate + block via snapshot-save/-load/-delete)

-- | Create a full-machine snapshot of a running VM: vmstate (RAM +
-- device model + CPU state) into the qcow2 of the carrier disk,
-- block snapshots into every disk in @paths@ (which MUST include
-- the carrier). Atomic on the QEMU side via a single
-- @snapshot-save@ async job.
--
-- No QGA fsfreeze bracketing — vmstate captures the in-flight page
-- cache and writeback queue, so the snapshot is inherently
-- guest-consistent. A multi-second freeze under a vmstate save
-- would be both unnecessary and harmful.
--
-- Capability-probes via @query-commands@ first; refuses cleanly
-- on QEMU < 6.0 (where @snapshot-save@ doesn't exist).
createSnapshotWithVmstate
  :: QemuConfig
  -> Int64
  -- ^ VM ID
  -> Text
  -- ^ snapshot tag (shared by carrier + sibling block snapshots)
  -> FilePath
  -- ^ vmstate carrier disk path
  -> [FilePath]
  -- ^ all writable disk paths to snapshot (must include carrier)
  -> IO NI.ImageResult
createSnapshotWithVmstate qcfg vmId tag vmstatePath paths =
  withVmstateCapability qcfg vmId $ do
    eDevs <- resolveNodesAndCarrier qcfg vmId vmstatePath paths
    case eDevs of
      Left err -> pure (NI.ImageError err)
      Right (carrierNode, nodes) -> do
        r <- Qmp.qmpSnapshotSave qcfg vmId tag carrierNode nodes
        pure (eitherToImageResult r)

-- | Load a full-machine snapshot back into a running QEMU process.
-- The caller MUST ensure the VM is paused (QMP @stop@) before
-- invoking — @snapshot-load@ refuses to run with the CPUs live.
-- The caller is ALSO responsible for issuing @cont@ once any
-- post-load setup (clock resync, QGA handshake) has run; this
-- function returns as soon as the load job concludes.
loadSnapshotWithVmstate
  :: QemuConfig
  -> Int64
  -- ^ VM ID
  -> Text
  -- ^ snapshot tag (must match the snapshot-save call)
  -> FilePath
  -- ^ vmstate carrier disk path
  -> [FilePath]
  -- ^ all disk paths that participated in the save
  -> IO NI.ImageResult
loadSnapshotWithVmstate qcfg vmId tag vmstatePath paths =
  withVmstateCapability qcfg vmId $ do
    eDevs <- resolveNodesAndCarrier qcfg vmId vmstatePath paths
    case eDevs of
      Left err -> pure (NI.ImageError err)
      Right (carrierNode, nodes) -> do
        r <- Qmp.qmpSnapshotLoad qcfg vmId tag carrierNode nodes
        pure (eitherToImageResult r)

-- | Delete a full-machine snapshot. Removes both the vmstate AND
-- the sibling block snapshots under one @tag@ atomically. Routed
-- through the async @snapshot-delete@ job because
-- @blockdev-snapshot-delete-internal-sync@ would leave vmstate
-- orphaned in the carrier qcow2.
deleteSnapshotWithVmstate
  :: QemuConfig
  -> Int64
  -- ^ VM ID
  -> Text
  -- ^ snapshot tag
  -> [FilePath]
  -- ^ all disk paths that participated in the snapshot
  -> IO NI.ImageResult
deleteSnapshotWithVmstate qcfg vmId tag paths =
  withVmstateCapability qcfg vmId $ do
    eDevs <- resolveBlockNodes qcfg vmId paths
    case eDevs of
      Left err -> pure (NI.ImageError err)
      Right nodes -> do
        r <- Qmp.qmpSnapshotDelete qcfg vmId tag nodes
        pure (eitherToImageResult r)

-- | Gate any vmstate-aware QMP op on the running QEMU actually
-- advertising @snapshot-save@. Saves the caller a confusing
-- @CommandNotFound@ trip through the job lifecycle on QEMU < 6.0.
withVmstateCapability
  :: QemuConfig
  -> Int64
  -> IO NI.ImageResult
  -> IO NI.ImageResult
withVmstateCapability qcfg vmId k = do
  cmds <- Qmp.qmpQueryCommands qcfg vmId
  case cmds of
    Left err ->
      pure $
        NI.ImageError ("QMP query-commands failed: " <> err)
    Right names
      | Set.member "snapshot-save" names -> k
      | otherwise ->
          pure $
            NI.ImageError
              "QEMU on this node does not support snapshot-save / snapshot-load \
              \(requires QEMU >= 6.0). Use disk-only snapshots instead."

-- | Resolve a list of disk paths to their BlockDriverState
-- @node-name@s. Stops at the first lookup failure and surfaces
-- the agent's error message verbatim.
resolveBlockNodes :: QemuConfig -> Int64 -> [FilePath] -> IO (Either Text [Text])
resolveBlockNodes _ _ [] = pure (Right [])
resolveBlockNodes cfg vmId (p : rest) = do
  e <- Qmp.qmpFindBlockNodeByPath cfg vmId p
  case e of
    Left err -> pure (Left err)
    Right node -> do
      r <- resolveBlockNodes cfg vmId rest
      case r of
        Left err -> pure (Left err)
        Right ns -> pure (Right (node : ns))

-- | Resolve the carrier + sibling list to block-node names and
-- return @(carrier, allNodes)@. Enforces the invariant that the
-- carrier path appears in the device list — the QMP commands
-- require it.
resolveNodesAndCarrier
  :: QemuConfig
  -> Int64
  -> FilePath
  -> [FilePath]
  -> IO (Either Text (Text, [Text]))
resolveNodesAndCarrier cfg vmId carrier devices
  | carrier `notElem` devices =
      pure $
        Left
          ( "vmstate carrier disk "
              <> T.pack carrier
              <> " must appear in the snapshotted disk list"
          )
  | otherwise = do
      eCarrier <- Qmp.qmpFindBlockNodeByPath cfg vmId carrier
      eDevs <- resolveBlockNodes cfg vmId devices
      pure $ case (eCarrier, eDevs) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right c, Right ds) -> Right (c, ds)

eitherToImageResult :: Either Text () -> NI.ImageResult
eitherToImageResult (Right ()) = NI.ImageSuccess
eitherToImageResult (Left err) = NI.ImageError err

-- ---------------------------------------------------------------------------
-- Internals

-- | Resolve whether the caller's 'QuiesceMode' should result in
-- an actual freeze given this VM's runtime state. 'Right True'
-- means freeze; 'Right False' means skip; 'Left' means the mode
-- demanded a freeze but the guest agent isn't reachable.
--
-- Probes QGA via 'guestPing'; the probe doubles as a way to
-- detect a wedged or disabled-from-spec guest agent.
decideQuiesce
  :: GuestAgentConns -> QemuConfig -> Int64 -> QuiesceMode -> IO (Either Text Bool)
decideQuiesce _ _ _ QuiesceSkip = pure (Right False)
decideQuiesce conns cfg vmId mode = do
  ok <- guestPing conns cfg vmId
  pure $ case (mode, ok) of
    (QuiesceRequire, False) ->
      Left
        ( "quiesce=require but the guest agent is not reachable for VM "
            <> T.pack (show vmId)
        )
    (QuiesceRequire, True) -> Right True
    (QuiesceAuto, True) -> Right True
    (QuiesceAuto, False) -> Right False
    (QuiesceSkip, _) -> Right False

-- | Run the action; guarantee thaw runs whether the action
-- succeeded, returned an error result, or threw.
--
-- Note: 'snapAndClassify' itself catches QMP-level failures and
-- returns them as 'ImageError', so the 'onException' branch only
-- catches genuinely unexpected exceptions (e.g. async
-- cancellation). We run 'guestFsThaw' on both paths to satisfy
-- the "thaw is mandatory" invariant.
runWithThaw
  :: GuestAgentConns
  -> QemuConfig
  -> Int64
  -> IO (NI.ImageResult, Bool)
  -> IO (NI.ImageResult, Bool)
runWithThaw conns cfg vmId action = do
  result <- action `onException` thawIgnoringError conns cfg vmId
  -- Normal path: thaw and merge any thaw-side error into the
  -- result. If the snapshot succeeded but thaw failed, surface
  -- the thaw error — the operator needs to see "guest may be
  -- wedged."
  thawRes <- guestFsThaw conns cfg vmId
  case (result, thawRes) of
    ((NI.ImageSuccess, q), Left thawErr) ->
      pure
        ( NI.ImageError
            ( "snapshot succeeded but guest-fsfreeze-thaw failed — "
                <> "guest may be wedged: "
                <> thawErr
            )
        , q
        )
    ((NI.ImageError msg, q), Left thawErr) ->
      pure
        ( NI.ImageError
            (msg <> "; additionally guest-fsfreeze-thaw failed: " <> thawErr)
        , q
        )
    (r, _) -> pure r

-- | Best-effort thaw used in the async-exception path. Any error
-- is swallowed — the outer exception is more important to
-- preserve.
thawIgnoringError :: GuestAgentConns -> QemuConfig -> Int64 -> IO ()
thawIgnoringError conns cfg vmId = do
  _ <- try (guestFsThaw conns cfg vmId) :: IO (Either SomeException (Either Text Int))
  pure ()

snapAndClassify
  :: QemuConfig -> Int64 -> Text -> Text -> IO NI.ImageResult
snapAndClassify cfg vmId device snapName = do
  r <- Qmp.qmpBlockSnapshotCreate cfg vmId device snapName
  pure (classifyQmpResult r)

-- | Translate the QMP-side 'QmpResult' into the existing
-- 'ImageResult' shape that the daemon-side handlers
-- pattern-match against. Both error paths funnel into
-- 'ImageError'; @ImageFormatNotSupported@ doesn't apply because
-- the file is qcow2 by construction (QEMU is running it).
classifyQmpResult :: Qmp.QmpResult -> NI.ImageResult
classifyQmpResult Qmp.QmpSuccess = NI.ImageSuccess
classifyQmpResult (Qmp.QmpError msg) = NI.ImageError msg
classifyQmpResult (Qmp.QmpConnectionFailed msg) =
  NI.ImageError ("QMP connect failed: " <> msg)
