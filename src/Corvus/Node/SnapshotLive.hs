{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , deleteSnapshotLive
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
