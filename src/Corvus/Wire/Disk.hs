{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

-- | Cap'n Proto conversion for disk and snapshot info structs.
module Corvus.Wire.Disk
  ( toCapnpDiskImageInfo
  , fromCapnpDiskImageInfo
  , toCapnpSnapshotInfo
  , toCapnpDiskCleanupReport
  , fromCapnpDiskCleanupReport
  , fromCapnpSnapshotInfo
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Disk as CGDisk
import qualified Corvus.Protocol.Disk as P
import Corvus.Wire.Common
  ( fromCapnpNamedRef
  , fromCapnpNamedRefOpt
  , toCapnpNamedRef
  , toCapnpNamedRefOpt
  )
import Corvus.Wire.Enums (fromCapnpDriveFormat, toCapnpDriveFormat)
import Corvus.Wire.Errors (WireError)
import Corvus.Wire.Time (nanosToUtcTime, utcTimeToNanos)

-- ---------------------------------------------------------------------
-- Disk image
-- ---------------------------------------------------------------------

toCapnpDiskImageInfo :: P.DiskImageInfo -> C.Parsed CGDisk.DiskImageInfo
toCapnpDiskImageInfo P.DiskImageInfo {..} =
  CGDisk.DiskImageInfo
    { CGDisk.id = diiId
    , CGDisk.name = diiName
    , CGDisk.tags = diiTags
    , CGDisk.placements = map mkPlacement diiPlacements
    , CGDisk.format = toCapnpDriveFormat diiFormat
    , CGDisk.size = maybe 0 fromIntegral diiSize
    , CGDisk.createdAt = utcTimeToNanos diiCreatedAt
    , CGDisk.attachedTo = map mkAttachment diiAttachedTo
    , CGDisk.backingImage = toCapnpNamedRefOpt diiBackingImage
    , CGDisk.ephemeral = diiEphemeral
    }
  where
    mkAttachment vmRef =
      CGDisk.DiskAttachment {CGDisk.vm = toCapnpNamedRef vmRef}
    mkPlacement p =
      CGDisk.DiskImagePlacement
        { CGDisk.node = toCapnpNamedRef (P.dipNode p)
        , CGDisk.filePath = P.dipFilePath p
        }

fromCapnpDiskImageInfo :: C.Parsed CGDisk.DiskImageInfo -> Either WireError P.DiskImageInfo
fromCapnpDiskImageInfo CGDisk.DiskImageInfo {..} = do
  format' <- fromCapnpDriveFormat format
  pure
    P.DiskImageInfo
      { P.diiId = id
      , P.diiName = name
      , P.diiTags = tags
      , P.diiPlacements =
          [ P.DiskImagePlacement
            { P.dipNode = fromCapnpNamedRef nodeRef
            , P.dipFilePath = fp
            }
          | CGDisk.DiskImagePlacement {CGDisk.node = nodeRef, CGDisk.filePath = fp} <-
              placements
          ]
      , P.diiFormat = format'
      , P.diiSize = if size == 0 then Nothing else Just (fromIntegral size)
      , P.diiCreatedAt = nanosToUtcTime createdAt
      , P.diiAttachedTo = [fromCapnpNamedRef (CGDisk.vm a) | a <- attachedTo]
      , P.diiBackingImage = fromCapnpNamedRefOpt backingImage
      , P.diiEphemeral = ephemeral
      }

-- ---------------------------------------------------------------------
-- Snapshot
-- ---------------------------------------------------------------------

toCapnpSnapshotInfo :: P.SnapshotInfo -> C.Parsed CGDisk.SnapshotInfo
toCapnpSnapshotInfo P.SnapshotInfo {..} =
  CGDisk.SnapshotInfo
    { CGDisk.id = sniId
    , CGDisk.name = sniName
    , CGDisk.createdAt = utcTimeToNanos sniCreatedAt
    , CGDisk.size = maybe 0 fromIntegral sniSize
    , CGDisk.live = sniLive
    , CGDisk.quiesced = sniQuiesced
    , CGDisk.hasVmstate = sniHasVmstate
    }

fromCapnpSnapshotInfo :: C.Parsed CGDisk.SnapshotInfo -> P.SnapshotInfo
fromCapnpSnapshotInfo CGDisk.SnapshotInfo {..} =
  P.SnapshotInfo
    { P.sniId = id
    , P.sniName = name
    , P.sniCreatedAt = nanosToUtcTime createdAt
    , P.sniSize = if size == 0 then Nothing else Just (fromIntegral size)
    , P.sniLive = live
    , P.sniQuiesced = quiesced
    , P.sniHasVmstate = hasVmstate
    }

toCapnpDiskCleanupReport :: P.DiskCleanupReport -> C.Parsed CGDisk.DiskCleanupReport
toCapnpDiskCleanupReport P.DiskCleanupReport {..} =
  CGDisk.DiskCleanupReport
    { CGDisk.dryRun = dcrDryRun
    , CGDisk.versions = map version dcrVersions
    , CGDisk.removedVersions = dcrRemovedVersions
    , CGDisk.removedPlacements = dcrRemovedPlacements
    , CGDisk.failures = dcrFailures
    }
  where
    version P.DiskCleanupVersion {..} =
      CGDisk.DiskCleanupVersion
        { CGDisk.diskImage = toCapnpNamedRef dcvDiskImage
        , CGDisk.tags = dcvTags
        , CGDisk.status = dcvStatus
        , CGDisk.reason = dcvReason
        , CGDisk.versionDeleted = dcvVersionDeleted
        , CGDisk.placements = map placement dcvPlacements
        }
    placement P.DiskCleanupPlacement {..} =
      CGDisk.DiskCleanupPlacement
        { CGDisk.node = toCapnpNamedRef dcpNode
        , CGDisk.filePath = dcpFilePath
        , CGDisk.status = dcpStatus
        , CGDisk.reason = dcpReason
        }

fromCapnpDiskCleanupReport :: C.Parsed CGDisk.DiskCleanupReport -> P.DiskCleanupReport
fromCapnpDiskCleanupReport CGDisk.DiskCleanupReport {..} =
  P.DiskCleanupReport
    dryRun
    (map version versions)
    removedVersions
    removedPlacements
    failures
  where
    version CGDisk.DiskCleanupVersion {..} =
      P.DiskCleanupVersion
        (fromCapnpNamedRef diskImage)
        tags
        status
        reason
        versionDeleted
        (map placement placements)
    placement CGDisk.DiskCleanupPlacement {..} =
      P.DiskCleanupPlacement
        (fromCapnpNamedRef node)
        filePath
        status
        reason
