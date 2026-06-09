{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

-- | Cap'n Proto conversion for disk and snapshot info structs.
module Corvus.Wire.Disk
  ( toCapnpDiskImageInfo
  , fromCapnpDiskImageInfo
  , toCapnpSnapshotInfo
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
import Data.Maybe (isJust)

-- ---------------------------------------------------------------------
-- Disk image
-- ---------------------------------------------------------------------

toCapnpDiskImageInfo :: P.DiskImageInfo -> C.Parsed CGDisk.DiskImageInfo
toCapnpDiskImageInfo P.DiskImageInfo {..} =
  CGDisk.DiskImageInfo
    { CGDisk.id = diiId
    , CGDisk.name = diiName
    , CGDisk.placements = map mkPlacement diiPlacements
    , CGDisk.format = toCapnpDriveFormat diiFormat
    , CGDisk.sizeMb = maybe 0 fromIntegral diiSizeMb
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
      , P.diiPlacements =
          [ P.DiskImagePlacement
            { P.dipNode = fromCapnpNamedRef nodeRef
            , P.dipFilePath = fp
            }
          | CGDisk.DiskImagePlacement {CGDisk.node = nodeRef, CGDisk.filePath = fp} <-
              placements
          ]
      , P.diiFormat = format'
      , P.diiSizeMb = if sizeMb == 0 then Nothing else Just (fromIntegral sizeMb)
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
    , CGDisk.sizeMb = maybe 0 fromIntegral sniSizeMb
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
    , P.sniSizeMb = if sizeMb == 0 then Nothing else Just (fromIntegral sizeMb)
    , P.sniLive = live
    , P.sniQuiesced = quiesced
    , P.sniHasVmstate = hasVmstate
    }
