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
import Corvus.Wire.Enums (fromCapnpDriveFormat, toCapnpDriveFormat)
import Corvus.Wire.Errors (WireError)
import Corvus.Wire.Time (nanosToUtcTime, utcTimeToNanos)
import Data.Maybe (fromMaybe, isJust)

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
    , CGDisk.backingImageId = fromMaybe 0 diiBackingImageId
    , CGDisk.backingImageName = fromMaybe mempty diiBackingImageName
    }
  where
    mkAttachment (vid, vname) =
      CGDisk.DiskAttachment {CGDisk.vmId = vid, CGDisk.vmName = vname}
    mkPlacement p =
      CGDisk.DiskImagePlacement
        { CGDisk.nodeId = P.dipNodeId p
        , CGDisk.nodeName = P.dipNodeName p
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
          [ case p of
            CGDisk.DiskImagePlacement {CGDisk.nodeId = nid, CGDisk.nodeName = nm, CGDisk.filePath = fp} ->
              P.DiskImagePlacement
                { P.dipNodeId = nid
                , P.dipNodeName = nm
                , P.dipFilePath = fp
                }
          | p <- placements
          ]
      , P.diiFormat = format'
      , P.diiSizeMb = if sizeMb == 0 then Nothing else Just (fromIntegral sizeMb)
      , P.diiCreatedAt = nanosToUtcTime createdAt
      , P.diiAttachedTo =
          [ (CGDisk.vmId a, CGDisk.vmName a)
          | a <- attachedTo
          ]
      , P.diiBackingImageId = if backingImageId == 0 then Nothing else Just backingImageId
      , P.diiBackingImageName =
          if backingImageName == mempty then Nothing else Just backingImageName
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
    }

fromCapnpSnapshotInfo :: C.Parsed CGDisk.SnapshotInfo -> P.SnapshotInfo
fromCapnpSnapshotInfo CGDisk.SnapshotInfo {..} =
  P.SnapshotInfo
    { P.sniId = id
    , P.sniName = name
    , P.sniCreatedAt = nanosToUtcTime createdAt
    , P.sniSizeMb = if sizeMb == 0 then Nothing else Just (fromIntegral sizeMb)
    }
