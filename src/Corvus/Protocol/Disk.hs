{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Disk and snapshot response data.
module Corvus.Protocol.Disk
  ( DiskImageInfo (..)
  , SnapshotInfo (..)
  )
where

import Corvus.Model (DriveFormat)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Binary (Binary)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Disk image info for list/show view
data DiskImageInfo = DiskImageInfo
  { diiId :: !Int64
  , diiName :: !Text
  , diiFilePath :: !Text
  , diiFormat :: !DriveFormat
  , diiSizeMb :: !(Maybe Int)
  , diiCreatedAt :: !UTCTime
  , diiAttachedTo :: ![(Int64, Text)]
  -- ^ VM (ID, name) pairs this disk is attached to
  , diiBackingImageId :: !(Maybe Int64)
  -- ^ Backing image ID (if this is an overlay)
  , diiBackingImageName :: !(Maybe Text)
  -- ^ Backing image name (if this is an overlay)
  }
  deriving (Eq, Show, Generic, Binary)

-- | Snapshot info
data SnapshotInfo = SnapshotInfo
  { sniId :: !Int64
  , sniName :: !Text
  , sniCreatedAt :: !UTCTime
  , sniSizeMb :: !(Maybe Int)
  }
  deriving (Eq, Show, Generic, Binary)

instance ToJSON DiskImageInfo where
  toJSON d =
    object
      [ "id" .= diiId d
      , "name" .= diiName d
      , "filePath" .= diiFilePath d
      , "format" .= diiFormat d
      , "sizeMb" .= diiSizeMb d
      , "createdAt" .= diiCreatedAt d
      , "attachedTo" .= diiAttachedTo d
      , "backingImageId" .= diiBackingImageId d
      , "backingImageName" .= diiBackingImageName d
      ]

instance ToJSON SnapshotInfo where
  toJSON s =
    object
      [ "id" .= sniId s
      , "name" .= sniName s
      , "createdAt" .= sniCreatedAt s
      , "sizeMb" .= sniSizeMb s
      ]
