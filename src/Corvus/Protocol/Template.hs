{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | VM template response data.
--
-- 'TemplateVmInfo' is the list-view summary; 'TemplateDetails' is the
-- full per-template payload including drives, network interfaces, SSH
-- keys, and optional cloud-init config.
module Corvus.Protocol.Template
  ( TemplateVmInfo (..)
  , TemplateDriveInfo (..)
  , TemplateNetIfInfo (..)
  , TemplateSshKeyInfo (..)
  , TemplateDetails (..)
  )
where

import Corvus.Model (CacheType, DriveFormat, DriveInterface, DriveMedia, NetInterfaceType, TemplateCloneStrategy)
import Corvus.Protocol.CloudInit (CloudInitInfo)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Binary (Binary)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Template VM summary for list view
data TemplateVmInfo = TemplateVmInfo
  { tviId :: !Int64
  , tviName :: !Text
  , tviCpuCount :: !Int
  , tviRamMb :: !Int
  , tviDescription :: !(Maybe Text)
  , tviHeadless :: !Bool
  , tviGuestAgent :: !Bool
  , tviAutostart :: !Bool
  }
  deriving (Eq, Show, Generic, Binary)

-- | Template drive info for details view
data TemplateDriveInfo = TemplateDriveInfo
  { tvdiDiskImageId :: !(Maybe Int64)
  , tvdiDiskImageName :: !(Maybe Text)
  , tvdiInterface :: !DriveInterface
  , tvdiMedia :: !(Maybe DriveMedia)
  , tvdiReadOnly :: !Bool
  , tvdiCacheType :: !CacheType
  , tvdiDiscard :: !Bool
  , tvdiCloneStrategy :: !TemplateCloneStrategy
  , tvdiSizeMb :: !(Maybe Int)
  , tvdiFormat :: !(Maybe DriveFormat)
  }
  deriving (Eq, Show, Generic, Binary)

-- | Template network interface info
data TemplateNetIfInfo = TemplateNetIfInfo
  { tvniType :: !NetInterfaceType
  , tvniHostDevice :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, Binary)

-- | Template SSH key info
data TemplateSshKeyInfo = TemplateSshKeyInfo
  { tvskiId :: !Int64
  , tvskiName :: !Text
  }
  deriving (Eq, Show, Generic, Binary)

-- | Template VM full details
data TemplateDetails = TemplateDetails
  { tvdId :: !Int64
  , tvdName :: !Text
  , tvdCpuCount :: !Int
  , tvdRamMb :: !Int
  , tvdDescription :: !(Maybe Text)
  , tvdHeadless :: !Bool
  , tvdCloudInit :: !Bool
  , tvdGuestAgent :: !Bool
  , tvdAutostart :: !Bool
  , tvdCloudInitConfig :: !(Maybe CloudInitInfo)
  , tvdCreatedAt :: !UTCTime
  , tvdDrives :: ![TemplateDriveInfo]
  , tvdNetIfs :: ![TemplateNetIfInfo]
  , tvdSshKeys :: ![TemplateSshKeyInfo]
  }
  deriving (Eq, Show, Generic, Binary)

instance ToJSON TemplateVmInfo where
  toJSON t =
    object
      [ "id" .= tviId t
      , "name" .= tviName t
      , "cpuCount" .= tviCpuCount t
      , "ramMb" .= tviRamMb t
      , "description" .= tviDescription t
      , "headless" .= tviHeadless t
      , "guestAgent" .= tviGuestAgent t
      , "autostart" .= tviAutostart t
      ]

instance ToJSON TemplateDriveInfo where
  toJSON d =
    object
      [ "diskImageId" .= tvdiDiskImageId d
      , "diskImageName" .= tvdiDiskImageName d
      , "interface" .= tvdiInterface d
      , "media" .= tvdiMedia d
      , "readOnly" .= tvdiReadOnly d
      , "cacheType" .= tvdiCacheType d
      , "discard" .= tvdiDiscard d
      , "cloneStrategy" .= tvdiCloneStrategy d
      , "sizeMb" .= tvdiSizeMb d
      , "format" .= tvdiFormat d
      ]

instance ToJSON TemplateNetIfInfo where
  toJSON n =
    object
      [ "type" .= tvniType n
      , "hostDevice" .= tvniHostDevice n
      ]

instance ToJSON TemplateSshKeyInfo where
  toJSON k =
    object
      [ "id" .= tvskiId k
      , "name" .= tvskiName k
      ]

instance ToJSON TemplateDetails where
  toJSON t =
    object
      [ "id" .= tvdId t
      , "name" .= tvdName t
      , "cpuCount" .= tvdCpuCount t
      , "ramMb" .= tvdRamMb t
      , "description" .= tvdDescription t
      , "headless" .= tvdHeadless t
      , "cloudInit" .= tvdCloudInit t
      , "guestAgent" .= tvdGuestAgent t
      , "autostart" .= tvdAutostart t
      , "cloudInitConfig" .= tvdCloudInitConfig t
      , "createdAt" .= tvdCreatedAt t
      , "drives" .= tvdDrives t
      , "networkInterfaces" .= tvdNetIfs t
      , "sshKeys" .= tvdSshKeys t
      ]
