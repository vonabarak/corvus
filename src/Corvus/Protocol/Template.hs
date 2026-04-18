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
import Corvus.Protocol.Aeson (innerOptions)
import Corvus.Protocol.CloudInit (CloudInitInfo)
import Data.Aeson (ToJSON (..), genericToJSON)
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
  toJSON = genericToJSON innerOptions

instance ToJSON TemplateDriveInfo where
  toJSON = genericToJSON innerOptions

instance ToJSON TemplateNetIfInfo where
  toJSON = genericToJSON innerOptions

instance ToJSON TemplateSshKeyInfo where
  toJSON = genericToJSON innerOptions

instance ToJSON TemplateDetails where
  toJSON = genericToJSON innerOptions
