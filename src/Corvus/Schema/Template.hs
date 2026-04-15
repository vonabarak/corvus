{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | YAML input schema for VM templates.
--
-- Parsed by @crv template create@ / @crv template edit@ and reused by
-- @crv apply@ for inline @templates:@ entries.
module Corvus.Schema.Template
  ( TemplateYaml (..)
  , TemplateDriveYaml (..)
  , TemplateNetworkInterfaceYaml (..)
  , TemplateSshKeyYaml (..)
  )
where

import Corvus.Model
import Corvus.Schema.CloudInit (CloudInitConfigYaml)
import Data.Text (Text)
import Data.Yaml (FromJSON (..), withObject, (.!=), (.:), (.:?))
import GHC.Generics (Generic)

data TemplateYaml = TemplateYaml
  { tyName :: Text
  , tyCpuCount :: Int
  , tyRamMb :: Int
  , tyDescription :: Maybe Text
  , tyHeadless :: Bool
  , tyCloudInit :: Bool
  , tyGuestAgent :: Bool
  , tyAutostart :: Bool
  , tyCloudInitConfig :: Maybe CloudInitConfigYaml
  , tyDrives :: [TemplateDriveYaml]
  , tyNetworkInterfaces :: [TemplateNetworkInterfaceYaml]
  , tySshKeys :: [TemplateSshKeyYaml]
  }
  deriving (Show, Generic)

instance FromJSON TemplateYaml where
  parseJSON = withObject "TemplateYaml" $ \o ->
    TemplateYaml
      <$> o .: "name"
      <*> o .: "cpuCount"
      <*> o .: "ramMb"
      <*> o .:? "description"
      <*> o .:? "headless" .!= False
      <*> o .:? "cloudInit" .!= False
      <*> o .:? "guestAgent" .!= False
      <*> o .:? "autostart" .!= False
      <*> o .:? "cloudInitConfig"
      <*> o .: "drives"
      <*> o .:? "networkInterfaces" .!= []
      <*> o .:? "sshKeys" .!= []

data TemplateDriveYaml = TemplateDriveYaml
  { tdyDiskImageName :: Maybe Text
  , tdyInterface :: DriveInterface
  , tdyMedia :: Maybe DriveMedia
  , tdyReadOnly :: Maybe Bool
  , tdyCacheType :: Maybe CacheType
  , tdyDiscard :: Maybe Bool
  , tdyStrategy :: TemplateCloneStrategy
  , tdySizeMb :: Maybe Int
  , tdyFormat :: Maybe DriveFormat
  }
  deriving (Show, Generic)

instance FromJSON TemplateDriveYaml where
  parseJSON = withObject "TemplateDriveYaml" $ \o ->
    TemplateDriveYaml
      <$> o .:? "diskImageName"
      <*> o .: "interface"
      <*> o .:? "media"
      <*> o .:? "readOnly"
      <*> o .:? "cacheType"
      <*> o .:? "discard"
      <*> o .: "strategy"
      <*> o .:? "sizeMb"
      <*> o .:? "format"

data TemplateNetworkInterfaceYaml = TemplateNetworkInterfaceYaml
  { tnyType :: NetInterfaceType
  , tnyHostDevice :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON TemplateNetworkInterfaceYaml where
  parseJSON = withObject "TemplateNetworkInterfaceYaml" $ \o ->
    TemplateNetworkInterfaceYaml
      <$> o .: "type"
      <*> o .:? "hostDevice"

newtype TemplateSshKeyYaml = TemplateSshKeyYaml
  { tkyName :: Text
  }
  deriving (Show, Generic)

instance FromJSON TemplateSshKeyYaml where
  parseJSON = withObject "TemplateSshKeyYaml" $ \o ->
    TemplateSshKeyYaml
      <$> o .: "name"
