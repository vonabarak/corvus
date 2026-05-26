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
  , tyRebootQuirk :: Bool
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
      <*> o .:? "rebootQuirk" .!= False
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
  , tdyEphemeral :: Maybe Bool
  -- ^ Override the strategy-driven ephemeral default. 'Nothing' picks
  -- the default for the strategy: True for clone/overlay/create
  -- (disks materialised during instantiation), False for direct
  -- (an existing image is attached as-is).
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
      <*> o .:? "ephemeral"

data TemplateNetworkInterfaceYaml = TemplateNetworkInterfaceYaml
  { tnyType :: NetInterfaceType
  , tnyHostDevice :: Maybe Text
  , tnyNetwork :: Maybe Text
  -- ^ Name of the managed network to attach this NIC to.
  -- Required for @type: managed@; ignored otherwise.
  }
  deriving (Show, Generic)

instance FromJSON TemplateNetworkInterfaceYaml where
  parseJSON = withObject "TemplateNetworkInterfaceYaml" $ \o -> do
    mType <- o .:? "type"
    hostDevice <- o .:? "hostDevice"
    network <- o .:? "network"
    -- Mirror the 'ApplyNetIf' contract: omitting 'type' is fine
    -- when 'network' is set ('managed' is the only legal pairing).
    -- 'type: managed' without 'network' is rejected later by the
    -- validator in 'Handlers.Template.insertTemplateYaml'.
    ifType <- case (mType, network) of
      (Nothing, Just _) -> pure NetManaged
      (Just t, Just _)
        | t /= NetManaged ->
            fail "network interface with 'network' must have type 'managed' or omit 'type'"
        | otherwise -> pure NetManaged
      (Just t, Nothing) -> pure t
      (Nothing, Nothing) ->
        fail "network interface must specify 'type' (or 'network' for a managed NIC)"
    pure (TemplateNetworkInterfaceYaml ifType hostDevice network)

newtype TemplateSshKeyYaml = TemplateSshKeyYaml
  { tkyName :: Text
  }
  deriving (Show, Generic)

instance FromJSON TemplateSshKeyYaml where
  parseJSON = withObject "TemplateSshKeyYaml" $ \o ->
    TemplateSshKeyYaml
      <$> o .: "name"
