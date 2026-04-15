{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | YAML input schema for cloud-init config.
--
-- Used by templates, apply configs, and the @crv cloud-init set@ CLI flow.
-- @userData@ and @networkConfig@ are parsed as YAML 'Value's and serialised
-- to text for DB storage, so both structured YAML (objects/arrays) and raw
-- text strings (e.g. PowerShell scripts for cloudbase-init on Windows) are
-- accepted.
module Corvus.Schema.CloudInit
  ( CloudInitConfigYaml (..)
  )
where

import Data.Aeson (Value (..))
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Yaml (FromJSON (..), withObject, (.!=), (.:?))
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)

data CloudInitConfigYaml = CloudInitConfigYaml
  { cicyUserData :: Maybe Text
  , cicyNetworkConfig :: Maybe Text
  , cicyInjectSshKeys :: Bool
  }
  deriving (Show, Generic)

instance FromJSON CloudInitConfigYaml where
  parseJSON = withObject "CloudInitConfigYaml" $ \o -> do
    mUserDataVal <- o .:? "userData" :: Yaml.Parser (Maybe Value)
    mNetworkConfigVal <- o .:? "networkConfig" :: Yaml.Parser (Maybe Value)
    injectKeys <- o .:? "injectSshKeys" .!= True
    let valueToText (String t) = t
        valueToText v = T.decodeUtf8 (Yaml.encode v)
    pure
      CloudInitConfigYaml
        { cicyUserData = fmap valueToText mUserDataVal
        , cicyNetworkConfig = fmap valueToText mNetworkConfigVal
        , cicyInjectSshKeys = injectKeys
        }
