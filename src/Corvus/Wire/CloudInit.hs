{-# LANGUAGE RecordWildCards #-}

-- | Cap'n Proto conversion for the cloud-init payload.
module Corvus.Wire.CloudInit
  ( toCapnpCloudInitInfo
  , fromCapnpCloudInitInfo
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Cloudinit as CGCloud
import qualified Corvus.Protocol.CloudInit as P
import Data.Maybe (fromMaybe, isJust)

-- The schema uses paired `hasX :Bool` + `x :Text` fields to model
-- 'Maybe Text'; an unset `hasX` means @Nothing@.
toCapnpCloudInitInfo :: P.CloudInitInfo -> C.Parsed CGCloud.CloudInitInfo
toCapnpCloudInitInfo P.CloudInitInfo {..} =
  CGCloud.CloudInitInfo
    { CGCloud.hasUserData = isJust ciiUserData
    , CGCloud.userData = fromMaybe mempty ciiUserData
    , CGCloud.hasNetworkConfig = isJust ciiNetworkConfig
    , CGCloud.networkConfig = fromMaybe mempty ciiNetworkConfig
    , CGCloud.injectSshKeys = ciiInjectSshKeys
    }

fromCapnpCloudInitInfo :: C.Parsed CGCloud.CloudInitInfo -> P.CloudInitInfo
fromCapnpCloudInitInfo CGCloud.CloudInitInfo {..} =
  P.CloudInitInfo
    { P.ciiUserData = if hasUserData then Just userData else Nothing
    , P.ciiNetworkConfig = if hasNetworkConfig then Just networkConfig else Nothing
    , P.ciiInjectSshKeys = injectSshKeys
    }
