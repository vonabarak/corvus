{-# LANGUAGE RecordWildCards #-}

-- | Cap'n Proto conversion for the @crv apply@ result payload.
module Corvus.Wire.Apply
  ( toCapnpApplyCreated
  , fromCapnpApplyCreated
  , toCapnpApplyResult
  , fromCapnpApplyResult
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Corvus as CGCorvus
import qualified Corvus.Protocol.Apply as P

toCapnpApplyCreated :: P.ApplyCreated -> C.Parsed CGCorvus.ApplyCreated
toCapnpApplyCreated P.ApplyCreated {..} =
  CGCorvus.ApplyCreated {CGCorvus.name = acName, CGCorvus.id = acId}

fromCapnpApplyCreated :: C.Parsed CGCorvus.ApplyCreated -> P.ApplyCreated
fromCapnpApplyCreated CGCorvus.ApplyCreated {..} =
  P.ApplyCreated {P.acName = name, P.acId = id}

toCapnpApplyResult :: P.ApplyResult -> C.Parsed CGCorvus.ApplyResult
toCapnpApplyResult P.ApplyResult {..} =
  CGCorvus.ApplyResult
    { CGCorvus.sshKeys = map toCapnpApplyCreated arSshKeys
    , CGCorvus.disks = map toCapnpApplyCreated arDisks
    , CGCorvus.networks = map toCapnpApplyCreated arNetworks
    , CGCorvus.vms = map toCapnpApplyCreated arVms
    , CGCorvus.templates = map toCapnpApplyCreated arTemplates
    }

fromCapnpApplyResult :: C.Parsed CGCorvus.ApplyResult -> P.ApplyResult
fromCapnpApplyResult CGCorvus.ApplyResult {..} =
  P.ApplyResult
    { P.arSshKeys = map fromCapnpApplyCreated sshKeys
    , P.arDisks = map fromCapnpApplyCreated disks
    , P.arNetworks = map fromCapnpApplyCreated networks
    , P.arVms = map fromCapnpApplyCreated vms
    , P.arTemplates = map fromCapnpApplyCreated templates
    }
