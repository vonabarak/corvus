{-# LANGUAGE RecordWildCards #-}

-- | Cap'n Proto conversion for SSH key info.
module Corvus.Wire.SshKey
  ( toCapnpSshKeyInfo
  , fromCapnpSshKeyInfo
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Sshkey as CGSsh
import qualified Corvus.Protocol.SshKey as P
import Corvus.Wire.Common (fromCapnpNamedRef, toCapnpNamedRef)
import Corvus.Wire.Time (nanosToUtcTime, utcTimeToNanos)

toCapnpSshKeyInfo :: P.SshKeyInfo -> C.Parsed CGSsh.SshKeyInfo
toCapnpSshKeyInfo P.SshKeyInfo {..} =
  CGSsh.SshKeyInfo
    { CGSsh.id = skiId
    , CGSsh.name = skiName
    , CGSsh.publicKey = skiPublicKey
    , CGSsh.createdAt = utcTimeToNanos skiCreatedAt
    , CGSsh.attachedVms = map mkAttachment skiAttachedVms
    }
  where
    mkAttachment vmRef = CGSsh.VmAttachment {CGSsh.vm = toCapnpNamedRef vmRef}

fromCapnpSshKeyInfo :: C.Parsed CGSsh.SshKeyInfo -> P.SshKeyInfo
fromCapnpSshKeyInfo CGSsh.SshKeyInfo {..} =
  P.SshKeyInfo
    { P.skiId = id
    , P.skiName = name
    , P.skiPublicKey = publicKey
    , P.skiCreatedAt = nanosToUtcTime createdAt
    , P.skiAttachedVms = [fromCapnpNamedRef (CGSsh.vm a) | a <- attachedVms]
    }
