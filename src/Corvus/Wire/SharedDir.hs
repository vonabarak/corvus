{-# LANGUAGE RecordWildCards #-}

-- | Cap'n Proto conversion for virtiofs shared directory info.
module Corvus.Wire.SharedDir
  ( toCapnpSharedDirInfo
  , fromCapnpSharedDirInfo
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Vm as CGVm
import qualified Corvus.Protocol.SharedDir as P
import Corvus.Wire.Enums (fromCapnpSharedDirCache, toCapnpSharedDirCache)
import Corvus.Wire.Errors (WireError)

toCapnpSharedDirInfo :: P.SharedDirInfo -> C.Parsed CGVm.SharedDirInfo
toCapnpSharedDirInfo P.SharedDirInfo {..} =
  CGVm.SharedDirInfo
    { CGVm.id = sdiId
    , CGVm.path = sdiPath
    , CGVm.tag = sdiTag
    , CGVm.cache = toCapnpSharedDirCache sdiCache
    , CGVm.readOnly = sdiReadOnly
    , CGVm.pid = maybe 0 fromIntegral sdiPid
    }

fromCapnpSharedDirInfo :: C.Parsed CGVm.SharedDirInfo -> Either WireError P.SharedDirInfo
fromCapnpSharedDirInfo CGVm.SharedDirInfo {..} = do
  cache' <- fromCapnpSharedDirCache cache
  pure
    P.SharedDirInfo
      { P.sdiId = id
      , P.sdiPath = path
      , P.sdiTag = tag
      , P.sdiCache = cache'
      , P.sdiReadOnly = readOnly
      , P.sdiPid = if pid == 0 then Nothing else Just (fromIntegral pid)
      }
