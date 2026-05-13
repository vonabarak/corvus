{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | CloudInitManager cap implementation.
module Corvus.Rpc.CloudInit
  ( CloudInitManagerCap (..)
  , newCloudInitManagerCap
  )
where

import qualified Capnp.Gen.Cloudinit as CGCI
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer, handleParsed, methodUnimplemented)
import Corvus.Handlers.CloudInit (handleCloudInitGet)
import Corvus.Handlers.Resolve (resolveVm)
import Corvus.Protocol (Response (..))
import qualified Corvus.Protocol.CloudInit as P
import Corvus.Rpc.Common (capnpRefToRef, failOnLeft)
import Corvus.Types (ServerState (..))
import Corvus.Wire.CloudInit (toCapnpCloudInitInfo)
import Supervisors (Supervisor)

data CloudInitManagerCap = CloudInitManagerCap
  { ciState :: !ServerState
  , ciSup :: !Supervisor
  }

newCloudInitManagerCap :: ServerState -> Supervisor -> IO CloudInitManagerCap
newCloudInitManagerCap st sup = pure (CloudInitManagerCap st sup)

instance SomeServer CloudInitManagerCap

instance CGCI.CloudInitManager'server_ CloudInitManagerCap where
  cloudInitManager'get (CloudInitManagerCap st _) = handleParsed $ \CGCI.CloudInitManager'get'params {..} -> do
    ref' <- capnpRefToRef vmRef
    eid <- failOnLeft =<< resolveVm ref' (ssDbPool st)
    resp <- handleCloudInitGet st eid
    case resp of
      RespCloudInitConfig mInfo ->
        let empty =
              -- Wire 'no config' as an all-default CloudInitInfo struct;
              -- callers can detect this via the hasUserData /
              -- hasNetworkConfig flags being False.
              toCapnpCloudInitInfo
                P.CloudInitInfo
                  { P.ciiUserData = Nothing
                  , P.ciiNetworkConfig = Nothing
                  , P.ciiInjectSshKeys = False
                  }
            cfg = maybe empty toCapnpCloudInitInfo mInfo
         in pure CGCI.CloudInitManager'get'results {CGCI.config = cfg}
      RespVmNotFound -> throwFailed "VM not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "cloudInitManager'get: unexpected response"

  cloudInitManager'set _ = methodUnimplemented
  cloudInitManager'delete _ = methodUnimplemented
