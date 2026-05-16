{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | CloudInitManager cap implementation.
--
-- Phase 4c adds @set@ and @delete@ alongside the existing @get@.
-- The CloudInit "manager" is really a per-VM accessor — there's
-- no per-config resource cap.
module Corvus.Rpc.CloudInit
  ( CloudInitManagerCap (..)
  , newCloudInitManagerCap
  )
where

import qualified Capnp.Gen.Cloudinit as CGCI
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer)
import Corvus.Action (runAction)
import Corvus.Handlers.CloudInit (CloudInitDelete (..), CloudInitSet (..), handleCloudInitGet)
import Corvus.Handlers.Resolve (resolveVm)
import Corvus.Protocol (Response (..))
import qualified Corvus.Protocol.CloudInit as P
import Corvus.Rpc.Common (capnpRefToRef, failOnLeft, handleParsed)
import Corvus.Types (ServerState (..))
import Corvus.Wire.CloudInit (fromCapnpCloudInitInfo, toCapnpCloudInitInfo)
import Data.Maybe (fromMaybe)
import Supervisors (Supervisor)

data CloudInitManagerCap = CloudInitManagerCap
  { ciState :: !ServerState
  , ciSup :: !Supervisor
  }

newCloudInitManagerCap :: ServerState -> Supervisor -> IO CloudInitManagerCap
newCloudInitManagerCap st sup = pure (CloudInitManagerCap st sup)

instance SomeServer CloudInitManagerCap

instance CGCI.CloudInitManager'server_ CloudInitManagerCap where
  cloudInitManager'get (CloudInitManagerCap st _) =
    handleParsed $ \CGCI.CloudInitManager'get'params {..} -> do
      ref' <- capnpRefToRef vmRef
      eid <- failOnLeft =<< resolveVm ref' (ssDbPool st)
      resp <- handleCloudInitGet st eid
      let emptyInfo =
            P.CloudInitInfo
              { P.ciiUserData = Nothing
              , P.ciiNetworkConfig = Nothing
              , P.ciiInjectSshKeys = False
              }
      case resp of
        RespCloudInitConfig mInfo ->
          let cfg = toCapnpCloudInitInfo (fromMaybe emptyInfo mInfo)
           in pure CGCI.CloudInitManager'get'results {CGCI.config = cfg}
        RespVmNotFound -> throwFailed "VM not found"
        RespError msg -> throwFailed msg
        _ -> throwFailed "cloudInitManager'get: unexpected response"

  cloudInitManager'set (CloudInitManagerCap st _) =
    handleParsed $ \CGCI.CloudInitManager'set'params {params = CGCI.CloudInitSetParams {..}} -> do
      ref' <- capnpRefToRef vmRef
      eid <- failOnLeft =<< resolveVm ref' (ssDbPool st)
      let info = fromCapnpCloudInitInfo config
          act =
            CloudInitSet
              { cisVmId = eid
              , cisUserData = P.ciiUserData info
              , cisNetworkConfig = P.ciiNetworkConfig info
              , cisInjectKeys = P.ciiInjectSshKeys info
              }
      resp <- runAction st act
      case resp of
        RespCloudInitOk -> pure CGCI.CloudInitManager'set'results
        RespVmNotFound -> throwFailed "VM not found"
        RespError msg -> throwFailed msg
        _ -> throwFailed "cloudInitManager'set: unexpected response"

  cloudInitManager'delete (CloudInitManagerCap st _) =
    handleParsed $ \CGCI.CloudInitManager'delete'params {..} -> do
      ref' <- capnpRefToRef vmRef
      eid <- failOnLeft =<< resolveVm ref' (ssDbPool st)
      resp <- runAction st (CloudInitDelete eid)
      case resp of
        RespCloudInitOk -> pure CGCI.CloudInitManager'delete'results
        RespVmNotFound -> throwFailed "VM not found"
        RespError msg -> throwFailed msg
        _ -> throwFailed "cloudInitManager'delete: unexpected response"
