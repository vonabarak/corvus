{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Streaming RPC wrappers (Apply, Build, ByteSink, GuestAgentStatus)
-- extracted from "Corvus.Client.Capnp.Rpc".
module Corvus.Client.Capnp.Rpc.Stream
  ( -- * Apply (streaming)
    rpcApply
  , rpcApplyStream
  , ClientApplyEventSink (..)

    -- * Build (streaming)
  , rpcBuild
  , ClientBuildEventSink (..)

    -- * Cloud-init
  , rpcCloudInitSet
  , rpcCloudInitGet
  , rpcCloudInitDelete
  )
where

import Capnp (export)
import qualified Capnp as C
import qualified Capnp.Gen.Cloudinit as CGCI
import qualified Capnp.Gen.Common as CGCommon
import qualified Capnp.Gen.Corvus as CGCorvus
import qualified Capnp.Gen.Streams as CGS
import Capnp.Rpc (fromClient)
import Capnp.Rpc.Server (SomeServer, handleParsed)
import Capnp.Rpc.Untyped (nullClient)
import qualified Capnp.Rpc.Untyped (nullClient)
import Control.Exception (SomeException, try)
import qualified Control.Monad
import Corvus.Client.Capnp.Connection (CapnpConnection (..))
import qualified Corvus.Protocol.Apply as PA
import Corvus.Protocol.Build (BuildEvent)
import qualified Corvus.Protocol.CloudInit as PCI
import Corvus.Wire.Apply (fromCapnpApplyEvent, fromCapnpApplyResult)
import Corvus.Wire.Build (fromCapnpBuildEvent)
import Corvus.Wire.CloudInit (fromCapnpCloudInitInfo, toCapnpCloudInitInfo)
import Corvus.Wire.Common (EntityRef, toCapnpEntityRef)
import Corvus.Wire.Errors (WireError, showWireError)
import Data.Function ((&))
import Data.Int (Int32, Int64)
import Data.Text (Text)

-- | Call a method on a cap and return its parsed results struct.
callOn
  :: ( C.IsCap iface
     , C.IsStruct params
     , C.IsStruct results
     , C.Parse params (C.Parsed params)
     , C.Parse results (C.Parsed results)
     )
  => C.Method iface params results
  -> C.Parsed params
  -> C.Client iface
  -> IO (C.Parsed results)
callOn method p client = do
  raw <- (client & C.callP method p) >>= C.waitPipeline
  C.evalLimitT C.defaultLimit (C.parse raw)

failOnWire :: Either WireError a -> IO a
failOnWire (Right a) = pure a
failOnWire (Left e) = fail ("wire decode error: " <> show (showWireError e))

-- =====================================================================
-- Apply (streaming)
-- =====================================================================

rpcApply :: CapnpConnection -> Text -> Bool -> Bool -> IO (PA.ApplyResult, Int64)
rpcApply conn yaml skipExisting wait = do
  CGCorvus.Daemon'apply'results {CGCorvus.result = r, CGCorvus.taskId = tid} <-
    callOn
      #apply
      CGCorvus.Daemon'apply'params
        { CGCorvus.yaml = yaml
        , CGCorvus.skipExisting = skipExisting
        , CGCorvus.wait = wait
        , CGCorvus.sink = fromClient nullClient
        }
      (ccDaemon conn)
  pure (fromCapnpApplyResult r, tid)

data ClientApplyEventSink = ClientApplyEventSink
  { caesOnEvent :: PA.ApplyEvent -> IO ()
  , caesOnEnd :: IO ()
  }

instance SomeServer ClientApplyEventSink

instance CGS.ApplyEventSink'server_ ClientApplyEventSink where
  applyEventSink'push (ClientApplyEventSink onEv _) =
    handleParsed $ \CGS.ApplyEventSink'push'params {CGS.event = cev} -> do
      case fromCapnpApplyEvent cev of
        Right ev -> do
          _ <- try (onEv ev) :: IO (Either SomeException ())
          pure ()
        Left _ -> pure ()
      pure CGS.ApplyEventSink'push'results

  applyEventSink'end (ClientApplyEventSink _ onEnd) =
    handleParsed $ \_ -> do
      _ <- try onEnd :: IO (Either SomeException ())
      pure CGS.ApplyEventSink'end'results

rpcApplyStream
  :: CapnpConnection
  -> Text
  -> Bool
  -> (PA.ApplyEvent -> IO ())
  -> IO ()
  -> IO Int64
rpcApplyStream conn yaml skipExisting onEvent onEnd = do
  sinkClient <-
    export @CGS.ApplyEventSink (ccSupervisor conn) (ClientApplyEventSink onEvent onEnd)
  CGCorvus.Daemon'apply'results {CGCorvus.taskId = tid} <-
    callOn
      #apply
      CGCorvus.Daemon'apply'params
        { CGCorvus.yaml = yaml
        , CGCorvus.skipExisting = skipExisting
        , CGCorvus.wait = True
        , CGCorvus.sink = sinkClient
        }
      (ccDaemon conn)
  pure tid

-- =====================================================================
-- Build (streaming)
-- =====================================================================

data ClientBuildEventSink = ClientBuildEventSink
  { cbsOnEvent :: BuildEvent -> IO ()
  , cbsOnEnd :: IO ()
  }

instance SomeServer ClientBuildEventSink

instance CGS.BuildEventSink'server_ ClientBuildEventSink where
  buildEventSink'push (ClientBuildEventSink onEv _) =
    handleParsed $ \CGS.BuildEventSink'push'params {CGS.event = cev} -> do
      case fromCapnpBuildEvent cev of
        Right ev -> do
          _ <- try (onEv ev) :: IO (Either SomeException ())
          pure ()
        Left _ -> pure ()
      pure CGS.BuildEventSink'push'results

  buildEventSink'end (ClientBuildEventSink _ onEnd) =
    handleParsed $ \_ -> do
      _ <- try onEnd :: IO (Either SomeException ())
      pure CGS.BuildEventSink'end'results

rpcBuild
  :: CapnpConnection
  -> Text
  -> Bool
  -> Bool
  -> Int32
  -> (BuildEvent -> IO ())
  -> IO ()
  -> IO Int64
rpcBuild conn yaml useCache buildCache rebuildFromStep onEvent onEnd = do
  sinkClient <- export @CGS.BuildEventSink (ccSupervisor conn) (ClientBuildEventSink onEvent onEnd)
  CGCorvus.Daemon'build'results {CGCorvus.taskId = tid} <-
    callOn
      #build
      CGCorvus.Daemon'build'params
        { CGCorvus.yaml = yaml
        , CGCorvus.sink = sinkClient
        , CGCorvus.useCache = useCache
        , CGCorvus.buildCache = buildCache
        , CGCorvus.rebuildFrom = rebuildFromStep
        }
      (ccDaemon conn)
  pure tid

-- =====================================================================
-- Cloud-init manager
-- =====================================================================

rpcCloudInitSet
  :: CapnpConnection
  -> EntityRef
  -> Maybe Text
  -> Maybe Text
  -> Bool
  -> IO ()
rpcCloudInitSet conn vmRef mUserData mNetworkConfig injectKeys = do
  CGCorvus.Daemon'cloudInit'results {CGCorvus.mgr = mgr} <-
    callOn #cloudInit CGCorvus.Daemon'cloudInit'params (ccDaemon conn)
  let info =
        PCI.CloudInitInfo
          { PCI.ciiUserData = mUserData
          , PCI.ciiNetworkConfig = mNetworkConfig
          , PCI.ciiInjectSshKeys = injectKeys
          }
      p =
        CGCI.CloudInitSetParams
          { CGCI.vmRef = toCapnpEntityRef vmRef
          , CGCI.config = toCapnpCloudInitInfo info
          }
  _ <- callOn #set CGCI.CloudInitManager'set'params {CGCI.params = p} mgr
  pure ()

rpcCloudInitGet :: CapnpConnection -> EntityRef -> IO (Maybe PCI.CloudInitInfo)
rpcCloudInitGet conn vmRef = do
  CGCorvus.Daemon'cloudInit'results {CGCorvus.mgr = mgr} <-
    callOn #cloudInit CGCorvus.Daemon'cloudInit'params (ccDaemon conn)
  CGCI.CloudInitManager'get'results {CGCI.config = cfg} <-
    callOn
      #get
      CGCI.CloudInitManager'get'params {CGCI.vmRef = toCapnpEntityRef vmRef}
      mgr
  let parsed = fromCapnpCloudInitInfo cfg
  case parsed of
    PCI.CloudInitInfo {PCI.ciiUserData = Nothing, PCI.ciiNetworkConfig = Nothing, PCI.ciiInjectSshKeys = False} ->
      pure Nothing
    other -> pure (Just other)

rpcCloudInitDelete :: CapnpConnection -> EntityRef -> IO ()
rpcCloudInitDelete conn vmRef = do
  CGCorvus.Daemon'cloudInit'results {CGCorvus.mgr = mgr} <-
    callOn #cloudInit CGCorvus.Daemon'cloudInit'params (ccDaemon conn)
  _ <-
    callOn
      #delete
      CGCI.CloudInitManager'delete'params {CGCI.vmRef = toCapnpEntityRef vmRef}
      mgr
  pure ()
