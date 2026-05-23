{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | TemplateManager + Template cap implementations.
--
-- Phase 4c lands the synchronous template lifecycle (create /
-- delete / update). The instantiate method runs asynchronously
-- through a parent task and is therefore deferred to Phase 6
-- alongside the TaskProgressSink machinery.
module Corvus.Rpc.Template
  ( TemplateManagerCap (..)
  , TemplateCap (..)
  , newTemplateManagerCap
  )
where

import Capnp (export)
import qualified Capnp.Gen.Template as CGT
import qualified Capnp.Gen.Vm as CGVm
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer)
import Corvus.Action (runAction)
import Corvus.Handlers.Resolve (resolveTemplate)
import Corvus.Handlers.Template
  ( TemplateCreate (..)
  , TemplateDelete (..)
  , TemplateInstantiate (..)
  , TemplateUpdate (..)
  , handleTemplateList
  , handleTemplateShow
  )
import Corvus.Protocol (Response (..))
import qualified Corvus.Protocol as P
import Corvus.Rpc.Common (capnpRefToRef, failOnLeft, handleParsed)
import Corvus.Rpc.Vm (VmCap (..))
import Corvus.Types (ServerState (..))
import Corvus.Wire.Template (toCapnpTemplateDetails, toCapnpTemplateVmInfo)
import Data.Int (Int64)
import qualified Data.Text as T
import Supervisors (Supervisor)

data TemplateManagerCap = TemplateManagerCap
  { tmState :: !ServerState
  , tmSup :: !Supervisor
  , tmClientName :: !T.Text
  }

newTemplateManagerCap :: ServerState -> Supervisor -> T.Text -> IO TemplateManagerCap
newTemplateManagerCap st sup cn = pure (TemplateManagerCap st sup cn)

instance SomeServer TemplateManagerCap

instance CGT.TemplateManager'server_ TemplateManagerCap where
  templateManager'list (TemplateManagerCap st _ _) = handleParsed $ \_ -> do
    resp <- handleTemplateList st
    case resp of
      RespTemplateList templates ->
        pure
          CGT.TemplateManager'list'results
            { CGT.templates = map toCapnpTemplateVmInfo templates
            }
      RespError msg -> throwFailed msg
      _ -> throwFailed "templateManager'list: unexpected response"

  templateManager'get (TemplateManagerCap st sup cn) =
    handleParsed $ \CGT.TemplateManager'get'params {..} -> do
      ref' <- capnpRefToRef ref
      eid <- failOnLeft =<< resolveTemplate ref' (ssDbPool st)
      client <- export @CGT.Template sup (TemplateCap st sup eid cn)
      pure CGT.TemplateManager'get'results {CGT.template = client}

  templateManager'create (TemplateManagerCap st sup cn) =
    handleParsed $ \CGT.TemplateManager'create'params {..} -> do
      resp <- runAction st cn (TemplateCreate {tcrYaml = yaml})
      case resp of
        RespTemplateCreated tid -> do
          client <- export @CGT.Template sup (TemplateCap st sup tid cn)
          pure CGT.TemplateManager'create'results {CGT.template = client}
        RespError msg -> throwFailed msg
        _ -> throwFailed (T.pack ("templateManager'create: unexpected: " <> show resp))

data TemplateCap = TemplateCap
  { _tmplState :: !ServerState
  , _tmplSup :: !Supervisor
  , _tmplId :: !Int64
  , _tmplClientName :: !T.Text
  }

instance SomeServer TemplateCap

instance CGT.Template'server_ TemplateCap where
  template'show (TemplateCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- handleTemplateShow st eid
    case resp of
      RespTemplateInfo det ->
        pure CGT.Template'show'results {CGT.details = toCapnpTemplateDetails det}
      RespTemplateNotFound -> throwFailed "Template not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "template'show: unexpected response"

  template'delete (TemplateCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- runAction st cn (TemplateDelete eid)
    case resp of
      RespTemplateDeleted -> pure CGT.Template'delete'results
      RespTemplateNotFound -> throwFailed "Template not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "template'delete: unexpected response"

  template'update (TemplateCap st _ eid cn) =
    handleParsed $ \CGT.Template'update'params {..} -> do
      resp <- runAction st cn (TemplateUpdate {tupOldId = eid, tupYaml = yaml})
      case resp of
        RespTemplateUpdated _ -> pure CGT.Template'update'results
        RespTemplateNotFound -> throwFailed "Template not found"
        RespError msg -> throwFailed msg
        _ -> throwFailed "template'update: unexpected response"

  template'instantiate (TemplateCap st sup eid cn) =
    handleParsed $ \CGT.Template'instantiate'params {..} -> do
      nodeRef' <- capnpRefToRef node
      resp <-
        runAction
          st
          cn
          ( TemplateInstantiate
              { tiTemplateId = eid
              , tiName = name
              , tiNodeRef = P.unRef nodeRef'
              }
          )
      case resp of
        RespTemplateInstantiated newVmId -> do
          client <- export @CGVm.Vm sup (VmCap st sup newVmId cn)
          pure CGT.Template'instantiate'results {CGT.vm = client}
        RespTemplateNotFound -> throwFailed "Template not found"
        RespError msg -> throwFailed msg
        _ -> throwFailed "template'instantiate: unexpected response"
