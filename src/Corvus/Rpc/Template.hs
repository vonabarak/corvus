{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | TemplateManager + Template cap implementations.
module Corvus.Rpc.Template
  ( TemplateManagerCap (..)
  , TemplateCap (..)
  , newTemplateManagerCap
  )
where

import Capnp (export)
import qualified Capnp.Gen.Template as CGT
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer, handleParsed, methodUnimplemented)
import Corvus.Handlers.Resolve (resolveTemplate)
import Corvus.Handlers.Template (handleTemplateList, handleTemplateShow)
import Corvus.Protocol (Response (..))
import Corvus.Rpc.Common (capnpRefToRef, failOnLeft)
import Corvus.Types (ServerState (..))
import Corvus.Wire.Template (toCapnpTemplateDetails, toCapnpTemplateVmInfo)
import Data.Int (Int64)
import Supervisors (Supervisor)

data TemplateManagerCap = TemplateManagerCap
  { tmState :: !ServerState
  , tmSup :: !Supervisor
  }

newTemplateManagerCap :: ServerState -> Supervisor -> IO TemplateManagerCap
newTemplateManagerCap st sup = pure (TemplateManagerCap st sup)

instance SomeServer TemplateManagerCap

instance CGT.TemplateManager'server_ TemplateManagerCap where
  templateManager'list (TemplateManagerCap st _) = handleParsed $ \_ -> do
    resp <- handleTemplateList st
    case resp of
      RespTemplateList templates ->
        pure CGT.TemplateManager'list'results {CGT.templates = map toCapnpTemplateVmInfo templates}
      RespError msg -> throwFailed msg
      _ -> throwFailed "templateManager'list: unexpected response"

  templateManager'get (TemplateManagerCap st sup) = handleParsed $ \CGT.TemplateManager'get'params {..} -> do
    ref' <- capnpRefToRef ref
    eid <- failOnLeft =<< resolveTemplate ref' (ssDbPool st)
    client <- export @CGT.Template sup (TemplateCap st eid)
    pure CGT.TemplateManager'get'results {CGT.template = client}

  templateManager'create _ = methodUnimplemented

data TemplateCap = TemplateCap
  { _tmplState :: !ServerState
  , _tmplId :: !Int64
  }

instance SomeServer TemplateCap

instance CGT.Template'server_ TemplateCap where
  template'show (TemplateCap st eid) = handleParsed $ \_ -> do
    resp <- handleTemplateShow st eid
    case resp of
      RespTemplateInfo det ->
        pure CGT.Template'show'results {CGT.details = toCapnpTemplateDetails det}
      RespTemplateNotFound -> throwFailed "Template not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "template'show: unexpected response"

  template'delete _ = methodUnimplemented
  template'instantiate _ = methodUnimplemented
  template'update _ = methodUnimplemented
