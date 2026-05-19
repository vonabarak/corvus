{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Capnp.Gen.Corvus where
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP
import qualified Capnp.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers as GH
import qualified Capnp.Classes as C
import qualified GHC.Generics as Generics
import qualified Capnp.GenHelpers.Rpc as GH
import qualified Capnp.Gen.ById.X9b1373e2334a09e9
import qualified Capnp.Gen.ById.X9bd452a518ed3917
import qualified Capnp.Gen.ById.Xa40f88677f695703
import qualified Capnp.Gen.ById.Xa6341bd086aa89f6
import qualified Capnp.Gen.ById.Xa7366eabdb0b1db4
import qualified Capnp.Gen.ById.Xd3867de7dfe678c7
import qualified Capnp.Gen.ById.Xd6cec0faa39e02dc
import qualified Capnp.Gen.ById.Xe996a8e112b41e28
import qualified Capnp.Gen.ById.Xeb6a435f11477f84
import qualified Capnp.Gen.ById.Xec449e11027b2949
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data Daemon 
type instance (R.ReprFor Daemon) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Daemon) where
    typeId  = 10075231120243769355
instance (C.Parse Daemon (GH.Client Daemon)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Daemon) where
    type Server Daemon = Daemon'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Daemon)) [(GH.toUntypedMethodHandler ((daemon'ping) s_))
                                                                        ,(GH.toUntypedMethodHandler ((daemon'status) s_))
                                                                        ,(GH.toUntypedMethodHandler ((daemon'shutdown) s_))
                                                                        ,(GH.toUntypedMethodHandler ((daemon'vms) s_))
                                                                        ,(GH.toUntypedMethodHandler ((daemon'disks) s_))
                                                                        ,(GH.toUntypedMethodHandler ((daemon'networks) s_))
                                                                        ,(GH.toUntypedMethodHandler ((daemon'sshKeys) s_))
                                                                        ,(GH.toUntypedMethodHandler ((daemon'templates) s_))
                                                                        ,(GH.toUntypedMethodHandler ((daemon'tasks) s_))
                                                                        ,(GH.toUntypedMethodHandler ((daemon'cloudInit) s_))
                                                                        ,(GH.toUntypedMethodHandler ((daemon'apply) s_))
                                                                        ,(GH.toUntypedMethodHandler ((daemon'build) s_))
                                                                        ,(GH.toUntypedMethodHandler ((daemon'nodes) s_))] [])
class (Daemon'server_ s_) where
    {-# MINIMAL daemon'ping,daemon'status,daemon'shutdown,daemon'vms,daemon'disks,daemon'networks,daemon'sshKeys,daemon'templates,daemon'tasks,daemon'cloudInit,daemon'apply,daemon'build,daemon'nodes #-}
    daemon'ping :: s_ -> (GH.MethodHandler Daemon'ping'params Daemon'ping'results)
    daemon'ping _ = GH.methodUnimplemented
    daemon'status :: s_ -> (GH.MethodHandler Daemon'status'params Daemon'status'results)
    daemon'status _ = GH.methodUnimplemented
    daemon'shutdown :: s_ -> (GH.MethodHandler Daemon'shutdown'params Daemon'shutdown'results)
    daemon'shutdown _ = GH.methodUnimplemented
    daemon'vms :: s_ -> (GH.MethodHandler Daemon'vms'params Daemon'vms'results)
    daemon'vms _ = GH.methodUnimplemented
    daemon'disks :: s_ -> (GH.MethodHandler Daemon'disks'params Daemon'disks'results)
    daemon'disks _ = GH.methodUnimplemented
    daemon'networks :: s_ -> (GH.MethodHandler Daemon'networks'params Daemon'networks'results)
    daemon'networks _ = GH.methodUnimplemented
    daemon'sshKeys :: s_ -> (GH.MethodHandler Daemon'sshKeys'params Daemon'sshKeys'results)
    daemon'sshKeys _ = GH.methodUnimplemented
    daemon'templates :: s_ -> (GH.MethodHandler Daemon'templates'params Daemon'templates'results)
    daemon'templates _ = GH.methodUnimplemented
    daemon'tasks :: s_ -> (GH.MethodHandler Daemon'tasks'params Daemon'tasks'results)
    daemon'tasks _ = GH.methodUnimplemented
    daemon'cloudInit :: s_ -> (GH.MethodHandler Daemon'cloudInit'params Daemon'cloudInit'results)
    daemon'cloudInit _ = GH.methodUnimplemented
    daemon'apply :: s_ -> (GH.MethodHandler Daemon'apply'params Daemon'apply'results)
    daemon'apply _ = GH.methodUnimplemented
    daemon'build :: s_ -> (GH.MethodHandler Daemon'build'params Daemon'build'results)
    daemon'build _ = GH.methodUnimplemented
    daemon'nodes :: s_ -> (GH.MethodHandler Daemon'nodes'params Daemon'nodes'results)
    daemon'nodes _ = GH.methodUnimplemented
instance (GH.HasMethod "ping" Daemon Daemon'ping'params Daemon'ping'results) where
    methodByLabel  = (GH.Method 10075231120243769355 0)
instance (GH.HasMethod "status" Daemon Daemon'status'params Daemon'status'results) where
    methodByLabel  = (GH.Method 10075231120243769355 1)
instance (GH.HasMethod "shutdown" Daemon Daemon'shutdown'params Daemon'shutdown'results) where
    methodByLabel  = (GH.Method 10075231120243769355 2)
instance (GH.HasMethod "vms" Daemon Daemon'vms'params Daemon'vms'results) where
    methodByLabel  = (GH.Method 10075231120243769355 3)
instance (GH.HasMethod "disks" Daemon Daemon'disks'params Daemon'disks'results) where
    methodByLabel  = (GH.Method 10075231120243769355 4)
instance (GH.HasMethod "networks" Daemon Daemon'networks'params Daemon'networks'results) where
    methodByLabel  = (GH.Method 10075231120243769355 5)
instance (GH.HasMethod "sshKeys" Daemon Daemon'sshKeys'params Daemon'sshKeys'results) where
    methodByLabel  = (GH.Method 10075231120243769355 6)
instance (GH.HasMethod "templates" Daemon Daemon'templates'params Daemon'templates'results) where
    methodByLabel  = (GH.Method 10075231120243769355 7)
instance (GH.HasMethod "tasks" Daemon Daemon'tasks'params Daemon'tasks'results) where
    methodByLabel  = (GH.Method 10075231120243769355 8)
instance (GH.HasMethod "cloudInit" Daemon Daemon'cloudInit'params Daemon'cloudInit'results) where
    methodByLabel  = (GH.Method 10075231120243769355 9)
instance (GH.HasMethod "apply" Daemon Daemon'apply'params Daemon'apply'results) where
    methodByLabel  = (GH.Method 10075231120243769355 10)
instance (GH.HasMethod "build" Daemon Daemon'build'params Daemon'build'results) where
    methodByLabel  = (GH.Method 10075231120243769355 11)
instance (GH.HasMethod "nodes" Daemon Daemon'nodes'params Daemon'nodes'results) where
    methodByLabel  = (GH.Method 10075231120243769355 12)
data Daemon'ping'params 
type instance (R.ReprFor Daemon'ping'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'ping'params) where
    typeId  = 13255540453340966864
instance (C.TypedStruct Daemon'ping'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Daemon'ping'params) where
    type AllocHint Daemon'ping'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'ping'params (C.Parsed Daemon'ping'params))
instance (C.AllocateList Daemon'ping'params) where
    type ListAllocHint Daemon'ping'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'ping'params (C.Parsed Daemon'ping'params))
data instance C.Parsed Daemon'ping'params
    = Daemon'ping'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'ping'params))
deriving instance (Std_.Eq (C.Parsed Daemon'ping'params))
instance (C.Parse Daemon'ping'params (C.Parsed Daemon'ping'params)) where
    parse raw_ = (Std_.pure Daemon'ping'params)
instance (C.Marshal Daemon'ping'params (C.Parsed Daemon'ping'params)) where
    marshalInto _raw (Daemon'ping'params) = (Std_.pure ())
data Daemon'ping'results 
type instance (R.ReprFor Daemon'ping'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'ping'results) where
    typeId  = 15280386004455105369
instance (C.TypedStruct Daemon'ping'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Daemon'ping'results) where
    type AllocHint Daemon'ping'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'ping'results (C.Parsed Daemon'ping'results))
instance (C.AllocateList Daemon'ping'results) where
    type ListAllocHint Daemon'ping'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'ping'results (C.Parsed Daemon'ping'results))
data instance C.Parsed Daemon'ping'results
    = Daemon'ping'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'ping'results))
deriving instance (Std_.Eq (C.Parsed Daemon'ping'results))
instance (C.Parse Daemon'ping'results (C.Parsed Daemon'ping'results)) where
    parse raw_ = (Std_.pure Daemon'ping'results)
instance (C.Marshal Daemon'ping'results (C.Parsed Daemon'ping'results)) where
    marshalInto _raw (Daemon'ping'results) = (Std_.pure ())
data Daemon'status'params 
type instance (R.ReprFor Daemon'status'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'status'params) where
    typeId  = 17422380655454623580
instance (C.TypedStruct Daemon'status'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Daemon'status'params) where
    type AllocHint Daemon'status'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'status'params (C.Parsed Daemon'status'params))
instance (C.AllocateList Daemon'status'params) where
    type ListAllocHint Daemon'status'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'status'params (C.Parsed Daemon'status'params))
data instance C.Parsed Daemon'status'params
    = Daemon'status'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'status'params))
deriving instance (Std_.Eq (C.Parsed Daemon'status'params))
instance (C.Parse Daemon'status'params (C.Parsed Daemon'status'params)) where
    parse raw_ = (Std_.pure Daemon'status'params)
instance (C.Marshal Daemon'status'params (C.Parsed Daemon'status'params)) where
    marshalInto _raw (Daemon'status'params) = (Std_.pure ())
data Daemon'status'results 
type instance (R.ReprFor Daemon'status'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'status'results) where
    typeId  = 9561010492983344974
instance (C.TypedStruct Daemon'status'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Daemon'status'results) where
    type AllocHint Daemon'status'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'status'results (C.Parsed Daemon'status'results))
instance (C.AllocateList Daemon'status'results) where
    type ListAllocHint Daemon'status'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'status'results (C.Parsed Daemon'status'results))
data instance C.Parsed Daemon'status'results
    = Daemon'status'results 
        {info :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.StatusInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'status'results))
deriving instance (Std_.Eq (C.Parsed Daemon'status'results))
instance (C.Parse Daemon'status'results (C.Parsed Daemon'status'results)) where
    parse raw_ = (Daemon'status'results <$> (GH.parseField #info raw_))
instance (C.Marshal Daemon'status'results (C.Parsed Daemon'status'results)) where
    marshalInto raw_ Daemon'status'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot Daemon'status'results Capnp.Gen.ById.X9b1373e2334a09e9.StatusInfo) where
    fieldByLabel  = (GH.ptrField 0)
data Daemon'shutdown'params 
type instance (R.ReprFor Daemon'shutdown'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'shutdown'params) where
    typeId  = 15220140976734773680
instance (C.TypedStruct Daemon'shutdown'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Daemon'shutdown'params) where
    type AllocHint Daemon'shutdown'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'shutdown'params (C.Parsed Daemon'shutdown'params))
instance (C.AllocateList Daemon'shutdown'params) where
    type ListAllocHint Daemon'shutdown'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'shutdown'params (C.Parsed Daemon'shutdown'params))
data instance C.Parsed Daemon'shutdown'params
    = Daemon'shutdown'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'shutdown'params))
deriving instance (Std_.Eq (C.Parsed Daemon'shutdown'params))
instance (C.Parse Daemon'shutdown'params (C.Parsed Daemon'shutdown'params)) where
    parse raw_ = (Std_.pure Daemon'shutdown'params)
instance (C.Marshal Daemon'shutdown'params (C.Parsed Daemon'shutdown'params)) where
    marshalInto _raw (Daemon'shutdown'params) = (Std_.pure ())
data Daemon'shutdown'results 
type instance (R.ReprFor Daemon'shutdown'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'shutdown'results) where
    typeId  = 17963219852663183071
instance (C.TypedStruct Daemon'shutdown'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Daemon'shutdown'results) where
    type AllocHint Daemon'shutdown'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'shutdown'results (C.Parsed Daemon'shutdown'results))
instance (C.AllocateList Daemon'shutdown'results) where
    type ListAllocHint Daemon'shutdown'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'shutdown'results (C.Parsed Daemon'shutdown'results))
data instance C.Parsed Daemon'shutdown'results
    = Daemon'shutdown'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'shutdown'results))
deriving instance (Std_.Eq (C.Parsed Daemon'shutdown'results))
instance (C.Parse Daemon'shutdown'results (C.Parsed Daemon'shutdown'results)) where
    parse raw_ = (Std_.pure Daemon'shutdown'results)
instance (C.Marshal Daemon'shutdown'results (C.Parsed Daemon'shutdown'results)) where
    marshalInto _raw (Daemon'shutdown'results) = (Std_.pure ())
data Daemon'vms'params 
type instance (R.ReprFor Daemon'vms'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'vms'params) where
    typeId  = 16293187915961599193
instance (C.TypedStruct Daemon'vms'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Daemon'vms'params) where
    type AllocHint Daemon'vms'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'vms'params (C.Parsed Daemon'vms'params))
instance (C.AllocateList Daemon'vms'params) where
    type ListAllocHint Daemon'vms'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'vms'params (C.Parsed Daemon'vms'params))
data instance C.Parsed Daemon'vms'params
    = Daemon'vms'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'vms'params))
deriving instance (Std_.Eq (C.Parsed Daemon'vms'params))
instance (C.Parse Daemon'vms'params (C.Parsed Daemon'vms'params)) where
    parse raw_ = (Std_.pure Daemon'vms'params)
instance (C.Marshal Daemon'vms'params (C.Parsed Daemon'vms'params)) where
    marshalInto _raw (Daemon'vms'params) = (Std_.pure ())
data Daemon'vms'results 
type instance (R.ReprFor Daemon'vms'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'vms'results) where
    typeId  = 9264039317352353451
instance (C.TypedStruct Daemon'vms'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Daemon'vms'results) where
    type AllocHint Daemon'vms'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'vms'results (C.Parsed Daemon'vms'results))
instance (C.AllocateList Daemon'vms'results) where
    type ListAllocHint Daemon'vms'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'vms'results (C.Parsed Daemon'vms'results))
data instance C.Parsed Daemon'vms'results
    = Daemon'vms'results 
        {mgr :: (RP.Parsed Capnp.Gen.ById.Xa7366eabdb0b1db4.VmManager)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'vms'results))
deriving instance (Std_.Eq (C.Parsed Daemon'vms'results))
instance (C.Parse Daemon'vms'results (C.Parsed Daemon'vms'results)) where
    parse raw_ = (Daemon'vms'results <$> (GH.parseField #mgr raw_))
instance (C.Marshal Daemon'vms'results (C.Parsed Daemon'vms'results)) where
    marshalInto raw_ Daemon'vms'results{..} = (do
        (GH.encodeField #mgr mgr raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mgr" GH.Slot Daemon'vms'results Capnp.Gen.ById.Xa7366eabdb0b1db4.VmManager) where
    fieldByLabel  = (GH.ptrField 0)
data Daemon'disks'params 
type instance (R.ReprFor Daemon'disks'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'disks'params) where
    typeId  = 14398923136903134655
instance (C.TypedStruct Daemon'disks'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Daemon'disks'params) where
    type AllocHint Daemon'disks'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'disks'params (C.Parsed Daemon'disks'params))
instance (C.AllocateList Daemon'disks'params) where
    type ListAllocHint Daemon'disks'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'disks'params (C.Parsed Daemon'disks'params))
data instance C.Parsed Daemon'disks'params
    = Daemon'disks'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'disks'params))
deriving instance (Std_.Eq (C.Parsed Daemon'disks'params))
instance (C.Parse Daemon'disks'params (C.Parsed Daemon'disks'params)) where
    parse raw_ = (Std_.pure Daemon'disks'params)
instance (C.Marshal Daemon'disks'params (C.Parsed Daemon'disks'params)) where
    marshalInto _raw (Daemon'disks'params) = (Std_.pure ())
data Daemon'disks'results 
type instance (R.ReprFor Daemon'disks'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'disks'results) where
    typeId  = 10719606867824804276
instance (C.TypedStruct Daemon'disks'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Daemon'disks'results) where
    type AllocHint Daemon'disks'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'disks'results (C.Parsed Daemon'disks'results))
instance (C.AllocateList Daemon'disks'results) where
    type ListAllocHint Daemon'disks'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'disks'results (C.Parsed Daemon'disks'results))
data instance C.Parsed Daemon'disks'results
    = Daemon'disks'results 
        {mgr :: (RP.Parsed Capnp.Gen.ById.Xec449e11027b2949.DiskManager)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'disks'results))
deriving instance (Std_.Eq (C.Parsed Daemon'disks'results))
instance (C.Parse Daemon'disks'results (C.Parsed Daemon'disks'results)) where
    parse raw_ = (Daemon'disks'results <$> (GH.parseField #mgr raw_))
instance (C.Marshal Daemon'disks'results (C.Parsed Daemon'disks'results)) where
    marshalInto raw_ Daemon'disks'results{..} = (do
        (GH.encodeField #mgr mgr raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mgr" GH.Slot Daemon'disks'results Capnp.Gen.ById.Xec449e11027b2949.DiskManager) where
    fieldByLabel  = (GH.ptrField 0)
data Daemon'networks'params 
type instance (R.ReprFor Daemon'networks'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'networks'params) where
    typeId  = 10133525933545396352
instance (C.TypedStruct Daemon'networks'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Daemon'networks'params) where
    type AllocHint Daemon'networks'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'networks'params (C.Parsed Daemon'networks'params))
instance (C.AllocateList Daemon'networks'params) where
    type ListAllocHint Daemon'networks'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'networks'params (C.Parsed Daemon'networks'params))
data instance C.Parsed Daemon'networks'params
    = Daemon'networks'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'networks'params))
deriving instance (Std_.Eq (C.Parsed Daemon'networks'params))
instance (C.Parse Daemon'networks'params (C.Parsed Daemon'networks'params)) where
    parse raw_ = (Std_.pure Daemon'networks'params)
instance (C.Marshal Daemon'networks'params (C.Parsed Daemon'networks'params)) where
    marshalInto _raw (Daemon'networks'params) = (Std_.pure ())
data Daemon'networks'results 
type instance (R.ReprFor Daemon'networks'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'networks'results) where
    typeId  = 15952653162121831854
instance (C.TypedStruct Daemon'networks'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Daemon'networks'results) where
    type AllocHint Daemon'networks'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'networks'results (C.Parsed Daemon'networks'results))
instance (C.AllocateList Daemon'networks'results) where
    type ListAllocHint Daemon'networks'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'networks'results (C.Parsed Daemon'networks'results))
data instance C.Parsed Daemon'networks'results
    = Daemon'networks'results 
        {mgr :: (RP.Parsed Capnp.Gen.ById.Xe996a8e112b41e28.NetworkManager)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'networks'results))
deriving instance (Std_.Eq (C.Parsed Daemon'networks'results))
instance (C.Parse Daemon'networks'results (C.Parsed Daemon'networks'results)) where
    parse raw_ = (Daemon'networks'results <$> (GH.parseField #mgr raw_))
instance (C.Marshal Daemon'networks'results (C.Parsed Daemon'networks'results)) where
    marshalInto raw_ Daemon'networks'results{..} = (do
        (GH.encodeField #mgr mgr raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mgr" GH.Slot Daemon'networks'results Capnp.Gen.ById.Xe996a8e112b41e28.NetworkManager) where
    fieldByLabel  = (GH.ptrField 0)
data Daemon'sshKeys'params 
type instance (R.ReprFor Daemon'sshKeys'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'sshKeys'params) where
    typeId  = 11956247210656419418
instance (C.TypedStruct Daemon'sshKeys'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Daemon'sshKeys'params) where
    type AllocHint Daemon'sshKeys'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'sshKeys'params (C.Parsed Daemon'sshKeys'params))
instance (C.AllocateList Daemon'sshKeys'params) where
    type ListAllocHint Daemon'sshKeys'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'sshKeys'params (C.Parsed Daemon'sshKeys'params))
data instance C.Parsed Daemon'sshKeys'params
    = Daemon'sshKeys'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'sshKeys'params))
deriving instance (Std_.Eq (C.Parsed Daemon'sshKeys'params))
instance (C.Parse Daemon'sshKeys'params (C.Parsed Daemon'sshKeys'params)) where
    parse raw_ = (Std_.pure Daemon'sshKeys'params)
instance (C.Marshal Daemon'sshKeys'params (C.Parsed Daemon'sshKeys'params)) where
    marshalInto _raw (Daemon'sshKeys'params) = (Std_.pure ())
data Daemon'sshKeys'results 
type instance (R.ReprFor Daemon'sshKeys'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'sshKeys'results) where
    typeId  = 10011159708721135637
instance (C.TypedStruct Daemon'sshKeys'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Daemon'sshKeys'results) where
    type AllocHint Daemon'sshKeys'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'sshKeys'results (C.Parsed Daemon'sshKeys'results))
instance (C.AllocateList Daemon'sshKeys'results) where
    type ListAllocHint Daemon'sshKeys'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'sshKeys'results (C.Parsed Daemon'sshKeys'results))
data instance C.Parsed Daemon'sshKeys'results
    = Daemon'sshKeys'results 
        {mgr :: (RP.Parsed Capnp.Gen.ById.Xa6341bd086aa89f6.SshKeyManager)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'sshKeys'results))
deriving instance (Std_.Eq (C.Parsed Daemon'sshKeys'results))
instance (C.Parse Daemon'sshKeys'results (C.Parsed Daemon'sshKeys'results)) where
    parse raw_ = (Daemon'sshKeys'results <$> (GH.parseField #mgr raw_))
instance (C.Marshal Daemon'sshKeys'results (C.Parsed Daemon'sshKeys'results)) where
    marshalInto raw_ Daemon'sshKeys'results{..} = (do
        (GH.encodeField #mgr mgr raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mgr" GH.Slot Daemon'sshKeys'results Capnp.Gen.ById.Xa6341bd086aa89f6.SshKeyManager) where
    fieldByLabel  = (GH.ptrField 0)
data Daemon'templates'params 
type instance (R.ReprFor Daemon'templates'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'templates'params) where
    typeId  = 16991001583848856701
instance (C.TypedStruct Daemon'templates'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Daemon'templates'params) where
    type AllocHint Daemon'templates'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'templates'params (C.Parsed Daemon'templates'params))
instance (C.AllocateList Daemon'templates'params) where
    type ListAllocHint Daemon'templates'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'templates'params (C.Parsed Daemon'templates'params))
data instance C.Parsed Daemon'templates'params
    = Daemon'templates'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'templates'params))
deriving instance (Std_.Eq (C.Parsed Daemon'templates'params))
instance (C.Parse Daemon'templates'params (C.Parsed Daemon'templates'params)) where
    parse raw_ = (Std_.pure Daemon'templates'params)
instance (C.Marshal Daemon'templates'params (C.Parsed Daemon'templates'params)) where
    marshalInto _raw (Daemon'templates'params) = (Std_.pure ())
data Daemon'templates'results 
type instance (R.ReprFor Daemon'templates'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'templates'results) where
    typeId  = 18138422170438116415
instance (C.TypedStruct Daemon'templates'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Daemon'templates'results) where
    type AllocHint Daemon'templates'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'templates'results (C.Parsed Daemon'templates'results))
instance (C.AllocateList Daemon'templates'results) where
    type ListAllocHint Daemon'templates'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'templates'results (C.Parsed Daemon'templates'results))
data instance C.Parsed Daemon'templates'results
    = Daemon'templates'results 
        {mgr :: (RP.Parsed Capnp.Gen.ById.Xd6cec0faa39e02dc.TemplateManager)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'templates'results))
deriving instance (Std_.Eq (C.Parsed Daemon'templates'results))
instance (C.Parse Daemon'templates'results (C.Parsed Daemon'templates'results)) where
    parse raw_ = (Daemon'templates'results <$> (GH.parseField #mgr raw_))
instance (C.Marshal Daemon'templates'results (C.Parsed Daemon'templates'results)) where
    marshalInto raw_ Daemon'templates'results{..} = (do
        (GH.encodeField #mgr mgr raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mgr" GH.Slot Daemon'templates'results Capnp.Gen.ById.Xd6cec0faa39e02dc.TemplateManager) where
    fieldByLabel  = (GH.ptrField 0)
data Daemon'tasks'params 
type instance (R.ReprFor Daemon'tasks'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'tasks'params) where
    typeId  = 9531307684953217110
instance (C.TypedStruct Daemon'tasks'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Daemon'tasks'params) where
    type AllocHint Daemon'tasks'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'tasks'params (C.Parsed Daemon'tasks'params))
instance (C.AllocateList Daemon'tasks'params) where
    type ListAllocHint Daemon'tasks'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'tasks'params (C.Parsed Daemon'tasks'params))
data instance C.Parsed Daemon'tasks'params
    = Daemon'tasks'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'tasks'params))
deriving instance (Std_.Eq (C.Parsed Daemon'tasks'params))
instance (C.Parse Daemon'tasks'params (C.Parsed Daemon'tasks'params)) where
    parse raw_ = (Std_.pure Daemon'tasks'params)
instance (C.Marshal Daemon'tasks'params (C.Parsed Daemon'tasks'params)) where
    marshalInto _raw (Daemon'tasks'params) = (Std_.pure ())
data Daemon'tasks'results 
type instance (R.ReprFor Daemon'tasks'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'tasks'results) where
    typeId  = 16044087242264537451
instance (C.TypedStruct Daemon'tasks'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Daemon'tasks'results) where
    type AllocHint Daemon'tasks'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'tasks'results (C.Parsed Daemon'tasks'results))
instance (C.AllocateList Daemon'tasks'results) where
    type ListAllocHint Daemon'tasks'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'tasks'results (C.Parsed Daemon'tasks'results))
data instance C.Parsed Daemon'tasks'results
    = Daemon'tasks'results 
        {mgr :: (RP.Parsed Capnp.Gen.ById.Xd3867de7dfe678c7.TaskManager)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'tasks'results))
deriving instance (Std_.Eq (C.Parsed Daemon'tasks'results))
instance (C.Parse Daemon'tasks'results (C.Parsed Daemon'tasks'results)) where
    parse raw_ = (Daemon'tasks'results <$> (GH.parseField #mgr raw_))
instance (C.Marshal Daemon'tasks'results (C.Parsed Daemon'tasks'results)) where
    marshalInto raw_ Daemon'tasks'results{..} = (do
        (GH.encodeField #mgr mgr raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mgr" GH.Slot Daemon'tasks'results Capnp.Gen.ById.Xd3867de7dfe678c7.TaskManager) where
    fieldByLabel  = (GH.ptrField 0)
data Daemon'cloudInit'params 
type instance (R.ReprFor Daemon'cloudInit'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'cloudInit'params) where
    typeId  = 13723625741851582373
instance (C.TypedStruct Daemon'cloudInit'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Daemon'cloudInit'params) where
    type AllocHint Daemon'cloudInit'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'cloudInit'params (C.Parsed Daemon'cloudInit'params))
instance (C.AllocateList Daemon'cloudInit'params) where
    type ListAllocHint Daemon'cloudInit'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'cloudInit'params (C.Parsed Daemon'cloudInit'params))
data instance C.Parsed Daemon'cloudInit'params
    = Daemon'cloudInit'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'cloudInit'params))
deriving instance (Std_.Eq (C.Parsed Daemon'cloudInit'params))
instance (C.Parse Daemon'cloudInit'params (C.Parsed Daemon'cloudInit'params)) where
    parse raw_ = (Std_.pure Daemon'cloudInit'params)
instance (C.Marshal Daemon'cloudInit'params (C.Parsed Daemon'cloudInit'params)) where
    marshalInto _raw (Daemon'cloudInit'params) = (Std_.pure ())
data Daemon'cloudInit'results 
type instance (R.ReprFor Daemon'cloudInit'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'cloudInit'results) where
    typeId  = 10932219350736185504
instance (C.TypedStruct Daemon'cloudInit'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Daemon'cloudInit'results) where
    type AllocHint Daemon'cloudInit'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'cloudInit'results (C.Parsed Daemon'cloudInit'results))
instance (C.AllocateList Daemon'cloudInit'results) where
    type ListAllocHint Daemon'cloudInit'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'cloudInit'results (C.Parsed Daemon'cloudInit'results))
data instance C.Parsed Daemon'cloudInit'results
    = Daemon'cloudInit'results 
        {mgr :: (RP.Parsed Capnp.Gen.ById.Xeb6a435f11477f84.CloudInitManager)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'cloudInit'results))
deriving instance (Std_.Eq (C.Parsed Daemon'cloudInit'results))
instance (C.Parse Daemon'cloudInit'results (C.Parsed Daemon'cloudInit'results)) where
    parse raw_ = (Daemon'cloudInit'results <$> (GH.parseField #mgr raw_))
instance (C.Marshal Daemon'cloudInit'results (C.Parsed Daemon'cloudInit'results)) where
    marshalInto raw_ Daemon'cloudInit'results{..} = (do
        (GH.encodeField #mgr mgr raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mgr" GH.Slot Daemon'cloudInit'results Capnp.Gen.ById.Xeb6a435f11477f84.CloudInitManager) where
    fieldByLabel  = (GH.ptrField 0)
data Daemon'apply'params 
type instance (R.ReprFor Daemon'apply'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'apply'params) where
    typeId  = 18084501204537376626
instance (C.TypedStruct Daemon'apply'params) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Daemon'apply'params) where
    type AllocHint Daemon'apply'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'apply'params (C.Parsed Daemon'apply'params))
instance (C.AllocateList Daemon'apply'params) where
    type ListAllocHint Daemon'apply'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'apply'params (C.Parsed Daemon'apply'params))
data instance C.Parsed Daemon'apply'params
    = Daemon'apply'params 
        {yaml :: (RP.Parsed Basics.Text)
        ,skipExisting :: (RP.Parsed Std_.Bool)
        ,wait :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'apply'params))
deriving instance (Std_.Eq (C.Parsed Daemon'apply'params))
instance (C.Parse Daemon'apply'params (C.Parsed Daemon'apply'params)) where
    parse raw_ = (Daemon'apply'params <$> (GH.parseField #yaml raw_)
                                      <*> (GH.parseField #skipExisting raw_)
                                      <*> (GH.parseField #wait raw_))
instance (C.Marshal Daemon'apply'params (C.Parsed Daemon'apply'params)) where
    marshalInto raw_ Daemon'apply'params{..} = (do
        (GH.encodeField #yaml yaml raw_)
        (GH.encodeField #skipExisting skipExisting raw_)
        (GH.encodeField #wait wait raw_)
        (Std_.pure ())
        )
instance (GH.HasField "yaml" GH.Slot Daemon'apply'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "skipExisting" GH.Slot Daemon'apply'params Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (GH.HasField "wait" GH.Slot Daemon'apply'params Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 0 1 0)
data Daemon'apply'results 
type instance (R.ReprFor Daemon'apply'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'apply'results) where
    typeId  = 12093927715274901926
instance (C.TypedStruct Daemon'apply'results) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Daemon'apply'results) where
    type AllocHint Daemon'apply'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'apply'results (C.Parsed Daemon'apply'results))
instance (C.AllocateList Daemon'apply'results) where
    type ListAllocHint Daemon'apply'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'apply'results (C.Parsed Daemon'apply'results))
data instance C.Parsed Daemon'apply'results
    = Daemon'apply'results 
        {result :: (RP.Parsed ApplyResult)
        ,taskId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'apply'results))
deriving instance (Std_.Eq (C.Parsed Daemon'apply'results))
instance (C.Parse Daemon'apply'results (C.Parsed Daemon'apply'results)) where
    parse raw_ = (Daemon'apply'results <$> (GH.parseField #result raw_)
                                       <*> (GH.parseField #taskId raw_))
instance (C.Marshal Daemon'apply'results (C.Parsed Daemon'apply'results)) where
    marshalInto raw_ Daemon'apply'results{..} = (do
        (GH.encodeField #result result raw_)
        (GH.encodeField #taskId taskId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Daemon'apply'results ApplyResult) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "taskId" GH.Slot Daemon'apply'results Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Daemon'build'params 
type instance (R.ReprFor Daemon'build'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'build'params) where
    typeId  = 14489102828123824718
instance (C.TypedStruct Daemon'build'params) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Daemon'build'params) where
    type AllocHint Daemon'build'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'build'params (C.Parsed Daemon'build'params))
instance (C.AllocateList Daemon'build'params) where
    type ListAllocHint Daemon'build'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'build'params (C.Parsed Daemon'build'params))
data instance C.Parsed Daemon'build'params
    = Daemon'build'params 
        {yaml :: (RP.Parsed Basics.Text)
        ,sink :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.BuildEventSink)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'build'params))
deriving instance (Std_.Eq (C.Parsed Daemon'build'params))
instance (C.Parse Daemon'build'params (C.Parsed Daemon'build'params)) where
    parse raw_ = (Daemon'build'params <$> (GH.parseField #yaml raw_)
                                      <*> (GH.parseField #sink raw_))
instance (C.Marshal Daemon'build'params (C.Parsed Daemon'build'params)) where
    marshalInto raw_ Daemon'build'params{..} = (do
        (GH.encodeField #yaml yaml raw_)
        (GH.encodeField #sink sink raw_)
        (Std_.pure ())
        )
instance (GH.HasField "yaml" GH.Slot Daemon'build'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "sink" GH.Slot Daemon'build'params Capnp.Gen.ById.X9bd452a518ed3917.BuildEventSink) where
    fieldByLabel  = (GH.ptrField 1)
data Daemon'build'results 
type instance (R.ReprFor Daemon'build'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'build'results) where
    typeId  = 16900763068727882009
instance (C.TypedStruct Daemon'build'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Daemon'build'results) where
    type AllocHint Daemon'build'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'build'results (C.Parsed Daemon'build'results))
instance (C.AllocateList Daemon'build'results) where
    type ListAllocHint Daemon'build'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'build'results (C.Parsed Daemon'build'results))
data instance C.Parsed Daemon'build'results
    = Daemon'build'results 
        {taskId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'build'results))
deriving instance (Std_.Eq (C.Parsed Daemon'build'results))
instance (C.Parse Daemon'build'results (C.Parsed Daemon'build'results)) where
    parse raw_ = (Daemon'build'results <$> (GH.parseField #taskId raw_))
instance (C.Marshal Daemon'build'results (C.Parsed Daemon'build'results)) where
    marshalInto raw_ Daemon'build'results{..} = (do
        (GH.encodeField #taskId taskId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "taskId" GH.Slot Daemon'build'results Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Daemon'nodes'params 
type instance (R.ReprFor Daemon'nodes'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'nodes'params) where
    typeId  = 14675049721470429859
instance (C.TypedStruct Daemon'nodes'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Daemon'nodes'params) where
    type AllocHint Daemon'nodes'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'nodes'params (C.Parsed Daemon'nodes'params))
instance (C.AllocateList Daemon'nodes'params) where
    type ListAllocHint Daemon'nodes'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'nodes'params (C.Parsed Daemon'nodes'params))
data instance C.Parsed Daemon'nodes'params
    = Daemon'nodes'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'nodes'params))
deriving instance (Std_.Eq (C.Parsed Daemon'nodes'params))
instance (C.Parse Daemon'nodes'params (C.Parsed Daemon'nodes'params)) where
    parse raw_ = (Std_.pure Daemon'nodes'params)
instance (C.Marshal Daemon'nodes'params (C.Parsed Daemon'nodes'params)) where
    marshalInto _raw (Daemon'nodes'params) = (Std_.pure ())
data Daemon'nodes'results 
type instance (R.ReprFor Daemon'nodes'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Daemon'nodes'results) where
    typeId  = 15635407695846282007
instance (C.TypedStruct Daemon'nodes'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Daemon'nodes'results) where
    type AllocHint Daemon'nodes'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Daemon'nodes'results (C.Parsed Daemon'nodes'results))
instance (C.AllocateList Daemon'nodes'results) where
    type ListAllocHint Daemon'nodes'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Daemon'nodes'results (C.Parsed Daemon'nodes'results))
data instance C.Parsed Daemon'nodes'results
    = Daemon'nodes'results 
        {mgr :: (RP.Parsed Capnp.Gen.ById.Xa40f88677f695703.NodeManager)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Daemon'nodes'results))
deriving instance (Std_.Eq (C.Parsed Daemon'nodes'results))
instance (C.Parse Daemon'nodes'results (C.Parsed Daemon'nodes'results)) where
    parse raw_ = (Daemon'nodes'results <$> (GH.parseField #mgr raw_))
instance (C.Marshal Daemon'nodes'results (C.Parsed Daemon'nodes'results)) where
    marshalInto raw_ Daemon'nodes'results{..} = (do
        (GH.encodeField #mgr mgr raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mgr" GH.Slot Daemon'nodes'results Capnp.Gen.ById.Xa40f88677f695703.NodeManager) where
    fieldByLabel  = (GH.ptrField 0)
data ApplyCreated 
type instance (R.ReprFor ApplyCreated) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId ApplyCreated) where
    typeId  = 13718780975861931671
instance (C.TypedStruct ApplyCreated) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate ApplyCreated) where
    type AllocHint ApplyCreated = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc ApplyCreated (C.Parsed ApplyCreated))
instance (C.AllocateList ApplyCreated) where
    type ListAllocHint ApplyCreated = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc ApplyCreated (C.Parsed ApplyCreated))
data instance C.Parsed ApplyCreated
    = ApplyCreated 
        {name :: (RP.Parsed Basics.Text)
        ,id :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed ApplyCreated))
deriving instance (Std_.Eq (C.Parsed ApplyCreated))
instance (C.Parse ApplyCreated (C.Parsed ApplyCreated)) where
    parse raw_ = (ApplyCreated <$> (GH.parseField #name raw_)
                               <*> (GH.parseField #id raw_))
instance (C.Marshal ApplyCreated (C.Parsed ApplyCreated)) where
    marshalInto raw_ ApplyCreated{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #id id raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot ApplyCreated Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "id" GH.Slot ApplyCreated Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data ApplyResult 
type instance (R.ReprFor ApplyResult) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId ApplyResult) where
    typeId  = 11539697773667837684
instance (C.TypedStruct ApplyResult) where
    numStructWords  = 0
    numStructPtrs  = 5
instance (C.Allocate ApplyResult) where
    type AllocHint ApplyResult = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc ApplyResult (C.Parsed ApplyResult))
instance (C.AllocateList ApplyResult) where
    type ListAllocHint ApplyResult = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc ApplyResult (C.Parsed ApplyResult))
data instance C.Parsed ApplyResult
    = ApplyResult 
        {sshKeys :: (RP.Parsed (R.List ApplyCreated))
        ,disks :: (RP.Parsed (R.List ApplyCreated))
        ,networks :: (RP.Parsed (R.List ApplyCreated))
        ,vms :: (RP.Parsed (R.List ApplyCreated))
        ,templates :: (RP.Parsed (R.List ApplyCreated))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed ApplyResult))
deriving instance (Std_.Eq (C.Parsed ApplyResult))
instance (C.Parse ApplyResult (C.Parsed ApplyResult)) where
    parse raw_ = (ApplyResult <$> (GH.parseField #sshKeys raw_)
                              <*> (GH.parseField #disks raw_)
                              <*> (GH.parseField #networks raw_)
                              <*> (GH.parseField #vms raw_)
                              <*> (GH.parseField #templates raw_))
instance (C.Marshal ApplyResult (C.Parsed ApplyResult)) where
    marshalInto raw_ ApplyResult{..} = (do
        (GH.encodeField #sshKeys sshKeys raw_)
        (GH.encodeField #disks disks raw_)
        (GH.encodeField #networks networks raw_)
        (GH.encodeField #vms vms raw_)
        (GH.encodeField #templates templates raw_)
        (Std_.pure ())
        )
instance (GH.HasField "sshKeys" GH.Slot ApplyResult (R.List ApplyCreated)) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "disks" GH.Slot ApplyResult (R.List ApplyCreated)) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "networks" GH.Slot ApplyResult (R.List ApplyCreated)) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "vms" GH.Slot ApplyResult (R.List ApplyCreated)) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "templates" GH.Slot ApplyResult (R.List ApplyCreated)) where
    fieldByLabel  = (GH.ptrField 4)