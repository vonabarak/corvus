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
module Capnp.Gen.Sshkey where
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP
import qualified Capnp.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers as GH
import qualified Capnp.Classes as C
import qualified GHC.Generics as Generics
import qualified Capnp.GenHelpers.Rpc as GH
import qualified Capnp.Gen.ById.X9b1373e2334a09e9
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data SshKeyInfo 
type instance (R.ReprFor SshKeyInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId SshKeyInfo) where
    typeId  = 12149016458122548403
instance (C.TypedStruct SshKeyInfo) where
    numStructWords  = 2
    numStructPtrs  = 3
instance (C.Allocate SshKeyInfo) where
    type AllocHint SshKeyInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc SshKeyInfo (C.Parsed SshKeyInfo))
instance (C.AllocateList SshKeyInfo) where
    type ListAllocHint SshKeyInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc SshKeyInfo (C.Parsed SshKeyInfo))
data instance C.Parsed SshKeyInfo
    = SshKeyInfo 
        {id :: (RP.Parsed Std_.Int64)
        ,name :: (RP.Parsed Basics.Text)
        ,publicKey :: (RP.Parsed Basics.Text)
        ,createdAt :: (RP.Parsed Std_.Int64)
        ,attachedVms :: (RP.Parsed (R.List VmAttachment))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed SshKeyInfo))
deriving instance (Std_.Eq (C.Parsed SshKeyInfo))
instance (C.Parse SshKeyInfo (C.Parsed SshKeyInfo)) where
    parse raw_ = (SshKeyInfo <$> (GH.parseField #id raw_)
                             <*> (GH.parseField #name raw_)
                             <*> (GH.parseField #publicKey raw_)
                             <*> (GH.parseField #createdAt raw_)
                             <*> (GH.parseField #attachedVms raw_))
instance (C.Marshal SshKeyInfo (C.Parsed SshKeyInfo)) where
    marshalInto raw_ SshKeyInfo{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #publicKey publicKey raw_)
        (GH.encodeField #createdAt createdAt raw_)
        (GH.encodeField #attachedVms attachedVms raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot SshKeyInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "name" GH.Slot SshKeyInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "publicKey" GH.Slot SshKeyInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "createdAt" GH.Slot SshKeyInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "attachedVms" GH.Slot SshKeyInfo (R.List VmAttachment)) where
    fieldByLabel  = (GH.ptrField 2)
data VmAttachment 
type instance (R.ReprFor VmAttachment) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmAttachment) where
    typeId  = 12003266486494357338
instance (C.TypedStruct VmAttachment) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate VmAttachment) where
    type AllocHint VmAttachment = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmAttachment (C.Parsed VmAttachment))
instance (C.AllocateList VmAttachment) where
    type ListAllocHint VmAttachment = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmAttachment (C.Parsed VmAttachment))
data instance C.Parsed VmAttachment
    = VmAttachment 
        {vmId :: (RP.Parsed Std_.Int64)
        ,vmName :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmAttachment))
deriving instance (Std_.Eq (C.Parsed VmAttachment))
instance (C.Parse VmAttachment (C.Parsed VmAttachment)) where
    parse raw_ = (VmAttachment <$> (GH.parseField #vmId raw_)
                               <*> (GH.parseField #vmName raw_))
instance (C.Marshal VmAttachment (C.Parsed VmAttachment)) where
    marshalInto raw_ VmAttachment{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (GH.encodeField #vmName vmName raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot VmAttachment Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "vmName" GH.Slot VmAttachment Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data SshKeyCreateParams 
type instance (R.ReprFor SshKeyCreateParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId SshKeyCreateParams) where
    typeId  = 11358976192894150037
instance (C.TypedStruct SshKeyCreateParams) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate SshKeyCreateParams) where
    type AllocHint SshKeyCreateParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc SshKeyCreateParams (C.Parsed SshKeyCreateParams))
instance (C.AllocateList SshKeyCreateParams) where
    type ListAllocHint SshKeyCreateParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc SshKeyCreateParams (C.Parsed SshKeyCreateParams))
data instance C.Parsed SshKeyCreateParams
    = SshKeyCreateParams 
        {name :: (RP.Parsed Basics.Text)
        ,publicKey :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed SshKeyCreateParams))
deriving instance (Std_.Eq (C.Parsed SshKeyCreateParams))
instance (C.Parse SshKeyCreateParams (C.Parsed SshKeyCreateParams)) where
    parse raw_ = (SshKeyCreateParams <$> (GH.parseField #name raw_)
                                     <*> (GH.parseField #publicKey raw_))
instance (C.Marshal SshKeyCreateParams (C.Parsed SshKeyCreateParams)) where
    marshalInto raw_ SshKeyCreateParams{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #publicKey publicKey raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot SshKeyCreateParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "publicKey" GH.Slot SshKeyCreateParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
data SshKeyManager 
type instance (R.ReprFor SshKeyManager) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId SshKeyManager) where
    typeId  = 13405144292276923843
instance (C.Parse SshKeyManager (GH.Client SshKeyManager)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export SshKeyManager) where
    type Server SshKeyManager = SshKeyManager'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(SshKeyManager)) [(GH.toUntypedMethodHandler ((sshKeyManager'list) s_))
                                                                               ,(GH.toUntypedMethodHandler ((sshKeyManager'get) s_))
                                                                               ,(GH.toUntypedMethodHandler ((sshKeyManager'create) s_))] [])
class (SshKeyManager'server_ s_) where
    {-# MINIMAL sshKeyManager'list,sshKeyManager'get,sshKeyManager'create #-}
    sshKeyManager'list :: s_ -> (GH.MethodHandler SshKeyManager'list'params SshKeyManager'list'results)
    sshKeyManager'list _ = GH.methodUnimplemented
    sshKeyManager'get :: s_ -> (GH.MethodHandler SshKeyManager'get'params SshKeyManager'get'results)
    sshKeyManager'get _ = GH.methodUnimplemented
    sshKeyManager'create :: s_ -> (GH.MethodHandler SshKeyManager'create'params SshKeyManager'create'results)
    sshKeyManager'create _ = GH.methodUnimplemented
instance (GH.HasMethod "list" SshKeyManager SshKeyManager'list'params SshKeyManager'list'results) where
    methodByLabel  = (GH.Method 13405144292276923843 0)
instance (GH.HasMethod "get" SshKeyManager SshKeyManager'get'params SshKeyManager'get'results) where
    methodByLabel  = (GH.Method 13405144292276923843 1)
instance (GH.HasMethod "create" SshKeyManager SshKeyManager'create'params SshKeyManager'create'results) where
    methodByLabel  = (GH.Method 13405144292276923843 2)
data SshKeyManager'list'params 
type instance (R.ReprFor SshKeyManager'list'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId SshKeyManager'list'params) where
    typeId  = 17156793543321232763
instance (C.TypedStruct SshKeyManager'list'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate SshKeyManager'list'params) where
    type AllocHint SshKeyManager'list'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc SshKeyManager'list'params (C.Parsed SshKeyManager'list'params))
instance (C.AllocateList SshKeyManager'list'params) where
    type ListAllocHint SshKeyManager'list'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc SshKeyManager'list'params (C.Parsed SshKeyManager'list'params))
data instance C.Parsed SshKeyManager'list'params
    = SshKeyManager'list'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed SshKeyManager'list'params))
deriving instance (Std_.Eq (C.Parsed SshKeyManager'list'params))
instance (C.Parse SshKeyManager'list'params (C.Parsed SshKeyManager'list'params)) where
    parse raw_ = (Std_.pure SshKeyManager'list'params)
instance (C.Marshal SshKeyManager'list'params (C.Parsed SshKeyManager'list'params)) where
    marshalInto _raw (SshKeyManager'list'params) = (Std_.pure ())
data SshKeyManager'list'results 
type instance (R.ReprFor SshKeyManager'list'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId SshKeyManager'list'results) where
    typeId  = 9976727726707963813
instance (C.TypedStruct SshKeyManager'list'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate SshKeyManager'list'results) where
    type AllocHint SshKeyManager'list'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc SshKeyManager'list'results (C.Parsed SshKeyManager'list'results))
instance (C.AllocateList SshKeyManager'list'results) where
    type ListAllocHint SshKeyManager'list'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc SshKeyManager'list'results (C.Parsed SshKeyManager'list'results))
data instance C.Parsed SshKeyManager'list'results
    = SshKeyManager'list'results 
        {keys :: (RP.Parsed (R.List SshKeyInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed SshKeyManager'list'results))
deriving instance (Std_.Eq (C.Parsed SshKeyManager'list'results))
instance (C.Parse SshKeyManager'list'results (C.Parsed SshKeyManager'list'results)) where
    parse raw_ = (SshKeyManager'list'results <$> (GH.parseField #keys raw_))
instance (C.Marshal SshKeyManager'list'results (C.Parsed SshKeyManager'list'results)) where
    marshalInto raw_ SshKeyManager'list'results{..} = (do
        (GH.encodeField #keys keys raw_)
        (Std_.pure ())
        )
instance (GH.HasField "keys" GH.Slot SshKeyManager'list'results (R.List SshKeyInfo)) where
    fieldByLabel  = (GH.ptrField 0)
data SshKeyManager'get'params 
type instance (R.ReprFor SshKeyManager'get'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId SshKeyManager'get'params) where
    typeId  = 14318162138554633549
instance (C.TypedStruct SshKeyManager'get'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate SshKeyManager'get'params) where
    type AllocHint SshKeyManager'get'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc SshKeyManager'get'params (C.Parsed SshKeyManager'get'params))
instance (C.AllocateList SshKeyManager'get'params) where
    type ListAllocHint SshKeyManager'get'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc SshKeyManager'get'params (C.Parsed SshKeyManager'get'params))
data instance C.Parsed SshKeyManager'get'params
    = SshKeyManager'get'params 
        {ref :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed SshKeyManager'get'params))
deriving instance (Std_.Eq (C.Parsed SshKeyManager'get'params))
instance (C.Parse SshKeyManager'get'params (C.Parsed SshKeyManager'get'params)) where
    parse raw_ = (SshKeyManager'get'params <$> (GH.parseField #ref raw_))
instance (C.Marshal SshKeyManager'get'params (C.Parsed SshKeyManager'get'params)) where
    marshalInto raw_ SshKeyManager'get'params{..} = (do
        (GH.encodeField #ref ref raw_)
        (Std_.pure ())
        )
instance (GH.HasField "ref" GH.Slot SshKeyManager'get'params Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
data SshKeyManager'get'results 
type instance (R.ReprFor SshKeyManager'get'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId SshKeyManager'get'results) where
    typeId  = 9500265513071459468
instance (C.TypedStruct SshKeyManager'get'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate SshKeyManager'get'results) where
    type AllocHint SshKeyManager'get'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc SshKeyManager'get'results (C.Parsed SshKeyManager'get'results))
instance (C.AllocateList SshKeyManager'get'results) where
    type ListAllocHint SshKeyManager'get'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc SshKeyManager'get'results (C.Parsed SshKeyManager'get'results))
data instance C.Parsed SshKeyManager'get'results
    = SshKeyManager'get'results 
        {key :: (RP.Parsed SshKey)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed SshKeyManager'get'results))
deriving instance (Std_.Eq (C.Parsed SshKeyManager'get'results))
instance (C.Parse SshKeyManager'get'results (C.Parsed SshKeyManager'get'results)) where
    parse raw_ = (SshKeyManager'get'results <$> (GH.parseField #key raw_))
instance (C.Marshal SshKeyManager'get'results (C.Parsed SshKeyManager'get'results)) where
    marshalInto raw_ SshKeyManager'get'results{..} = (do
        (GH.encodeField #key key raw_)
        (Std_.pure ())
        )
instance (GH.HasField "key" GH.Slot SshKeyManager'get'results SshKey) where
    fieldByLabel  = (GH.ptrField 0)
data SshKeyManager'create'params 
type instance (R.ReprFor SshKeyManager'create'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId SshKeyManager'create'params) where
    typeId  = 16841339287446449482
instance (C.TypedStruct SshKeyManager'create'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate SshKeyManager'create'params) where
    type AllocHint SshKeyManager'create'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc SshKeyManager'create'params (C.Parsed SshKeyManager'create'params))
instance (C.AllocateList SshKeyManager'create'params) where
    type ListAllocHint SshKeyManager'create'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc SshKeyManager'create'params (C.Parsed SshKeyManager'create'params))
data instance C.Parsed SshKeyManager'create'params
    = SshKeyManager'create'params 
        {params :: (RP.Parsed SshKeyCreateParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed SshKeyManager'create'params))
deriving instance (Std_.Eq (C.Parsed SshKeyManager'create'params))
instance (C.Parse SshKeyManager'create'params (C.Parsed SshKeyManager'create'params)) where
    parse raw_ = (SshKeyManager'create'params <$> (GH.parseField #params raw_))
instance (C.Marshal SshKeyManager'create'params (C.Parsed SshKeyManager'create'params)) where
    marshalInto raw_ SshKeyManager'create'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot SshKeyManager'create'params SshKeyCreateParams) where
    fieldByLabel  = (GH.ptrField 0)
data SshKeyManager'create'results 
type instance (R.ReprFor SshKeyManager'create'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId SshKeyManager'create'results) where
    typeId  = 11892887811842885208
instance (C.TypedStruct SshKeyManager'create'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate SshKeyManager'create'results) where
    type AllocHint SshKeyManager'create'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc SshKeyManager'create'results (C.Parsed SshKeyManager'create'results))
instance (C.AllocateList SshKeyManager'create'results) where
    type ListAllocHint SshKeyManager'create'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc SshKeyManager'create'results (C.Parsed SshKeyManager'create'results))
data instance C.Parsed SshKeyManager'create'results
    = SshKeyManager'create'results 
        {key :: (RP.Parsed SshKey)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed SshKeyManager'create'results))
deriving instance (Std_.Eq (C.Parsed SshKeyManager'create'results))
instance (C.Parse SshKeyManager'create'results (C.Parsed SshKeyManager'create'results)) where
    parse raw_ = (SshKeyManager'create'results <$> (GH.parseField #key raw_))
instance (C.Marshal SshKeyManager'create'results (C.Parsed SshKeyManager'create'results)) where
    marshalInto raw_ SshKeyManager'create'results{..} = (do
        (GH.encodeField #key key raw_)
        (Std_.pure ())
        )
instance (GH.HasField "key" GH.Slot SshKeyManager'create'results SshKey) where
    fieldByLabel  = (GH.ptrField 0)
data SshKey 
type instance (R.ReprFor SshKey) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId SshKey) where
    typeId  = 9693425706148430903
instance (C.Parse SshKey (GH.Client SshKey)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export SshKey) where
    type Server SshKey = SshKey'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(SshKey)) [(GH.toUntypedMethodHandler ((sshKey'show) s_))
                                                                        ,(GH.toUntypedMethodHandler ((sshKey'delete) s_))] [])
class (SshKey'server_ s_) where
    {-# MINIMAL sshKey'show,sshKey'delete #-}
    sshKey'show :: s_ -> (GH.MethodHandler SshKey'show'params SshKey'show'results)
    sshKey'show _ = GH.methodUnimplemented
    sshKey'delete :: s_ -> (GH.MethodHandler SshKey'delete'params SshKey'delete'results)
    sshKey'delete _ = GH.methodUnimplemented
instance (GH.HasMethod "show" SshKey SshKey'show'params SshKey'show'results) where
    methodByLabel  = (GH.Method 9693425706148430903 0)
instance (GH.HasMethod "delete" SshKey SshKey'delete'params SshKey'delete'results) where
    methodByLabel  = (GH.Method 9693425706148430903 1)
data SshKey'show'params 
type instance (R.ReprFor SshKey'show'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId SshKey'show'params) where
    typeId  = 12572594081908728262
instance (C.TypedStruct SshKey'show'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate SshKey'show'params) where
    type AllocHint SshKey'show'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc SshKey'show'params (C.Parsed SshKey'show'params))
instance (C.AllocateList SshKey'show'params) where
    type ListAllocHint SshKey'show'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc SshKey'show'params (C.Parsed SshKey'show'params))
data instance C.Parsed SshKey'show'params
    = SshKey'show'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed SshKey'show'params))
deriving instance (Std_.Eq (C.Parsed SshKey'show'params))
instance (C.Parse SshKey'show'params (C.Parsed SshKey'show'params)) where
    parse raw_ = (Std_.pure SshKey'show'params)
instance (C.Marshal SshKey'show'params (C.Parsed SshKey'show'params)) where
    marshalInto _raw (SshKey'show'params) = (Std_.pure ())
data SshKey'show'results 
type instance (R.ReprFor SshKey'show'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId SshKey'show'results) where
    typeId  = 10786502645085070597
instance (C.TypedStruct SshKey'show'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate SshKey'show'results) where
    type AllocHint SshKey'show'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc SshKey'show'results (C.Parsed SshKey'show'results))
instance (C.AllocateList SshKey'show'results) where
    type ListAllocHint SshKey'show'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc SshKey'show'results (C.Parsed SshKey'show'results))
data instance C.Parsed SshKey'show'results
    = SshKey'show'results 
        {info :: (RP.Parsed SshKeyInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed SshKey'show'results))
deriving instance (Std_.Eq (C.Parsed SshKey'show'results))
instance (C.Parse SshKey'show'results (C.Parsed SshKey'show'results)) where
    parse raw_ = (SshKey'show'results <$> (GH.parseField #info raw_))
instance (C.Marshal SshKey'show'results (C.Parsed SshKey'show'results)) where
    marshalInto raw_ SshKey'show'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot SshKey'show'results SshKeyInfo) where
    fieldByLabel  = (GH.ptrField 0)
data SshKey'delete'params 
type instance (R.ReprFor SshKey'delete'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId SshKey'delete'params) where
    typeId  = 10915551917889512413
instance (C.TypedStruct SshKey'delete'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate SshKey'delete'params) where
    type AllocHint SshKey'delete'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc SshKey'delete'params (C.Parsed SshKey'delete'params))
instance (C.AllocateList SshKey'delete'params) where
    type ListAllocHint SshKey'delete'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc SshKey'delete'params (C.Parsed SshKey'delete'params))
data instance C.Parsed SshKey'delete'params
    = SshKey'delete'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed SshKey'delete'params))
deriving instance (Std_.Eq (C.Parsed SshKey'delete'params))
instance (C.Parse SshKey'delete'params (C.Parsed SshKey'delete'params)) where
    parse raw_ = (Std_.pure SshKey'delete'params)
instance (C.Marshal SshKey'delete'params (C.Parsed SshKey'delete'params)) where
    marshalInto _raw (SshKey'delete'params) = (Std_.pure ())
data SshKey'delete'results 
type instance (R.ReprFor SshKey'delete'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId SshKey'delete'results) where
    typeId  = 13010083073044525311
instance (C.TypedStruct SshKey'delete'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate SshKey'delete'results) where
    type AllocHint SshKey'delete'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc SshKey'delete'results (C.Parsed SshKey'delete'results))
instance (C.AllocateList SshKey'delete'results) where
    type ListAllocHint SshKey'delete'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc SshKey'delete'results (C.Parsed SshKey'delete'results))
data instance C.Parsed SshKey'delete'results
    = SshKey'delete'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed SshKey'delete'results))
deriving instance (Std_.Eq (C.Parsed SshKey'delete'results))
instance (C.Parse SshKey'delete'results (C.Parsed SshKey'delete'results)) where
    parse raw_ = (Std_.pure SshKey'delete'results)
instance (C.Marshal SshKey'delete'results (C.Parsed SshKey'delete'results)) where
    marshalInto _raw (SshKey'delete'results) = (Std_.pure ())