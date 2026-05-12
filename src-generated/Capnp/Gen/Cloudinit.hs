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
module Capnp.Gen.Cloudinit where
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
data CloudInitInfo 
type instance (R.ReprFor CloudInitInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CloudInitInfo) where
    typeId  = 14168719490710010560
instance (C.TypedStruct CloudInitInfo) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate CloudInitInfo) where
    type AllocHint CloudInitInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CloudInitInfo (C.Parsed CloudInitInfo))
instance (C.AllocateList CloudInitInfo) where
    type ListAllocHint CloudInitInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CloudInitInfo (C.Parsed CloudInitInfo))
data instance C.Parsed CloudInitInfo
    = CloudInitInfo 
        {hasUserData :: (RP.Parsed Std_.Bool)
        ,userData :: (RP.Parsed Basics.Text)
        ,hasNetworkConfig :: (RP.Parsed Std_.Bool)
        ,networkConfig :: (RP.Parsed Basics.Text)
        ,injectSshKeys :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CloudInitInfo))
deriving instance (Std_.Eq (C.Parsed CloudInitInfo))
instance (C.Parse CloudInitInfo (C.Parsed CloudInitInfo)) where
    parse raw_ = (CloudInitInfo <$> (GH.parseField #hasUserData raw_)
                                <*> (GH.parseField #userData raw_)
                                <*> (GH.parseField #hasNetworkConfig raw_)
                                <*> (GH.parseField #networkConfig raw_)
                                <*> (GH.parseField #injectSshKeys raw_))
instance (C.Marshal CloudInitInfo (C.Parsed CloudInitInfo)) where
    marshalInto raw_ CloudInitInfo{..} = (do
        (GH.encodeField #hasUserData hasUserData raw_)
        (GH.encodeField #userData userData raw_)
        (GH.encodeField #hasNetworkConfig hasNetworkConfig raw_)
        (GH.encodeField #networkConfig networkConfig raw_)
        (GH.encodeField #injectSshKeys injectSshKeys raw_)
        (Std_.pure ())
        )
instance (GH.HasField "hasUserData" GH.Slot CloudInitInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (GH.HasField "userData" GH.Slot CloudInitInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "hasNetworkConfig" GH.Slot CloudInitInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 0 1 0)
instance (GH.HasField "networkConfig" GH.Slot CloudInitInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "injectSshKeys" GH.Slot CloudInitInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 2 0 1 0)
data CloudInitSetParams 
type instance (R.ReprFor CloudInitSetParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CloudInitSetParams) where
    typeId  = 16042364119392960168
instance (C.TypedStruct CloudInitSetParams) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate CloudInitSetParams) where
    type AllocHint CloudInitSetParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CloudInitSetParams (C.Parsed CloudInitSetParams))
instance (C.AllocateList CloudInitSetParams) where
    type ListAllocHint CloudInitSetParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CloudInitSetParams (C.Parsed CloudInitSetParams))
data instance C.Parsed CloudInitSetParams
    = CloudInitSetParams 
        {vmRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)
        ,config :: (RP.Parsed CloudInitInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CloudInitSetParams))
deriving instance (Std_.Eq (C.Parsed CloudInitSetParams))
instance (C.Parse CloudInitSetParams (C.Parsed CloudInitSetParams)) where
    parse raw_ = (CloudInitSetParams <$> (GH.parseField #vmRef raw_)
                                     <*> (GH.parseField #config raw_))
instance (C.Marshal CloudInitSetParams (C.Parsed CloudInitSetParams)) where
    marshalInto raw_ CloudInitSetParams{..} = (do
        (GH.encodeField #vmRef vmRef raw_)
        (GH.encodeField #config config raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmRef" GH.Slot CloudInitSetParams Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "config" GH.Slot CloudInitSetParams CloudInitInfo) where
    fieldByLabel  = (GH.ptrField 1)
data CloudInitManager 
type instance (R.ReprFor CloudInitManager) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId CloudInitManager) where
    typeId  = 10746543759705864856
instance (C.Parse CloudInitManager (GH.Client CloudInitManager)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export CloudInitManager) where
    type Server CloudInitManager = CloudInitManager'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(CloudInitManager)) [(GH.toUntypedMethodHandler ((cloudInitManager'set) s_))
                                                                                  ,(GH.toUntypedMethodHandler ((cloudInitManager'get) s_))
                                                                                  ,(GH.toUntypedMethodHandler ((cloudInitManager'delete) s_))] [])
class (CloudInitManager'server_ s_) where
    {-# MINIMAL cloudInitManager'set,cloudInitManager'get,cloudInitManager'delete #-}
    cloudInitManager'set :: s_ -> (GH.MethodHandler CloudInitManager'set'params CloudInitManager'set'results)
    cloudInitManager'set _ = GH.methodUnimplemented
    cloudInitManager'get :: s_ -> (GH.MethodHandler CloudInitManager'get'params CloudInitManager'get'results)
    cloudInitManager'get _ = GH.methodUnimplemented
    cloudInitManager'delete :: s_ -> (GH.MethodHandler CloudInitManager'delete'params CloudInitManager'delete'results)
    cloudInitManager'delete _ = GH.methodUnimplemented
instance (GH.HasMethod "set" CloudInitManager CloudInitManager'set'params CloudInitManager'set'results) where
    methodByLabel  = (GH.Method 10746543759705864856 0)
instance (GH.HasMethod "get" CloudInitManager CloudInitManager'get'params CloudInitManager'get'results) where
    methodByLabel  = (GH.Method 10746543759705864856 1)
instance (GH.HasMethod "delete" CloudInitManager CloudInitManager'delete'params CloudInitManager'delete'results) where
    methodByLabel  = (GH.Method 10746543759705864856 2)
data CloudInitManager'set'params 
type instance (R.ReprFor CloudInitManager'set'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CloudInitManager'set'params) where
    typeId  = 17001324391376421157
instance (C.TypedStruct CloudInitManager'set'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate CloudInitManager'set'params) where
    type AllocHint CloudInitManager'set'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CloudInitManager'set'params (C.Parsed CloudInitManager'set'params))
instance (C.AllocateList CloudInitManager'set'params) where
    type ListAllocHint CloudInitManager'set'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CloudInitManager'set'params (C.Parsed CloudInitManager'set'params))
data instance C.Parsed CloudInitManager'set'params
    = CloudInitManager'set'params 
        {params :: (RP.Parsed CloudInitSetParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CloudInitManager'set'params))
deriving instance (Std_.Eq (C.Parsed CloudInitManager'set'params))
instance (C.Parse CloudInitManager'set'params (C.Parsed CloudInitManager'set'params)) where
    parse raw_ = (CloudInitManager'set'params <$> (GH.parseField #params raw_))
instance (C.Marshal CloudInitManager'set'params (C.Parsed CloudInitManager'set'params)) where
    marshalInto raw_ CloudInitManager'set'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot CloudInitManager'set'params CloudInitSetParams) where
    fieldByLabel  = (GH.ptrField 0)
data CloudInitManager'set'results 
type instance (R.ReprFor CloudInitManager'set'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CloudInitManager'set'results) where
    typeId  = 14506105811376658334
instance (C.TypedStruct CloudInitManager'set'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate CloudInitManager'set'results) where
    type AllocHint CloudInitManager'set'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CloudInitManager'set'results (C.Parsed CloudInitManager'set'results))
instance (C.AllocateList CloudInitManager'set'results) where
    type ListAllocHint CloudInitManager'set'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CloudInitManager'set'results (C.Parsed CloudInitManager'set'results))
data instance C.Parsed CloudInitManager'set'results
    = CloudInitManager'set'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CloudInitManager'set'results))
deriving instance (Std_.Eq (C.Parsed CloudInitManager'set'results))
instance (C.Parse CloudInitManager'set'results (C.Parsed CloudInitManager'set'results)) where
    parse raw_ = (Std_.pure CloudInitManager'set'results)
instance (C.Marshal CloudInitManager'set'results (C.Parsed CloudInitManager'set'results)) where
    marshalInto _raw (CloudInitManager'set'results) = (Std_.pure ())
data CloudInitManager'get'params 
type instance (R.ReprFor CloudInitManager'get'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CloudInitManager'get'params) where
    typeId  = 16805388362908792378
instance (C.TypedStruct CloudInitManager'get'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate CloudInitManager'get'params) where
    type AllocHint CloudInitManager'get'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CloudInitManager'get'params (C.Parsed CloudInitManager'get'params))
instance (C.AllocateList CloudInitManager'get'params) where
    type ListAllocHint CloudInitManager'get'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CloudInitManager'get'params (C.Parsed CloudInitManager'get'params))
data instance C.Parsed CloudInitManager'get'params
    = CloudInitManager'get'params 
        {vmRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CloudInitManager'get'params))
deriving instance (Std_.Eq (C.Parsed CloudInitManager'get'params))
instance (C.Parse CloudInitManager'get'params (C.Parsed CloudInitManager'get'params)) where
    parse raw_ = (CloudInitManager'get'params <$> (GH.parseField #vmRef raw_))
instance (C.Marshal CloudInitManager'get'params (C.Parsed CloudInitManager'get'params)) where
    marshalInto raw_ CloudInitManager'get'params{..} = (do
        (GH.encodeField #vmRef vmRef raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmRef" GH.Slot CloudInitManager'get'params Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
data CloudInitManager'get'results 
type instance (R.ReprFor CloudInitManager'get'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CloudInitManager'get'results) where
    typeId  = 15390921671145603526
instance (C.TypedStruct CloudInitManager'get'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate CloudInitManager'get'results) where
    type AllocHint CloudInitManager'get'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CloudInitManager'get'results (C.Parsed CloudInitManager'get'results))
instance (C.AllocateList CloudInitManager'get'results) where
    type ListAllocHint CloudInitManager'get'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CloudInitManager'get'results (C.Parsed CloudInitManager'get'results))
data instance C.Parsed CloudInitManager'get'results
    = CloudInitManager'get'results 
        {config :: (RP.Parsed CloudInitInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CloudInitManager'get'results))
deriving instance (Std_.Eq (C.Parsed CloudInitManager'get'results))
instance (C.Parse CloudInitManager'get'results (C.Parsed CloudInitManager'get'results)) where
    parse raw_ = (CloudInitManager'get'results <$> (GH.parseField #config raw_))
instance (C.Marshal CloudInitManager'get'results (C.Parsed CloudInitManager'get'results)) where
    marshalInto raw_ CloudInitManager'get'results{..} = (do
        (GH.encodeField #config config raw_)
        (Std_.pure ())
        )
instance (GH.HasField "config" GH.Slot CloudInitManager'get'results CloudInitInfo) where
    fieldByLabel  = (GH.ptrField 0)
data CloudInitManager'delete'params 
type instance (R.ReprFor CloudInitManager'delete'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CloudInitManager'delete'params) where
    typeId  = 14549673102402992306
instance (C.TypedStruct CloudInitManager'delete'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate CloudInitManager'delete'params) where
    type AllocHint CloudInitManager'delete'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CloudInitManager'delete'params (C.Parsed CloudInitManager'delete'params))
instance (C.AllocateList CloudInitManager'delete'params) where
    type ListAllocHint CloudInitManager'delete'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CloudInitManager'delete'params (C.Parsed CloudInitManager'delete'params))
data instance C.Parsed CloudInitManager'delete'params
    = CloudInitManager'delete'params 
        {vmRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CloudInitManager'delete'params))
deriving instance (Std_.Eq (C.Parsed CloudInitManager'delete'params))
instance (C.Parse CloudInitManager'delete'params (C.Parsed CloudInitManager'delete'params)) where
    parse raw_ = (CloudInitManager'delete'params <$> (GH.parseField #vmRef raw_))
instance (C.Marshal CloudInitManager'delete'params (C.Parsed CloudInitManager'delete'params)) where
    marshalInto raw_ CloudInitManager'delete'params{..} = (do
        (GH.encodeField #vmRef vmRef raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmRef" GH.Slot CloudInitManager'delete'params Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
data CloudInitManager'delete'results 
type instance (R.ReprFor CloudInitManager'delete'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CloudInitManager'delete'results) where
    typeId  = 13588713328644409794
instance (C.TypedStruct CloudInitManager'delete'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate CloudInitManager'delete'results) where
    type AllocHint CloudInitManager'delete'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CloudInitManager'delete'results (C.Parsed CloudInitManager'delete'results))
instance (C.AllocateList CloudInitManager'delete'results) where
    type ListAllocHint CloudInitManager'delete'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CloudInitManager'delete'results (C.Parsed CloudInitManager'delete'results))
data instance C.Parsed CloudInitManager'delete'results
    = CloudInitManager'delete'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CloudInitManager'delete'results))
deriving instance (Std_.Eq (C.Parsed CloudInitManager'delete'results))
instance (C.Parse CloudInitManager'delete'results (C.Parsed CloudInitManager'delete'results)) where
    parse raw_ = (Std_.pure CloudInitManager'delete'results)
instance (C.Marshal CloudInitManager'delete'results (C.Parsed CloudInitManager'delete'results)) where
    marshalInto _raw (CloudInitManager'delete'results) = (Std_.pure ())