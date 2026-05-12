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
module Capnp.Gen.Network where
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
data NetworkInfo 
type instance (R.ReprFor NetworkInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetworkInfo) where
    typeId  = 10226046599106768326
instance (C.TypedStruct NetworkInfo) where
    numStructWords  = 3
    numStructPtrs  = 2
instance (C.Allocate NetworkInfo) where
    type AllocHint NetworkInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetworkInfo (C.Parsed NetworkInfo))
instance (C.AllocateList NetworkInfo) where
    type ListAllocHint NetworkInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetworkInfo (C.Parsed NetworkInfo))
data instance C.Parsed NetworkInfo
    = NetworkInfo 
        {id :: (RP.Parsed Std_.Int64)
        ,name :: (RP.Parsed Basics.Text)
        ,subnet :: (RP.Parsed Basics.Text)
        ,dhcp :: (RP.Parsed Std_.Bool)
        ,nat :: (RP.Parsed Std_.Bool)
        ,running :: (RP.Parsed Std_.Bool)
        ,dnsmasqPid :: (RP.Parsed Std_.Int32)
        ,createdAt :: (RP.Parsed Std_.Int64)
        ,autostart :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetworkInfo))
deriving instance (Std_.Eq (C.Parsed NetworkInfo))
instance (C.Parse NetworkInfo (C.Parsed NetworkInfo)) where
    parse raw_ = (NetworkInfo <$> (GH.parseField #id raw_)
                              <*> (GH.parseField #name raw_)
                              <*> (GH.parseField #subnet raw_)
                              <*> (GH.parseField #dhcp raw_)
                              <*> (GH.parseField #nat raw_)
                              <*> (GH.parseField #running raw_)
                              <*> (GH.parseField #dnsmasqPid raw_)
                              <*> (GH.parseField #createdAt raw_)
                              <*> (GH.parseField #autostart raw_))
instance (C.Marshal NetworkInfo (C.Parsed NetworkInfo)) where
    marshalInto raw_ NetworkInfo{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #subnet subnet raw_)
        (GH.encodeField #dhcp dhcp raw_)
        (GH.encodeField #nat nat raw_)
        (GH.encodeField #running running raw_)
        (GH.encodeField #dnsmasqPid dnsmasqPid raw_)
        (GH.encodeField #createdAt createdAt raw_)
        (GH.encodeField #autostart autostart raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot NetworkInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "name" GH.Slot NetworkInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "subnet" GH.Slot NetworkInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "dhcp" GH.Slot NetworkInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 1 1 0)
instance (GH.HasField "nat" GH.Slot NetworkInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 1 1 0)
instance (GH.HasField "running" GH.Slot NetworkInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 2 1 1 0)
instance (GH.HasField "dnsmasqPid" GH.Slot NetworkInfo Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 1 32 0)
instance (GH.HasField "createdAt" GH.Slot NetworkInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
instance (GH.HasField "autostart" GH.Slot NetworkInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 3 1 1 0)
data NetworkCreateParams 
type instance (R.ReprFor NetworkCreateParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetworkCreateParams) where
    typeId  = 12742821130246526528
instance (C.TypedStruct NetworkCreateParams) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate NetworkCreateParams) where
    type AllocHint NetworkCreateParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetworkCreateParams (C.Parsed NetworkCreateParams))
instance (C.AllocateList NetworkCreateParams) where
    type ListAllocHint NetworkCreateParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetworkCreateParams (C.Parsed NetworkCreateParams))
data instance C.Parsed NetworkCreateParams
    = NetworkCreateParams 
        {name :: (RP.Parsed Basics.Text)
        ,subnet :: (RP.Parsed Basics.Text)
        ,dhcp :: (RP.Parsed Std_.Bool)
        ,nat :: (RP.Parsed Std_.Bool)
        ,autostart :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetworkCreateParams))
deriving instance (Std_.Eq (C.Parsed NetworkCreateParams))
instance (C.Parse NetworkCreateParams (C.Parsed NetworkCreateParams)) where
    parse raw_ = (NetworkCreateParams <$> (GH.parseField #name raw_)
                                      <*> (GH.parseField #subnet raw_)
                                      <*> (GH.parseField #dhcp raw_)
                                      <*> (GH.parseField #nat raw_)
                                      <*> (GH.parseField #autostart raw_))
instance (C.Marshal NetworkCreateParams (C.Parsed NetworkCreateParams)) where
    marshalInto raw_ NetworkCreateParams{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #subnet subnet raw_)
        (GH.encodeField #dhcp dhcp raw_)
        (GH.encodeField #nat nat raw_)
        (GH.encodeField #autostart autostart raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot NetworkCreateParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "subnet" GH.Slot NetworkCreateParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "dhcp" GH.Slot NetworkCreateParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (GH.HasField "nat" GH.Slot NetworkCreateParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 0 1 0)
instance (GH.HasField "autostart" GH.Slot NetworkCreateParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 2 0 1 0)
data NetworkEditParams 
type instance (R.ReprFor NetworkEditParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetworkEditParams) where
    typeId  = 13369617487645822400
instance (C.TypedStruct NetworkEditParams) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate NetworkEditParams) where
    type AllocHint NetworkEditParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetworkEditParams (C.Parsed NetworkEditParams))
instance (C.AllocateList NetworkEditParams) where
    type ListAllocHint NetworkEditParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetworkEditParams (C.Parsed NetworkEditParams))
data instance C.Parsed NetworkEditParams
    = NetworkEditParams 
        {hasName :: (RP.Parsed Std_.Bool)
        ,name :: (RP.Parsed Basics.Text)
        ,hasSubnet :: (RP.Parsed Std_.Bool)
        ,subnet :: (RP.Parsed Basics.Text)
        ,hasDhcp :: (RP.Parsed Std_.Bool)
        ,dhcp :: (RP.Parsed Std_.Bool)
        ,hasNat :: (RP.Parsed Std_.Bool)
        ,nat :: (RP.Parsed Std_.Bool)
        ,hasAutostart :: (RP.Parsed Std_.Bool)
        ,autostart :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetworkEditParams))
deriving instance (Std_.Eq (C.Parsed NetworkEditParams))
instance (C.Parse NetworkEditParams (C.Parsed NetworkEditParams)) where
    parse raw_ = (NetworkEditParams <$> (GH.parseField #hasName raw_)
                                    <*> (GH.parseField #name raw_)
                                    <*> (GH.parseField #hasSubnet raw_)
                                    <*> (GH.parseField #subnet raw_)
                                    <*> (GH.parseField #hasDhcp raw_)
                                    <*> (GH.parseField #dhcp raw_)
                                    <*> (GH.parseField #hasNat raw_)
                                    <*> (GH.parseField #nat raw_)
                                    <*> (GH.parseField #hasAutostart raw_)
                                    <*> (GH.parseField #autostart raw_))
instance (C.Marshal NetworkEditParams (C.Parsed NetworkEditParams)) where
    marshalInto raw_ NetworkEditParams{..} = (do
        (GH.encodeField #hasName hasName raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #hasSubnet hasSubnet raw_)
        (GH.encodeField #subnet subnet raw_)
        (GH.encodeField #hasDhcp hasDhcp raw_)
        (GH.encodeField #dhcp dhcp raw_)
        (GH.encodeField #hasNat hasNat raw_)
        (GH.encodeField #nat nat raw_)
        (GH.encodeField #hasAutostart hasAutostart raw_)
        (GH.encodeField #autostart autostart raw_)
        (Std_.pure ())
        )
instance (GH.HasField "hasName" GH.Slot NetworkEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (GH.HasField "name" GH.Slot NetworkEditParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "hasSubnet" GH.Slot NetworkEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 0 1 0)
instance (GH.HasField "subnet" GH.Slot NetworkEditParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "hasDhcp" GH.Slot NetworkEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 2 0 1 0)
instance (GH.HasField "dhcp" GH.Slot NetworkEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 3 0 1 0)
instance (GH.HasField "hasNat" GH.Slot NetworkEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 4 0 1 0)
instance (GH.HasField "nat" GH.Slot NetworkEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 5 0 1 0)
instance (GH.HasField "hasAutostart" GH.Slot NetworkEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 6 0 1 0)
instance (GH.HasField "autostart" GH.Slot NetworkEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 7 0 1 0)
data NetworkManager 
type instance (R.ReprFor NetworkManager) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId NetworkManager) where
    typeId  = 9339345826252139601
instance (C.Parse NetworkManager (GH.Client NetworkManager)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export NetworkManager) where
    type Server NetworkManager = NetworkManager'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(NetworkManager)) [(GH.toUntypedMethodHandler ((networkManager'list) s_))
                                                                                ,(GH.toUntypedMethodHandler ((networkManager'get) s_))
                                                                                ,(GH.toUntypedMethodHandler ((networkManager'create) s_))] [])
class (NetworkManager'server_ s_) where
    {-# MINIMAL networkManager'list,networkManager'get,networkManager'create #-}
    networkManager'list :: s_ -> (GH.MethodHandler NetworkManager'list'params NetworkManager'list'results)
    networkManager'list _ = GH.methodUnimplemented
    networkManager'get :: s_ -> (GH.MethodHandler NetworkManager'get'params NetworkManager'get'results)
    networkManager'get _ = GH.methodUnimplemented
    networkManager'create :: s_ -> (GH.MethodHandler NetworkManager'create'params NetworkManager'create'results)
    networkManager'create _ = GH.methodUnimplemented
instance (GH.HasMethod "list" NetworkManager NetworkManager'list'params NetworkManager'list'results) where
    methodByLabel  = (GH.Method 9339345826252139601 0)
instance (GH.HasMethod "get" NetworkManager NetworkManager'get'params NetworkManager'get'results) where
    methodByLabel  = (GH.Method 9339345826252139601 1)
instance (GH.HasMethod "create" NetworkManager NetworkManager'create'params NetworkManager'create'results) where
    methodByLabel  = (GH.Method 9339345826252139601 2)
data NetworkManager'list'params 
type instance (R.ReprFor NetworkManager'list'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetworkManager'list'params) where
    typeId  = 13657594431239711158
instance (C.TypedStruct NetworkManager'list'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate NetworkManager'list'params) where
    type AllocHint NetworkManager'list'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetworkManager'list'params (C.Parsed NetworkManager'list'params))
instance (C.AllocateList NetworkManager'list'params) where
    type ListAllocHint NetworkManager'list'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetworkManager'list'params (C.Parsed NetworkManager'list'params))
data instance C.Parsed NetworkManager'list'params
    = NetworkManager'list'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetworkManager'list'params))
deriving instance (Std_.Eq (C.Parsed NetworkManager'list'params))
instance (C.Parse NetworkManager'list'params (C.Parsed NetworkManager'list'params)) where
    parse raw_ = (Std_.pure NetworkManager'list'params)
instance (C.Marshal NetworkManager'list'params (C.Parsed NetworkManager'list'params)) where
    marshalInto _raw (NetworkManager'list'params) = (Std_.pure ())
data NetworkManager'list'results 
type instance (R.ReprFor NetworkManager'list'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetworkManager'list'results) where
    typeId  = 11860803590743233945
instance (C.TypedStruct NetworkManager'list'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NetworkManager'list'results) where
    type AllocHint NetworkManager'list'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetworkManager'list'results (C.Parsed NetworkManager'list'results))
instance (C.AllocateList NetworkManager'list'results) where
    type ListAllocHint NetworkManager'list'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetworkManager'list'results (C.Parsed NetworkManager'list'results))
data instance C.Parsed NetworkManager'list'results
    = NetworkManager'list'results 
        {networks :: (RP.Parsed (R.List NetworkInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetworkManager'list'results))
deriving instance (Std_.Eq (C.Parsed NetworkManager'list'results))
instance (C.Parse NetworkManager'list'results (C.Parsed NetworkManager'list'results)) where
    parse raw_ = (NetworkManager'list'results <$> (GH.parseField #networks raw_))
instance (C.Marshal NetworkManager'list'results (C.Parsed NetworkManager'list'results)) where
    marshalInto raw_ NetworkManager'list'results{..} = (do
        (GH.encodeField #networks networks raw_)
        (Std_.pure ())
        )
instance (GH.HasField "networks" GH.Slot NetworkManager'list'results (R.List NetworkInfo)) where
    fieldByLabel  = (GH.ptrField 0)
data NetworkManager'get'params 
type instance (R.ReprFor NetworkManager'get'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetworkManager'get'params) where
    typeId  = 11414204732190974054
instance (C.TypedStruct NetworkManager'get'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NetworkManager'get'params) where
    type AllocHint NetworkManager'get'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetworkManager'get'params (C.Parsed NetworkManager'get'params))
instance (C.AllocateList NetworkManager'get'params) where
    type ListAllocHint NetworkManager'get'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetworkManager'get'params (C.Parsed NetworkManager'get'params))
data instance C.Parsed NetworkManager'get'params
    = NetworkManager'get'params 
        {ref :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetworkManager'get'params))
deriving instance (Std_.Eq (C.Parsed NetworkManager'get'params))
instance (C.Parse NetworkManager'get'params (C.Parsed NetworkManager'get'params)) where
    parse raw_ = (NetworkManager'get'params <$> (GH.parseField #ref raw_))
instance (C.Marshal NetworkManager'get'params (C.Parsed NetworkManager'get'params)) where
    marshalInto raw_ NetworkManager'get'params{..} = (do
        (GH.encodeField #ref ref raw_)
        (Std_.pure ())
        )
instance (GH.HasField "ref" GH.Slot NetworkManager'get'params Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
data NetworkManager'get'results 
type instance (R.ReprFor NetworkManager'get'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetworkManager'get'results) where
    typeId  = 14184662750270654126
instance (C.TypedStruct NetworkManager'get'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NetworkManager'get'results) where
    type AllocHint NetworkManager'get'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetworkManager'get'results (C.Parsed NetworkManager'get'results))
instance (C.AllocateList NetworkManager'get'results) where
    type ListAllocHint NetworkManager'get'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetworkManager'get'results (C.Parsed NetworkManager'get'results))
data instance C.Parsed NetworkManager'get'results
    = NetworkManager'get'results 
        {network :: (RP.Parsed Network)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetworkManager'get'results))
deriving instance (Std_.Eq (C.Parsed NetworkManager'get'results))
instance (C.Parse NetworkManager'get'results (C.Parsed NetworkManager'get'results)) where
    parse raw_ = (NetworkManager'get'results <$> (GH.parseField #network raw_))
instance (C.Marshal NetworkManager'get'results (C.Parsed NetworkManager'get'results)) where
    marshalInto raw_ NetworkManager'get'results{..} = (do
        (GH.encodeField #network network raw_)
        (Std_.pure ())
        )
instance (GH.HasField "network" GH.Slot NetworkManager'get'results Network) where
    fieldByLabel  = (GH.ptrField 0)
data NetworkManager'create'params 
type instance (R.ReprFor NetworkManager'create'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetworkManager'create'params) where
    typeId  = 14285977124935965480
instance (C.TypedStruct NetworkManager'create'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NetworkManager'create'params) where
    type AllocHint NetworkManager'create'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetworkManager'create'params (C.Parsed NetworkManager'create'params))
instance (C.AllocateList NetworkManager'create'params) where
    type ListAllocHint NetworkManager'create'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetworkManager'create'params (C.Parsed NetworkManager'create'params))
data instance C.Parsed NetworkManager'create'params
    = NetworkManager'create'params 
        {params :: (RP.Parsed NetworkCreateParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetworkManager'create'params))
deriving instance (Std_.Eq (C.Parsed NetworkManager'create'params))
instance (C.Parse NetworkManager'create'params (C.Parsed NetworkManager'create'params)) where
    parse raw_ = (NetworkManager'create'params <$> (GH.parseField #params raw_))
instance (C.Marshal NetworkManager'create'params (C.Parsed NetworkManager'create'params)) where
    marshalInto raw_ NetworkManager'create'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot NetworkManager'create'params NetworkCreateParams) where
    fieldByLabel  = (GH.ptrField 0)
data NetworkManager'create'results 
type instance (R.ReprFor NetworkManager'create'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetworkManager'create'results) where
    typeId  = 13693720474151466465
instance (C.TypedStruct NetworkManager'create'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NetworkManager'create'results) where
    type AllocHint NetworkManager'create'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetworkManager'create'results (C.Parsed NetworkManager'create'results))
instance (C.AllocateList NetworkManager'create'results) where
    type ListAllocHint NetworkManager'create'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetworkManager'create'results (C.Parsed NetworkManager'create'results))
data instance C.Parsed NetworkManager'create'results
    = NetworkManager'create'results 
        {network :: (RP.Parsed Network)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetworkManager'create'results))
deriving instance (Std_.Eq (C.Parsed NetworkManager'create'results))
instance (C.Parse NetworkManager'create'results (C.Parsed NetworkManager'create'results)) where
    parse raw_ = (NetworkManager'create'results <$> (GH.parseField #network raw_))
instance (C.Marshal NetworkManager'create'results (C.Parsed NetworkManager'create'results)) where
    marshalInto raw_ NetworkManager'create'results{..} = (do
        (GH.encodeField #network network raw_)
        (Std_.pure ())
        )
instance (GH.HasField "network" GH.Slot NetworkManager'create'results Network) where
    fieldByLabel  = (GH.ptrField 0)
data Network 
type instance (R.ReprFor Network) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Network) where
    typeId  = 17573678317974358173
instance (C.Parse Network (GH.Client Network)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Network) where
    type Server Network = Network'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Network)) [(GH.toUntypedMethodHandler ((network'show) s_))
                                                                         ,(GH.toUntypedMethodHandler ((network'start) s_))
                                                                         ,(GH.toUntypedMethodHandler ((network'stop) s_))
                                                                         ,(GH.toUntypedMethodHandler ((network'edit) s_))
                                                                         ,(GH.toUntypedMethodHandler ((network'delete) s_))] [])
class (Network'server_ s_) where
    {-# MINIMAL network'show,network'start,network'stop,network'edit,network'delete #-}
    network'show :: s_ -> (GH.MethodHandler Network'show'params Network'show'results)
    network'show _ = GH.methodUnimplemented
    network'start :: s_ -> (GH.MethodHandler Network'start'params Network'start'results)
    network'start _ = GH.methodUnimplemented
    network'stop :: s_ -> (GH.MethodHandler Network'stop'params Network'stop'results)
    network'stop _ = GH.methodUnimplemented
    network'edit :: s_ -> (GH.MethodHandler Network'edit'params Network'edit'results)
    network'edit _ = GH.methodUnimplemented
    network'delete :: s_ -> (GH.MethodHandler Network'delete'params Network'delete'results)
    network'delete _ = GH.methodUnimplemented
instance (GH.HasMethod "show" Network Network'show'params Network'show'results) where
    methodByLabel  = (GH.Method 17573678317974358173 0)
instance (GH.HasMethod "start" Network Network'start'params Network'start'results) where
    methodByLabel  = (GH.Method 17573678317974358173 1)
instance (GH.HasMethod "stop" Network Network'stop'params Network'stop'results) where
    methodByLabel  = (GH.Method 17573678317974358173 2)
instance (GH.HasMethod "edit" Network Network'edit'params Network'edit'results) where
    methodByLabel  = (GH.Method 17573678317974358173 3)
instance (GH.HasMethod "delete" Network Network'delete'params Network'delete'results) where
    methodByLabel  = (GH.Method 17573678317974358173 4)
data Network'show'params 
type instance (R.ReprFor Network'show'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Network'show'params) where
    typeId  = 15454470225447976367
instance (C.TypedStruct Network'show'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Network'show'params) where
    type AllocHint Network'show'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Network'show'params (C.Parsed Network'show'params))
instance (C.AllocateList Network'show'params) where
    type ListAllocHint Network'show'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Network'show'params (C.Parsed Network'show'params))
data instance C.Parsed Network'show'params
    = Network'show'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Network'show'params))
deriving instance (Std_.Eq (C.Parsed Network'show'params))
instance (C.Parse Network'show'params (C.Parsed Network'show'params)) where
    parse raw_ = (Std_.pure Network'show'params)
instance (C.Marshal Network'show'params (C.Parsed Network'show'params)) where
    marshalInto _raw (Network'show'params) = (Std_.pure ())
data Network'show'results 
type instance (R.ReprFor Network'show'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Network'show'results) where
    typeId  = 9279050911626039970
instance (C.TypedStruct Network'show'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Network'show'results) where
    type AllocHint Network'show'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Network'show'results (C.Parsed Network'show'results))
instance (C.AllocateList Network'show'results) where
    type ListAllocHint Network'show'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Network'show'results (C.Parsed Network'show'results))
data instance C.Parsed Network'show'results
    = Network'show'results 
        {info :: (RP.Parsed NetworkInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Network'show'results))
deriving instance (Std_.Eq (C.Parsed Network'show'results))
instance (C.Parse Network'show'results (C.Parsed Network'show'results)) where
    parse raw_ = (Network'show'results <$> (GH.parseField #info raw_))
instance (C.Marshal Network'show'results (C.Parsed Network'show'results)) where
    marshalInto raw_ Network'show'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot Network'show'results NetworkInfo) where
    fieldByLabel  = (GH.ptrField 0)
data Network'start'params 
type instance (R.ReprFor Network'start'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Network'start'params) where
    typeId  = 14721489368767661155
instance (C.TypedStruct Network'start'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Network'start'params) where
    type AllocHint Network'start'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Network'start'params (C.Parsed Network'start'params))
instance (C.AllocateList Network'start'params) where
    type ListAllocHint Network'start'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Network'start'params (C.Parsed Network'start'params))
data instance C.Parsed Network'start'params
    = Network'start'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Network'start'params))
deriving instance (Std_.Eq (C.Parsed Network'start'params))
instance (C.Parse Network'start'params (C.Parsed Network'start'params)) where
    parse raw_ = (Std_.pure Network'start'params)
instance (C.Marshal Network'start'params (C.Parsed Network'start'params)) where
    marshalInto _raw (Network'start'params) = (Std_.pure ())
data Network'start'results 
type instance (R.ReprFor Network'start'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Network'start'results) where
    typeId  = 11873083409499425023
instance (C.TypedStruct Network'start'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Network'start'results) where
    type AllocHint Network'start'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Network'start'results (C.Parsed Network'start'results))
instance (C.AllocateList Network'start'results) where
    type ListAllocHint Network'start'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Network'start'results (C.Parsed Network'start'results))
data instance C.Parsed Network'start'results
    = Network'start'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Network'start'results))
deriving instance (Std_.Eq (C.Parsed Network'start'results))
instance (C.Parse Network'start'results (C.Parsed Network'start'results)) where
    parse raw_ = (Std_.pure Network'start'results)
instance (C.Marshal Network'start'results (C.Parsed Network'start'results)) where
    marshalInto _raw (Network'start'results) = (Std_.pure ())
data Network'stop'params 
type instance (R.ReprFor Network'stop'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Network'stop'params) where
    typeId  = 10065423552811116374
instance (C.TypedStruct Network'stop'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Network'stop'params) where
    type AllocHint Network'stop'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Network'stop'params (C.Parsed Network'stop'params))
instance (C.AllocateList Network'stop'params) where
    type ListAllocHint Network'stop'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Network'stop'params (C.Parsed Network'stop'params))
data instance C.Parsed Network'stop'params
    = Network'stop'params 
        {force :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Network'stop'params))
deriving instance (Std_.Eq (C.Parsed Network'stop'params))
instance (C.Parse Network'stop'params (C.Parsed Network'stop'params)) where
    parse raw_ = (Network'stop'params <$> (GH.parseField #force raw_))
instance (C.Marshal Network'stop'params (C.Parsed Network'stop'params)) where
    marshalInto raw_ Network'stop'params{..} = (do
        (GH.encodeField #force force raw_)
        (Std_.pure ())
        )
instance (GH.HasField "force" GH.Slot Network'stop'params Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
data Network'stop'results 
type instance (R.ReprFor Network'stop'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Network'stop'results) where
    typeId  = 14163700601681201953
instance (C.TypedStruct Network'stop'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Network'stop'results) where
    type AllocHint Network'stop'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Network'stop'results (C.Parsed Network'stop'results))
instance (C.AllocateList Network'stop'results) where
    type ListAllocHint Network'stop'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Network'stop'results (C.Parsed Network'stop'results))
data instance C.Parsed Network'stop'results
    = Network'stop'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Network'stop'results))
deriving instance (Std_.Eq (C.Parsed Network'stop'results))
instance (C.Parse Network'stop'results (C.Parsed Network'stop'results)) where
    parse raw_ = (Std_.pure Network'stop'results)
instance (C.Marshal Network'stop'results (C.Parsed Network'stop'results)) where
    marshalInto _raw (Network'stop'results) = (Std_.pure ())
data Network'edit'params 
type instance (R.ReprFor Network'edit'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Network'edit'params) where
    typeId  = 16281890927793832433
instance (C.TypedStruct Network'edit'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Network'edit'params) where
    type AllocHint Network'edit'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Network'edit'params (C.Parsed Network'edit'params))
instance (C.AllocateList Network'edit'params) where
    type ListAllocHint Network'edit'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Network'edit'params (C.Parsed Network'edit'params))
data instance C.Parsed Network'edit'params
    = Network'edit'params 
        {params :: (RP.Parsed NetworkEditParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Network'edit'params))
deriving instance (Std_.Eq (C.Parsed Network'edit'params))
instance (C.Parse Network'edit'params (C.Parsed Network'edit'params)) where
    parse raw_ = (Network'edit'params <$> (GH.parseField #params raw_))
instance (C.Marshal Network'edit'params (C.Parsed Network'edit'params)) where
    marshalInto raw_ Network'edit'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot Network'edit'params NetworkEditParams) where
    fieldByLabel  = (GH.ptrField 0)
data Network'edit'results 
type instance (R.ReprFor Network'edit'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Network'edit'results) where
    typeId  = 11640207943969462329
instance (C.TypedStruct Network'edit'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Network'edit'results) where
    type AllocHint Network'edit'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Network'edit'results (C.Parsed Network'edit'results))
instance (C.AllocateList Network'edit'results) where
    type ListAllocHint Network'edit'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Network'edit'results (C.Parsed Network'edit'results))
data instance C.Parsed Network'edit'results
    = Network'edit'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Network'edit'results))
deriving instance (Std_.Eq (C.Parsed Network'edit'results))
instance (C.Parse Network'edit'results (C.Parsed Network'edit'results)) where
    parse raw_ = (Std_.pure Network'edit'results)
instance (C.Marshal Network'edit'results (C.Parsed Network'edit'results)) where
    marshalInto _raw (Network'edit'results) = (Std_.pure ())
data Network'delete'params 
type instance (R.ReprFor Network'delete'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Network'delete'params) where
    typeId  = 11304768403472992034
instance (C.TypedStruct Network'delete'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Network'delete'params) where
    type AllocHint Network'delete'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Network'delete'params (C.Parsed Network'delete'params))
instance (C.AllocateList Network'delete'params) where
    type ListAllocHint Network'delete'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Network'delete'params (C.Parsed Network'delete'params))
data instance C.Parsed Network'delete'params
    = Network'delete'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Network'delete'params))
deriving instance (Std_.Eq (C.Parsed Network'delete'params))
instance (C.Parse Network'delete'params (C.Parsed Network'delete'params)) where
    parse raw_ = (Std_.pure Network'delete'params)
instance (C.Marshal Network'delete'params (C.Parsed Network'delete'params)) where
    marshalInto _raw (Network'delete'params) = (Std_.pure ())
data Network'delete'results 
type instance (R.ReprFor Network'delete'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Network'delete'results) where
    typeId  = 12323275715292359376
instance (C.TypedStruct Network'delete'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Network'delete'results) where
    type AllocHint Network'delete'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Network'delete'results (C.Parsed Network'delete'results))
instance (C.AllocateList Network'delete'results) where
    type ListAllocHint Network'delete'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Network'delete'results (C.Parsed Network'delete'results))
data instance C.Parsed Network'delete'results
    = Network'delete'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Network'delete'results))
deriving instance (Std_.Eq (C.Parsed Network'delete'results))
instance (C.Parse Network'delete'results (C.Parsed Network'delete'results)) where
    parse raw_ = (Std_.pure Network'delete'results)
instance (C.Marshal Network'delete'results (C.Parsed Network'delete'results)) where
    marshalInto _raw (Network'delete'results) = (Std_.pure ())