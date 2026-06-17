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
module Capnp.Gen.Netagent where
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP
import qualified Capnp.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers as GH
import qualified Capnp.Classes as C
import qualified GHC.Generics as Generics
import qualified Capnp.GenHelpers.Rpc as GH
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data NetAgent 
type instance (R.ReprFor NetAgent) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId NetAgent) where
    typeId  = 11116908524040466856
instance (C.Parse NetAgent (GH.Client NetAgent)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export NetAgent) where
    type Server NetAgent = NetAgent'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(NetAgent)) [(GH.toUntypedMethodHandler ((netAgent'session) s_))
                                                                          ,(GH.toUntypedMethodHandler ((netAgent'ping) s_))
                                                                          ,(GH.toUntypedMethodHandler ((netAgent'version) s_))] [])
class (NetAgent'server_ s_) where
    {-# MINIMAL netAgent'session,netAgent'ping,netAgent'version #-}
    netAgent'session :: s_ -> (GH.MethodHandler NetAgent'session'params NetAgent'session'results)
    netAgent'session _ = GH.methodUnimplemented
    netAgent'ping :: s_ -> (GH.MethodHandler NetAgent'ping'params NetAgent'ping'results)
    netAgent'ping _ = GH.methodUnimplemented
    netAgent'version :: s_ -> (GH.MethodHandler NetAgent'version'params NetAgent'version'results)
    netAgent'version _ = GH.methodUnimplemented
instance (GH.HasMethod "session" NetAgent NetAgent'session'params NetAgent'session'results) where
    methodByLabel  = (GH.Method 11116908524040466856 0)
instance (GH.HasMethod "ping" NetAgent NetAgent'ping'params NetAgent'ping'results) where
    methodByLabel  = (GH.Method 11116908524040466856 1)
instance (GH.HasMethod "version" NetAgent NetAgent'version'params NetAgent'version'results) where
    methodByLabel  = (GH.Method 11116908524040466856 2)
data NetAgent'session'params 
type instance (R.ReprFor NetAgent'session'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetAgent'session'params) where
    typeId  = 17440960974126769311
instance (C.TypedStruct NetAgent'session'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NetAgent'session'params) where
    type AllocHint NetAgent'session'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetAgent'session'params (C.Parsed NetAgent'session'params))
instance (C.AllocateList NetAgent'session'params) where
    type ListAllocHint NetAgent'session'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetAgent'session'params (C.Parsed NetAgent'session'params))
data instance C.Parsed NetAgent'session'params
    = NetAgent'session'params 
        {owner :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetAgent'session'params))
deriving instance (Std_.Eq (C.Parsed NetAgent'session'params))
instance (C.Parse NetAgent'session'params (C.Parsed NetAgent'session'params)) where
    parse raw_ = (NetAgent'session'params <$> (GH.parseField #owner raw_))
instance (C.Marshal NetAgent'session'params (C.Parsed NetAgent'session'params)) where
    marshalInto raw_ NetAgent'session'params{..} = (do
        (GH.encodeField #owner owner raw_)
        (Std_.pure ())
        )
instance (GH.HasField "owner" GH.Slot NetAgent'session'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data NetAgent'session'results 
type instance (R.ReprFor NetAgent'session'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetAgent'session'results) where
    typeId  = 10912420620258476034
instance (C.TypedStruct NetAgent'session'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NetAgent'session'results) where
    type AllocHint NetAgent'session'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetAgent'session'results (C.Parsed NetAgent'session'results))
instance (C.AllocateList NetAgent'session'results) where
    type ListAllocHint NetAgent'session'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetAgent'session'results (C.Parsed NetAgent'session'results))
data instance C.Parsed NetAgent'session'results
    = NetAgent'session'results 
        {session :: (RP.Parsed Session)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetAgent'session'results))
deriving instance (Std_.Eq (C.Parsed NetAgent'session'results))
instance (C.Parse NetAgent'session'results (C.Parsed NetAgent'session'results)) where
    parse raw_ = (NetAgent'session'results <$> (GH.parseField #session raw_))
instance (C.Marshal NetAgent'session'results (C.Parsed NetAgent'session'results)) where
    marshalInto raw_ NetAgent'session'results{..} = (do
        (GH.encodeField #session session raw_)
        (Std_.pure ())
        )
instance (GH.HasField "session" GH.Slot NetAgent'session'results Session) where
    fieldByLabel  = (GH.ptrField 0)
data NetAgent'ping'params 
type instance (R.ReprFor NetAgent'ping'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetAgent'ping'params) where
    typeId  = 16209421556680305749
instance (C.TypedStruct NetAgent'ping'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate NetAgent'ping'params) where
    type AllocHint NetAgent'ping'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetAgent'ping'params (C.Parsed NetAgent'ping'params))
instance (C.AllocateList NetAgent'ping'params) where
    type ListAllocHint NetAgent'ping'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetAgent'ping'params (C.Parsed NetAgent'ping'params))
data instance C.Parsed NetAgent'ping'params
    = NetAgent'ping'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetAgent'ping'params))
deriving instance (Std_.Eq (C.Parsed NetAgent'ping'params))
instance (C.Parse NetAgent'ping'params (C.Parsed NetAgent'ping'params)) where
    parse raw_ = (Std_.pure NetAgent'ping'params)
instance (C.Marshal NetAgent'ping'params (C.Parsed NetAgent'ping'params)) where
    marshalInto _raw (NetAgent'ping'params) = (Std_.pure ())
data NetAgent'ping'results 
type instance (R.ReprFor NetAgent'ping'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetAgent'ping'results) where
    typeId  = 11902055459973614266
instance (C.TypedStruct NetAgent'ping'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate NetAgent'ping'results) where
    type AllocHint NetAgent'ping'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetAgent'ping'results (C.Parsed NetAgent'ping'results))
instance (C.AllocateList NetAgent'ping'results) where
    type ListAllocHint NetAgent'ping'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetAgent'ping'results (C.Parsed NetAgent'ping'results))
data instance C.Parsed NetAgent'ping'results
    = NetAgent'ping'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetAgent'ping'results))
deriving instance (Std_.Eq (C.Parsed NetAgent'ping'results))
instance (C.Parse NetAgent'ping'results (C.Parsed NetAgent'ping'results)) where
    parse raw_ = (Std_.pure NetAgent'ping'results)
instance (C.Marshal NetAgent'ping'results (C.Parsed NetAgent'ping'results)) where
    marshalInto _raw (NetAgent'ping'results) = (Std_.pure ())
data NetAgent'version'params 
type instance (R.ReprFor NetAgent'version'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetAgent'version'params) where
    typeId  = 18189165422566961399
instance (C.TypedStruct NetAgent'version'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate NetAgent'version'params) where
    type AllocHint NetAgent'version'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetAgent'version'params (C.Parsed NetAgent'version'params))
instance (C.AllocateList NetAgent'version'params) where
    type ListAllocHint NetAgent'version'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetAgent'version'params (C.Parsed NetAgent'version'params))
data instance C.Parsed NetAgent'version'params
    = NetAgent'version'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetAgent'version'params))
deriving instance (Std_.Eq (C.Parsed NetAgent'version'params))
instance (C.Parse NetAgent'version'params (C.Parsed NetAgent'version'params)) where
    parse raw_ = (Std_.pure NetAgent'version'params)
instance (C.Marshal NetAgent'version'params (C.Parsed NetAgent'version'params)) where
    marshalInto _raw (NetAgent'version'params) = (Std_.pure ())
data NetAgent'version'results 
type instance (R.ReprFor NetAgent'version'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetAgent'version'results) where
    typeId  = 11250995495941231883
instance (C.TypedStruct NetAgent'version'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NetAgent'version'results) where
    type AllocHint NetAgent'version'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetAgent'version'results (C.Parsed NetAgent'version'results))
instance (C.AllocateList NetAgent'version'results) where
    type ListAllocHint NetAgent'version'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetAgent'version'results (C.Parsed NetAgent'version'results))
data instance C.Parsed NetAgent'version'results
    = NetAgent'version'results 
        {info :: (RP.Parsed AgentInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetAgent'version'results))
deriving instance (Std_.Eq (C.Parsed NetAgent'version'results))
instance (C.Parse NetAgent'version'results (C.Parsed NetAgent'version'results)) where
    parse raw_ = (NetAgent'version'results <$> (GH.parseField #info raw_))
instance (C.Marshal NetAgent'version'results (C.Parsed NetAgent'version'results)) where
    marshalInto raw_ NetAgent'version'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot NetAgent'version'results AgentInfo) where
    fieldByLabel  = (GH.ptrField 0)
data AgentInfo 
type instance (R.ReprFor AgentInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId AgentInfo) where
    typeId  = 12469945529563840519
instance (C.TypedStruct AgentInfo) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate AgentInfo) where
    type AllocHint AgentInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc AgentInfo (C.Parsed AgentInfo))
instance (C.AllocateList AgentInfo) where
    type ListAllocHint AgentInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc AgentInfo (C.Parsed AgentInfo))
data instance C.Parsed AgentInfo
    = AgentInfo 
        {semver :: (RP.Parsed Basics.Text)
        ,capabilities :: (RP.Parsed (R.List Basics.Text))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed AgentInfo))
deriving instance (Std_.Eq (C.Parsed AgentInfo))
instance (C.Parse AgentInfo (C.Parsed AgentInfo)) where
    parse raw_ = (AgentInfo <$> (GH.parseField #semver raw_)
                            <*> (GH.parseField #capabilities raw_))
instance (C.Marshal AgentInfo (C.Parsed AgentInfo)) where
    marshalInto raw_ AgentInfo{..} = (do
        (GH.encodeField #semver semver raw_)
        (GH.encodeField #capabilities capabilities raw_)
        (Std_.pure ())
        )
instance (GH.HasField "semver" GH.Slot AgentInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "capabilities" GH.Slot AgentInfo (R.List Basics.Text)) where
    fieldByLabel  = (GH.ptrField 1)
data Session 
type instance (R.ReprFor Session) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Session) where
    typeId  = 18377821789121950725
instance (C.Parse Session (GH.Client Session)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Session) where
    type Server Session = Session'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Session)) [(GH.toUntypedMethodHandler ((session'applyNetwork) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'listNetworks) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'deleteNetwork) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'applyTap) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'listTaps) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'deleteTap) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'setIpForwarding) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'subscribeEvents) s_))] [])
class (Session'server_ s_) where
    {-# MINIMAL session'applyNetwork,session'listNetworks,session'deleteNetwork,session'applyTap,session'listTaps,session'deleteTap,session'setIpForwarding,session'subscribeEvents #-}
    session'applyNetwork :: s_ -> (GH.MethodHandler Session'applyNetwork'params Session'applyNetwork'results)
    session'applyNetwork _ = GH.methodUnimplemented
    session'listNetworks :: s_ -> (GH.MethodHandler Session'listNetworks'params Session'listNetworks'results)
    session'listNetworks _ = GH.methodUnimplemented
    session'deleteNetwork :: s_ -> (GH.MethodHandler Session'deleteNetwork'params Session'deleteNetwork'results)
    session'deleteNetwork _ = GH.methodUnimplemented
    session'applyTap :: s_ -> (GH.MethodHandler Session'applyTap'params Session'applyTap'results)
    session'applyTap _ = GH.methodUnimplemented
    session'listTaps :: s_ -> (GH.MethodHandler Session'listTaps'params Session'listTaps'results)
    session'listTaps _ = GH.methodUnimplemented
    session'deleteTap :: s_ -> (GH.MethodHandler Session'deleteTap'params Session'deleteTap'results)
    session'deleteTap _ = GH.methodUnimplemented
    session'setIpForwarding :: s_ -> (GH.MethodHandler Session'setIpForwarding'params Session'setIpForwarding'results)
    session'setIpForwarding _ = GH.methodUnimplemented
    session'subscribeEvents :: s_ -> (GH.MethodHandler Session'subscribeEvents'params Session'subscribeEvents'results)
    session'subscribeEvents _ = GH.methodUnimplemented
instance (GH.HasMethod "applyNetwork" Session Session'applyNetwork'params Session'applyNetwork'results) where
    methodByLabel  = (GH.Method 18377821789121950725 0)
instance (GH.HasMethod "listNetworks" Session Session'listNetworks'params Session'listNetworks'results) where
    methodByLabel  = (GH.Method 18377821789121950725 1)
instance (GH.HasMethod "deleteNetwork" Session Session'deleteNetwork'params Session'deleteNetwork'results) where
    methodByLabel  = (GH.Method 18377821789121950725 2)
instance (GH.HasMethod "applyTap" Session Session'applyTap'params Session'applyTap'results) where
    methodByLabel  = (GH.Method 18377821789121950725 3)
instance (GH.HasMethod "listTaps" Session Session'listTaps'params Session'listTaps'results) where
    methodByLabel  = (GH.Method 18377821789121950725 4)
instance (GH.HasMethod "deleteTap" Session Session'deleteTap'params Session'deleteTap'results) where
    methodByLabel  = (GH.Method 18377821789121950725 5)
instance (GH.HasMethod "setIpForwarding" Session Session'setIpForwarding'params Session'setIpForwarding'results) where
    methodByLabel  = (GH.Method 18377821789121950725 6)
instance (GH.HasMethod "subscribeEvents" Session Session'subscribeEvents'params Session'subscribeEvents'results) where
    methodByLabel  = (GH.Method 18377821789121950725 7)
data Session'applyNetwork'params 
type instance (R.ReprFor Session'applyNetwork'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'applyNetwork'params) where
    typeId  = 16009969733571045190
instance (C.TypedStruct Session'applyNetwork'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'applyNetwork'params) where
    type AllocHint Session'applyNetwork'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'applyNetwork'params (C.Parsed Session'applyNetwork'params))
instance (C.AllocateList Session'applyNetwork'params) where
    type ListAllocHint Session'applyNetwork'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'applyNetwork'params (C.Parsed Session'applyNetwork'params))
data instance C.Parsed Session'applyNetwork'params
    = Session'applyNetwork'params 
        {spec :: (RP.Parsed NetworkSpec)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'applyNetwork'params))
deriving instance (Std_.Eq (C.Parsed Session'applyNetwork'params))
instance (C.Parse Session'applyNetwork'params (C.Parsed Session'applyNetwork'params)) where
    parse raw_ = (Session'applyNetwork'params <$> (GH.parseField #spec raw_))
instance (C.Marshal Session'applyNetwork'params (C.Parsed Session'applyNetwork'params)) where
    marshalInto raw_ Session'applyNetwork'params{..} = (do
        (GH.encodeField #spec spec raw_)
        (Std_.pure ())
        )
instance (GH.HasField "spec" GH.Slot Session'applyNetwork'params NetworkSpec) where
    fieldByLabel  = (GH.ptrField 0)
data Session'applyNetwork'results 
type instance (R.ReprFor Session'applyNetwork'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'applyNetwork'results) where
    typeId  = 12952689855789234858
instance (C.TypedStruct Session'applyNetwork'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'applyNetwork'results) where
    type AllocHint Session'applyNetwork'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'applyNetwork'results (C.Parsed Session'applyNetwork'results))
instance (C.AllocateList Session'applyNetwork'results) where
    type ListAllocHint Session'applyNetwork'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'applyNetwork'results (C.Parsed Session'applyNetwork'results))
data instance C.Parsed Session'applyNetwork'results
    = Session'applyNetwork'results 
        {info :: (RP.Parsed NetworkInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'applyNetwork'results))
deriving instance (Std_.Eq (C.Parsed Session'applyNetwork'results))
instance (C.Parse Session'applyNetwork'results (C.Parsed Session'applyNetwork'results)) where
    parse raw_ = (Session'applyNetwork'results <$> (GH.parseField #info raw_))
instance (C.Marshal Session'applyNetwork'results (C.Parsed Session'applyNetwork'results)) where
    marshalInto raw_ Session'applyNetwork'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot Session'applyNetwork'results NetworkInfo) where
    fieldByLabel  = (GH.ptrField 0)
data Session'listNetworks'params 
type instance (R.ReprFor Session'listNetworks'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'listNetworks'params) where
    typeId  = 16248932357397564441
instance (C.TypedStruct Session'listNetworks'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'listNetworks'params) where
    type AllocHint Session'listNetworks'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'listNetworks'params (C.Parsed Session'listNetworks'params))
instance (C.AllocateList Session'listNetworks'params) where
    type ListAllocHint Session'listNetworks'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'listNetworks'params (C.Parsed Session'listNetworks'params))
data instance C.Parsed Session'listNetworks'params
    = Session'listNetworks'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'listNetworks'params))
deriving instance (Std_.Eq (C.Parsed Session'listNetworks'params))
instance (C.Parse Session'listNetworks'params (C.Parsed Session'listNetworks'params)) where
    parse raw_ = (Std_.pure Session'listNetworks'params)
instance (C.Marshal Session'listNetworks'params (C.Parsed Session'listNetworks'params)) where
    marshalInto _raw (Session'listNetworks'params) = (Std_.pure ())
data Session'listNetworks'results 
type instance (R.ReprFor Session'listNetworks'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'listNetworks'results) where
    typeId  = 10897823512533511254
instance (C.TypedStruct Session'listNetworks'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'listNetworks'results) where
    type AllocHint Session'listNetworks'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'listNetworks'results (C.Parsed Session'listNetworks'results))
instance (C.AllocateList Session'listNetworks'results) where
    type ListAllocHint Session'listNetworks'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'listNetworks'results (C.Parsed Session'listNetworks'results))
data instance C.Parsed Session'listNetworks'results
    = Session'listNetworks'results 
        {networks :: (RP.Parsed (R.List NetworkInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'listNetworks'results))
deriving instance (Std_.Eq (C.Parsed Session'listNetworks'results))
instance (C.Parse Session'listNetworks'results (C.Parsed Session'listNetworks'results)) where
    parse raw_ = (Session'listNetworks'results <$> (GH.parseField #networks raw_))
instance (C.Marshal Session'listNetworks'results (C.Parsed Session'listNetworks'results)) where
    marshalInto raw_ Session'listNetworks'results{..} = (do
        (GH.encodeField #networks networks raw_)
        (Std_.pure ())
        )
instance (GH.HasField "networks" GH.Slot Session'listNetworks'results (R.List NetworkInfo)) where
    fieldByLabel  = (GH.ptrField 0)
data Session'deleteNetwork'params 
type instance (R.ReprFor Session'deleteNetwork'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'deleteNetwork'params) where
    typeId  = 10597725040083098732
instance (C.TypedStruct Session'deleteNetwork'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'deleteNetwork'params) where
    type AllocHint Session'deleteNetwork'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'deleteNetwork'params (C.Parsed Session'deleteNetwork'params))
instance (C.AllocateList Session'deleteNetwork'params) where
    type ListAllocHint Session'deleteNetwork'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'deleteNetwork'params (C.Parsed Session'deleteNetwork'params))
data instance C.Parsed Session'deleteNetwork'params
    = Session'deleteNetwork'params 
        {name :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'deleteNetwork'params))
deriving instance (Std_.Eq (C.Parsed Session'deleteNetwork'params))
instance (C.Parse Session'deleteNetwork'params (C.Parsed Session'deleteNetwork'params)) where
    parse raw_ = (Session'deleteNetwork'params <$> (GH.parseField #name raw_))
instance (C.Marshal Session'deleteNetwork'params (C.Parsed Session'deleteNetwork'params)) where
    marshalInto raw_ Session'deleteNetwork'params{..} = (do
        (GH.encodeField #name name raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot Session'deleteNetwork'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Session'deleteNetwork'results 
type instance (R.ReprFor Session'deleteNetwork'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'deleteNetwork'results) where
    typeId  = 13310718494801309865
instance (C.TypedStruct Session'deleteNetwork'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'deleteNetwork'results) where
    type AllocHint Session'deleteNetwork'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'deleteNetwork'results (C.Parsed Session'deleteNetwork'results))
instance (C.AllocateList Session'deleteNetwork'results) where
    type ListAllocHint Session'deleteNetwork'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'deleteNetwork'results (C.Parsed Session'deleteNetwork'results))
data instance C.Parsed Session'deleteNetwork'results
    = Session'deleteNetwork'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'deleteNetwork'results))
deriving instance (Std_.Eq (C.Parsed Session'deleteNetwork'results))
instance (C.Parse Session'deleteNetwork'results (C.Parsed Session'deleteNetwork'results)) where
    parse raw_ = (Std_.pure Session'deleteNetwork'results)
instance (C.Marshal Session'deleteNetwork'results (C.Parsed Session'deleteNetwork'results)) where
    marshalInto _raw (Session'deleteNetwork'results) = (Std_.pure ())
data Session'applyTap'params 
type instance (R.ReprFor Session'applyTap'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'applyTap'params) where
    typeId  = 17066202627105760396
instance (C.TypedStruct Session'applyTap'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'applyTap'params) where
    type AllocHint Session'applyTap'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'applyTap'params (C.Parsed Session'applyTap'params))
instance (C.AllocateList Session'applyTap'params) where
    type ListAllocHint Session'applyTap'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'applyTap'params (C.Parsed Session'applyTap'params))
data instance C.Parsed Session'applyTap'params
    = Session'applyTap'params 
        {spec :: (RP.Parsed TapSpec)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'applyTap'params))
deriving instance (Std_.Eq (C.Parsed Session'applyTap'params))
instance (C.Parse Session'applyTap'params (C.Parsed Session'applyTap'params)) where
    parse raw_ = (Session'applyTap'params <$> (GH.parseField #spec raw_))
instance (C.Marshal Session'applyTap'params (C.Parsed Session'applyTap'params)) where
    marshalInto raw_ Session'applyTap'params{..} = (do
        (GH.encodeField #spec spec raw_)
        (Std_.pure ())
        )
instance (GH.HasField "spec" GH.Slot Session'applyTap'params TapSpec) where
    fieldByLabel  = (GH.ptrField 0)
data Session'applyTap'results 
type instance (R.ReprFor Session'applyTap'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'applyTap'results) where
    typeId  = 13926779387823511762
instance (C.TypedStruct Session'applyTap'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'applyTap'results) where
    type AllocHint Session'applyTap'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'applyTap'results (C.Parsed Session'applyTap'results))
instance (C.AllocateList Session'applyTap'results) where
    type ListAllocHint Session'applyTap'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'applyTap'results (C.Parsed Session'applyTap'results))
data instance C.Parsed Session'applyTap'results
    = Session'applyTap'results 
        {info :: (RP.Parsed TapInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'applyTap'results))
deriving instance (Std_.Eq (C.Parsed Session'applyTap'results))
instance (C.Parse Session'applyTap'results (C.Parsed Session'applyTap'results)) where
    parse raw_ = (Session'applyTap'results <$> (GH.parseField #info raw_))
instance (C.Marshal Session'applyTap'results (C.Parsed Session'applyTap'results)) where
    marshalInto raw_ Session'applyTap'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot Session'applyTap'results TapInfo) where
    fieldByLabel  = (GH.ptrField 0)
data Session'listTaps'params 
type instance (R.ReprFor Session'listTaps'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'listTaps'params) where
    typeId  = 18419340564866344367
instance (C.TypedStruct Session'listTaps'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'listTaps'params) where
    type AllocHint Session'listTaps'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'listTaps'params (C.Parsed Session'listTaps'params))
instance (C.AllocateList Session'listTaps'params) where
    type ListAllocHint Session'listTaps'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'listTaps'params (C.Parsed Session'listTaps'params))
data instance C.Parsed Session'listTaps'params
    = Session'listTaps'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'listTaps'params))
deriving instance (Std_.Eq (C.Parsed Session'listTaps'params))
instance (C.Parse Session'listTaps'params (C.Parsed Session'listTaps'params)) where
    parse raw_ = (Std_.pure Session'listTaps'params)
instance (C.Marshal Session'listTaps'params (C.Parsed Session'listTaps'params)) where
    marshalInto _raw (Session'listTaps'params) = (Std_.pure ())
data Session'listTaps'results 
type instance (R.ReprFor Session'listTaps'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'listTaps'results) where
    typeId  = 10880813484077667812
instance (C.TypedStruct Session'listTaps'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'listTaps'results) where
    type AllocHint Session'listTaps'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'listTaps'results (C.Parsed Session'listTaps'results))
instance (C.AllocateList Session'listTaps'results) where
    type ListAllocHint Session'listTaps'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'listTaps'results (C.Parsed Session'listTaps'results))
data instance C.Parsed Session'listTaps'results
    = Session'listTaps'results 
        {taps :: (RP.Parsed (R.List TapInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'listTaps'results))
deriving instance (Std_.Eq (C.Parsed Session'listTaps'results))
instance (C.Parse Session'listTaps'results (C.Parsed Session'listTaps'results)) where
    parse raw_ = (Session'listTaps'results <$> (GH.parseField #taps raw_))
instance (C.Marshal Session'listTaps'results (C.Parsed Session'listTaps'results)) where
    marshalInto raw_ Session'listTaps'results{..} = (do
        (GH.encodeField #taps taps raw_)
        (Std_.pure ())
        )
instance (GH.HasField "taps" GH.Slot Session'listTaps'results (R.List TapInfo)) where
    fieldByLabel  = (GH.ptrField 0)
data Session'deleteTap'params 
type instance (R.ReprFor Session'deleteTap'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'deleteTap'params) where
    typeId  = 18343140468287461106
instance (C.TypedStruct Session'deleteTap'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'deleteTap'params) where
    type AllocHint Session'deleteTap'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'deleteTap'params (C.Parsed Session'deleteTap'params))
instance (C.AllocateList Session'deleteTap'params) where
    type ListAllocHint Session'deleteTap'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'deleteTap'params (C.Parsed Session'deleteTap'params))
data instance C.Parsed Session'deleteTap'params
    = Session'deleteTap'params 
        {name :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'deleteTap'params))
deriving instance (Std_.Eq (C.Parsed Session'deleteTap'params))
instance (C.Parse Session'deleteTap'params (C.Parsed Session'deleteTap'params)) where
    parse raw_ = (Session'deleteTap'params <$> (GH.parseField #name raw_))
instance (C.Marshal Session'deleteTap'params (C.Parsed Session'deleteTap'params)) where
    marshalInto raw_ Session'deleteTap'params{..} = (do
        (GH.encodeField #name name raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot Session'deleteTap'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Session'deleteTap'results 
type instance (R.ReprFor Session'deleteTap'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'deleteTap'results) where
    typeId  = 18208164700962459784
instance (C.TypedStruct Session'deleteTap'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'deleteTap'results) where
    type AllocHint Session'deleteTap'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'deleteTap'results (C.Parsed Session'deleteTap'results))
instance (C.AllocateList Session'deleteTap'results) where
    type ListAllocHint Session'deleteTap'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'deleteTap'results (C.Parsed Session'deleteTap'results))
data instance C.Parsed Session'deleteTap'results
    = Session'deleteTap'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'deleteTap'results))
deriving instance (Std_.Eq (C.Parsed Session'deleteTap'results))
instance (C.Parse Session'deleteTap'results (C.Parsed Session'deleteTap'results)) where
    parse raw_ = (Std_.pure Session'deleteTap'results)
instance (C.Marshal Session'deleteTap'results (C.Parsed Session'deleteTap'results)) where
    marshalInto _raw (Session'deleteTap'results) = (Std_.pure ())
data Session'setIpForwarding'params 
type instance (R.ReprFor Session'setIpForwarding'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'setIpForwarding'params) where
    typeId  = 10547781726648321946
instance (C.TypedStruct Session'setIpForwarding'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Session'setIpForwarding'params) where
    type AllocHint Session'setIpForwarding'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'setIpForwarding'params (C.Parsed Session'setIpForwarding'params))
instance (C.AllocateList Session'setIpForwarding'params) where
    type ListAllocHint Session'setIpForwarding'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'setIpForwarding'params (C.Parsed Session'setIpForwarding'params))
data instance C.Parsed Session'setIpForwarding'params
    = Session'setIpForwarding'params 
        {enabled :: (RP.Parsed Std_.Bool)
        ,family_ :: (RP.Parsed NetFamily)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'setIpForwarding'params))
deriving instance (Std_.Eq (C.Parsed Session'setIpForwarding'params))
instance (C.Parse Session'setIpForwarding'params (C.Parsed Session'setIpForwarding'params)) where
    parse raw_ = (Session'setIpForwarding'params <$> (GH.parseField #enabled raw_)
                                                 <*> (GH.parseField #family_ raw_))
instance (C.Marshal Session'setIpForwarding'params (C.Parsed Session'setIpForwarding'params)) where
    marshalInto raw_ Session'setIpForwarding'params{..} = (do
        (GH.encodeField #enabled enabled raw_)
        (GH.encodeField #family_ family_ raw_)
        (Std_.pure ())
        )
instance (GH.HasField "enabled" GH.Slot Session'setIpForwarding'params Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (GH.HasField "family_" GH.Slot Session'setIpForwarding'params NetFamily) where
    fieldByLabel  = (GH.dataField 16 0 16 0)
data Session'setIpForwarding'results 
type instance (R.ReprFor Session'setIpForwarding'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'setIpForwarding'results) where
    typeId  = 15710458737355548640
instance (C.TypedStruct Session'setIpForwarding'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'setIpForwarding'results) where
    type AllocHint Session'setIpForwarding'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'setIpForwarding'results (C.Parsed Session'setIpForwarding'results))
instance (C.AllocateList Session'setIpForwarding'results) where
    type ListAllocHint Session'setIpForwarding'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'setIpForwarding'results (C.Parsed Session'setIpForwarding'results))
data instance C.Parsed Session'setIpForwarding'results
    = Session'setIpForwarding'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'setIpForwarding'results))
deriving instance (Std_.Eq (C.Parsed Session'setIpForwarding'results))
instance (C.Parse Session'setIpForwarding'results (C.Parsed Session'setIpForwarding'results)) where
    parse raw_ = (Std_.pure Session'setIpForwarding'results)
instance (C.Marshal Session'setIpForwarding'results (C.Parsed Session'setIpForwarding'results)) where
    marshalInto _raw (Session'setIpForwarding'results) = (Std_.pure ())
data Session'subscribeEvents'params 
type instance (R.ReprFor Session'subscribeEvents'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'subscribeEvents'params) where
    typeId  = 9224720994102743237
instance (C.TypedStruct Session'subscribeEvents'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'subscribeEvents'params) where
    type AllocHint Session'subscribeEvents'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'subscribeEvents'params (C.Parsed Session'subscribeEvents'params))
instance (C.AllocateList Session'subscribeEvents'params) where
    type ListAllocHint Session'subscribeEvents'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'subscribeEvents'params (C.Parsed Session'subscribeEvents'params))
data instance C.Parsed Session'subscribeEvents'params
    = Session'subscribeEvents'params 
        {sink :: (RP.Parsed EventSink)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'subscribeEvents'params))
deriving instance (Std_.Eq (C.Parsed Session'subscribeEvents'params))
instance (C.Parse Session'subscribeEvents'params (C.Parsed Session'subscribeEvents'params)) where
    parse raw_ = (Session'subscribeEvents'params <$> (GH.parseField #sink raw_))
instance (C.Marshal Session'subscribeEvents'params (C.Parsed Session'subscribeEvents'params)) where
    marshalInto raw_ Session'subscribeEvents'params{..} = (do
        (GH.encodeField #sink sink raw_)
        (Std_.pure ())
        )
instance (GH.HasField "sink" GH.Slot Session'subscribeEvents'params EventSink) where
    fieldByLabel  = (GH.ptrField 0)
data Session'subscribeEvents'results 
type instance (R.ReprFor Session'subscribeEvents'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'subscribeEvents'results) where
    typeId  = 14771270395976879847
instance (C.TypedStruct Session'subscribeEvents'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'subscribeEvents'results) where
    type AllocHint Session'subscribeEvents'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'subscribeEvents'results (C.Parsed Session'subscribeEvents'results))
instance (C.AllocateList Session'subscribeEvents'results) where
    type ListAllocHint Session'subscribeEvents'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'subscribeEvents'results (C.Parsed Session'subscribeEvents'results))
data instance C.Parsed Session'subscribeEvents'results
    = Session'subscribeEvents'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'subscribeEvents'results))
deriving instance (Std_.Eq (C.Parsed Session'subscribeEvents'results))
instance (C.Parse Session'subscribeEvents'results (C.Parsed Session'subscribeEvents'results)) where
    parse raw_ = (Std_.pure Session'subscribeEvents'results)
instance (C.Marshal Session'subscribeEvents'results (C.Parsed Session'subscribeEvents'results)) where
    marshalInto _raw (Session'subscribeEvents'results) = (Std_.pure ())
data NetworkSpec 
type instance (R.ReprFor NetworkSpec) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetworkSpec) where
    typeId  = 11878780582777869737
instance (C.TypedStruct NetworkSpec) where
    numStructWords  = 1
    numStructPtrs  = 5
instance (C.Allocate NetworkSpec) where
    type AllocHint NetworkSpec = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetworkSpec (C.Parsed NetworkSpec))
instance (C.AllocateList NetworkSpec) where
    type ListAllocHint NetworkSpec = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetworkSpec (C.Parsed NetworkSpec))
data instance C.Parsed NetworkSpec
    = NetworkSpec 
        {name :: (RP.Parsed Basics.Text)
        ,cidr :: (RP.Parsed Basics.Text)
        ,mtu :: (RP.Parsed Std_.Word32)
        ,nat :: (RP.Parsed NatSpec)
        ,dhcp :: (RP.Parsed DhcpSpec)
        ,overlay :: (RP.Parsed OverlaySpec)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetworkSpec))
deriving instance (Std_.Eq (C.Parsed NetworkSpec))
instance (C.Parse NetworkSpec (C.Parsed NetworkSpec)) where
    parse raw_ = (NetworkSpec <$> (GH.parseField #name raw_)
                              <*> (GH.parseField #cidr raw_)
                              <*> (GH.parseField #mtu raw_)
                              <*> (GH.parseField #nat raw_)
                              <*> (GH.parseField #dhcp raw_)
                              <*> (GH.parseField #overlay raw_))
instance (C.Marshal NetworkSpec (C.Parsed NetworkSpec)) where
    marshalInto raw_ NetworkSpec{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #cidr cidr raw_)
        (GH.encodeField #mtu mtu raw_)
        (GH.encodeField #nat nat raw_)
        (GH.encodeField #dhcp dhcp raw_)
        (GH.encodeField #overlay overlay raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot NetworkSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "cidr" GH.Slot NetworkSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "mtu" GH.Slot NetworkSpec Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 1500)
instance (GH.HasField "nat" GH.Slot NetworkSpec NatSpec) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "dhcp" GH.Slot NetworkSpec DhcpSpec) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "overlay" GH.Slot NetworkSpec OverlaySpec) where
    fieldByLabel  = (GH.ptrField 4)
data NatSpec 
type instance (R.ReprFor NatSpec) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NatSpec) where
    typeId  = 15953567940299059486
instance (C.TypedStruct NatSpec) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate NatSpec) where
    type AllocHint NatSpec = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NatSpec (C.Parsed NatSpec))
instance (C.AllocateList NatSpec) where
    type ListAllocHint NatSpec = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NatSpec (C.Parsed NatSpec))
data instance C.Parsed NatSpec
    = NatSpec 
        {enabled :: (RP.Parsed Std_.Bool)
        ,uplinkIf :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NatSpec))
deriving instance (Std_.Eq (C.Parsed NatSpec))
instance (C.Parse NatSpec (C.Parsed NatSpec)) where
    parse raw_ = (NatSpec <$> (GH.parseField #enabled raw_)
                          <*> (GH.parseField #uplinkIf raw_))
instance (C.Marshal NatSpec (C.Parsed NatSpec)) where
    marshalInto raw_ NatSpec{..} = (do
        (GH.encodeField #enabled enabled raw_)
        (GH.encodeField #uplinkIf uplinkIf raw_)
        (Std_.pure ())
        )
instance (GH.HasField "enabled" GH.Slot NatSpec Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (GH.HasField "uplinkIf" GH.Slot NatSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data DhcpSpec 
type instance (R.ReprFor DhcpSpec) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DhcpSpec) where
    typeId  = 9911415732338660737
instance (C.TypedStruct DhcpSpec) where
    numStructWords  = 1
    numStructPtrs  = 7
instance (C.Allocate DhcpSpec) where
    type AllocHint DhcpSpec = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DhcpSpec (C.Parsed DhcpSpec))
instance (C.AllocateList DhcpSpec) where
    type ListAllocHint DhcpSpec = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DhcpSpec (C.Parsed DhcpSpec))
data instance C.Parsed DhcpSpec
    = DhcpSpec 
        {enabled :: (RP.Parsed Std_.Bool)
        ,rangeStart :: (RP.Parsed Basics.Text)
        ,rangeEnd :: (RP.Parsed Basics.Text)
        ,leaseTime :: (RP.Parsed Basics.Text)
        ,domain :: (RP.Parsed Basics.Text)
        ,extraArgs :: (RP.Parsed (R.List Basics.Text))
        ,hostReservations :: (RP.Parsed (R.List DhcpHostReservation))
        ,dnsServers :: (RP.Parsed (R.List Basics.Text))
        ,hostDns :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DhcpSpec))
deriving instance (Std_.Eq (C.Parsed DhcpSpec))
instance (C.Parse DhcpSpec (C.Parsed DhcpSpec)) where
    parse raw_ = (DhcpSpec <$> (GH.parseField #enabled raw_)
                           <*> (GH.parseField #rangeStart raw_)
                           <*> (GH.parseField #rangeEnd raw_)
                           <*> (GH.parseField #leaseTime raw_)
                           <*> (GH.parseField #domain raw_)
                           <*> (GH.parseField #extraArgs raw_)
                           <*> (GH.parseField #hostReservations raw_)
                           <*> (GH.parseField #dnsServers raw_)
                           <*> (GH.parseField #hostDns raw_))
instance (C.Marshal DhcpSpec (C.Parsed DhcpSpec)) where
    marshalInto raw_ DhcpSpec{..} = (do
        (GH.encodeField #enabled enabled raw_)
        (GH.encodeField #rangeStart rangeStart raw_)
        (GH.encodeField #rangeEnd rangeEnd raw_)
        (GH.encodeField #leaseTime leaseTime raw_)
        (GH.encodeField #domain domain raw_)
        (GH.encodeField #extraArgs extraArgs raw_)
        (GH.encodeField #hostReservations hostReservations raw_)
        (GH.encodeField #dnsServers dnsServers raw_)
        (GH.encodeField #hostDns hostDns raw_)
        (Std_.pure ())
        )
instance (GH.HasField "enabled" GH.Slot DhcpSpec Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (GH.HasField "rangeStart" GH.Slot DhcpSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "rangeEnd" GH.Slot DhcpSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "leaseTime" GH.Slot DhcpSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "domain" GH.Slot DhcpSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "extraArgs" GH.Slot DhcpSpec (R.List Basics.Text)) where
    fieldByLabel  = (GH.ptrField 4)
instance (GH.HasField "hostReservations" GH.Slot DhcpSpec (R.List DhcpHostReservation)) where
    fieldByLabel  = (GH.ptrField 5)
instance (GH.HasField "dnsServers" GH.Slot DhcpSpec (R.List Basics.Text)) where
    fieldByLabel  = (GH.ptrField 6)
instance (GH.HasField "hostDns" GH.Slot DhcpSpec Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 0 1 1)
data DhcpHostReservation 
type instance (R.ReprFor DhcpHostReservation) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DhcpHostReservation) where
    typeId  = 18059630684346861993
instance (C.TypedStruct DhcpHostReservation) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate DhcpHostReservation) where
    type AllocHint DhcpHostReservation = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DhcpHostReservation (C.Parsed DhcpHostReservation))
instance (C.AllocateList DhcpHostReservation) where
    type ListAllocHint DhcpHostReservation = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DhcpHostReservation (C.Parsed DhcpHostReservation))
data instance C.Parsed DhcpHostReservation
    = DhcpHostReservation 
        {mac :: (RP.Parsed Basics.Text)
        ,ip :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DhcpHostReservation))
deriving instance (Std_.Eq (C.Parsed DhcpHostReservation))
instance (C.Parse DhcpHostReservation (C.Parsed DhcpHostReservation)) where
    parse raw_ = (DhcpHostReservation <$> (GH.parseField #mac raw_)
                                      <*> (GH.parseField #ip raw_))
instance (C.Marshal DhcpHostReservation (C.Parsed DhcpHostReservation)) where
    marshalInto raw_ DhcpHostReservation{..} = (do
        (GH.encodeField #mac mac raw_)
        (GH.encodeField #ip ip raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mac" GH.Slot DhcpHostReservation Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "ip" GH.Slot DhcpHostReservation Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
data OverlaySpec 
type instance (R.ReprFor OverlaySpec) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId OverlaySpec) where
    typeId  = 11754725013735792531
instance (C.TypedStruct OverlaySpec) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate OverlaySpec) where
    type AllocHint OverlaySpec = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc OverlaySpec (C.Parsed OverlaySpec))
instance (C.AllocateList OverlaySpec) where
    type ListAllocHint OverlaySpec = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc OverlaySpec (C.Parsed OverlaySpec))
data instance C.Parsed OverlaySpec
    = OverlaySpec 
        {union' :: (C.Parsed (GH.Which OverlaySpec))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed OverlaySpec))
deriving instance (Std_.Eq (C.Parsed OverlaySpec))
instance (C.Parse OverlaySpec (C.Parsed OverlaySpec)) where
    parse raw_ = (OverlaySpec <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal OverlaySpec (C.Parsed OverlaySpec)) where
    marshalInto raw_ OverlaySpec{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion OverlaySpec) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich OverlaySpec mut_
        = RW_OverlaySpec'none (R.Raw () mut_)
        | RW_OverlaySpec'vxlan (R.Raw VxlanSpec mut_)
        | RW_OverlaySpec'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_OverlaySpec'none <$> (GH.readVariant #none struct_))
        1 ->
            (RW_OverlaySpec'vxlan <$> (GH.readVariant #vxlan struct_))
        _ ->
            (Std_.pure (RW_OverlaySpec'unknown' tag_))
    data Which OverlaySpec
instance (GH.HasVariant "none" GH.Slot OverlaySpec ()) where
    variantByLabel  = (GH.Variant GH.voidField 0)
instance (GH.HasVariant "vxlan" GH.Slot OverlaySpec VxlanSpec) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 1)
data instance C.Parsed (GH.Which OverlaySpec)
    = OverlaySpec'none 
    | OverlaySpec'vxlan (RP.Parsed VxlanSpec)
    | OverlaySpec'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which OverlaySpec)))
deriving instance (Std_.Eq (C.Parsed (GH.Which OverlaySpec)))
instance (C.Parse (GH.Which OverlaySpec) (C.Parsed (GH.Which OverlaySpec))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_OverlaySpec'none _) ->
                (Std_.pure OverlaySpec'none)
            (RW_OverlaySpec'vxlan rawArg_) ->
                (OverlaySpec'vxlan <$> (C.parse rawArg_))
            (RW_OverlaySpec'unknown' tag_) ->
                (Std_.pure (OverlaySpec'unknown' tag_))
        )
instance (C.Marshal (GH.Which OverlaySpec) (C.Parsed (GH.Which OverlaySpec))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (OverlaySpec'none) ->
            (GH.encodeVariant #none () (GH.unionStruct raw_))
        (OverlaySpec'vxlan arg_) ->
            (GH.encodeVariant #vxlan arg_ (GH.unionStruct raw_))
        (OverlaySpec'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data VxlanSpec 
type instance (R.ReprFor VxlanSpec) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VxlanSpec) where
    typeId  = 14439075868054381263
instance (C.TypedStruct VxlanSpec) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate VxlanSpec) where
    type AllocHint VxlanSpec = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VxlanSpec (C.Parsed VxlanSpec))
instance (C.AllocateList VxlanSpec) where
    type ListAllocHint VxlanSpec = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VxlanSpec (C.Parsed VxlanSpec))
data instance C.Parsed VxlanSpec
    = VxlanSpec 
        {vni :: (RP.Parsed Std_.Word32)
        ,localIp :: (RP.Parsed Basics.Text)
        ,peerIps :: (RP.Parsed (R.List Basics.Text))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VxlanSpec))
deriving instance (Std_.Eq (C.Parsed VxlanSpec))
instance (C.Parse VxlanSpec (C.Parsed VxlanSpec)) where
    parse raw_ = (VxlanSpec <$> (GH.parseField #vni raw_)
                            <*> (GH.parseField #localIp raw_)
                            <*> (GH.parseField #peerIps raw_))
instance (C.Marshal VxlanSpec (C.Parsed VxlanSpec)) where
    marshalInto raw_ VxlanSpec{..} = (do
        (GH.encodeField #vni vni raw_)
        (GH.encodeField #localIp localIp raw_)
        (GH.encodeField #peerIps peerIps raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vni" GH.Slot VxlanSpec Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "localIp" GH.Slot VxlanSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "peerIps" GH.Slot VxlanSpec (R.List Basics.Text)) where
    fieldByLabel  = (GH.ptrField 1)
data NetworkInfo 
type instance (R.ReprFor NetworkInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetworkInfo) where
    typeId  = 12594996834366898856
instance (C.TypedStruct NetworkInfo) where
    numStructWords  = 1
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
        {spec :: (RP.Parsed NetworkSpec)
        ,upState :: (RP.Parsed Basics.Text)
        ,dnsmasqPid :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetworkInfo))
deriving instance (Std_.Eq (C.Parsed NetworkInfo))
instance (C.Parse NetworkInfo (C.Parsed NetworkInfo)) where
    parse raw_ = (NetworkInfo <$> (GH.parseField #spec raw_)
                              <*> (GH.parseField #upState raw_)
                              <*> (GH.parseField #dnsmasqPid raw_))
instance (C.Marshal NetworkInfo (C.Parsed NetworkInfo)) where
    marshalInto raw_ NetworkInfo{..} = (do
        (GH.encodeField #spec spec raw_)
        (GH.encodeField #upState upState raw_)
        (GH.encodeField #dnsmasqPid dnsmasqPid raw_)
        (Std_.pure ())
        )
instance (GH.HasField "spec" GH.Slot NetworkInfo NetworkSpec) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "upState" GH.Slot NetworkInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "dnsmasqPid" GH.Slot NetworkInfo Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
data TapSpec 
type instance (R.ReprFor TapSpec) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TapSpec) where
    typeId  = 11999169455183606776
instance (C.TypedStruct TapSpec) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate TapSpec) where
    type AllocHint TapSpec = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TapSpec (C.Parsed TapSpec))
instance (C.AllocateList TapSpec) where
    type ListAllocHint TapSpec = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TapSpec (C.Parsed TapSpec))
data instance C.Parsed TapSpec
    = TapSpec 
        {name :: (RP.Parsed Basics.Text)
        ,bridge :: (RP.Parsed Basics.Text)
        ,uid :: (RP.Parsed Std_.Word32)
        ,gid :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TapSpec))
deriving instance (Std_.Eq (C.Parsed TapSpec))
instance (C.Parse TapSpec (C.Parsed TapSpec)) where
    parse raw_ = (TapSpec <$> (GH.parseField #name raw_)
                          <*> (GH.parseField #bridge raw_)
                          <*> (GH.parseField #uid raw_)
                          <*> (GH.parseField #gid raw_))
instance (C.Marshal TapSpec (C.Parsed TapSpec)) where
    marshalInto raw_ TapSpec{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #bridge bridge raw_)
        (GH.encodeField #uid uid raw_)
        (GH.encodeField #gid gid raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot TapSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "bridge" GH.Slot TapSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "uid" GH.Slot TapSpec Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "gid" GH.Slot TapSpec Std_.Word32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
data TapInfo 
type instance (R.ReprFor TapInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TapInfo) where
    typeId  = 11740084065335998076
instance (C.TypedStruct TapInfo) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate TapInfo) where
    type AllocHint TapInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TapInfo (C.Parsed TapInfo))
instance (C.AllocateList TapInfo) where
    type ListAllocHint TapInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TapInfo (C.Parsed TapInfo))
data instance C.Parsed TapInfo
    = TapInfo 
        {spec :: (RP.Parsed TapSpec)
        ,upState :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TapInfo))
deriving instance (Std_.Eq (C.Parsed TapInfo))
instance (C.Parse TapInfo (C.Parsed TapInfo)) where
    parse raw_ = (TapInfo <$> (GH.parseField #spec raw_)
                          <*> (GH.parseField #upState raw_))
instance (C.Marshal TapInfo (C.Parsed TapInfo)) where
    marshalInto raw_ TapInfo{..} = (do
        (GH.encodeField #spec spec raw_)
        (GH.encodeField #upState upState raw_)
        (Std_.pure ())
        )
instance (GH.HasField "spec" GH.Slot TapInfo TapSpec) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "upState" GH.Slot TapInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
data EventSink 
type instance (R.ReprFor EventSink) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId EventSink) where
    typeId  = 10926041113407843623
instance (C.Parse EventSink (GH.Client EventSink)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export EventSink) where
    type Server EventSink = EventSink'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(EventSink)) [(GH.toUntypedMethodHandler ((eventSink'onResourceVanished) s_))] [])
class (EventSink'server_ s_) where
    {-# MINIMAL eventSink'onResourceVanished #-}
    eventSink'onResourceVanished :: s_ -> (GH.MethodHandler EventSink'onResourceVanished'params EventSink'onResourceVanished'results)
    eventSink'onResourceVanished _ = GH.methodUnimplemented
instance (GH.HasMethod "onResourceVanished" EventSink EventSink'onResourceVanished'params EventSink'onResourceVanished'results) where
    methodByLabel  = (GH.Method 10926041113407843623 0)
data EventSink'onResourceVanished'params 
type instance (R.ReprFor EventSink'onResourceVanished'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId EventSink'onResourceVanished'params) where
    typeId  = 11510088759556361347
instance (C.TypedStruct EventSink'onResourceVanished'params) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate EventSink'onResourceVanished'params) where
    type AllocHint EventSink'onResourceVanished'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc EventSink'onResourceVanished'params (C.Parsed EventSink'onResourceVanished'params))
instance (C.AllocateList EventSink'onResourceVanished'params) where
    type ListAllocHint EventSink'onResourceVanished'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc EventSink'onResourceVanished'params (C.Parsed EventSink'onResourceVanished'params))
data instance C.Parsed EventSink'onResourceVanished'params
    = EventSink'onResourceVanished'params 
        {kind :: (RP.Parsed Basics.Text)
        ,name :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed EventSink'onResourceVanished'params))
deriving instance (Std_.Eq (C.Parsed EventSink'onResourceVanished'params))
instance (C.Parse EventSink'onResourceVanished'params (C.Parsed EventSink'onResourceVanished'params)) where
    parse raw_ = (EventSink'onResourceVanished'params <$> (GH.parseField #kind raw_)
                                                      <*> (GH.parseField #name raw_))
instance (C.Marshal EventSink'onResourceVanished'params (C.Parsed EventSink'onResourceVanished'params)) where
    marshalInto raw_ EventSink'onResourceVanished'params{..} = (do
        (GH.encodeField #kind kind raw_)
        (GH.encodeField #name name raw_)
        (Std_.pure ())
        )
instance (GH.HasField "kind" GH.Slot EventSink'onResourceVanished'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "name" GH.Slot EventSink'onResourceVanished'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
data EventSink'onResourceVanished'results 
type instance (R.ReprFor EventSink'onResourceVanished'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId EventSink'onResourceVanished'results) where
    typeId  = 12628124038435489706
instance (C.TypedStruct EventSink'onResourceVanished'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate EventSink'onResourceVanished'results) where
    type AllocHint EventSink'onResourceVanished'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc EventSink'onResourceVanished'results (C.Parsed EventSink'onResourceVanished'results))
instance (C.AllocateList EventSink'onResourceVanished'results) where
    type ListAllocHint EventSink'onResourceVanished'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc EventSink'onResourceVanished'results (C.Parsed EventSink'onResourceVanished'results))
data instance C.Parsed EventSink'onResourceVanished'results
    = EventSink'onResourceVanished'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed EventSink'onResourceVanished'results))
deriving instance (Std_.Eq (C.Parsed EventSink'onResourceVanished'results))
instance (C.Parse EventSink'onResourceVanished'results (C.Parsed EventSink'onResourceVanished'results)) where
    parse raw_ = (Std_.pure EventSink'onResourceVanished'results)
instance (C.Marshal EventSink'onResourceVanished'results (C.Parsed EventSink'onResourceVanished'results)) where
    marshalInto _raw (EventSink'onResourceVanished'results) = (Std_.pure ())
data NetFamily 
    = NetFamily'v4 
    | NetFamily'v6 
    | NetFamily'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor NetFamily) = (R.Data R.Sz16)
instance (C.HasTypeId NetFamily) where
    typeId  = 9589428430245918457
instance (Std_.Enum NetFamily) where
    toEnum n_ = case n_ of
        0 ->
            NetFamily'v4
        1 ->
            NetFamily'v6
        tag_ ->
            (NetFamily'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (NetFamily'v4) ->
            0
        (NetFamily'v6) ->
            1
        (NetFamily'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord NetFamily) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse NetFamily NetFamily) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList NetFamily) where
    type ListAllocHint NetFamily = Std_.Int
instance (C.EstimateListAlloc NetFamily NetFamily)