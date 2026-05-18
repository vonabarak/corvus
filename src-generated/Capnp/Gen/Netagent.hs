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
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Session)) [(GH.toUntypedMethodHandler ((session'createBridge) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'listBridges) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'claimBridge) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'createTap) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'claimTap) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'setIpForwarding) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'installNat) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'startDnsmasq) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'subscribeEvents) s_))] [])
class (Session'server_ s_) where
    {-# MINIMAL session'createBridge,session'listBridges,session'claimBridge,session'createTap,session'claimTap,session'setIpForwarding,session'installNat,session'startDnsmasq,session'subscribeEvents #-}
    session'createBridge :: s_ -> (GH.MethodHandler Session'createBridge'params Session'createBridge'results)
    session'createBridge _ = GH.methodUnimplemented
    session'listBridges :: s_ -> (GH.MethodHandler Session'listBridges'params Session'listBridges'results)
    session'listBridges _ = GH.methodUnimplemented
    session'claimBridge :: s_ -> (GH.MethodHandler Session'claimBridge'params Session'claimBridge'results)
    session'claimBridge _ = GH.methodUnimplemented
    session'createTap :: s_ -> (GH.MethodHandler Session'createTap'params Session'createTap'results)
    session'createTap _ = GH.methodUnimplemented
    session'claimTap :: s_ -> (GH.MethodHandler Session'claimTap'params Session'claimTap'results)
    session'claimTap _ = GH.methodUnimplemented
    session'setIpForwarding :: s_ -> (GH.MethodHandler Session'setIpForwarding'params Session'setIpForwarding'results)
    session'setIpForwarding _ = GH.methodUnimplemented
    session'installNat :: s_ -> (GH.MethodHandler Session'installNat'params Session'installNat'results)
    session'installNat _ = GH.methodUnimplemented
    session'startDnsmasq :: s_ -> (GH.MethodHandler Session'startDnsmasq'params Session'startDnsmasq'results)
    session'startDnsmasq _ = GH.methodUnimplemented
    session'subscribeEvents :: s_ -> (GH.MethodHandler Session'subscribeEvents'params Session'subscribeEvents'results)
    session'subscribeEvents _ = GH.methodUnimplemented
instance (GH.HasMethod "createBridge" Session Session'createBridge'params Session'createBridge'results) where
    methodByLabel  = (GH.Method 18377821789121950725 0)
instance (GH.HasMethod "listBridges" Session Session'listBridges'params Session'listBridges'results) where
    methodByLabel  = (GH.Method 18377821789121950725 1)
instance (GH.HasMethod "claimBridge" Session Session'claimBridge'params Session'claimBridge'results) where
    methodByLabel  = (GH.Method 18377821789121950725 2)
instance (GH.HasMethod "createTap" Session Session'createTap'params Session'createTap'results) where
    methodByLabel  = (GH.Method 18377821789121950725 3)
instance (GH.HasMethod "claimTap" Session Session'claimTap'params Session'claimTap'results) where
    methodByLabel  = (GH.Method 18377821789121950725 4)
instance (GH.HasMethod "setIpForwarding" Session Session'setIpForwarding'params Session'setIpForwarding'results) where
    methodByLabel  = (GH.Method 18377821789121950725 5)
instance (GH.HasMethod "installNat" Session Session'installNat'params Session'installNat'results) where
    methodByLabel  = (GH.Method 18377821789121950725 6)
instance (GH.HasMethod "startDnsmasq" Session Session'startDnsmasq'params Session'startDnsmasq'results) where
    methodByLabel  = (GH.Method 18377821789121950725 7)
instance (GH.HasMethod "subscribeEvents" Session Session'subscribeEvents'params Session'subscribeEvents'results) where
    methodByLabel  = (GH.Method 18377821789121950725 8)
data Session'createBridge'params 
type instance (R.ReprFor Session'createBridge'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'createBridge'params) where
    typeId  = 16009969733571045190
instance (C.TypedStruct Session'createBridge'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'createBridge'params) where
    type AllocHint Session'createBridge'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'createBridge'params (C.Parsed Session'createBridge'params))
instance (C.AllocateList Session'createBridge'params) where
    type ListAllocHint Session'createBridge'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'createBridge'params (C.Parsed Session'createBridge'params))
data instance C.Parsed Session'createBridge'params
    = Session'createBridge'params 
        {params :: (RP.Parsed BridgeParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'createBridge'params))
deriving instance (Std_.Eq (C.Parsed Session'createBridge'params))
instance (C.Parse Session'createBridge'params (C.Parsed Session'createBridge'params)) where
    parse raw_ = (Session'createBridge'params <$> (GH.parseField #params raw_))
instance (C.Marshal Session'createBridge'params (C.Parsed Session'createBridge'params)) where
    marshalInto raw_ Session'createBridge'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot Session'createBridge'params BridgeParams) where
    fieldByLabel  = (GH.ptrField 0)
data Session'createBridge'results 
type instance (R.ReprFor Session'createBridge'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'createBridge'results) where
    typeId  = 12952689855789234858
instance (C.TypedStruct Session'createBridge'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'createBridge'results) where
    type AllocHint Session'createBridge'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'createBridge'results (C.Parsed Session'createBridge'results))
instance (C.AllocateList Session'createBridge'results) where
    type ListAllocHint Session'createBridge'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'createBridge'results (C.Parsed Session'createBridge'results))
data instance C.Parsed Session'createBridge'results
    = Session'createBridge'results 
        {bridge :: (RP.Parsed Bridge)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'createBridge'results))
deriving instance (Std_.Eq (C.Parsed Session'createBridge'results))
instance (C.Parse Session'createBridge'results (C.Parsed Session'createBridge'results)) where
    parse raw_ = (Session'createBridge'results <$> (GH.parseField #bridge raw_))
instance (C.Marshal Session'createBridge'results (C.Parsed Session'createBridge'results)) where
    marshalInto raw_ Session'createBridge'results{..} = (do
        (GH.encodeField #bridge bridge raw_)
        (Std_.pure ())
        )
instance (GH.HasField "bridge" GH.Slot Session'createBridge'results Bridge) where
    fieldByLabel  = (GH.ptrField 0)
data Session'listBridges'params 
type instance (R.ReprFor Session'listBridges'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'listBridges'params) where
    typeId  = 16248932357397564441
instance (C.TypedStruct Session'listBridges'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'listBridges'params) where
    type AllocHint Session'listBridges'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'listBridges'params (C.Parsed Session'listBridges'params))
instance (C.AllocateList Session'listBridges'params) where
    type ListAllocHint Session'listBridges'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'listBridges'params (C.Parsed Session'listBridges'params))
data instance C.Parsed Session'listBridges'params
    = Session'listBridges'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'listBridges'params))
deriving instance (Std_.Eq (C.Parsed Session'listBridges'params))
instance (C.Parse Session'listBridges'params (C.Parsed Session'listBridges'params)) where
    parse raw_ = (Std_.pure Session'listBridges'params)
instance (C.Marshal Session'listBridges'params (C.Parsed Session'listBridges'params)) where
    marshalInto _raw (Session'listBridges'params) = (Std_.pure ())
data Session'listBridges'results 
type instance (R.ReprFor Session'listBridges'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'listBridges'results) where
    typeId  = 10897823512533511254
instance (C.TypedStruct Session'listBridges'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'listBridges'results) where
    type AllocHint Session'listBridges'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'listBridges'results (C.Parsed Session'listBridges'results))
instance (C.AllocateList Session'listBridges'results) where
    type ListAllocHint Session'listBridges'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'listBridges'results (C.Parsed Session'listBridges'results))
data instance C.Parsed Session'listBridges'results
    = Session'listBridges'results 
        {bridges :: (RP.Parsed (R.List BridgeInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'listBridges'results))
deriving instance (Std_.Eq (C.Parsed Session'listBridges'results))
instance (C.Parse Session'listBridges'results (C.Parsed Session'listBridges'results)) where
    parse raw_ = (Session'listBridges'results <$> (GH.parseField #bridges raw_))
instance (C.Marshal Session'listBridges'results (C.Parsed Session'listBridges'results)) where
    marshalInto raw_ Session'listBridges'results{..} = (do
        (GH.encodeField #bridges bridges raw_)
        (Std_.pure ())
        )
instance (GH.HasField "bridges" GH.Slot Session'listBridges'results (R.List BridgeInfo)) where
    fieldByLabel  = (GH.ptrField 0)
data Session'claimBridge'params 
type instance (R.ReprFor Session'claimBridge'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'claimBridge'params) where
    typeId  = 10597725040083098732
instance (C.TypedStruct Session'claimBridge'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'claimBridge'params) where
    type AllocHint Session'claimBridge'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'claimBridge'params (C.Parsed Session'claimBridge'params))
instance (C.AllocateList Session'claimBridge'params) where
    type ListAllocHint Session'claimBridge'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'claimBridge'params (C.Parsed Session'claimBridge'params))
data instance C.Parsed Session'claimBridge'params
    = Session'claimBridge'params 
        {name :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'claimBridge'params))
deriving instance (Std_.Eq (C.Parsed Session'claimBridge'params))
instance (C.Parse Session'claimBridge'params (C.Parsed Session'claimBridge'params)) where
    parse raw_ = (Session'claimBridge'params <$> (GH.parseField #name raw_))
instance (C.Marshal Session'claimBridge'params (C.Parsed Session'claimBridge'params)) where
    marshalInto raw_ Session'claimBridge'params{..} = (do
        (GH.encodeField #name name raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot Session'claimBridge'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Session'claimBridge'results 
type instance (R.ReprFor Session'claimBridge'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'claimBridge'results) where
    typeId  = 13310718494801309865
instance (C.TypedStruct Session'claimBridge'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'claimBridge'results) where
    type AllocHint Session'claimBridge'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'claimBridge'results (C.Parsed Session'claimBridge'results))
instance (C.AllocateList Session'claimBridge'results) where
    type ListAllocHint Session'claimBridge'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'claimBridge'results (C.Parsed Session'claimBridge'results))
data instance C.Parsed Session'claimBridge'results
    = Session'claimBridge'results 
        {bridge :: (RP.Parsed Bridge)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'claimBridge'results))
deriving instance (Std_.Eq (C.Parsed Session'claimBridge'results))
instance (C.Parse Session'claimBridge'results (C.Parsed Session'claimBridge'results)) where
    parse raw_ = (Session'claimBridge'results <$> (GH.parseField #bridge raw_))
instance (C.Marshal Session'claimBridge'results (C.Parsed Session'claimBridge'results)) where
    marshalInto raw_ Session'claimBridge'results{..} = (do
        (GH.encodeField #bridge bridge raw_)
        (Std_.pure ())
        )
instance (GH.HasField "bridge" GH.Slot Session'claimBridge'results Bridge) where
    fieldByLabel  = (GH.ptrField 0)
data Session'createTap'params 
type instance (R.ReprFor Session'createTap'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'createTap'params) where
    typeId  = 17066202627105760396
instance (C.TypedStruct Session'createTap'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'createTap'params) where
    type AllocHint Session'createTap'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'createTap'params (C.Parsed Session'createTap'params))
instance (C.AllocateList Session'createTap'params) where
    type ListAllocHint Session'createTap'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'createTap'params (C.Parsed Session'createTap'params))
data instance C.Parsed Session'createTap'params
    = Session'createTap'params 
        {params :: (RP.Parsed TapParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'createTap'params))
deriving instance (Std_.Eq (C.Parsed Session'createTap'params))
instance (C.Parse Session'createTap'params (C.Parsed Session'createTap'params)) where
    parse raw_ = (Session'createTap'params <$> (GH.parseField #params raw_))
instance (C.Marshal Session'createTap'params (C.Parsed Session'createTap'params)) where
    marshalInto raw_ Session'createTap'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot Session'createTap'params TapParams) where
    fieldByLabel  = (GH.ptrField 0)
data Session'createTap'results 
type instance (R.ReprFor Session'createTap'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'createTap'results) where
    typeId  = 13926779387823511762
instance (C.TypedStruct Session'createTap'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'createTap'results) where
    type AllocHint Session'createTap'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'createTap'results (C.Parsed Session'createTap'results))
instance (C.AllocateList Session'createTap'results) where
    type ListAllocHint Session'createTap'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'createTap'results (C.Parsed Session'createTap'results))
data instance C.Parsed Session'createTap'results
    = Session'createTap'results 
        {tap :: (RP.Parsed Tap)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'createTap'results))
deriving instance (Std_.Eq (C.Parsed Session'createTap'results))
instance (C.Parse Session'createTap'results (C.Parsed Session'createTap'results)) where
    parse raw_ = (Session'createTap'results <$> (GH.parseField #tap raw_))
instance (C.Marshal Session'createTap'results (C.Parsed Session'createTap'results)) where
    marshalInto raw_ Session'createTap'results{..} = (do
        (GH.encodeField #tap tap raw_)
        (Std_.pure ())
        )
instance (GH.HasField "tap" GH.Slot Session'createTap'results Tap) where
    fieldByLabel  = (GH.ptrField 0)
data Session'claimTap'params 
type instance (R.ReprFor Session'claimTap'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'claimTap'params) where
    typeId  = 18419340564866344367
instance (C.TypedStruct Session'claimTap'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'claimTap'params) where
    type AllocHint Session'claimTap'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'claimTap'params (C.Parsed Session'claimTap'params))
instance (C.AllocateList Session'claimTap'params) where
    type ListAllocHint Session'claimTap'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'claimTap'params (C.Parsed Session'claimTap'params))
data instance C.Parsed Session'claimTap'params
    = Session'claimTap'params 
        {name :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'claimTap'params))
deriving instance (Std_.Eq (C.Parsed Session'claimTap'params))
instance (C.Parse Session'claimTap'params (C.Parsed Session'claimTap'params)) where
    parse raw_ = (Session'claimTap'params <$> (GH.parseField #name raw_))
instance (C.Marshal Session'claimTap'params (C.Parsed Session'claimTap'params)) where
    marshalInto raw_ Session'claimTap'params{..} = (do
        (GH.encodeField #name name raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot Session'claimTap'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Session'claimTap'results 
type instance (R.ReprFor Session'claimTap'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'claimTap'results) where
    typeId  = 10880813484077667812
instance (C.TypedStruct Session'claimTap'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'claimTap'results) where
    type AllocHint Session'claimTap'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'claimTap'results (C.Parsed Session'claimTap'results))
instance (C.AllocateList Session'claimTap'results) where
    type ListAllocHint Session'claimTap'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'claimTap'results (C.Parsed Session'claimTap'results))
data instance C.Parsed Session'claimTap'results
    = Session'claimTap'results 
        {tap :: (RP.Parsed Tap)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'claimTap'results))
deriving instance (Std_.Eq (C.Parsed Session'claimTap'results))
instance (C.Parse Session'claimTap'results (C.Parsed Session'claimTap'results)) where
    parse raw_ = (Session'claimTap'results <$> (GH.parseField #tap raw_))
instance (C.Marshal Session'claimTap'results (C.Parsed Session'claimTap'results)) where
    marshalInto raw_ Session'claimTap'results{..} = (do
        (GH.encodeField #tap tap raw_)
        (Std_.pure ())
        )
instance (GH.HasField "tap" GH.Slot Session'claimTap'results Tap) where
    fieldByLabel  = (GH.ptrField 0)
data Session'setIpForwarding'params 
type instance (R.ReprFor Session'setIpForwarding'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'setIpForwarding'params) where
    typeId  = 18343140468287461106
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
    typeId  = 18208164700962459784
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
data Session'installNat'params 
type instance (R.ReprFor Session'installNat'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'installNat'params) where
    typeId  = 10547781726648321946
instance (C.TypedStruct Session'installNat'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'installNat'params) where
    type AllocHint Session'installNat'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'installNat'params (C.Parsed Session'installNat'params))
instance (C.AllocateList Session'installNat'params) where
    type ListAllocHint Session'installNat'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'installNat'params (C.Parsed Session'installNat'params))
data instance C.Parsed Session'installNat'params
    = Session'installNat'params 
        {params :: (RP.Parsed NatParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'installNat'params))
deriving instance (Std_.Eq (C.Parsed Session'installNat'params))
instance (C.Parse Session'installNat'params (C.Parsed Session'installNat'params)) where
    parse raw_ = (Session'installNat'params <$> (GH.parseField #params raw_))
instance (C.Marshal Session'installNat'params (C.Parsed Session'installNat'params)) where
    marshalInto raw_ Session'installNat'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot Session'installNat'params NatParams) where
    fieldByLabel  = (GH.ptrField 0)
data Session'installNat'results 
type instance (R.ReprFor Session'installNat'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'installNat'results) where
    typeId  = 15710458737355548640
instance (C.TypedStruct Session'installNat'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'installNat'results) where
    type AllocHint Session'installNat'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'installNat'results (C.Parsed Session'installNat'results))
instance (C.AllocateList Session'installNat'results) where
    type ListAllocHint Session'installNat'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'installNat'results (C.Parsed Session'installNat'results))
data instance C.Parsed Session'installNat'results
    = Session'installNat'results 
        {nat :: (RP.Parsed NatRule)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'installNat'results))
deriving instance (Std_.Eq (C.Parsed Session'installNat'results))
instance (C.Parse Session'installNat'results (C.Parsed Session'installNat'results)) where
    parse raw_ = (Session'installNat'results <$> (GH.parseField #nat raw_))
instance (C.Marshal Session'installNat'results (C.Parsed Session'installNat'results)) where
    marshalInto raw_ Session'installNat'results{..} = (do
        (GH.encodeField #nat nat raw_)
        (Std_.pure ())
        )
instance (GH.HasField "nat" GH.Slot Session'installNat'results NatRule) where
    fieldByLabel  = (GH.ptrField 0)
data Session'startDnsmasq'params 
type instance (R.ReprFor Session'startDnsmasq'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'startDnsmasq'params) where
    typeId  = 9224720994102743237
instance (C.TypedStruct Session'startDnsmasq'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'startDnsmasq'params) where
    type AllocHint Session'startDnsmasq'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'startDnsmasq'params (C.Parsed Session'startDnsmasq'params))
instance (C.AllocateList Session'startDnsmasq'params) where
    type ListAllocHint Session'startDnsmasq'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'startDnsmasq'params (C.Parsed Session'startDnsmasq'params))
data instance C.Parsed Session'startDnsmasq'params
    = Session'startDnsmasq'params 
        {params :: (RP.Parsed DnsmasqParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'startDnsmasq'params))
deriving instance (Std_.Eq (C.Parsed Session'startDnsmasq'params))
instance (C.Parse Session'startDnsmasq'params (C.Parsed Session'startDnsmasq'params)) where
    parse raw_ = (Session'startDnsmasq'params <$> (GH.parseField #params raw_))
instance (C.Marshal Session'startDnsmasq'params (C.Parsed Session'startDnsmasq'params)) where
    marshalInto raw_ Session'startDnsmasq'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot Session'startDnsmasq'params DnsmasqParams) where
    fieldByLabel  = (GH.ptrField 0)
data Session'startDnsmasq'results 
type instance (R.ReprFor Session'startDnsmasq'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'startDnsmasq'results) where
    typeId  = 14771270395976879847
instance (C.TypedStruct Session'startDnsmasq'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'startDnsmasq'results) where
    type AllocHint Session'startDnsmasq'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'startDnsmasq'results (C.Parsed Session'startDnsmasq'results))
instance (C.AllocateList Session'startDnsmasq'results) where
    type ListAllocHint Session'startDnsmasq'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'startDnsmasq'results (C.Parsed Session'startDnsmasq'results))
data instance C.Parsed Session'startDnsmasq'results
    = Session'startDnsmasq'results 
        {server :: (RP.Parsed DnsmasqHandle)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'startDnsmasq'results))
deriving instance (Std_.Eq (C.Parsed Session'startDnsmasq'results))
instance (C.Parse Session'startDnsmasq'results (C.Parsed Session'startDnsmasq'results)) where
    parse raw_ = (Session'startDnsmasq'results <$> (GH.parseField #server raw_))
instance (C.Marshal Session'startDnsmasq'results (C.Parsed Session'startDnsmasq'results)) where
    marshalInto raw_ Session'startDnsmasq'results{..} = (do
        (GH.encodeField #server server raw_)
        (Std_.pure ())
        )
instance (GH.HasField "server" GH.Slot Session'startDnsmasq'results DnsmasqHandle) where
    fieldByLabel  = (GH.ptrField 0)
data Session'subscribeEvents'params 
type instance (R.ReprFor Session'subscribeEvents'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'subscribeEvents'params) where
    typeId  = 13520912288573234711
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
    typeId  = 17373170811596722340
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
data Bridge 
type instance (R.ReprFor Bridge) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Bridge) where
    typeId  = 15228823294429127710
instance (C.Parse Bridge (GH.Client Bridge)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Bridge) where
    type Server Bridge = Bridge'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Bridge)) [(GH.toUntypedMethodHandler ((bridge'info) s_))
                                                                        ,(GH.toUntypedMethodHandler ((bridge'attachTap) s_))
                                                                        ,(GH.toUntypedMethodHandler ((bridge'detachTap) s_))
                                                                        ,(GH.toUntypedMethodHandler ((bridge'destroy) s_))] [])
class (Bridge'server_ s_) where
    {-# MINIMAL bridge'info,bridge'attachTap,bridge'detachTap,bridge'destroy #-}
    bridge'info :: s_ -> (GH.MethodHandler Bridge'info'params Bridge'info'results)
    bridge'info _ = GH.methodUnimplemented
    bridge'attachTap :: s_ -> (GH.MethodHandler Bridge'attachTap'params Bridge'attachTap'results)
    bridge'attachTap _ = GH.methodUnimplemented
    bridge'detachTap :: s_ -> (GH.MethodHandler Bridge'detachTap'params Bridge'detachTap'results)
    bridge'detachTap _ = GH.methodUnimplemented
    bridge'destroy :: s_ -> (GH.MethodHandler Bridge'destroy'params Bridge'destroy'results)
    bridge'destroy _ = GH.methodUnimplemented
instance (GH.HasMethod "info" Bridge Bridge'info'params Bridge'info'results) where
    methodByLabel  = (GH.Method 15228823294429127710 0)
instance (GH.HasMethod "attachTap" Bridge Bridge'attachTap'params Bridge'attachTap'results) where
    methodByLabel  = (GH.Method 15228823294429127710 1)
instance (GH.HasMethod "detachTap" Bridge Bridge'detachTap'params Bridge'detachTap'results) where
    methodByLabel  = (GH.Method 15228823294429127710 2)
instance (GH.HasMethod "destroy" Bridge Bridge'destroy'params Bridge'destroy'results) where
    methodByLabel  = (GH.Method 15228823294429127710 3)
data Bridge'info'params 
type instance (R.ReprFor Bridge'info'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Bridge'info'params) where
    typeId  = 14121883518414666957
instance (C.TypedStruct Bridge'info'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Bridge'info'params) where
    type AllocHint Bridge'info'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Bridge'info'params (C.Parsed Bridge'info'params))
instance (C.AllocateList Bridge'info'params) where
    type ListAllocHint Bridge'info'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Bridge'info'params (C.Parsed Bridge'info'params))
data instance C.Parsed Bridge'info'params
    = Bridge'info'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Bridge'info'params))
deriving instance (Std_.Eq (C.Parsed Bridge'info'params))
instance (C.Parse Bridge'info'params (C.Parsed Bridge'info'params)) where
    parse raw_ = (Std_.pure Bridge'info'params)
instance (C.Marshal Bridge'info'params (C.Parsed Bridge'info'params)) where
    marshalInto _raw (Bridge'info'params) = (Std_.pure ())
data Bridge'info'results 
type instance (R.ReprFor Bridge'info'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Bridge'info'results) where
    typeId  = 10860270917571808543
instance (C.TypedStruct Bridge'info'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Bridge'info'results) where
    type AllocHint Bridge'info'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Bridge'info'results (C.Parsed Bridge'info'results))
instance (C.AllocateList Bridge'info'results) where
    type ListAllocHint Bridge'info'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Bridge'info'results (C.Parsed Bridge'info'results))
data instance C.Parsed Bridge'info'results
    = Bridge'info'results 
        {info :: (RP.Parsed BridgeInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Bridge'info'results))
deriving instance (Std_.Eq (C.Parsed Bridge'info'results))
instance (C.Parse Bridge'info'results (C.Parsed Bridge'info'results)) where
    parse raw_ = (Bridge'info'results <$> (GH.parseField #info raw_))
instance (C.Marshal Bridge'info'results (C.Parsed Bridge'info'results)) where
    marshalInto raw_ Bridge'info'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot Bridge'info'results BridgeInfo) where
    fieldByLabel  = (GH.ptrField 0)
data Bridge'attachTap'params 
type instance (R.ReprFor Bridge'attachTap'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Bridge'attachTap'params) where
    typeId  = 14220040048583197280
instance (C.TypedStruct Bridge'attachTap'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Bridge'attachTap'params) where
    type AllocHint Bridge'attachTap'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Bridge'attachTap'params (C.Parsed Bridge'attachTap'params))
instance (C.AllocateList Bridge'attachTap'params) where
    type ListAllocHint Bridge'attachTap'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Bridge'attachTap'params (C.Parsed Bridge'attachTap'params))
data instance C.Parsed Bridge'attachTap'params
    = Bridge'attachTap'params 
        {tap :: (RP.Parsed Tap)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Bridge'attachTap'params))
deriving instance (Std_.Eq (C.Parsed Bridge'attachTap'params))
instance (C.Parse Bridge'attachTap'params (C.Parsed Bridge'attachTap'params)) where
    parse raw_ = (Bridge'attachTap'params <$> (GH.parseField #tap raw_))
instance (C.Marshal Bridge'attachTap'params (C.Parsed Bridge'attachTap'params)) where
    marshalInto raw_ Bridge'attachTap'params{..} = (do
        (GH.encodeField #tap tap raw_)
        (Std_.pure ())
        )
instance (GH.HasField "tap" GH.Slot Bridge'attachTap'params Tap) where
    fieldByLabel  = (GH.ptrField 0)
data Bridge'attachTap'results 
type instance (R.ReprFor Bridge'attachTap'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Bridge'attachTap'results) where
    typeId  = 10334531946265509895
instance (C.TypedStruct Bridge'attachTap'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Bridge'attachTap'results) where
    type AllocHint Bridge'attachTap'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Bridge'attachTap'results (C.Parsed Bridge'attachTap'results))
instance (C.AllocateList Bridge'attachTap'results) where
    type ListAllocHint Bridge'attachTap'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Bridge'attachTap'results (C.Parsed Bridge'attachTap'results))
data instance C.Parsed Bridge'attachTap'results
    = Bridge'attachTap'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Bridge'attachTap'results))
deriving instance (Std_.Eq (C.Parsed Bridge'attachTap'results))
instance (C.Parse Bridge'attachTap'results (C.Parsed Bridge'attachTap'results)) where
    parse raw_ = (Std_.pure Bridge'attachTap'results)
instance (C.Marshal Bridge'attachTap'results (C.Parsed Bridge'attachTap'results)) where
    marshalInto _raw (Bridge'attachTap'results) = (Std_.pure ())
data Bridge'detachTap'params 
type instance (R.ReprFor Bridge'detachTap'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Bridge'detachTap'params) where
    typeId  = 11780084587057667658
instance (C.TypedStruct Bridge'detachTap'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Bridge'detachTap'params) where
    type AllocHint Bridge'detachTap'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Bridge'detachTap'params (C.Parsed Bridge'detachTap'params))
instance (C.AllocateList Bridge'detachTap'params) where
    type ListAllocHint Bridge'detachTap'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Bridge'detachTap'params (C.Parsed Bridge'detachTap'params))
data instance C.Parsed Bridge'detachTap'params
    = Bridge'detachTap'params 
        {tap :: (RP.Parsed Tap)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Bridge'detachTap'params))
deriving instance (Std_.Eq (C.Parsed Bridge'detachTap'params))
instance (C.Parse Bridge'detachTap'params (C.Parsed Bridge'detachTap'params)) where
    parse raw_ = (Bridge'detachTap'params <$> (GH.parseField #tap raw_))
instance (C.Marshal Bridge'detachTap'params (C.Parsed Bridge'detachTap'params)) where
    marshalInto raw_ Bridge'detachTap'params{..} = (do
        (GH.encodeField #tap tap raw_)
        (Std_.pure ())
        )
instance (GH.HasField "tap" GH.Slot Bridge'detachTap'params Tap) where
    fieldByLabel  = (GH.ptrField 0)
data Bridge'detachTap'results 
type instance (R.ReprFor Bridge'detachTap'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Bridge'detachTap'results) where
    typeId  = 17853659699247329414
instance (C.TypedStruct Bridge'detachTap'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Bridge'detachTap'results) where
    type AllocHint Bridge'detachTap'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Bridge'detachTap'results (C.Parsed Bridge'detachTap'results))
instance (C.AllocateList Bridge'detachTap'results) where
    type ListAllocHint Bridge'detachTap'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Bridge'detachTap'results (C.Parsed Bridge'detachTap'results))
data instance C.Parsed Bridge'detachTap'results
    = Bridge'detachTap'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Bridge'detachTap'results))
deriving instance (Std_.Eq (C.Parsed Bridge'detachTap'results))
instance (C.Parse Bridge'detachTap'results (C.Parsed Bridge'detachTap'results)) where
    parse raw_ = (Std_.pure Bridge'detachTap'results)
instance (C.Marshal Bridge'detachTap'results (C.Parsed Bridge'detachTap'results)) where
    marshalInto _raw (Bridge'detachTap'results) = (Std_.pure ())
data Bridge'destroy'params 
type instance (R.ReprFor Bridge'destroy'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Bridge'destroy'params) where
    typeId  = 16009212128023870215
instance (C.TypedStruct Bridge'destroy'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Bridge'destroy'params) where
    type AllocHint Bridge'destroy'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Bridge'destroy'params (C.Parsed Bridge'destroy'params))
instance (C.AllocateList Bridge'destroy'params) where
    type ListAllocHint Bridge'destroy'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Bridge'destroy'params (C.Parsed Bridge'destroy'params))
data instance C.Parsed Bridge'destroy'params
    = Bridge'destroy'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Bridge'destroy'params))
deriving instance (Std_.Eq (C.Parsed Bridge'destroy'params))
instance (C.Parse Bridge'destroy'params (C.Parsed Bridge'destroy'params)) where
    parse raw_ = (Std_.pure Bridge'destroy'params)
instance (C.Marshal Bridge'destroy'params (C.Parsed Bridge'destroy'params)) where
    marshalInto _raw (Bridge'destroy'params) = (Std_.pure ())
data Bridge'destroy'results 
type instance (R.ReprFor Bridge'destroy'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Bridge'destroy'results) where
    typeId  = 10922562760604793041
instance (C.TypedStruct Bridge'destroy'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Bridge'destroy'results) where
    type AllocHint Bridge'destroy'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Bridge'destroy'results (C.Parsed Bridge'destroy'results))
instance (C.AllocateList Bridge'destroy'results) where
    type ListAllocHint Bridge'destroy'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Bridge'destroy'results (C.Parsed Bridge'destroy'results))
data instance C.Parsed Bridge'destroy'results
    = Bridge'destroy'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Bridge'destroy'results))
deriving instance (Std_.Eq (C.Parsed Bridge'destroy'results))
instance (C.Parse Bridge'destroy'results (C.Parsed Bridge'destroy'results)) where
    parse raw_ = (Std_.pure Bridge'destroy'results)
instance (C.Marshal Bridge'destroy'results (C.Parsed Bridge'destroy'results)) where
    marshalInto _raw (Bridge'destroy'results) = (Std_.pure ())
data Tap 
type instance (R.ReprFor Tap) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Tap) where
    typeId  = 11840786045203173135
instance (C.Parse Tap (GH.Client Tap)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Tap) where
    type Server Tap = Tap'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Tap)) [(GH.toUntypedMethodHandler ((tap'info) s_))
                                                                     ,(GH.toUntypedMethodHandler ((tap'destroy) s_))] [])
class (Tap'server_ s_) where
    {-# MINIMAL tap'info,tap'destroy #-}
    tap'info :: s_ -> (GH.MethodHandler Tap'info'params Tap'info'results)
    tap'info _ = GH.methodUnimplemented
    tap'destroy :: s_ -> (GH.MethodHandler Tap'destroy'params Tap'destroy'results)
    tap'destroy _ = GH.methodUnimplemented
instance (GH.HasMethod "info" Tap Tap'info'params Tap'info'results) where
    methodByLabel  = (GH.Method 11840786045203173135 0)
instance (GH.HasMethod "destroy" Tap Tap'destroy'params Tap'destroy'results) where
    methodByLabel  = (GH.Method 11840786045203173135 1)
data Tap'info'params 
type instance (R.ReprFor Tap'info'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Tap'info'params) where
    typeId  = 10696248306423477726
instance (C.TypedStruct Tap'info'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Tap'info'params) where
    type AllocHint Tap'info'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Tap'info'params (C.Parsed Tap'info'params))
instance (C.AllocateList Tap'info'params) where
    type ListAllocHint Tap'info'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Tap'info'params (C.Parsed Tap'info'params))
data instance C.Parsed Tap'info'params
    = Tap'info'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Tap'info'params))
deriving instance (Std_.Eq (C.Parsed Tap'info'params))
instance (C.Parse Tap'info'params (C.Parsed Tap'info'params)) where
    parse raw_ = (Std_.pure Tap'info'params)
instance (C.Marshal Tap'info'params (C.Parsed Tap'info'params)) where
    marshalInto _raw (Tap'info'params) = (Std_.pure ())
data Tap'info'results 
type instance (R.ReprFor Tap'info'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Tap'info'results) where
    typeId  = 13242321853614821219
instance (C.TypedStruct Tap'info'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Tap'info'results) where
    type AllocHint Tap'info'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Tap'info'results (C.Parsed Tap'info'results))
instance (C.AllocateList Tap'info'results) where
    type ListAllocHint Tap'info'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Tap'info'results (C.Parsed Tap'info'results))
data instance C.Parsed Tap'info'results
    = Tap'info'results 
        {info :: (RP.Parsed TapInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Tap'info'results))
deriving instance (Std_.Eq (C.Parsed Tap'info'results))
instance (C.Parse Tap'info'results (C.Parsed Tap'info'results)) where
    parse raw_ = (Tap'info'results <$> (GH.parseField #info raw_))
instance (C.Marshal Tap'info'results (C.Parsed Tap'info'results)) where
    marshalInto raw_ Tap'info'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot Tap'info'results TapInfo) where
    fieldByLabel  = (GH.ptrField 0)
data Tap'destroy'params 
type instance (R.ReprFor Tap'destroy'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Tap'destroy'params) where
    typeId  = 15492858303015507365
instance (C.TypedStruct Tap'destroy'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Tap'destroy'params) where
    type AllocHint Tap'destroy'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Tap'destroy'params (C.Parsed Tap'destroy'params))
instance (C.AllocateList Tap'destroy'params) where
    type ListAllocHint Tap'destroy'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Tap'destroy'params (C.Parsed Tap'destroy'params))
data instance C.Parsed Tap'destroy'params
    = Tap'destroy'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Tap'destroy'params))
deriving instance (Std_.Eq (C.Parsed Tap'destroy'params))
instance (C.Parse Tap'destroy'params (C.Parsed Tap'destroy'params)) where
    parse raw_ = (Std_.pure Tap'destroy'params)
instance (C.Marshal Tap'destroy'params (C.Parsed Tap'destroy'params)) where
    marshalInto _raw (Tap'destroy'params) = (Std_.pure ())
data Tap'destroy'results 
type instance (R.ReprFor Tap'destroy'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Tap'destroy'results) where
    typeId  = 13664523648603993982
instance (C.TypedStruct Tap'destroy'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Tap'destroy'results) where
    type AllocHint Tap'destroy'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Tap'destroy'results (C.Parsed Tap'destroy'results))
instance (C.AllocateList Tap'destroy'results) where
    type ListAllocHint Tap'destroy'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Tap'destroy'results (C.Parsed Tap'destroy'results))
data instance C.Parsed Tap'destroy'results
    = Tap'destroy'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Tap'destroy'results))
deriving instance (Std_.Eq (C.Parsed Tap'destroy'results))
instance (C.Parse Tap'destroy'results (C.Parsed Tap'destroy'results)) where
    parse raw_ = (Std_.pure Tap'destroy'results)
instance (C.Marshal Tap'destroy'results (C.Parsed Tap'destroy'results)) where
    marshalInto _raw (Tap'destroy'results) = (Std_.pure ())
data DnsmasqHandle 
type instance (R.ReprFor DnsmasqHandle) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId DnsmasqHandle) where
    typeId  = 11629963042053483859
instance (C.Parse DnsmasqHandle (GH.Client DnsmasqHandle)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export DnsmasqHandle) where
    type Server DnsmasqHandle = DnsmasqHandle'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(DnsmasqHandle)) [(GH.toUntypedMethodHandler ((dnsmasqHandle'pid) s_))
                                                                               ,(GH.toUntypedMethodHandler ((dnsmasqHandle'stop) s_))] [])
class (DnsmasqHandle'server_ s_) where
    {-# MINIMAL dnsmasqHandle'pid,dnsmasqHandle'stop #-}
    dnsmasqHandle'pid :: s_ -> (GH.MethodHandler DnsmasqHandle'pid'params DnsmasqHandle'pid'results)
    dnsmasqHandle'pid _ = GH.methodUnimplemented
    dnsmasqHandle'stop :: s_ -> (GH.MethodHandler DnsmasqHandle'stop'params DnsmasqHandle'stop'results)
    dnsmasqHandle'stop _ = GH.methodUnimplemented
instance (GH.HasMethod "pid" DnsmasqHandle DnsmasqHandle'pid'params DnsmasqHandle'pid'results) where
    methodByLabel  = (GH.Method 11629963042053483859 0)
instance (GH.HasMethod "stop" DnsmasqHandle DnsmasqHandle'stop'params DnsmasqHandle'stop'results) where
    methodByLabel  = (GH.Method 11629963042053483859 1)
data DnsmasqHandle'pid'params 
type instance (R.ReprFor DnsmasqHandle'pid'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DnsmasqHandle'pid'params) where
    typeId  = 9337725199629036741
instance (C.TypedStruct DnsmasqHandle'pid'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate DnsmasqHandle'pid'params) where
    type AllocHint DnsmasqHandle'pid'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DnsmasqHandle'pid'params (C.Parsed DnsmasqHandle'pid'params))
instance (C.AllocateList DnsmasqHandle'pid'params) where
    type ListAllocHint DnsmasqHandle'pid'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DnsmasqHandle'pid'params (C.Parsed DnsmasqHandle'pid'params))
data instance C.Parsed DnsmasqHandle'pid'params
    = DnsmasqHandle'pid'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DnsmasqHandle'pid'params))
deriving instance (Std_.Eq (C.Parsed DnsmasqHandle'pid'params))
instance (C.Parse DnsmasqHandle'pid'params (C.Parsed DnsmasqHandle'pid'params)) where
    parse raw_ = (Std_.pure DnsmasqHandle'pid'params)
instance (C.Marshal DnsmasqHandle'pid'params (C.Parsed DnsmasqHandle'pid'params)) where
    marshalInto _raw (DnsmasqHandle'pid'params) = (Std_.pure ())
data DnsmasqHandle'pid'results 
type instance (R.ReprFor DnsmasqHandle'pid'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DnsmasqHandle'pid'results) where
    typeId  = 13764737362056936449
instance (C.TypedStruct DnsmasqHandle'pid'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate DnsmasqHandle'pid'results) where
    type AllocHint DnsmasqHandle'pid'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DnsmasqHandle'pid'results (C.Parsed DnsmasqHandle'pid'results))
instance (C.AllocateList DnsmasqHandle'pid'results) where
    type ListAllocHint DnsmasqHandle'pid'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DnsmasqHandle'pid'results (C.Parsed DnsmasqHandle'pid'results))
data instance C.Parsed DnsmasqHandle'pid'results
    = DnsmasqHandle'pid'results 
        {pid :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DnsmasqHandle'pid'results))
deriving instance (Std_.Eq (C.Parsed DnsmasqHandle'pid'results))
instance (C.Parse DnsmasqHandle'pid'results (C.Parsed DnsmasqHandle'pid'results)) where
    parse raw_ = (DnsmasqHandle'pid'results <$> (GH.parseField #pid raw_))
instance (C.Marshal DnsmasqHandle'pid'results (C.Parsed DnsmasqHandle'pid'results)) where
    marshalInto raw_ DnsmasqHandle'pid'results{..} = (do
        (GH.encodeField #pid pid raw_)
        (Std_.pure ())
        )
instance (GH.HasField "pid" GH.Slot DnsmasqHandle'pid'results Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
data DnsmasqHandle'stop'params 
type instance (R.ReprFor DnsmasqHandle'stop'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DnsmasqHandle'stop'params) where
    typeId  = 17940238300929128601
instance (C.TypedStruct DnsmasqHandle'stop'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate DnsmasqHandle'stop'params) where
    type AllocHint DnsmasqHandle'stop'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DnsmasqHandle'stop'params (C.Parsed DnsmasqHandle'stop'params))
instance (C.AllocateList DnsmasqHandle'stop'params) where
    type ListAllocHint DnsmasqHandle'stop'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DnsmasqHandle'stop'params (C.Parsed DnsmasqHandle'stop'params))
data instance C.Parsed DnsmasqHandle'stop'params
    = DnsmasqHandle'stop'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DnsmasqHandle'stop'params))
deriving instance (Std_.Eq (C.Parsed DnsmasqHandle'stop'params))
instance (C.Parse DnsmasqHandle'stop'params (C.Parsed DnsmasqHandle'stop'params)) where
    parse raw_ = (Std_.pure DnsmasqHandle'stop'params)
instance (C.Marshal DnsmasqHandle'stop'params (C.Parsed DnsmasqHandle'stop'params)) where
    marshalInto _raw (DnsmasqHandle'stop'params) = (Std_.pure ())
data DnsmasqHandle'stop'results 
type instance (R.ReprFor DnsmasqHandle'stop'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DnsmasqHandle'stop'results) where
    typeId  = 15249830764499247254
instance (C.TypedStruct DnsmasqHandle'stop'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate DnsmasqHandle'stop'results) where
    type AllocHint DnsmasqHandle'stop'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DnsmasqHandle'stop'results (C.Parsed DnsmasqHandle'stop'results))
instance (C.AllocateList DnsmasqHandle'stop'results) where
    type ListAllocHint DnsmasqHandle'stop'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DnsmasqHandle'stop'results (C.Parsed DnsmasqHandle'stop'results))
data instance C.Parsed DnsmasqHandle'stop'results
    = DnsmasqHandle'stop'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DnsmasqHandle'stop'results))
deriving instance (Std_.Eq (C.Parsed DnsmasqHandle'stop'results))
instance (C.Parse DnsmasqHandle'stop'results (C.Parsed DnsmasqHandle'stop'results)) where
    parse raw_ = (Std_.pure DnsmasqHandle'stop'results)
instance (C.Marshal DnsmasqHandle'stop'results (C.Parsed DnsmasqHandle'stop'results)) where
    marshalInto _raw (DnsmasqHandle'stop'results) = (Std_.pure ())
data NatRule 
type instance (R.ReprFor NatRule) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId NatRule) where
    typeId  = 12678499534413776546
instance (C.Parse NatRule (GH.Client NatRule)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export NatRule) where
    type Server NatRule = NatRule'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(NatRule)) [(GH.toUntypedMethodHandler ((natRule'destroy) s_))] [])
class (NatRule'server_ s_) where
    {-# MINIMAL natRule'destroy #-}
    natRule'destroy :: s_ -> (GH.MethodHandler NatRule'destroy'params NatRule'destroy'results)
    natRule'destroy _ = GH.methodUnimplemented
instance (GH.HasMethod "destroy" NatRule NatRule'destroy'params NatRule'destroy'results) where
    methodByLabel  = (GH.Method 12678499534413776546 0)
data NatRule'destroy'params 
type instance (R.ReprFor NatRule'destroy'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NatRule'destroy'params) where
    typeId  = 17107885031248249200
instance (C.TypedStruct NatRule'destroy'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate NatRule'destroy'params) where
    type AllocHint NatRule'destroy'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NatRule'destroy'params (C.Parsed NatRule'destroy'params))
instance (C.AllocateList NatRule'destroy'params) where
    type ListAllocHint NatRule'destroy'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NatRule'destroy'params (C.Parsed NatRule'destroy'params))
data instance C.Parsed NatRule'destroy'params
    = NatRule'destroy'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NatRule'destroy'params))
deriving instance (Std_.Eq (C.Parsed NatRule'destroy'params))
instance (C.Parse NatRule'destroy'params (C.Parsed NatRule'destroy'params)) where
    parse raw_ = (Std_.pure NatRule'destroy'params)
instance (C.Marshal NatRule'destroy'params (C.Parsed NatRule'destroy'params)) where
    marshalInto _raw (NatRule'destroy'params) = (Std_.pure ())
data NatRule'destroy'results 
type instance (R.ReprFor NatRule'destroy'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NatRule'destroy'results) where
    typeId  = 17905922773284228442
instance (C.TypedStruct NatRule'destroy'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate NatRule'destroy'results) where
    type AllocHint NatRule'destroy'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NatRule'destroy'results (C.Parsed NatRule'destroy'results))
instance (C.AllocateList NatRule'destroy'results) where
    type ListAllocHint NatRule'destroy'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NatRule'destroy'results (C.Parsed NatRule'destroy'results))
data instance C.Parsed NatRule'destroy'results
    = NatRule'destroy'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NatRule'destroy'results))
deriving instance (Std_.Eq (C.Parsed NatRule'destroy'results))
instance (C.Parse NatRule'destroy'results (C.Parsed NatRule'destroy'results)) where
    parse raw_ = (Std_.pure NatRule'destroy'results)
instance (C.Marshal NatRule'destroy'results (C.Parsed NatRule'destroy'results)) where
    marshalInto _raw (NatRule'destroy'results) = (Std_.pure ())
data EventSink 
type instance (R.ReprFor EventSink) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId EventSink) where
    typeId  = 10926041113407843623
instance (C.Parse EventSink (GH.Client EventSink)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export EventSink) where
    type Server EventSink = EventSink'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(EventSink)) [(GH.toUntypedMethodHandler ((eventSink'onResourceVanished) s_))
                                                                           ,(GH.toUntypedMethodHandler ((eventSink'onDnsmasqExited) s_))] [])
class (EventSink'server_ s_) where
    {-# MINIMAL eventSink'onResourceVanished,eventSink'onDnsmasqExited #-}
    eventSink'onResourceVanished :: s_ -> (GH.MethodHandler EventSink'onResourceVanished'params EventSink'onResourceVanished'results)
    eventSink'onResourceVanished _ = GH.methodUnimplemented
    eventSink'onDnsmasqExited :: s_ -> (GH.MethodHandler EventSink'onDnsmasqExited'params EventSink'onDnsmasqExited'results)
    eventSink'onDnsmasqExited _ = GH.methodUnimplemented
instance (GH.HasMethod "onResourceVanished" EventSink EventSink'onResourceVanished'params EventSink'onResourceVanished'results) where
    methodByLabel  = (GH.Method 10926041113407843623 0)
instance (GH.HasMethod "onDnsmasqExited" EventSink EventSink'onDnsmasqExited'params EventSink'onDnsmasqExited'results) where
    methodByLabel  = (GH.Method 10926041113407843623 1)
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
data EventSink'onDnsmasqExited'params 
type instance (R.ReprFor EventSink'onDnsmasqExited'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId EventSink'onDnsmasqExited'params) where
    typeId  = 9937040125606265366
instance (C.TypedStruct EventSink'onDnsmasqExited'params) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate EventSink'onDnsmasqExited'params) where
    type AllocHint EventSink'onDnsmasqExited'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc EventSink'onDnsmasqExited'params (C.Parsed EventSink'onDnsmasqExited'params))
instance (C.AllocateList EventSink'onDnsmasqExited'params) where
    type ListAllocHint EventSink'onDnsmasqExited'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc EventSink'onDnsmasqExited'params (C.Parsed EventSink'onDnsmasqExited'params))
data instance C.Parsed EventSink'onDnsmasqExited'params
    = EventSink'onDnsmasqExited'params 
        {name :: (RP.Parsed Basics.Text)
        ,exitStatus :: (RP.Parsed Std_.Int32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed EventSink'onDnsmasqExited'params))
deriving instance (Std_.Eq (C.Parsed EventSink'onDnsmasqExited'params))
instance (C.Parse EventSink'onDnsmasqExited'params (C.Parsed EventSink'onDnsmasqExited'params)) where
    parse raw_ = (EventSink'onDnsmasqExited'params <$> (GH.parseField #name raw_)
                                                   <*> (GH.parseField #exitStatus raw_))
instance (C.Marshal EventSink'onDnsmasqExited'params (C.Parsed EventSink'onDnsmasqExited'params)) where
    marshalInto raw_ EventSink'onDnsmasqExited'params{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #exitStatus exitStatus raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot EventSink'onDnsmasqExited'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "exitStatus" GH.Slot EventSink'onDnsmasqExited'params Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
data EventSink'onDnsmasqExited'results 
type instance (R.ReprFor EventSink'onDnsmasqExited'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId EventSink'onDnsmasqExited'results) where
    typeId  = 10918140656045725280
instance (C.TypedStruct EventSink'onDnsmasqExited'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate EventSink'onDnsmasqExited'results) where
    type AllocHint EventSink'onDnsmasqExited'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc EventSink'onDnsmasqExited'results (C.Parsed EventSink'onDnsmasqExited'results))
instance (C.AllocateList EventSink'onDnsmasqExited'results) where
    type ListAllocHint EventSink'onDnsmasqExited'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc EventSink'onDnsmasqExited'results (C.Parsed EventSink'onDnsmasqExited'results))
data instance C.Parsed EventSink'onDnsmasqExited'results
    = EventSink'onDnsmasqExited'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed EventSink'onDnsmasqExited'results))
deriving instance (Std_.Eq (C.Parsed EventSink'onDnsmasqExited'results))
instance (C.Parse EventSink'onDnsmasqExited'results (C.Parsed EventSink'onDnsmasqExited'results)) where
    parse raw_ = (Std_.pure EventSink'onDnsmasqExited'results)
instance (C.Marshal EventSink'onDnsmasqExited'results (C.Parsed EventSink'onDnsmasqExited'results)) where
    marshalInto _raw (EventSink'onDnsmasqExited'results) = (Std_.pure ())
data BridgeParams 
type instance (R.ReprFor BridgeParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId BridgeParams) where
    typeId  = 10611128163957732089
instance (C.TypedStruct BridgeParams) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate BridgeParams) where
    type AllocHint BridgeParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc BridgeParams (C.Parsed BridgeParams))
instance (C.AllocateList BridgeParams) where
    type ListAllocHint BridgeParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc BridgeParams (C.Parsed BridgeParams))
data instance C.Parsed BridgeParams
    = BridgeParams 
        {name :: (RP.Parsed Basics.Text)
        ,cidr :: (RP.Parsed Basics.Text)
        ,mtu :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed BridgeParams))
deriving instance (Std_.Eq (C.Parsed BridgeParams))
instance (C.Parse BridgeParams (C.Parsed BridgeParams)) where
    parse raw_ = (BridgeParams <$> (GH.parseField #name raw_)
                               <*> (GH.parseField #cidr raw_)
                               <*> (GH.parseField #mtu raw_))
instance (C.Marshal BridgeParams (C.Parsed BridgeParams)) where
    marshalInto raw_ BridgeParams{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #cidr cidr raw_)
        (GH.encodeField #mtu mtu raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot BridgeParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "cidr" GH.Slot BridgeParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "mtu" GH.Slot BridgeParams Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 1500)
data TapParams 
type instance (R.ReprFor TapParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TapParams) where
    typeId  = 14459889060590007792
instance (C.TypedStruct TapParams) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate TapParams) where
    type AllocHint TapParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TapParams (C.Parsed TapParams))
instance (C.AllocateList TapParams) where
    type ListAllocHint TapParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TapParams (C.Parsed TapParams))
data instance C.Parsed TapParams
    = TapParams 
        {name :: (RP.Parsed Basics.Text)
        ,bridge :: (RP.Parsed Bridge)
        ,uid :: (RP.Parsed Std_.Word32)
        ,gid :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TapParams))
deriving instance (Std_.Eq (C.Parsed TapParams))
instance (C.Parse TapParams (C.Parsed TapParams)) where
    parse raw_ = (TapParams <$> (GH.parseField #name raw_)
                            <*> (GH.parseField #bridge raw_)
                            <*> (GH.parseField #uid raw_)
                            <*> (GH.parseField #gid raw_))
instance (C.Marshal TapParams (C.Parsed TapParams)) where
    marshalInto raw_ TapParams{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #bridge bridge raw_)
        (GH.encodeField #uid uid raw_)
        (GH.encodeField #gid gid raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot TapParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "bridge" GH.Slot TapParams Bridge) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "uid" GH.Slot TapParams Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "gid" GH.Slot TapParams Std_.Word32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
data NatParams 
type instance (R.ReprFor NatParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NatParams) where
    typeId  = 13350350051298029102
instance (C.TypedStruct NatParams) where
    numStructWords  = 0
    numStructPtrs  = 3
instance (C.Allocate NatParams) where
    type AllocHint NatParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NatParams (C.Parsed NatParams))
instance (C.AllocateList NatParams) where
    type ListAllocHint NatParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NatParams (C.Parsed NatParams))
data instance C.Parsed NatParams
    = NatParams 
        {bridge :: (RP.Parsed Bridge)
        ,uplinkIf :: (RP.Parsed Basics.Text)
        ,subnet :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NatParams))
deriving instance (Std_.Eq (C.Parsed NatParams))
instance (C.Parse NatParams (C.Parsed NatParams)) where
    parse raw_ = (NatParams <$> (GH.parseField #bridge raw_)
                            <*> (GH.parseField #uplinkIf raw_)
                            <*> (GH.parseField #subnet raw_))
instance (C.Marshal NatParams (C.Parsed NatParams)) where
    marshalInto raw_ NatParams{..} = (do
        (GH.encodeField #bridge bridge raw_)
        (GH.encodeField #uplinkIf uplinkIf raw_)
        (GH.encodeField #subnet subnet raw_)
        (Std_.pure ())
        )
instance (GH.HasField "bridge" GH.Slot NatParams Bridge) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "uplinkIf" GH.Slot NatParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "subnet" GH.Slot NatParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
data DnsmasqParams 
type instance (R.ReprFor DnsmasqParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DnsmasqParams) where
    typeId  = 12539282558229736473
instance (C.TypedStruct DnsmasqParams) where
    numStructWords  = 0
    numStructPtrs  = 5
instance (C.Allocate DnsmasqParams) where
    type AllocHint DnsmasqParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DnsmasqParams (C.Parsed DnsmasqParams))
instance (C.AllocateList DnsmasqParams) where
    type ListAllocHint DnsmasqParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DnsmasqParams (C.Parsed DnsmasqParams))
data instance C.Parsed DnsmasqParams
    = DnsmasqParams 
        {bridge :: (RP.Parsed Bridge)
        ,listenAddr :: (RP.Parsed Basics.Text)
        ,dhcpRange :: (RP.Parsed Basics.Text)
        ,domain :: (RP.Parsed Basics.Text)
        ,extraArgs :: (RP.Parsed (R.List Basics.Text))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DnsmasqParams))
deriving instance (Std_.Eq (C.Parsed DnsmasqParams))
instance (C.Parse DnsmasqParams (C.Parsed DnsmasqParams)) where
    parse raw_ = (DnsmasqParams <$> (GH.parseField #bridge raw_)
                                <*> (GH.parseField #listenAddr raw_)
                                <*> (GH.parseField #dhcpRange raw_)
                                <*> (GH.parseField #domain raw_)
                                <*> (GH.parseField #extraArgs raw_))
instance (C.Marshal DnsmasqParams (C.Parsed DnsmasqParams)) where
    marshalInto raw_ DnsmasqParams{..} = (do
        (GH.encodeField #bridge bridge raw_)
        (GH.encodeField #listenAddr listenAddr raw_)
        (GH.encodeField #dhcpRange dhcpRange raw_)
        (GH.encodeField #domain domain raw_)
        (GH.encodeField #extraArgs extraArgs raw_)
        (Std_.pure ())
        )
instance (GH.HasField "bridge" GH.Slot DnsmasqParams Bridge) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "listenAddr" GH.Slot DnsmasqParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "dhcpRange" GH.Slot DnsmasqParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "domain" GH.Slot DnsmasqParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "extraArgs" GH.Slot DnsmasqParams (R.List Basics.Text)) where
    fieldByLabel  = (GH.ptrField 4)
data BridgeInfo 
type instance (R.ReprFor BridgeInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId BridgeInfo) where
    typeId  = 17245487301240504485
instance (C.TypedStruct BridgeInfo) where
    numStructWords  = 1
    numStructPtrs  = 4
instance (C.Allocate BridgeInfo) where
    type AllocHint BridgeInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc BridgeInfo (C.Parsed BridgeInfo))
instance (C.AllocateList BridgeInfo) where
    type ListAllocHint BridgeInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc BridgeInfo (C.Parsed BridgeInfo))
data instance C.Parsed BridgeInfo
    = BridgeInfo 
        {name :: (RP.Parsed Basics.Text)
        ,cidr :: (RP.Parsed Basics.Text)
        ,mtu :: (RP.Parsed Std_.Word32)
        ,upState :: (RP.Parsed Basics.Text)
        ,owner :: (RP.Parsed Basics.Text)
        ,tapCount :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed BridgeInfo))
deriving instance (Std_.Eq (C.Parsed BridgeInfo))
instance (C.Parse BridgeInfo (C.Parsed BridgeInfo)) where
    parse raw_ = (BridgeInfo <$> (GH.parseField #name raw_)
                             <*> (GH.parseField #cidr raw_)
                             <*> (GH.parseField #mtu raw_)
                             <*> (GH.parseField #upState raw_)
                             <*> (GH.parseField #owner raw_)
                             <*> (GH.parseField #tapCount raw_))
instance (C.Marshal BridgeInfo (C.Parsed BridgeInfo)) where
    marshalInto raw_ BridgeInfo{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #cidr cidr raw_)
        (GH.encodeField #mtu mtu raw_)
        (GH.encodeField #upState upState raw_)
        (GH.encodeField #owner owner raw_)
        (GH.encodeField #tapCount tapCount raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot BridgeInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "cidr" GH.Slot BridgeInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "mtu" GH.Slot BridgeInfo Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "upState" GH.Slot BridgeInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "owner" GH.Slot BridgeInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "tapCount" GH.Slot BridgeInfo Std_.Word32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
data TapInfo 
type instance (R.ReprFor TapInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TapInfo) where
    typeId  = 11740084065335998076
instance (C.TypedStruct TapInfo) where
    numStructWords  = 1
    numStructPtrs  = 3
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
        {name :: (RP.Parsed Basics.Text)
        ,bridge :: (RP.Parsed Basics.Text)
        ,uid :: (RP.Parsed Std_.Word32)
        ,gid :: (RP.Parsed Std_.Word32)
        ,owner :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TapInfo))
deriving instance (Std_.Eq (C.Parsed TapInfo))
instance (C.Parse TapInfo (C.Parsed TapInfo)) where
    parse raw_ = (TapInfo <$> (GH.parseField #name raw_)
                          <*> (GH.parseField #bridge raw_)
                          <*> (GH.parseField #uid raw_)
                          <*> (GH.parseField #gid raw_)
                          <*> (GH.parseField #owner raw_))
instance (C.Marshal TapInfo (C.Parsed TapInfo)) where
    marshalInto raw_ TapInfo{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #bridge bridge raw_)
        (GH.encodeField #uid uid raw_)
        (GH.encodeField #gid gid raw_)
        (GH.encodeField #owner owner raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot TapInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "bridge" GH.Slot TapInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "uid" GH.Slot TapInfo Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "gid" GH.Slot TapInfo Std_.Word32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
instance (GH.HasField "owner" GH.Slot TapInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
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