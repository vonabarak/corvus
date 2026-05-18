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
module Capnp.Gen.Nodeagent where
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
data NodeAgent 
type instance (R.ReprFor NodeAgent) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId NodeAgent) where
    typeId  = 15897469928431442643
instance (C.Parse NodeAgent (GH.Client NodeAgent)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export NodeAgent) where
    type Server NodeAgent = NodeAgent'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(NodeAgent)) [(GH.toUntypedMethodHandler ((nodeAgent'session) s_))
                                                                           ,(GH.toUntypedMethodHandler ((nodeAgent'ping) s_))
                                                                           ,(GH.toUntypedMethodHandler ((nodeAgent'version) s_))] [])
class (NodeAgent'server_ s_) where
    {-# MINIMAL nodeAgent'session,nodeAgent'ping,nodeAgent'version #-}
    nodeAgent'session :: s_ -> (GH.MethodHandler NodeAgent'session'params NodeAgent'session'results)
    nodeAgent'session _ = GH.methodUnimplemented
    nodeAgent'ping :: s_ -> (GH.MethodHandler NodeAgent'ping'params NodeAgent'ping'results)
    nodeAgent'ping _ = GH.methodUnimplemented
    nodeAgent'version :: s_ -> (GH.MethodHandler NodeAgent'version'params NodeAgent'version'results)
    nodeAgent'version _ = GH.methodUnimplemented
instance (GH.HasMethod "session" NodeAgent NodeAgent'session'params NodeAgent'session'results) where
    methodByLabel  = (GH.Method 15897469928431442643 0)
instance (GH.HasMethod "ping" NodeAgent NodeAgent'ping'params NodeAgent'ping'results) where
    methodByLabel  = (GH.Method 15897469928431442643 1)
instance (GH.HasMethod "version" NodeAgent NodeAgent'version'params NodeAgent'version'results) where
    methodByLabel  = (GH.Method 15897469928431442643 2)
data NodeAgent'session'params 
type instance (R.ReprFor NodeAgent'session'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeAgent'session'params) where
    typeId  = 15221358664158945242
instance (C.TypedStruct NodeAgent'session'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NodeAgent'session'params) where
    type AllocHint NodeAgent'session'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeAgent'session'params (C.Parsed NodeAgent'session'params))
instance (C.AllocateList NodeAgent'session'params) where
    type ListAllocHint NodeAgent'session'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeAgent'session'params (C.Parsed NodeAgent'session'params))
data instance C.Parsed NodeAgent'session'params
    = NodeAgent'session'params 
        {owner :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeAgent'session'params))
deriving instance (Std_.Eq (C.Parsed NodeAgent'session'params))
instance (C.Parse NodeAgent'session'params (C.Parsed NodeAgent'session'params)) where
    parse raw_ = (NodeAgent'session'params <$> (GH.parseField #owner raw_))
instance (C.Marshal NodeAgent'session'params (C.Parsed NodeAgent'session'params)) where
    marshalInto raw_ NodeAgent'session'params{..} = (do
        (GH.encodeField #owner owner raw_)
        (Std_.pure ())
        )
instance (GH.HasField "owner" GH.Slot NodeAgent'session'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data NodeAgent'session'results 
type instance (R.ReprFor NodeAgent'session'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeAgent'session'results) where
    typeId  = 12295131494272100632
instance (C.TypedStruct NodeAgent'session'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NodeAgent'session'results) where
    type AllocHint NodeAgent'session'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeAgent'session'results (C.Parsed NodeAgent'session'results))
instance (C.AllocateList NodeAgent'session'results) where
    type ListAllocHint NodeAgent'session'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeAgent'session'results (C.Parsed NodeAgent'session'results))
data instance C.Parsed NodeAgent'session'results
    = NodeAgent'session'results 
        {session :: (RP.Parsed Session)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeAgent'session'results))
deriving instance (Std_.Eq (C.Parsed NodeAgent'session'results))
instance (C.Parse NodeAgent'session'results (C.Parsed NodeAgent'session'results)) where
    parse raw_ = (NodeAgent'session'results <$> (GH.parseField #session raw_))
instance (C.Marshal NodeAgent'session'results (C.Parsed NodeAgent'session'results)) where
    marshalInto raw_ NodeAgent'session'results{..} = (do
        (GH.encodeField #session session raw_)
        (Std_.pure ())
        )
instance (GH.HasField "session" GH.Slot NodeAgent'session'results Session) where
    fieldByLabel  = (GH.ptrField 0)
data NodeAgent'ping'params 
type instance (R.ReprFor NodeAgent'ping'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeAgent'ping'params) where
    typeId  = 14536519863103395621
instance (C.TypedStruct NodeAgent'ping'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate NodeAgent'ping'params) where
    type AllocHint NodeAgent'ping'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeAgent'ping'params (C.Parsed NodeAgent'ping'params))
instance (C.AllocateList NodeAgent'ping'params) where
    type ListAllocHint NodeAgent'ping'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeAgent'ping'params (C.Parsed NodeAgent'ping'params))
data instance C.Parsed NodeAgent'ping'params
    = NodeAgent'ping'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeAgent'ping'params))
deriving instance (Std_.Eq (C.Parsed NodeAgent'ping'params))
instance (C.Parse NodeAgent'ping'params (C.Parsed NodeAgent'ping'params)) where
    parse raw_ = (Std_.pure NodeAgent'ping'params)
instance (C.Marshal NodeAgent'ping'params (C.Parsed NodeAgent'ping'params)) where
    marshalInto _raw (NodeAgent'ping'params) = (Std_.pure ())
data NodeAgent'ping'results 
type instance (R.ReprFor NodeAgent'ping'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeAgent'ping'results) where
    typeId  = 15217690398450557697
instance (C.TypedStruct NodeAgent'ping'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate NodeAgent'ping'results) where
    type AllocHint NodeAgent'ping'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeAgent'ping'results (C.Parsed NodeAgent'ping'results))
instance (C.AllocateList NodeAgent'ping'results) where
    type ListAllocHint NodeAgent'ping'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeAgent'ping'results (C.Parsed NodeAgent'ping'results))
data instance C.Parsed NodeAgent'ping'results
    = NodeAgent'ping'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeAgent'ping'results))
deriving instance (Std_.Eq (C.Parsed NodeAgent'ping'results))
instance (C.Parse NodeAgent'ping'results (C.Parsed NodeAgent'ping'results)) where
    parse raw_ = (Std_.pure NodeAgent'ping'results)
instance (C.Marshal NodeAgent'ping'results (C.Parsed NodeAgent'ping'results)) where
    marshalInto _raw (NodeAgent'ping'results) = (Std_.pure ())
data NodeAgent'version'params 
type instance (R.ReprFor NodeAgent'version'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeAgent'version'params) where
    typeId  = 15858759637600798596
instance (C.TypedStruct NodeAgent'version'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate NodeAgent'version'params) where
    type AllocHint NodeAgent'version'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeAgent'version'params (C.Parsed NodeAgent'version'params))
instance (C.AllocateList NodeAgent'version'params) where
    type ListAllocHint NodeAgent'version'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeAgent'version'params (C.Parsed NodeAgent'version'params))
data instance C.Parsed NodeAgent'version'params
    = NodeAgent'version'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeAgent'version'params))
deriving instance (Std_.Eq (C.Parsed NodeAgent'version'params))
instance (C.Parse NodeAgent'version'params (C.Parsed NodeAgent'version'params)) where
    parse raw_ = (Std_.pure NodeAgent'version'params)
instance (C.Marshal NodeAgent'version'params (C.Parsed NodeAgent'version'params)) where
    marshalInto _raw (NodeAgent'version'params) = (Std_.pure ())
data NodeAgent'version'results 
type instance (R.ReprFor NodeAgent'version'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeAgent'version'results) where
    typeId  = 12830026916437762687
instance (C.TypedStruct NodeAgent'version'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NodeAgent'version'results) where
    type AllocHint NodeAgent'version'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeAgent'version'results (C.Parsed NodeAgent'version'results))
instance (C.AllocateList NodeAgent'version'results) where
    type ListAllocHint NodeAgent'version'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeAgent'version'results (C.Parsed NodeAgent'version'results))
data instance C.Parsed NodeAgent'version'results
    = NodeAgent'version'results 
        {info :: (RP.Parsed AgentInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeAgent'version'results))
deriving instance (Std_.Eq (C.Parsed NodeAgent'version'results))
instance (C.Parse NodeAgent'version'results (C.Parsed NodeAgent'version'results)) where
    parse raw_ = (NodeAgent'version'results <$> (GH.parseField #info raw_))
instance (C.Marshal NodeAgent'version'results (C.Parsed NodeAgent'version'results)) where
    marshalInto raw_ NodeAgent'version'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot NodeAgent'version'results AgentInfo) where
    fieldByLabel  = (GH.ptrField 0)
data AgentInfo 
type instance (R.ReprFor AgentInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId AgentInfo) where
    typeId  = 14127385698379965687
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
    typeId  = 11450192344861352079
instance (C.Parse Session (GH.Client Session)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Session) where
    type Server Session = Session'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Session)) [(GH.toUntypedMethodHandler ((session'ping) s_))] [])
class (Session'server_ s_) where
    {-# MINIMAL session'ping #-}
    session'ping :: s_ -> (GH.MethodHandler Session'ping'params Session'ping'results)
    session'ping _ = GH.methodUnimplemented
instance (GH.HasMethod "ping" Session Session'ping'params Session'ping'results) where
    methodByLabel  = (GH.Method 11450192344861352079 0)
data Session'ping'params 
type instance (R.ReprFor Session'ping'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'ping'params) where
    typeId  = 17120307981116249418
instance (C.TypedStruct Session'ping'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'ping'params) where
    type AllocHint Session'ping'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'ping'params (C.Parsed Session'ping'params))
instance (C.AllocateList Session'ping'params) where
    type ListAllocHint Session'ping'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'ping'params (C.Parsed Session'ping'params))
data instance C.Parsed Session'ping'params
    = Session'ping'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'ping'params))
deriving instance (Std_.Eq (C.Parsed Session'ping'params))
instance (C.Parse Session'ping'params (C.Parsed Session'ping'params)) where
    parse raw_ = (Std_.pure Session'ping'params)
instance (C.Marshal Session'ping'params (C.Parsed Session'ping'params)) where
    marshalInto _raw (Session'ping'params) = (Std_.pure ())
data Session'ping'results 
type instance (R.ReprFor Session'ping'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'ping'results) where
    typeId  = 9319192938553916195
instance (C.TypedStruct Session'ping'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'ping'results) where
    type AllocHint Session'ping'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'ping'results (C.Parsed Session'ping'results))
instance (C.AllocateList Session'ping'results) where
    type ListAllocHint Session'ping'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'ping'results (C.Parsed Session'ping'results))
data instance C.Parsed Session'ping'results
    = Session'ping'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'ping'results))
deriving instance (Std_.Eq (C.Parsed Session'ping'results))
instance (C.Parse Session'ping'results (C.Parsed Session'ping'results)) where
    parse raw_ = (Std_.pure Session'ping'results)
instance (C.Marshal Session'ping'results (C.Parsed Session'ping'results)) where
    marshalInto _raw (Session'ping'results) = (Std_.pure ())