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
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Session)) [(GH.toUntypedMethodHandler ((session'ping) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'diskCreate) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'diskCreateOverlay) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'diskDelete) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'diskResize) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'diskRebase) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'diskClone) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'diskInspect) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'snapshotCreate) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'snapshotDelete) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'snapshotRollback) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'diskDownload) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'diskDecompressXz) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'diskMd5) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'cloudInitGenerateIso) s_))] [])
class (Session'server_ s_) where
    {-# MINIMAL session'ping,session'diskCreate,session'diskCreateOverlay,session'diskDelete,session'diskResize,session'diskRebase,session'diskClone,session'diskInspect,session'snapshotCreate,session'snapshotDelete,session'snapshotRollback,session'diskDownload,session'diskDecompressXz,session'diskMd5,session'cloudInitGenerateIso #-}
    session'ping :: s_ -> (GH.MethodHandler Session'ping'params Session'ping'results)
    session'ping _ = GH.methodUnimplemented
    session'diskCreate :: s_ -> (GH.MethodHandler Session'diskCreate'params Session'diskCreate'results)
    session'diskCreate _ = GH.methodUnimplemented
    session'diskCreateOverlay :: s_ -> (GH.MethodHandler Session'diskCreateOverlay'params Session'diskCreateOverlay'results)
    session'diskCreateOverlay _ = GH.methodUnimplemented
    session'diskDelete :: s_ -> (GH.MethodHandler Session'diskDelete'params Session'diskDelete'results)
    session'diskDelete _ = GH.methodUnimplemented
    session'diskResize :: s_ -> (GH.MethodHandler Session'diskResize'params Session'diskResize'results)
    session'diskResize _ = GH.methodUnimplemented
    session'diskRebase :: s_ -> (GH.MethodHandler Session'diskRebase'params Session'diskRebase'results)
    session'diskRebase _ = GH.methodUnimplemented
    session'diskClone :: s_ -> (GH.MethodHandler Session'diskClone'params Session'diskClone'results)
    session'diskClone _ = GH.methodUnimplemented
    session'diskInspect :: s_ -> (GH.MethodHandler Session'diskInspect'params Session'diskInspect'results)
    session'diskInspect _ = GH.methodUnimplemented
    session'snapshotCreate :: s_ -> (GH.MethodHandler Session'snapshotCreate'params Session'snapshotCreate'results)
    session'snapshotCreate _ = GH.methodUnimplemented
    session'snapshotDelete :: s_ -> (GH.MethodHandler Session'snapshotDelete'params Session'snapshotDelete'results)
    session'snapshotDelete _ = GH.methodUnimplemented
    session'snapshotRollback :: s_ -> (GH.MethodHandler Session'snapshotRollback'params Session'snapshotRollback'results)
    session'snapshotRollback _ = GH.methodUnimplemented
    session'diskDownload :: s_ -> (GH.MethodHandler Session'diskDownload'params Session'diskDownload'results)
    session'diskDownload _ = GH.methodUnimplemented
    session'diskDecompressXz :: s_ -> (GH.MethodHandler Session'diskDecompressXz'params Session'diskDecompressXz'results)
    session'diskDecompressXz _ = GH.methodUnimplemented
    session'diskMd5 :: s_ -> (GH.MethodHandler Session'diskMd5'params Session'diskMd5'results)
    session'diskMd5 _ = GH.methodUnimplemented
    session'cloudInitGenerateIso :: s_ -> (GH.MethodHandler Session'cloudInitGenerateIso'params Session'cloudInitGenerateIso'results)
    session'cloudInitGenerateIso _ = GH.methodUnimplemented
instance (GH.HasMethod "ping" Session Session'ping'params Session'ping'results) where
    methodByLabel  = (GH.Method 11450192344861352079 0)
instance (GH.HasMethod "diskCreate" Session Session'diskCreate'params Session'diskCreate'results) where
    methodByLabel  = (GH.Method 11450192344861352079 1)
instance (GH.HasMethod "diskCreateOverlay" Session Session'diskCreateOverlay'params Session'diskCreateOverlay'results) where
    methodByLabel  = (GH.Method 11450192344861352079 2)
instance (GH.HasMethod "diskDelete" Session Session'diskDelete'params Session'diskDelete'results) where
    methodByLabel  = (GH.Method 11450192344861352079 3)
instance (GH.HasMethod "diskResize" Session Session'diskResize'params Session'diskResize'results) where
    methodByLabel  = (GH.Method 11450192344861352079 4)
instance (GH.HasMethod "diskRebase" Session Session'diskRebase'params Session'diskRebase'results) where
    methodByLabel  = (GH.Method 11450192344861352079 5)
instance (GH.HasMethod "diskClone" Session Session'diskClone'params Session'diskClone'results) where
    methodByLabel  = (GH.Method 11450192344861352079 6)
instance (GH.HasMethod "diskInspect" Session Session'diskInspect'params Session'diskInspect'results) where
    methodByLabel  = (GH.Method 11450192344861352079 7)
instance (GH.HasMethod "snapshotCreate" Session Session'snapshotCreate'params Session'snapshotCreate'results) where
    methodByLabel  = (GH.Method 11450192344861352079 8)
instance (GH.HasMethod "snapshotDelete" Session Session'snapshotDelete'params Session'snapshotDelete'results) where
    methodByLabel  = (GH.Method 11450192344861352079 9)
instance (GH.HasMethod "snapshotRollback" Session Session'snapshotRollback'params Session'snapshotRollback'results) where
    methodByLabel  = (GH.Method 11450192344861352079 10)
instance (GH.HasMethod "diskDownload" Session Session'diskDownload'params Session'diskDownload'results) where
    methodByLabel  = (GH.Method 11450192344861352079 11)
instance (GH.HasMethod "diskDecompressXz" Session Session'diskDecompressXz'params Session'diskDecompressXz'results) where
    methodByLabel  = (GH.Method 11450192344861352079 12)
instance (GH.HasMethod "diskMd5" Session Session'diskMd5'params Session'diskMd5'results) where
    methodByLabel  = (GH.Method 11450192344861352079 13)
instance (GH.HasMethod "cloudInitGenerateIso" Session Session'cloudInitGenerateIso'params Session'cloudInitGenerateIso'results) where
    methodByLabel  = (GH.Method 11450192344861352079 14)
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
data Session'diskCreate'params 
type instance (R.ReprFor Session'diskCreate'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskCreate'params) where
    typeId  = 16127592023001067955
instance (C.TypedStruct Session'diskCreate'params) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Session'diskCreate'params) where
    type AllocHint Session'diskCreate'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskCreate'params (C.Parsed Session'diskCreate'params))
instance (C.AllocateList Session'diskCreate'params) where
    type ListAllocHint Session'diskCreate'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskCreate'params (C.Parsed Session'diskCreate'params))
data instance C.Parsed Session'diskCreate'params
    = Session'diskCreate'params 
        {path :: (RP.Parsed Basics.Text)
        ,format :: (RP.Parsed Basics.Text)
        ,sizeMb :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskCreate'params))
deriving instance (Std_.Eq (C.Parsed Session'diskCreate'params))
instance (C.Parse Session'diskCreate'params (C.Parsed Session'diskCreate'params)) where
    parse raw_ = (Session'diskCreate'params <$> (GH.parseField #path raw_)
                                            <*> (GH.parseField #format raw_)
                                            <*> (GH.parseField #sizeMb raw_))
instance (C.Marshal Session'diskCreate'params (C.Parsed Session'diskCreate'params)) where
    marshalInto raw_ Session'diskCreate'params{..} = (do
        (GH.encodeField #path path raw_)
        (GH.encodeField #format format raw_)
        (GH.encodeField #sizeMb sizeMb raw_)
        (Std_.pure ())
        )
instance (GH.HasField "path" GH.Slot Session'diskCreate'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "format" GH.Slot Session'diskCreate'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "sizeMb" GH.Slot Session'diskCreate'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Session'diskCreate'results 
type instance (R.ReprFor Session'diskCreate'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskCreate'results) where
    typeId  = 13761120203856121079
instance (C.TypedStruct Session'diskCreate'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'diskCreate'results) where
    type AllocHint Session'diskCreate'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskCreate'results (C.Parsed Session'diskCreate'results))
instance (C.AllocateList Session'diskCreate'results) where
    type ListAllocHint Session'diskCreate'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskCreate'results (C.Parsed Session'diskCreate'results))
data instance C.Parsed Session'diskCreate'results
    = Session'diskCreate'results 
        {result :: (RP.Parsed DiskOpResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskCreate'results))
deriving instance (Std_.Eq (C.Parsed Session'diskCreate'results))
instance (C.Parse Session'diskCreate'results (C.Parsed Session'diskCreate'results)) where
    parse raw_ = (Session'diskCreate'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'diskCreate'results (C.Parsed Session'diskCreate'results)) where
    marshalInto raw_ Session'diskCreate'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'diskCreate'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
data Session'diskCreateOverlay'params 
type instance (R.ReprFor Session'diskCreateOverlay'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskCreateOverlay'params) where
    typeId  = 14516785854889517845
instance (C.TypedStruct Session'diskCreateOverlay'params) where
    numStructWords  = 0
    numStructPtrs  = 3
instance (C.Allocate Session'diskCreateOverlay'params) where
    type AllocHint Session'diskCreateOverlay'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskCreateOverlay'params (C.Parsed Session'diskCreateOverlay'params))
instance (C.AllocateList Session'diskCreateOverlay'params) where
    type ListAllocHint Session'diskCreateOverlay'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskCreateOverlay'params (C.Parsed Session'diskCreateOverlay'params))
data instance C.Parsed Session'diskCreateOverlay'params
    = Session'diskCreateOverlay'params 
        {overlayPath :: (RP.Parsed Basics.Text)
        ,backingPath :: (RP.Parsed Basics.Text)
        ,backingFormat :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskCreateOverlay'params))
deriving instance (Std_.Eq (C.Parsed Session'diskCreateOverlay'params))
instance (C.Parse Session'diskCreateOverlay'params (C.Parsed Session'diskCreateOverlay'params)) where
    parse raw_ = (Session'diskCreateOverlay'params <$> (GH.parseField #overlayPath raw_)
                                                   <*> (GH.parseField #backingPath raw_)
                                                   <*> (GH.parseField #backingFormat raw_))
instance (C.Marshal Session'diskCreateOverlay'params (C.Parsed Session'diskCreateOverlay'params)) where
    marshalInto raw_ Session'diskCreateOverlay'params{..} = (do
        (GH.encodeField #overlayPath overlayPath raw_)
        (GH.encodeField #backingPath backingPath raw_)
        (GH.encodeField #backingFormat backingFormat raw_)
        (Std_.pure ())
        )
instance (GH.HasField "overlayPath" GH.Slot Session'diskCreateOverlay'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "backingPath" GH.Slot Session'diskCreateOverlay'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "backingFormat" GH.Slot Session'diskCreateOverlay'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
data Session'diskCreateOverlay'results 
type instance (R.ReprFor Session'diskCreateOverlay'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskCreateOverlay'results) where
    typeId  = 18296988007277893098
instance (C.TypedStruct Session'diskCreateOverlay'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'diskCreateOverlay'results) where
    type AllocHint Session'diskCreateOverlay'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskCreateOverlay'results (C.Parsed Session'diskCreateOverlay'results))
instance (C.AllocateList Session'diskCreateOverlay'results) where
    type ListAllocHint Session'diskCreateOverlay'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskCreateOverlay'results (C.Parsed Session'diskCreateOverlay'results))
data instance C.Parsed Session'diskCreateOverlay'results
    = Session'diskCreateOverlay'results 
        {result :: (RP.Parsed DiskOpResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskCreateOverlay'results))
deriving instance (Std_.Eq (C.Parsed Session'diskCreateOverlay'results))
instance (C.Parse Session'diskCreateOverlay'results (C.Parsed Session'diskCreateOverlay'results)) where
    parse raw_ = (Session'diskCreateOverlay'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'diskCreateOverlay'results (C.Parsed Session'diskCreateOverlay'results)) where
    marshalInto raw_ Session'diskCreateOverlay'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'diskCreateOverlay'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
data Session'diskDelete'params 
type instance (R.ReprFor Session'diskDelete'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskDelete'params) where
    typeId  = 10676587244486002852
instance (C.TypedStruct Session'diskDelete'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'diskDelete'params) where
    type AllocHint Session'diskDelete'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskDelete'params (C.Parsed Session'diskDelete'params))
instance (C.AllocateList Session'diskDelete'params) where
    type ListAllocHint Session'diskDelete'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskDelete'params (C.Parsed Session'diskDelete'params))
data instance C.Parsed Session'diskDelete'params
    = Session'diskDelete'params 
        {path :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskDelete'params))
deriving instance (Std_.Eq (C.Parsed Session'diskDelete'params))
instance (C.Parse Session'diskDelete'params (C.Parsed Session'diskDelete'params)) where
    parse raw_ = (Session'diskDelete'params <$> (GH.parseField #path raw_))
instance (C.Marshal Session'diskDelete'params (C.Parsed Session'diskDelete'params)) where
    marshalInto raw_ Session'diskDelete'params{..} = (do
        (GH.encodeField #path path raw_)
        (Std_.pure ())
        )
instance (GH.HasField "path" GH.Slot Session'diskDelete'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Session'diskDelete'results 
type instance (R.ReprFor Session'diskDelete'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskDelete'results) where
    typeId  = 11948452925487114338
instance (C.TypedStruct Session'diskDelete'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'diskDelete'results) where
    type AllocHint Session'diskDelete'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskDelete'results (C.Parsed Session'diskDelete'results))
instance (C.AllocateList Session'diskDelete'results) where
    type ListAllocHint Session'diskDelete'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskDelete'results (C.Parsed Session'diskDelete'results))
data instance C.Parsed Session'diskDelete'results
    = Session'diskDelete'results 
        {result :: (RP.Parsed DiskOpResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskDelete'results))
deriving instance (Std_.Eq (C.Parsed Session'diskDelete'results))
instance (C.Parse Session'diskDelete'results (C.Parsed Session'diskDelete'results)) where
    parse raw_ = (Session'diskDelete'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'diskDelete'results (C.Parsed Session'diskDelete'results)) where
    marshalInto raw_ Session'diskDelete'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'diskDelete'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
data Session'diskResize'params 
type instance (R.ReprFor Session'diskResize'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskResize'params) where
    typeId  = 15646331601880300946
instance (C.TypedStruct Session'diskResize'params) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Session'diskResize'params) where
    type AllocHint Session'diskResize'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskResize'params (C.Parsed Session'diskResize'params))
instance (C.AllocateList Session'diskResize'params) where
    type ListAllocHint Session'diskResize'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskResize'params (C.Parsed Session'diskResize'params))
data instance C.Parsed Session'diskResize'params
    = Session'diskResize'params 
        {path :: (RP.Parsed Basics.Text)
        ,newSizeMb :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskResize'params))
deriving instance (Std_.Eq (C.Parsed Session'diskResize'params))
instance (C.Parse Session'diskResize'params (C.Parsed Session'diskResize'params)) where
    parse raw_ = (Session'diskResize'params <$> (GH.parseField #path raw_)
                                            <*> (GH.parseField #newSizeMb raw_))
instance (C.Marshal Session'diskResize'params (C.Parsed Session'diskResize'params)) where
    marshalInto raw_ Session'diskResize'params{..} = (do
        (GH.encodeField #path path raw_)
        (GH.encodeField #newSizeMb newSizeMb raw_)
        (Std_.pure ())
        )
instance (GH.HasField "path" GH.Slot Session'diskResize'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "newSizeMb" GH.Slot Session'diskResize'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Session'diskResize'results 
type instance (R.ReprFor Session'diskResize'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskResize'results) where
    typeId  = 13891217123988901794
instance (C.TypedStruct Session'diskResize'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'diskResize'results) where
    type AllocHint Session'diskResize'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskResize'results (C.Parsed Session'diskResize'results))
instance (C.AllocateList Session'diskResize'results) where
    type ListAllocHint Session'diskResize'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskResize'results (C.Parsed Session'diskResize'results))
data instance C.Parsed Session'diskResize'results
    = Session'diskResize'results 
        {result :: (RP.Parsed DiskOpResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskResize'results))
deriving instance (Std_.Eq (C.Parsed Session'diskResize'results))
instance (C.Parse Session'diskResize'results (C.Parsed Session'diskResize'results)) where
    parse raw_ = (Session'diskResize'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'diskResize'results (C.Parsed Session'diskResize'results)) where
    marshalInto raw_ Session'diskResize'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'diskResize'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
data Session'diskRebase'params 
type instance (R.ReprFor Session'diskRebase'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskRebase'params) where
    typeId  = 15582431297534686939
instance (C.TypedStruct Session'diskRebase'params) where
    numStructWords  = 1
    numStructPtrs  = 3
instance (C.Allocate Session'diskRebase'params) where
    type AllocHint Session'diskRebase'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskRebase'params (C.Parsed Session'diskRebase'params))
instance (C.AllocateList Session'diskRebase'params) where
    type ListAllocHint Session'diskRebase'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskRebase'params (C.Parsed Session'diskRebase'params))
data instance C.Parsed Session'diskRebase'params
    = Session'diskRebase'params 
        {overlayPath :: (RP.Parsed Basics.Text)
        ,newBacking :: (RP.Parsed Basics.Text)
        ,newBackingFormat :: (RP.Parsed Basics.Text)
        ,hasNewBacking :: (RP.Parsed Std_.Bool)
        ,unsafeUpdate :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskRebase'params))
deriving instance (Std_.Eq (C.Parsed Session'diskRebase'params))
instance (C.Parse Session'diskRebase'params (C.Parsed Session'diskRebase'params)) where
    parse raw_ = (Session'diskRebase'params <$> (GH.parseField #overlayPath raw_)
                                            <*> (GH.parseField #newBacking raw_)
                                            <*> (GH.parseField #newBackingFormat raw_)
                                            <*> (GH.parseField #hasNewBacking raw_)
                                            <*> (GH.parseField #unsafeUpdate raw_))
instance (C.Marshal Session'diskRebase'params (C.Parsed Session'diskRebase'params)) where
    marshalInto raw_ Session'diskRebase'params{..} = (do
        (GH.encodeField #overlayPath overlayPath raw_)
        (GH.encodeField #newBacking newBacking raw_)
        (GH.encodeField #newBackingFormat newBackingFormat raw_)
        (GH.encodeField #hasNewBacking hasNewBacking raw_)
        (GH.encodeField #unsafeUpdate unsafeUpdate raw_)
        (Std_.pure ())
        )
instance (GH.HasField "overlayPath" GH.Slot Session'diskRebase'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "newBacking" GH.Slot Session'diskRebase'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "newBackingFormat" GH.Slot Session'diskRebase'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "hasNewBacking" GH.Slot Session'diskRebase'params Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (GH.HasField "unsafeUpdate" GH.Slot Session'diskRebase'params Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 0 1 0)
data Session'diskRebase'results 
type instance (R.ReprFor Session'diskRebase'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskRebase'results) where
    typeId  = 13943800101944505537
instance (C.TypedStruct Session'diskRebase'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'diskRebase'results) where
    type AllocHint Session'diskRebase'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskRebase'results (C.Parsed Session'diskRebase'results))
instance (C.AllocateList Session'diskRebase'results) where
    type ListAllocHint Session'diskRebase'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskRebase'results (C.Parsed Session'diskRebase'results))
data instance C.Parsed Session'diskRebase'results
    = Session'diskRebase'results 
        {result :: (RP.Parsed DiskOpResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskRebase'results))
deriving instance (Std_.Eq (C.Parsed Session'diskRebase'results))
instance (C.Parse Session'diskRebase'results (C.Parsed Session'diskRebase'results)) where
    parse raw_ = (Session'diskRebase'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'diskRebase'results (C.Parsed Session'diskRebase'results)) where
    marshalInto raw_ Session'diskRebase'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'diskRebase'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
data Session'diskClone'params 
type instance (R.ReprFor Session'diskClone'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskClone'params) where
    typeId  = 14875352729381787711
instance (C.TypedStruct Session'diskClone'params) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Session'diskClone'params) where
    type AllocHint Session'diskClone'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskClone'params (C.Parsed Session'diskClone'params))
instance (C.AllocateList Session'diskClone'params) where
    type ListAllocHint Session'diskClone'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskClone'params (C.Parsed Session'diskClone'params))
data instance C.Parsed Session'diskClone'params
    = Session'diskClone'params 
        {sourcePath :: (RP.Parsed Basics.Text)
        ,destPath :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskClone'params))
deriving instance (Std_.Eq (C.Parsed Session'diskClone'params))
instance (C.Parse Session'diskClone'params (C.Parsed Session'diskClone'params)) where
    parse raw_ = (Session'diskClone'params <$> (GH.parseField #sourcePath raw_)
                                           <*> (GH.parseField #destPath raw_))
instance (C.Marshal Session'diskClone'params (C.Parsed Session'diskClone'params)) where
    marshalInto raw_ Session'diskClone'params{..} = (do
        (GH.encodeField #sourcePath sourcePath raw_)
        (GH.encodeField #destPath destPath raw_)
        (Std_.pure ())
        )
instance (GH.HasField "sourcePath" GH.Slot Session'diskClone'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "destPath" GH.Slot Session'diskClone'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
data Session'diskClone'results 
type instance (R.ReprFor Session'diskClone'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskClone'results) where
    typeId  = 10482624371876660872
instance (C.TypedStruct Session'diskClone'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'diskClone'results) where
    type AllocHint Session'diskClone'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskClone'results (C.Parsed Session'diskClone'results))
instance (C.AllocateList Session'diskClone'results) where
    type ListAllocHint Session'diskClone'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskClone'results (C.Parsed Session'diskClone'results))
data instance C.Parsed Session'diskClone'results
    = Session'diskClone'results 
        {result :: (RP.Parsed DiskOpResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskClone'results))
deriving instance (Std_.Eq (C.Parsed Session'diskClone'results))
instance (C.Parse Session'diskClone'results (C.Parsed Session'diskClone'results)) where
    parse raw_ = (Session'diskClone'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'diskClone'results (C.Parsed Session'diskClone'results)) where
    marshalInto raw_ Session'diskClone'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'diskClone'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
data Session'diskInspect'params 
type instance (R.ReprFor Session'diskInspect'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskInspect'params) where
    typeId  = 17002640096473422215
instance (C.TypedStruct Session'diskInspect'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'diskInspect'params) where
    type AllocHint Session'diskInspect'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskInspect'params (C.Parsed Session'diskInspect'params))
instance (C.AllocateList Session'diskInspect'params) where
    type ListAllocHint Session'diskInspect'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskInspect'params (C.Parsed Session'diskInspect'params))
data instance C.Parsed Session'diskInspect'params
    = Session'diskInspect'params 
        {path :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskInspect'params))
deriving instance (Std_.Eq (C.Parsed Session'diskInspect'params))
instance (C.Parse Session'diskInspect'params (C.Parsed Session'diskInspect'params)) where
    parse raw_ = (Session'diskInspect'params <$> (GH.parseField #path raw_))
instance (C.Marshal Session'diskInspect'params (C.Parsed Session'diskInspect'params)) where
    marshalInto raw_ Session'diskInspect'params{..} = (do
        (GH.encodeField #path path raw_)
        (Std_.pure ())
        )
instance (GH.HasField "path" GH.Slot Session'diskInspect'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Session'diskInspect'results 
type instance (R.ReprFor Session'diskInspect'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskInspect'results) where
    typeId  = 16749289768481078801
instance (C.TypedStruct Session'diskInspect'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'diskInspect'results) where
    type AllocHint Session'diskInspect'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskInspect'results (C.Parsed Session'diskInspect'results))
instance (C.AllocateList Session'diskInspect'results) where
    type ListAllocHint Session'diskInspect'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskInspect'results (C.Parsed Session'diskInspect'results))
data instance C.Parsed Session'diskInspect'results
    = Session'diskInspect'results 
        {info :: (RP.Parsed DiskInspectInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskInspect'results))
deriving instance (Std_.Eq (C.Parsed Session'diskInspect'results))
instance (C.Parse Session'diskInspect'results (C.Parsed Session'diskInspect'results)) where
    parse raw_ = (Session'diskInspect'results <$> (GH.parseField #info raw_))
instance (C.Marshal Session'diskInspect'results (C.Parsed Session'diskInspect'results)) where
    marshalInto raw_ Session'diskInspect'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot Session'diskInspect'results DiskInspectInfo) where
    fieldByLabel  = (GH.ptrField 0)
data Session'snapshotCreate'params 
type instance (R.ReprFor Session'snapshotCreate'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotCreate'params) where
    typeId  = 17273836855587515204
instance (C.TypedStruct Session'snapshotCreate'params) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Session'snapshotCreate'params) where
    type AllocHint Session'snapshotCreate'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotCreate'params (C.Parsed Session'snapshotCreate'params))
instance (C.AllocateList Session'snapshotCreate'params) where
    type ListAllocHint Session'snapshotCreate'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotCreate'params (C.Parsed Session'snapshotCreate'params))
data instance C.Parsed Session'snapshotCreate'params
    = Session'snapshotCreate'params 
        {path :: (RP.Parsed Basics.Text)
        ,name :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotCreate'params))
deriving instance (Std_.Eq (C.Parsed Session'snapshotCreate'params))
instance (C.Parse Session'snapshotCreate'params (C.Parsed Session'snapshotCreate'params)) where
    parse raw_ = (Session'snapshotCreate'params <$> (GH.parseField #path raw_)
                                                <*> (GH.parseField #name raw_))
instance (C.Marshal Session'snapshotCreate'params (C.Parsed Session'snapshotCreate'params)) where
    marshalInto raw_ Session'snapshotCreate'params{..} = (do
        (GH.encodeField #path path raw_)
        (GH.encodeField #name name raw_)
        (Std_.pure ())
        )
instance (GH.HasField "path" GH.Slot Session'snapshotCreate'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "name" GH.Slot Session'snapshotCreate'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
data Session'snapshotCreate'results 
type instance (R.ReprFor Session'snapshotCreate'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotCreate'results) where
    typeId  = 16193070465825742717
instance (C.TypedStruct Session'snapshotCreate'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'snapshotCreate'results) where
    type AllocHint Session'snapshotCreate'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotCreate'results (C.Parsed Session'snapshotCreate'results))
instance (C.AllocateList Session'snapshotCreate'results) where
    type ListAllocHint Session'snapshotCreate'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotCreate'results (C.Parsed Session'snapshotCreate'results))
data instance C.Parsed Session'snapshotCreate'results
    = Session'snapshotCreate'results 
        {result :: (RP.Parsed DiskOpResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotCreate'results))
deriving instance (Std_.Eq (C.Parsed Session'snapshotCreate'results))
instance (C.Parse Session'snapshotCreate'results (C.Parsed Session'snapshotCreate'results)) where
    parse raw_ = (Session'snapshotCreate'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'snapshotCreate'results (C.Parsed Session'snapshotCreate'results)) where
    marshalInto raw_ Session'snapshotCreate'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'snapshotCreate'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
data Session'snapshotDelete'params 
type instance (R.ReprFor Session'snapshotDelete'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotDelete'params) where
    typeId  = 16838127620127419893
instance (C.TypedStruct Session'snapshotDelete'params) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Session'snapshotDelete'params) where
    type AllocHint Session'snapshotDelete'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotDelete'params (C.Parsed Session'snapshotDelete'params))
instance (C.AllocateList Session'snapshotDelete'params) where
    type ListAllocHint Session'snapshotDelete'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotDelete'params (C.Parsed Session'snapshotDelete'params))
data instance C.Parsed Session'snapshotDelete'params
    = Session'snapshotDelete'params 
        {path :: (RP.Parsed Basics.Text)
        ,name :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotDelete'params))
deriving instance (Std_.Eq (C.Parsed Session'snapshotDelete'params))
instance (C.Parse Session'snapshotDelete'params (C.Parsed Session'snapshotDelete'params)) where
    parse raw_ = (Session'snapshotDelete'params <$> (GH.parseField #path raw_)
                                                <*> (GH.parseField #name raw_))
instance (C.Marshal Session'snapshotDelete'params (C.Parsed Session'snapshotDelete'params)) where
    marshalInto raw_ Session'snapshotDelete'params{..} = (do
        (GH.encodeField #path path raw_)
        (GH.encodeField #name name raw_)
        (Std_.pure ())
        )
instance (GH.HasField "path" GH.Slot Session'snapshotDelete'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "name" GH.Slot Session'snapshotDelete'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
data Session'snapshotDelete'results 
type instance (R.ReprFor Session'snapshotDelete'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotDelete'results) where
    typeId  = 13336217285621226050
instance (C.TypedStruct Session'snapshotDelete'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'snapshotDelete'results) where
    type AllocHint Session'snapshotDelete'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotDelete'results (C.Parsed Session'snapshotDelete'results))
instance (C.AllocateList Session'snapshotDelete'results) where
    type ListAllocHint Session'snapshotDelete'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotDelete'results (C.Parsed Session'snapshotDelete'results))
data instance C.Parsed Session'snapshotDelete'results
    = Session'snapshotDelete'results 
        {result :: (RP.Parsed DiskOpResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotDelete'results))
deriving instance (Std_.Eq (C.Parsed Session'snapshotDelete'results))
instance (C.Parse Session'snapshotDelete'results (C.Parsed Session'snapshotDelete'results)) where
    parse raw_ = (Session'snapshotDelete'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'snapshotDelete'results (C.Parsed Session'snapshotDelete'results)) where
    marshalInto raw_ Session'snapshotDelete'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'snapshotDelete'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
data Session'snapshotRollback'params 
type instance (R.ReprFor Session'snapshotRollback'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotRollback'params) where
    typeId  = 13108850606835041933
instance (C.TypedStruct Session'snapshotRollback'params) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Session'snapshotRollback'params) where
    type AllocHint Session'snapshotRollback'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotRollback'params (C.Parsed Session'snapshotRollback'params))
instance (C.AllocateList Session'snapshotRollback'params) where
    type ListAllocHint Session'snapshotRollback'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotRollback'params (C.Parsed Session'snapshotRollback'params))
data instance C.Parsed Session'snapshotRollback'params
    = Session'snapshotRollback'params 
        {path :: (RP.Parsed Basics.Text)
        ,name :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotRollback'params))
deriving instance (Std_.Eq (C.Parsed Session'snapshotRollback'params))
instance (C.Parse Session'snapshotRollback'params (C.Parsed Session'snapshotRollback'params)) where
    parse raw_ = (Session'snapshotRollback'params <$> (GH.parseField #path raw_)
                                                  <*> (GH.parseField #name raw_))
instance (C.Marshal Session'snapshotRollback'params (C.Parsed Session'snapshotRollback'params)) where
    marshalInto raw_ Session'snapshotRollback'params{..} = (do
        (GH.encodeField #path path raw_)
        (GH.encodeField #name name raw_)
        (Std_.pure ())
        )
instance (GH.HasField "path" GH.Slot Session'snapshotRollback'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "name" GH.Slot Session'snapshotRollback'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
data Session'snapshotRollback'results 
type instance (R.ReprFor Session'snapshotRollback'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotRollback'results) where
    typeId  = 15251245268679165986
instance (C.TypedStruct Session'snapshotRollback'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'snapshotRollback'results) where
    type AllocHint Session'snapshotRollback'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotRollback'results (C.Parsed Session'snapshotRollback'results))
instance (C.AllocateList Session'snapshotRollback'results) where
    type ListAllocHint Session'snapshotRollback'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotRollback'results (C.Parsed Session'snapshotRollback'results))
data instance C.Parsed Session'snapshotRollback'results
    = Session'snapshotRollback'results 
        {result :: (RP.Parsed DiskOpResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotRollback'results))
deriving instance (Std_.Eq (C.Parsed Session'snapshotRollback'results))
instance (C.Parse Session'snapshotRollback'results (C.Parsed Session'snapshotRollback'results)) where
    parse raw_ = (Session'snapshotRollback'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'snapshotRollback'results (C.Parsed Session'snapshotRollback'results)) where
    marshalInto raw_ Session'snapshotRollback'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'snapshotRollback'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
data Session'diskDownload'params 
type instance (R.ReprFor Session'diskDownload'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskDownload'params) where
    typeId  = 17291283613946040912
instance (C.TypedStruct Session'diskDownload'params) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Session'diskDownload'params) where
    type AllocHint Session'diskDownload'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskDownload'params (C.Parsed Session'diskDownload'params))
instance (C.AllocateList Session'diskDownload'params) where
    type ListAllocHint Session'diskDownload'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskDownload'params (C.Parsed Session'diskDownload'params))
data instance C.Parsed Session'diskDownload'params
    = Session'diskDownload'params 
        {destPath :: (RP.Parsed Basics.Text)
        ,url :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskDownload'params))
deriving instance (Std_.Eq (C.Parsed Session'diskDownload'params))
instance (C.Parse Session'diskDownload'params (C.Parsed Session'diskDownload'params)) where
    parse raw_ = (Session'diskDownload'params <$> (GH.parseField #destPath raw_)
                                              <*> (GH.parseField #url raw_))
instance (C.Marshal Session'diskDownload'params (C.Parsed Session'diskDownload'params)) where
    marshalInto raw_ Session'diskDownload'params{..} = (do
        (GH.encodeField #destPath destPath raw_)
        (GH.encodeField #url url raw_)
        (Std_.pure ())
        )
instance (GH.HasField "destPath" GH.Slot Session'diskDownload'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "url" GH.Slot Session'diskDownload'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
data Session'diskDownload'results 
type instance (R.ReprFor Session'diskDownload'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskDownload'results) where
    typeId  = 16337221411403463124
instance (C.TypedStruct Session'diskDownload'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'diskDownload'results) where
    type AllocHint Session'diskDownload'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskDownload'results (C.Parsed Session'diskDownload'results))
instance (C.AllocateList Session'diskDownload'results) where
    type ListAllocHint Session'diskDownload'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskDownload'results (C.Parsed Session'diskDownload'results))
data instance C.Parsed Session'diskDownload'results
    = Session'diskDownload'results 
        {result :: (RP.Parsed DiskOpResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskDownload'results))
deriving instance (Std_.Eq (C.Parsed Session'diskDownload'results))
instance (C.Parse Session'diskDownload'results (C.Parsed Session'diskDownload'results)) where
    parse raw_ = (Session'diskDownload'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'diskDownload'results (C.Parsed Session'diskDownload'results)) where
    marshalInto raw_ Session'diskDownload'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'diskDownload'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
data Session'diskDecompressXz'params 
type instance (R.ReprFor Session'diskDecompressXz'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskDecompressXz'params) where
    typeId  = 9824205008478466439
instance (C.TypedStruct Session'diskDecompressXz'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'diskDecompressXz'params) where
    type AllocHint Session'diskDecompressXz'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskDecompressXz'params (C.Parsed Session'diskDecompressXz'params))
instance (C.AllocateList Session'diskDecompressXz'params) where
    type ListAllocHint Session'diskDecompressXz'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskDecompressXz'params (C.Parsed Session'diskDecompressXz'params))
data instance C.Parsed Session'diskDecompressXz'params
    = Session'diskDecompressXz'params 
        {xzPath :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskDecompressXz'params))
deriving instance (Std_.Eq (C.Parsed Session'diskDecompressXz'params))
instance (C.Parse Session'diskDecompressXz'params (C.Parsed Session'diskDecompressXz'params)) where
    parse raw_ = (Session'diskDecompressXz'params <$> (GH.parseField #xzPath raw_))
instance (C.Marshal Session'diskDecompressXz'params (C.Parsed Session'diskDecompressXz'params)) where
    marshalInto raw_ Session'diskDecompressXz'params{..} = (do
        (GH.encodeField #xzPath xzPath raw_)
        (Std_.pure ())
        )
instance (GH.HasField "xzPath" GH.Slot Session'diskDecompressXz'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Session'diskDecompressXz'results 
type instance (R.ReprFor Session'diskDecompressXz'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskDecompressXz'results) where
    typeId  = 9813928533026383249
instance (C.TypedStruct Session'diskDecompressXz'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'diskDecompressXz'results) where
    type AllocHint Session'diskDecompressXz'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskDecompressXz'results (C.Parsed Session'diskDecompressXz'results))
instance (C.AllocateList Session'diskDecompressXz'results) where
    type ListAllocHint Session'diskDecompressXz'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskDecompressXz'results (C.Parsed Session'diskDecompressXz'results))
data instance C.Parsed Session'diskDecompressXz'results
    = Session'diskDecompressXz'results 
        {finalPath :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskDecompressXz'results))
deriving instance (Std_.Eq (C.Parsed Session'diskDecompressXz'results))
instance (C.Parse Session'diskDecompressXz'results (C.Parsed Session'diskDecompressXz'results)) where
    parse raw_ = (Session'diskDecompressXz'results <$> (GH.parseField #finalPath raw_))
instance (C.Marshal Session'diskDecompressXz'results (C.Parsed Session'diskDecompressXz'results)) where
    marshalInto raw_ Session'diskDecompressXz'results{..} = (do
        (GH.encodeField #finalPath finalPath raw_)
        (Std_.pure ())
        )
instance (GH.HasField "finalPath" GH.Slot Session'diskDecompressXz'results Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Session'diskMd5'params 
type instance (R.ReprFor Session'diskMd5'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskMd5'params) where
    typeId  = 9409244432301040014
instance (C.TypedStruct Session'diskMd5'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'diskMd5'params) where
    type AllocHint Session'diskMd5'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskMd5'params (C.Parsed Session'diskMd5'params))
instance (C.AllocateList Session'diskMd5'params) where
    type ListAllocHint Session'diskMd5'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskMd5'params (C.Parsed Session'diskMd5'params))
data instance C.Parsed Session'diskMd5'params
    = Session'diskMd5'params 
        {path :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskMd5'params))
deriving instance (Std_.Eq (C.Parsed Session'diskMd5'params))
instance (C.Parse Session'diskMd5'params (C.Parsed Session'diskMd5'params)) where
    parse raw_ = (Session'diskMd5'params <$> (GH.parseField #path raw_))
instance (C.Marshal Session'diskMd5'params (C.Parsed Session'diskMd5'params)) where
    marshalInto raw_ Session'diskMd5'params{..} = (do
        (GH.encodeField #path path raw_)
        (Std_.pure ())
        )
instance (GH.HasField "path" GH.Slot Session'diskMd5'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Session'diskMd5'results 
type instance (R.ReprFor Session'diskMd5'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskMd5'results) where
    typeId  = 10814768419322956081
instance (C.TypedStruct Session'diskMd5'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'diskMd5'results) where
    type AllocHint Session'diskMd5'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskMd5'results (C.Parsed Session'diskMd5'results))
instance (C.AllocateList Session'diskMd5'results) where
    type ListAllocHint Session'diskMd5'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskMd5'results (C.Parsed Session'diskMd5'results))
data instance C.Parsed Session'diskMd5'results
    = Session'diskMd5'results 
        {hex :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskMd5'results))
deriving instance (Std_.Eq (C.Parsed Session'diskMd5'results))
instance (C.Parse Session'diskMd5'results (C.Parsed Session'diskMd5'results)) where
    parse raw_ = (Session'diskMd5'results <$> (GH.parseField #hex raw_))
instance (C.Marshal Session'diskMd5'results (C.Parsed Session'diskMd5'results)) where
    marshalInto raw_ Session'diskMd5'results{..} = (do
        (GH.encodeField #hex hex raw_)
        (Std_.pure ())
        )
instance (GH.HasField "hex" GH.Slot Session'diskMd5'results Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Session'cloudInitGenerateIso'params 
type instance (R.ReprFor Session'cloudInitGenerateIso'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'cloudInitGenerateIso'params) where
    typeId  = 12688412389628670543
instance (C.TypedStruct Session'cloudInitGenerateIso'params) where
    numStructWords  = 1
    numStructPtrs  = 4
instance (C.Allocate Session'cloudInitGenerateIso'params) where
    type AllocHint Session'cloudInitGenerateIso'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'cloudInitGenerateIso'params (C.Parsed Session'cloudInitGenerateIso'params))
instance (C.AllocateList Session'cloudInitGenerateIso'params) where
    type ListAllocHint Session'cloudInitGenerateIso'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'cloudInitGenerateIso'params (C.Parsed Session'cloudInitGenerateIso'params))
data instance C.Parsed Session'cloudInitGenerateIso'params
    = Session'cloudInitGenerateIso'params 
        {targetDir :: (RP.Parsed Basics.Text)
        ,userData :: (RP.Parsed Basics.Text)
        ,metaData :: (RP.Parsed Basics.Text)
        ,networkConfig :: (RP.Parsed Basics.Text)
        ,hasNetworkConfig :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'cloudInitGenerateIso'params))
deriving instance (Std_.Eq (C.Parsed Session'cloudInitGenerateIso'params))
instance (C.Parse Session'cloudInitGenerateIso'params (C.Parsed Session'cloudInitGenerateIso'params)) where
    parse raw_ = (Session'cloudInitGenerateIso'params <$> (GH.parseField #targetDir raw_)
                                                      <*> (GH.parseField #userData raw_)
                                                      <*> (GH.parseField #metaData raw_)
                                                      <*> (GH.parseField #networkConfig raw_)
                                                      <*> (GH.parseField #hasNetworkConfig raw_))
instance (C.Marshal Session'cloudInitGenerateIso'params (C.Parsed Session'cloudInitGenerateIso'params)) where
    marshalInto raw_ Session'cloudInitGenerateIso'params{..} = (do
        (GH.encodeField #targetDir targetDir raw_)
        (GH.encodeField #userData userData raw_)
        (GH.encodeField #metaData metaData raw_)
        (GH.encodeField #networkConfig networkConfig raw_)
        (GH.encodeField #hasNetworkConfig hasNetworkConfig raw_)
        (Std_.pure ())
        )
instance (GH.HasField "targetDir" GH.Slot Session'cloudInitGenerateIso'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "userData" GH.Slot Session'cloudInitGenerateIso'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "metaData" GH.Slot Session'cloudInitGenerateIso'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "networkConfig" GH.Slot Session'cloudInitGenerateIso'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "hasNetworkConfig" GH.Slot Session'cloudInitGenerateIso'params Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
data Session'cloudInitGenerateIso'results 
type instance (R.ReprFor Session'cloudInitGenerateIso'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'cloudInitGenerateIso'results) where
    typeId  = 13707265026989532345
instance (C.TypedStruct Session'cloudInitGenerateIso'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'cloudInitGenerateIso'results) where
    type AllocHint Session'cloudInitGenerateIso'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'cloudInitGenerateIso'results (C.Parsed Session'cloudInitGenerateIso'results))
instance (C.AllocateList Session'cloudInitGenerateIso'results) where
    type ListAllocHint Session'cloudInitGenerateIso'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'cloudInitGenerateIso'results (C.Parsed Session'cloudInitGenerateIso'results))
data instance C.Parsed Session'cloudInitGenerateIso'results
    = Session'cloudInitGenerateIso'results 
        {isoPath :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'cloudInitGenerateIso'results))
deriving instance (Std_.Eq (C.Parsed Session'cloudInitGenerateIso'results))
instance (C.Parse Session'cloudInitGenerateIso'results (C.Parsed Session'cloudInitGenerateIso'results)) where
    parse raw_ = (Session'cloudInitGenerateIso'results <$> (GH.parseField #isoPath raw_))
instance (C.Marshal Session'cloudInitGenerateIso'results (C.Parsed Session'cloudInitGenerateIso'results)) where
    marshalInto raw_ Session'cloudInitGenerateIso'results{..} = (do
        (GH.encodeField #isoPath isoPath raw_)
        (Std_.pure ())
        )
instance (GH.HasField "isoPath" GH.Slot Session'cloudInitGenerateIso'results Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data DiskOpResult 
type instance (R.ReprFor DiskOpResult) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskOpResult) where
    typeId  = 12446158261308417967
instance (C.TypedStruct DiskOpResult) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate DiskOpResult) where
    type AllocHint DiskOpResult = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskOpResult (C.Parsed DiskOpResult))
instance (C.AllocateList DiskOpResult) where
    type ListAllocHint DiskOpResult = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskOpResult (C.Parsed DiskOpResult))
data instance C.Parsed DiskOpResult
    = DiskOpResult 
        {kind :: (RP.Parsed DiskOpKind)
        ,message :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskOpResult))
deriving instance (Std_.Eq (C.Parsed DiskOpResult))
instance (C.Parse DiskOpResult (C.Parsed DiskOpResult)) where
    parse raw_ = (DiskOpResult <$> (GH.parseField #kind raw_)
                               <*> (GH.parseField #message raw_))
instance (C.Marshal DiskOpResult (C.Parsed DiskOpResult)) where
    marshalInto raw_ DiskOpResult{..} = (do
        (GH.encodeField #kind kind raw_)
        (GH.encodeField #message message raw_)
        (Std_.pure ())
        )
instance (GH.HasField "kind" GH.Slot DiskOpResult DiskOpKind) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "message" GH.Slot DiskOpResult Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data DiskOpKind 
    = DiskOpKind'success 
    | DiskOpKind'errorGeneric 
    | DiskOpKind'errorNotFound 
    | DiskOpKind'errorFormatUnsupported 
    | DiskOpKind'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor DiskOpKind) = (R.Data R.Sz16)
instance (C.HasTypeId DiskOpKind) where
    typeId  = 12037334094114664910
instance (Std_.Enum DiskOpKind) where
    toEnum n_ = case n_ of
        0 ->
            DiskOpKind'success
        1 ->
            DiskOpKind'errorGeneric
        2 ->
            DiskOpKind'errorNotFound
        3 ->
            DiskOpKind'errorFormatUnsupported
        tag_ ->
            (DiskOpKind'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (DiskOpKind'success) ->
            0
        (DiskOpKind'errorGeneric) ->
            1
        (DiskOpKind'errorNotFound) ->
            2
        (DiskOpKind'errorFormatUnsupported) ->
            3
        (DiskOpKind'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord DiskOpKind) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse DiskOpKind DiskOpKind) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList DiskOpKind) where
    type ListAllocHint DiskOpKind = Std_.Int
instance (C.EstimateListAlloc DiskOpKind DiskOpKind)
data DiskInspectInfo 
type instance (R.ReprFor DiskInspectInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskInspectInfo) where
    typeId  = 10271191268044001535
instance (C.TypedStruct DiskInspectInfo) where
    numStructWords  = 3
    numStructPtrs  = 2
instance (C.Allocate DiskInspectInfo) where
    type AllocHint DiskInspectInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskInspectInfo (C.Parsed DiskInspectInfo))
instance (C.AllocateList DiskInspectInfo) where
    type ListAllocHint DiskInspectInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskInspectInfo (C.Parsed DiskInspectInfo))
data instance C.Parsed DiskInspectInfo
    = DiskInspectInfo 
        {format :: (RP.Parsed Basics.Text)
        ,virtualSizeMb :: (RP.Parsed Std_.Int64)
        ,actualSizeMb :: (RP.Parsed Std_.Int64)
        ,hasActualSize :: (RP.Parsed Std_.Bool)
        ,snapshots :: (RP.Parsed (R.List DiskSnapshotInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskInspectInfo))
deriving instance (Std_.Eq (C.Parsed DiskInspectInfo))
instance (C.Parse DiskInspectInfo (C.Parsed DiskInspectInfo)) where
    parse raw_ = (DiskInspectInfo <$> (GH.parseField #format raw_)
                                  <*> (GH.parseField #virtualSizeMb raw_)
                                  <*> (GH.parseField #actualSizeMb raw_)
                                  <*> (GH.parseField #hasActualSize raw_)
                                  <*> (GH.parseField #snapshots raw_))
instance (C.Marshal DiskInspectInfo (C.Parsed DiskInspectInfo)) where
    marshalInto raw_ DiskInspectInfo{..} = (do
        (GH.encodeField #format format raw_)
        (GH.encodeField #virtualSizeMb virtualSizeMb raw_)
        (GH.encodeField #actualSizeMb actualSizeMb raw_)
        (GH.encodeField #hasActualSize hasActualSize raw_)
        (GH.encodeField #snapshots snapshots raw_)
        (Std_.pure ())
        )
instance (GH.HasField "format" GH.Slot DiskInspectInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "virtualSizeMb" GH.Slot DiskInspectInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "actualSizeMb" GH.Slot DiskInspectInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "hasActualSize" GH.Slot DiskInspectInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 2 1 0)
instance (GH.HasField "snapshots" GH.Slot DiskInspectInfo (R.List DiskSnapshotInfo)) where
    fieldByLabel  = (GH.ptrField 1)
data DiskSnapshotInfo 
type instance (R.ReprFor DiskSnapshotInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskSnapshotInfo) where
    typeId  = 10776493868898389048
instance (C.TypedStruct DiskSnapshotInfo) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate DiskSnapshotInfo) where
    type AllocHint DiskSnapshotInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskSnapshotInfo (C.Parsed DiskSnapshotInfo))
instance (C.AllocateList DiskSnapshotInfo) where
    type ListAllocHint DiskSnapshotInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskSnapshotInfo (C.Parsed DiskSnapshotInfo))
data instance C.Parsed DiskSnapshotInfo
    = DiskSnapshotInfo 
        {id :: (RP.Parsed Basics.Text)
        ,name :: (RP.Parsed Basics.Text)
        ,sizeMb :: (RP.Parsed Std_.Int64)
        ,hasSize :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskSnapshotInfo))
deriving instance (Std_.Eq (C.Parsed DiskSnapshotInfo))
instance (C.Parse DiskSnapshotInfo (C.Parsed DiskSnapshotInfo)) where
    parse raw_ = (DiskSnapshotInfo <$> (GH.parseField #id raw_)
                                   <*> (GH.parseField #name raw_)
                                   <*> (GH.parseField #sizeMb raw_)
                                   <*> (GH.parseField #hasSize raw_))
instance (C.Marshal DiskSnapshotInfo (C.Parsed DiskSnapshotInfo)) where
    marshalInto raw_ DiskSnapshotInfo{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #sizeMb sizeMb raw_)
        (GH.encodeField #hasSize hasSize raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot DiskSnapshotInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "name" GH.Slot DiskSnapshotInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "sizeMb" GH.Slot DiskSnapshotInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "hasSize" GH.Slot DiskSnapshotInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 1 1 0)