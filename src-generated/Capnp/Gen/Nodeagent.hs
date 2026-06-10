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
import qualified Capnp.Gen.ById.X9bd452a518ed3917
import qualified Capnp.Gen.ById.Xa7366eabdb0b1db4
import qualified Capnp.Gen.ById.Xbf9b09f64c0dd40d
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
                                                                           ,(GH.toUntypedMethodHandler ((nodeAgent'version) s_))
                                                                           ,(GH.toUntypedMethodHandler ((nodeAgent'defaultBasePath) s_))] [])
class (NodeAgent'server_ s_) where
    {-# MINIMAL nodeAgent'session,nodeAgent'ping,nodeAgent'version,nodeAgent'defaultBasePath #-}
    nodeAgent'session :: s_ -> (GH.MethodHandler NodeAgent'session'params NodeAgent'session'results)
    nodeAgent'session _ = GH.methodUnimplemented
    nodeAgent'ping :: s_ -> (GH.MethodHandler NodeAgent'ping'params NodeAgent'ping'results)
    nodeAgent'ping _ = GH.methodUnimplemented
    nodeAgent'version :: s_ -> (GH.MethodHandler NodeAgent'version'params NodeAgent'version'results)
    nodeAgent'version _ = GH.methodUnimplemented
    nodeAgent'defaultBasePath :: s_ -> (GH.MethodHandler NodeAgent'defaultBasePath'params NodeAgent'defaultBasePath'results)
    nodeAgent'defaultBasePath _ = GH.methodUnimplemented
instance (GH.HasMethod "session" NodeAgent NodeAgent'session'params NodeAgent'session'results) where
    methodByLabel  = (GH.Method 15897469928431442643 0)
instance (GH.HasMethod "ping" NodeAgent NodeAgent'ping'params NodeAgent'ping'results) where
    methodByLabel  = (GH.Method 15897469928431442643 1)
instance (GH.HasMethod "version" NodeAgent NodeAgent'version'params NodeAgent'version'results) where
    methodByLabel  = (GH.Method 15897469928431442643 2)
instance (GH.HasMethod "defaultBasePath" NodeAgent NodeAgent'defaultBasePath'params NodeAgent'defaultBasePath'results) where
    methodByLabel  = (GH.Method 15897469928431442643 3)
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
data NodeAgent'defaultBasePath'params 
type instance (R.ReprFor NodeAgent'defaultBasePath'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeAgent'defaultBasePath'params) where
    typeId  = 16608591501214317651
instance (C.TypedStruct NodeAgent'defaultBasePath'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate NodeAgent'defaultBasePath'params) where
    type AllocHint NodeAgent'defaultBasePath'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeAgent'defaultBasePath'params (C.Parsed NodeAgent'defaultBasePath'params))
instance (C.AllocateList NodeAgent'defaultBasePath'params) where
    type ListAllocHint NodeAgent'defaultBasePath'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeAgent'defaultBasePath'params (C.Parsed NodeAgent'defaultBasePath'params))
data instance C.Parsed NodeAgent'defaultBasePath'params
    = NodeAgent'defaultBasePath'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeAgent'defaultBasePath'params))
deriving instance (Std_.Eq (C.Parsed NodeAgent'defaultBasePath'params))
instance (C.Parse NodeAgent'defaultBasePath'params (C.Parsed NodeAgent'defaultBasePath'params)) where
    parse raw_ = (Std_.pure NodeAgent'defaultBasePath'params)
instance (C.Marshal NodeAgent'defaultBasePath'params (C.Parsed NodeAgent'defaultBasePath'params)) where
    marshalInto _raw (NodeAgent'defaultBasePath'params) = (Std_.pure ())
data NodeAgent'defaultBasePath'results 
type instance (R.ReprFor NodeAgent'defaultBasePath'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeAgent'defaultBasePath'results) where
    typeId  = 14042383546414730042
instance (C.TypedStruct NodeAgent'defaultBasePath'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NodeAgent'defaultBasePath'results) where
    type AllocHint NodeAgent'defaultBasePath'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeAgent'defaultBasePath'results (C.Parsed NodeAgent'defaultBasePath'results))
instance (C.AllocateList NodeAgent'defaultBasePath'results) where
    type ListAllocHint NodeAgent'defaultBasePath'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeAgent'defaultBasePath'results (C.Parsed NodeAgent'defaultBasePath'results))
data instance C.Parsed NodeAgent'defaultBasePath'results
    = NodeAgent'defaultBasePath'results 
        {path :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeAgent'defaultBasePath'results))
deriving instance (Std_.Eq (C.Parsed NodeAgent'defaultBasePath'results))
instance (C.Parse NodeAgent'defaultBasePath'results (C.Parsed NodeAgent'defaultBasePath'results)) where
    parse raw_ = (NodeAgent'defaultBasePath'results <$> (GH.parseField #path raw_))
instance (C.Marshal NodeAgent'defaultBasePath'results (C.Parsed NodeAgent'defaultBasePath'results)) where
    marshalInto raw_ NodeAgent'defaultBasePath'results{..} = (do
        (GH.encodeField #path path raw_)
        (Std_.pure ())
        )
instance (GH.HasField "path" GH.Slot NodeAgent'defaultBasePath'results Basics.Text) where
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
                                                                         ,(GH.toUntypedMethodHandler ((session'cloudInitGenerateIso) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'vmStart) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'vmStopGraceful) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'vmStopHard) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'vmPause) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'vmResume) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'vmGuestExec) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'vmStatus) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'vmSetSpiceTicket) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'subscribeVmStatus) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'openSerialConsole) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'openHmpMonitor) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'flushSerialConsole) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'flushHmpMonitor) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'vmAttachDrive) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'vmDetachDrive) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'probeVsockCid) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'diskOpenRead) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'attachReader) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'diskImportFromPeer) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'vmGuestExecStream) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'vmSave) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'deleteSavedState) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'snapshotCreateLive) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'snapshotDeleteLive) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'snapshotCreateLiveMany) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'snapshotCreateWithVmstate) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'snapshotLoadWithVmstate) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'snapshotDeleteWithVmstate) s_))
                                                                         ,(GH.toUntypedMethodHandler ((session'guestSetTime) s_))] [])
class (Session'server_ s_) where
    {-# MINIMAL session'ping,session'diskCreate,session'diskCreateOverlay,session'diskDelete,session'diskResize,session'diskRebase,session'diskClone,session'diskInspect,session'snapshotCreate,session'snapshotDelete,session'snapshotRollback,session'diskDownload,session'diskDecompressXz,session'diskMd5,session'cloudInitGenerateIso,session'vmStart,session'vmStopGraceful,session'vmStopHard,session'vmPause,session'vmResume,session'vmGuestExec,session'vmStatus,session'vmSetSpiceTicket,session'subscribeVmStatus,session'openSerialConsole,session'openHmpMonitor,session'flushSerialConsole,session'flushHmpMonitor,session'vmAttachDrive,session'vmDetachDrive,session'probeVsockCid,session'diskOpenRead,session'attachReader,session'diskImportFromPeer,session'vmGuestExecStream,session'vmSave,session'deleteSavedState,session'snapshotCreateLive,session'snapshotDeleteLive,session'snapshotCreateLiveMany,session'snapshotCreateWithVmstate,session'snapshotLoadWithVmstate,session'snapshotDeleteWithVmstate,session'guestSetTime #-}
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
    session'vmStart :: s_ -> (GH.MethodHandler Session'vmStart'params Session'vmStart'results)
    session'vmStart _ = GH.methodUnimplemented
    session'vmStopGraceful :: s_ -> (GH.MethodHandler Session'vmStopGraceful'params Session'vmStopGraceful'results)
    session'vmStopGraceful _ = GH.methodUnimplemented
    session'vmStopHard :: s_ -> (GH.MethodHandler Session'vmStopHard'params Session'vmStopHard'results)
    session'vmStopHard _ = GH.methodUnimplemented
    session'vmPause :: s_ -> (GH.MethodHandler Session'vmPause'params Session'vmPause'results)
    session'vmPause _ = GH.methodUnimplemented
    session'vmResume :: s_ -> (GH.MethodHandler Session'vmResume'params Session'vmResume'results)
    session'vmResume _ = GH.methodUnimplemented
    session'vmGuestExec :: s_ -> (GH.MethodHandler Session'vmGuestExec'params Session'vmGuestExec'results)
    session'vmGuestExec _ = GH.methodUnimplemented
    session'vmStatus :: s_ -> (GH.MethodHandler Session'vmStatus'params Session'vmStatus'results)
    session'vmStatus _ = GH.methodUnimplemented
    session'vmSetSpiceTicket :: s_ -> (GH.MethodHandler Session'vmSetSpiceTicket'params Session'vmSetSpiceTicket'results)
    session'vmSetSpiceTicket _ = GH.methodUnimplemented
    session'subscribeVmStatus :: s_ -> (GH.MethodHandler Session'subscribeVmStatus'params Session'subscribeVmStatus'results)
    session'subscribeVmStatus _ = GH.methodUnimplemented
    session'openSerialConsole :: s_ -> (GH.MethodHandler Session'openSerialConsole'params Session'openSerialConsole'results)
    session'openSerialConsole _ = GH.methodUnimplemented
    session'openHmpMonitor :: s_ -> (GH.MethodHandler Session'openHmpMonitor'params Session'openHmpMonitor'results)
    session'openHmpMonitor _ = GH.methodUnimplemented
    session'flushSerialConsole :: s_ -> (GH.MethodHandler Session'flushSerialConsole'params Session'flushSerialConsole'results)
    session'flushSerialConsole _ = GH.methodUnimplemented
    session'flushHmpMonitor :: s_ -> (GH.MethodHandler Session'flushHmpMonitor'params Session'flushHmpMonitor'results)
    session'flushHmpMonitor _ = GH.methodUnimplemented
    session'vmAttachDrive :: s_ -> (GH.MethodHandler Session'vmAttachDrive'params Session'vmAttachDrive'results)
    session'vmAttachDrive _ = GH.methodUnimplemented
    session'vmDetachDrive :: s_ -> (GH.MethodHandler Session'vmDetachDrive'params Session'vmDetachDrive'results)
    session'vmDetachDrive _ = GH.methodUnimplemented
    session'probeVsockCid :: s_ -> (GH.MethodHandler Session'probeVsockCid'params Session'probeVsockCid'results)
    session'probeVsockCid _ = GH.methodUnimplemented
    session'diskOpenRead :: s_ -> (GH.MethodHandler Session'diskOpenRead'params Session'diskOpenRead'results)
    session'diskOpenRead _ = GH.methodUnimplemented
    session'attachReader :: s_ -> (GH.MethodHandler Session'attachReader'params Session'attachReader'results)
    session'attachReader _ = GH.methodUnimplemented
    session'diskImportFromPeer :: s_ -> (GH.MethodHandler Session'diskImportFromPeer'params Session'diskImportFromPeer'results)
    session'diskImportFromPeer _ = GH.methodUnimplemented
    session'vmGuestExecStream :: s_ -> (GH.MethodHandler Session'vmGuestExecStream'params Session'vmGuestExecStream'results)
    session'vmGuestExecStream _ = GH.methodUnimplemented
    session'vmSave :: s_ -> (GH.MethodHandler Session'vmSave'params Session'vmSave'results)
    session'vmSave _ = GH.methodUnimplemented
    session'deleteSavedState :: s_ -> (GH.MethodHandler Session'deleteSavedState'params Session'deleteSavedState'results)
    session'deleteSavedState _ = GH.methodUnimplemented
    session'snapshotCreateLive :: s_ -> (GH.MethodHandler Session'snapshotCreateLive'params Session'snapshotCreateLive'results)
    session'snapshotCreateLive _ = GH.methodUnimplemented
    session'snapshotDeleteLive :: s_ -> (GH.MethodHandler Session'snapshotDeleteLive'params Session'snapshotDeleteLive'results)
    session'snapshotDeleteLive _ = GH.methodUnimplemented
    session'snapshotCreateLiveMany :: s_ -> (GH.MethodHandler Session'snapshotCreateLiveMany'params Session'snapshotCreateLiveMany'results)
    session'snapshotCreateLiveMany _ = GH.methodUnimplemented
    session'snapshotCreateWithVmstate :: s_ -> (GH.MethodHandler Session'snapshotCreateWithVmstate'params Session'snapshotCreateWithVmstate'results)
    session'snapshotCreateWithVmstate _ = GH.methodUnimplemented
    session'snapshotLoadWithVmstate :: s_ -> (GH.MethodHandler Session'snapshotLoadWithVmstate'params Session'snapshotLoadWithVmstate'results)
    session'snapshotLoadWithVmstate _ = GH.methodUnimplemented
    session'snapshotDeleteWithVmstate :: s_ -> (GH.MethodHandler Session'snapshotDeleteWithVmstate'params Session'snapshotDeleteWithVmstate'results)
    session'snapshotDeleteWithVmstate _ = GH.methodUnimplemented
    session'guestSetTime :: s_ -> (GH.MethodHandler Session'guestSetTime'params Session'guestSetTime'results)
    session'guestSetTime _ = GH.methodUnimplemented
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
instance (GH.HasMethod "vmStart" Session Session'vmStart'params Session'vmStart'results) where
    methodByLabel  = (GH.Method 11450192344861352079 15)
instance (GH.HasMethod "vmStopGraceful" Session Session'vmStopGraceful'params Session'vmStopGraceful'results) where
    methodByLabel  = (GH.Method 11450192344861352079 16)
instance (GH.HasMethod "vmStopHard" Session Session'vmStopHard'params Session'vmStopHard'results) where
    methodByLabel  = (GH.Method 11450192344861352079 17)
instance (GH.HasMethod "vmPause" Session Session'vmPause'params Session'vmPause'results) where
    methodByLabel  = (GH.Method 11450192344861352079 18)
instance (GH.HasMethod "vmResume" Session Session'vmResume'params Session'vmResume'results) where
    methodByLabel  = (GH.Method 11450192344861352079 19)
instance (GH.HasMethod "vmGuestExec" Session Session'vmGuestExec'params Session'vmGuestExec'results) where
    methodByLabel  = (GH.Method 11450192344861352079 20)
instance (GH.HasMethod "vmStatus" Session Session'vmStatus'params Session'vmStatus'results) where
    methodByLabel  = (GH.Method 11450192344861352079 21)
instance (GH.HasMethod "vmSetSpiceTicket" Session Session'vmSetSpiceTicket'params Session'vmSetSpiceTicket'results) where
    methodByLabel  = (GH.Method 11450192344861352079 22)
instance (GH.HasMethod "subscribeVmStatus" Session Session'subscribeVmStatus'params Session'subscribeVmStatus'results) where
    methodByLabel  = (GH.Method 11450192344861352079 23)
instance (GH.HasMethod "openSerialConsole" Session Session'openSerialConsole'params Session'openSerialConsole'results) where
    methodByLabel  = (GH.Method 11450192344861352079 24)
instance (GH.HasMethod "openHmpMonitor" Session Session'openHmpMonitor'params Session'openHmpMonitor'results) where
    methodByLabel  = (GH.Method 11450192344861352079 25)
instance (GH.HasMethod "flushSerialConsole" Session Session'flushSerialConsole'params Session'flushSerialConsole'results) where
    methodByLabel  = (GH.Method 11450192344861352079 26)
instance (GH.HasMethod "flushHmpMonitor" Session Session'flushHmpMonitor'params Session'flushHmpMonitor'results) where
    methodByLabel  = (GH.Method 11450192344861352079 27)
instance (GH.HasMethod "vmAttachDrive" Session Session'vmAttachDrive'params Session'vmAttachDrive'results) where
    methodByLabel  = (GH.Method 11450192344861352079 28)
instance (GH.HasMethod "vmDetachDrive" Session Session'vmDetachDrive'params Session'vmDetachDrive'results) where
    methodByLabel  = (GH.Method 11450192344861352079 29)
instance (GH.HasMethod "probeVsockCid" Session Session'probeVsockCid'params Session'probeVsockCid'results) where
    methodByLabel  = (GH.Method 11450192344861352079 30)
instance (GH.HasMethod "diskOpenRead" Session Session'diskOpenRead'params Session'diskOpenRead'results) where
    methodByLabel  = (GH.Method 11450192344861352079 31)
instance (GH.HasMethod "attachReader" Session Session'attachReader'params Session'attachReader'results) where
    methodByLabel  = (GH.Method 11450192344861352079 32)
instance (GH.HasMethod "diskImportFromPeer" Session Session'diskImportFromPeer'params Session'diskImportFromPeer'results) where
    methodByLabel  = (GH.Method 11450192344861352079 33)
instance (GH.HasMethod "vmGuestExecStream" Session Session'vmGuestExecStream'params Session'vmGuestExecStream'results) where
    methodByLabel  = (GH.Method 11450192344861352079 34)
instance (GH.HasMethod "vmSave" Session Session'vmSave'params Session'vmSave'results) where
    methodByLabel  = (GH.Method 11450192344861352079 35)
instance (GH.HasMethod "deleteSavedState" Session Session'deleteSavedState'params Session'deleteSavedState'results) where
    methodByLabel  = (GH.Method 11450192344861352079 36)
instance (GH.HasMethod "snapshotCreateLive" Session Session'snapshotCreateLive'params Session'snapshotCreateLive'results) where
    methodByLabel  = (GH.Method 11450192344861352079 37)
instance (GH.HasMethod "snapshotDeleteLive" Session Session'snapshotDeleteLive'params Session'snapshotDeleteLive'results) where
    methodByLabel  = (GH.Method 11450192344861352079 38)
instance (GH.HasMethod "snapshotCreateLiveMany" Session Session'snapshotCreateLiveMany'params Session'snapshotCreateLiveMany'results) where
    methodByLabel  = (GH.Method 11450192344861352079 39)
instance (GH.HasMethod "snapshotCreateWithVmstate" Session Session'snapshotCreateWithVmstate'params Session'snapshotCreateWithVmstate'results) where
    methodByLabel  = (GH.Method 11450192344861352079 40)
instance (GH.HasMethod "snapshotLoadWithVmstate" Session Session'snapshotLoadWithVmstate'params Session'snapshotLoadWithVmstate'results) where
    methodByLabel  = (GH.Method 11450192344861352079 41)
instance (GH.HasMethod "snapshotDeleteWithVmstate" Session Session'snapshotDeleteWithVmstate'params Session'snapshotDeleteWithVmstate'results) where
    methodByLabel  = (GH.Method 11450192344861352079 42)
instance (GH.HasMethod "guestSetTime" Session Session'guestSetTime'params Session'guestSetTime'results) where
    methodByLabel  = (GH.Method 11450192344861352079 43)
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
    numStructPtrs  = 3
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
        ,destPath :: (RP.Parsed Basics.Text)
        ,destFormat :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskClone'params))
deriving instance (Std_.Eq (C.Parsed Session'diskClone'params))
instance (C.Parse Session'diskClone'params (C.Parsed Session'diskClone'params)) where
    parse raw_ = (Session'diskClone'params <$> (GH.parseField #sourcePath raw_)
                                           <*> (GH.parseField #destPath raw_)
                                           <*> (GH.parseField #destFormat raw_))
instance (C.Marshal Session'diskClone'params (C.Parsed Session'diskClone'params)) where
    marshalInto raw_ Session'diskClone'params{..} = (do
        (GH.encodeField #sourcePath sourcePath raw_)
        (GH.encodeField #destPath destPath raw_)
        (GH.encodeField #destFormat destFormat raw_)
        (Std_.pure ())
        )
instance (GH.HasField "sourcePath" GH.Slot Session'diskClone'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "destPath" GH.Slot Session'diskClone'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "destFormat" GH.Slot Session'diskClone'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
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
    numStructPtrs  = 3
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
        ,url :: (RP.Parsed Basics.Text)
        ,sink :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.DiskDownloadSink)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskDownload'params))
deriving instance (Std_.Eq (C.Parsed Session'diskDownload'params))
instance (C.Parse Session'diskDownload'params (C.Parsed Session'diskDownload'params)) where
    parse raw_ = (Session'diskDownload'params <$> (GH.parseField #destPath raw_)
                                              <*> (GH.parseField #url raw_)
                                              <*> (GH.parseField #sink raw_))
instance (C.Marshal Session'diskDownload'params (C.Parsed Session'diskDownload'params)) where
    marshalInto raw_ Session'diskDownload'params{..} = (do
        (GH.encodeField #destPath destPath raw_)
        (GH.encodeField #url url raw_)
        (GH.encodeField #sink sink raw_)
        (Std_.pure ())
        )
instance (GH.HasField "destPath" GH.Slot Session'diskDownload'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "url" GH.Slot Session'diskDownload'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "sink" GH.Slot Session'diskDownload'params Capnp.Gen.ById.X9bd452a518ed3917.DiskDownloadSink) where
    fieldByLabel  = (GH.ptrField 2)
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
data Session'vmStart'params 
type instance (R.ReprFor Session'vmStart'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmStart'params) where
    typeId  = 10823871877857771010
instance (C.TypedStruct Session'vmStart'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'vmStart'params) where
    type AllocHint Session'vmStart'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmStart'params (C.Parsed Session'vmStart'params))
instance (C.AllocateList Session'vmStart'params) where
    type ListAllocHint Session'vmStart'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmStart'params (C.Parsed Session'vmStart'params))
data instance C.Parsed Session'vmStart'params
    = Session'vmStart'params 
        {spec :: (RP.Parsed VmSpec)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmStart'params))
deriving instance (Std_.Eq (C.Parsed Session'vmStart'params))
instance (C.Parse Session'vmStart'params (C.Parsed Session'vmStart'params)) where
    parse raw_ = (Session'vmStart'params <$> (GH.parseField #spec raw_))
instance (C.Marshal Session'vmStart'params (C.Parsed Session'vmStart'params)) where
    marshalInto raw_ Session'vmStart'params{..} = (do
        (GH.encodeField #spec spec raw_)
        (Std_.pure ())
        )
instance (GH.HasField "spec" GH.Slot Session'vmStart'params VmSpec) where
    fieldByLabel  = (GH.ptrField 0)
data Session'vmStart'results 
type instance (R.ReprFor Session'vmStart'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmStart'results) where
    typeId  = 12248210371593590372
instance (C.TypedStruct Session'vmStart'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'vmStart'results) where
    type AllocHint Session'vmStart'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmStart'results (C.Parsed Session'vmStart'results))
instance (C.AllocateList Session'vmStart'results) where
    type ListAllocHint Session'vmStart'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmStart'results (C.Parsed Session'vmStart'results))
data instance C.Parsed Session'vmStart'results
    = Session'vmStart'results 
        {info :: (RP.Parsed VmRuntimeInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmStart'results))
deriving instance (Std_.Eq (C.Parsed Session'vmStart'results))
instance (C.Parse Session'vmStart'results (C.Parsed Session'vmStart'results)) where
    parse raw_ = (Session'vmStart'results <$> (GH.parseField #info raw_))
instance (C.Marshal Session'vmStart'results (C.Parsed Session'vmStart'results)) where
    marshalInto raw_ Session'vmStart'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot Session'vmStart'results VmRuntimeInfo) where
    fieldByLabel  = (GH.ptrField 0)
data Session'vmStopGraceful'params 
type instance (R.ReprFor Session'vmStopGraceful'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmStopGraceful'params) where
    typeId  = 16142730254105394244
instance (C.TypedStruct Session'vmStopGraceful'params) where
    numStructWords  = 2
    numStructPtrs  = 0
instance (C.Allocate Session'vmStopGraceful'params) where
    type AllocHint Session'vmStopGraceful'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmStopGraceful'params (C.Parsed Session'vmStopGraceful'params))
instance (C.AllocateList Session'vmStopGraceful'params) where
    type ListAllocHint Session'vmStopGraceful'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmStopGraceful'params (C.Parsed Session'vmStopGraceful'params))
data instance C.Parsed Session'vmStopGraceful'params
    = Session'vmStopGraceful'params 
        {vmId :: (RP.Parsed Std_.Int64)
        ,timeoutSec :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmStopGraceful'params))
deriving instance (Std_.Eq (C.Parsed Session'vmStopGraceful'params))
instance (C.Parse Session'vmStopGraceful'params (C.Parsed Session'vmStopGraceful'params)) where
    parse raw_ = (Session'vmStopGraceful'params <$> (GH.parseField #vmId raw_)
                                                <*> (GH.parseField #timeoutSec raw_))
instance (C.Marshal Session'vmStopGraceful'params (C.Parsed Session'vmStopGraceful'params)) where
    marshalInto raw_ Session'vmStopGraceful'params{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (GH.encodeField #timeoutSec timeoutSec raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot Session'vmStopGraceful'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "timeoutSec" GH.Slot Session'vmStopGraceful'params Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
data Session'vmStopGraceful'results 
type instance (R.ReprFor Session'vmStopGraceful'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmStopGraceful'results) where
    typeId  = 11443307811371017683
instance (C.TypedStruct Session'vmStopGraceful'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'vmStopGraceful'results) where
    type AllocHint Session'vmStopGraceful'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmStopGraceful'results (C.Parsed Session'vmStopGraceful'results))
instance (C.AllocateList Session'vmStopGraceful'results) where
    type ListAllocHint Session'vmStopGraceful'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmStopGraceful'results (C.Parsed Session'vmStopGraceful'results))
data instance C.Parsed Session'vmStopGraceful'results
    = Session'vmStopGraceful'results 
        {result :: (RP.Parsed VmStopResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmStopGraceful'results))
deriving instance (Std_.Eq (C.Parsed Session'vmStopGraceful'results))
instance (C.Parse Session'vmStopGraceful'results (C.Parsed Session'vmStopGraceful'results)) where
    parse raw_ = (Session'vmStopGraceful'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'vmStopGraceful'results (C.Parsed Session'vmStopGraceful'results)) where
    marshalInto raw_ Session'vmStopGraceful'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'vmStopGraceful'results VmStopResult) where
    fieldByLabel  = (GH.ptrField 0)
data Session'vmStopHard'params 
type instance (R.ReprFor Session'vmStopHard'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmStopHard'params) where
    typeId  = 18426405290603545736
instance (C.TypedStruct Session'vmStopHard'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Session'vmStopHard'params) where
    type AllocHint Session'vmStopHard'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmStopHard'params (C.Parsed Session'vmStopHard'params))
instance (C.AllocateList Session'vmStopHard'params) where
    type ListAllocHint Session'vmStopHard'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmStopHard'params (C.Parsed Session'vmStopHard'params))
data instance C.Parsed Session'vmStopHard'params
    = Session'vmStopHard'params 
        {vmId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmStopHard'params))
deriving instance (Std_.Eq (C.Parsed Session'vmStopHard'params))
instance (C.Parse Session'vmStopHard'params (C.Parsed Session'vmStopHard'params)) where
    parse raw_ = (Session'vmStopHard'params <$> (GH.parseField #vmId raw_))
instance (C.Marshal Session'vmStopHard'params (C.Parsed Session'vmStopHard'params)) where
    marshalInto raw_ Session'vmStopHard'params{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot Session'vmStopHard'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Session'vmStopHard'results 
type instance (R.ReprFor Session'vmStopHard'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmStopHard'results) where
    typeId  = 11350962112209429015
instance (C.TypedStruct Session'vmStopHard'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'vmStopHard'results) where
    type AllocHint Session'vmStopHard'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmStopHard'results (C.Parsed Session'vmStopHard'results))
instance (C.AllocateList Session'vmStopHard'results) where
    type ListAllocHint Session'vmStopHard'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmStopHard'results (C.Parsed Session'vmStopHard'results))
data instance C.Parsed Session'vmStopHard'results
    = Session'vmStopHard'results 
        {result :: (RP.Parsed VmStopResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmStopHard'results))
deriving instance (Std_.Eq (C.Parsed Session'vmStopHard'results))
instance (C.Parse Session'vmStopHard'results (C.Parsed Session'vmStopHard'results)) where
    parse raw_ = (Session'vmStopHard'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'vmStopHard'results (C.Parsed Session'vmStopHard'results)) where
    marshalInto raw_ Session'vmStopHard'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'vmStopHard'results VmStopResult) where
    fieldByLabel  = (GH.ptrField 0)
data Session'vmPause'params 
type instance (R.ReprFor Session'vmPause'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmPause'params) where
    typeId  = 10346172043406008825
instance (C.TypedStruct Session'vmPause'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Session'vmPause'params) where
    type AllocHint Session'vmPause'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmPause'params (C.Parsed Session'vmPause'params))
instance (C.AllocateList Session'vmPause'params) where
    type ListAllocHint Session'vmPause'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmPause'params (C.Parsed Session'vmPause'params))
data instance C.Parsed Session'vmPause'params
    = Session'vmPause'params 
        {vmId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmPause'params))
deriving instance (Std_.Eq (C.Parsed Session'vmPause'params))
instance (C.Parse Session'vmPause'params (C.Parsed Session'vmPause'params)) where
    parse raw_ = (Session'vmPause'params <$> (GH.parseField #vmId raw_))
instance (C.Marshal Session'vmPause'params (C.Parsed Session'vmPause'params)) where
    marshalInto raw_ Session'vmPause'params{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot Session'vmPause'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Session'vmPause'results 
type instance (R.ReprFor Session'vmPause'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmPause'results) where
    typeId  = 11461267413760495112
instance (C.TypedStruct Session'vmPause'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'vmPause'results) where
    type AllocHint Session'vmPause'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmPause'results (C.Parsed Session'vmPause'results))
instance (C.AllocateList Session'vmPause'results) where
    type ListAllocHint Session'vmPause'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmPause'results (C.Parsed Session'vmPause'results))
data instance C.Parsed Session'vmPause'results
    = Session'vmPause'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmPause'results))
deriving instance (Std_.Eq (C.Parsed Session'vmPause'results))
instance (C.Parse Session'vmPause'results (C.Parsed Session'vmPause'results)) where
    parse raw_ = (Std_.pure Session'vmPause'results)
instance (C.Marshal Session'vmPause'results (C.Parsed Session'vmPause'results)) where
    marshalInto _raw (Session'vmPause'results) = (Std_.pure ())
data Session'vmResume'params 
type instance (R.ReprFor Session'vmResume'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmResume'params) where
    typeId  = 17633964403724124090
instance (C.TypedStruct Session'vmResume'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Session'vmResume'params) where
    type AllocHint Session'vmResume'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmResume'params (C.Parsed Session'vmResume'params))
instance (C.AllocateList Session'vmResume'params) where
    type ListAllocHint Session'vmResume'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmResume'params (C.Parsed Session'vmResume'params))
data instance C.Parsed Session'vmResume'params
    = Session'vmResume'params 
        {vmId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmResume'params))
deriving instance (Std_.Eq (C.Parsed Session'vmResume'params))
instance (C.Parse Session'vmResume'params (C.Parsed Session'vmResume'params)) where
    parse raw_ = (Session'vmResume'params <$> (GH.parseField #vmId raw_))
instance (C.Marshal Session'vmResume'params (C.Parsed Session'vmResume'params)) where
    marshalInto raw_ Session'vmResume'params{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot Session'vmResume'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Session'vmResume'results 
type instance (R.ReprFor Session'vmResume'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmResume'results) where
    typeId  = 14290231988743062119
instance (C.TypedStruct Session'vmResume'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'vmResume'results) where
    type AllocHint Session'vmResume'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmResume'results (C.Parsed Session'vmResume'results))
instance (C.AllocateList Session'vmResume'results) where
    type ListAllocHint Session'vmResume'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmResume'results (C.Parsed Session'vmResume'results))
data instance C.Parsed Session'vmResume'results
    = Session'vmResume'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmResume'results))
deriving instance (Std_.Eq (C.Parsed Session'vmResume'results))
instance (C.Parse Session'vmResume'results (C.Parsed Session'vmResume'results)) where
    parse raw_ = (Std_.pure Session'vmResume'results)
instance (C.Marshal Session'vmResume'results (C.Parsed Session'vmResume'results)) where
    marshalInto _raw (Session'vmResume'results) = (Std_.pure ())
data Session'vmGuestExec'params 
type instance (R.ReprFor Session'vmGuestExec'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmGuestExec'params) where
    typeId  = 17580761475470500883
instance (C.TypedStruct Session'vmGuestExec'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'vmGuestExec'params) where
    type AllocHint Session'vmGuestExec'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmGuestExec'params (C.Parsed Session'vmGuestExec'params))
instance (C.AllocateList Session'vmGuestExec'params) where
    type ListAllocHint Session'vmGuestExec'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmGuestExec'params (C.Parsed Session'vmGuestExec'params))
data instance C.Parsed Session'vmGuestExec'params
    = Session'vmGuestExec'params 
        {req :: (RP.Parsed VmGuestExecReq)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmGuestExec'params))
deriving instance (Std_.Eq (C.Parsed Session'vmGuestExec'params))
instance (C.Parse Session'vmGuestExec'params (C.Parsed Session'vmGuestExec'params)) where
    parse raw_ = (Session'vmGuestExec'params <$> (GH.parseField #req raw_))
instance (C.Marshal Session'vmGuestExec'params (C.Parsed Session'vmGuestExec'params)) where
    marshalInto raw_ Session'vmGuestExec'params{..} = (do
        (GH.encodeField #req req raw_)
        (Std_.pure ())
        )
instance (GH.HasField "req" GH.Slot Session'vmGuestExec'params VmGuestExecReq) where
    fieldByLabel  = (GH.ptrField 0)
data Session'vmGuestExec'results 
type instance (R.ReprFor Session'vmGuestExec'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmGuestExec'results) where
    typeId  = 9521999371041989570
instance (C.TypedStruct Session'vmGuestExec'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'vmGuestExec'results) where
    type AllocHint Session'vmGuestExec'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmGuestExec'results (C.Parsed Session'vmGuestExec'results))
instance (C.AllocateList Session'vmGuestExec'results) where
    type ListAllocHint Session'vmGuestExec'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmGuestExec'results (C.Parsed Session'vmGuestExec'results))
data instance C.Parsed Session'vmGuestExec'results
    = Session'vmGuestExec'results 
        {info :: (RP.Parsed VmGuestExecInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmGuestExec'results))
deriving instance (Std_.Eq (C.Parsed Session'vmGuestExec'results))
instance (C.Parse Session'vmGuestExec'results (C.Parsed Session'vmGuestExec'results)) where
    parse raw_ = (Session'vmGuestExec'results <$> (GH.parseField #info raw_))
instance (C.Marshal Session'vmGuestExec'results (C.Parsed Session'vmGuestExec'results)) where
    marshalInto raw_ Session'vmGuestExec'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot Session'vmGuestExec'results VmGuestExecInfo) where
    fieldByLabel  = (GH.ptrField 0)
data Session'vmStatus'params 
type instance (R.ReprFor Session'vmStatus'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmStatus'params) where
    typeId  = 11648773013205736465
instance (C.TypedStruct Session'vmStatus'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Session'vmStatus'params) where
    type AllocHint Session'vmStatus'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmStatus'params (C.Parsed Session'vmStatus'params))
instance (C.AllocateList Session'vmStatus'params) where
    type ListAllocHint Session'vmStatus'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmStatus'params (C.Parsed Session'vmStatus'params))
data instance C.Parsed Session'vmStatus'params
    = Session'vmStatus'params 
        {vmId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmStatus'params))
deriving instance (Std_.Eq (C.Parsed Session'vmStatus'params))
instance (C.Parse Session'vmStatus'params (C.Parsed Session'vmStatus'params)) where
    parse raw_ = (Session'vmStatus'params <$> (GH.parseField #vmId raw_))
instance (C.Marshal Session'vmStatus'params (C.Parsed Session'vmStatus'params)) where
    marshalInto raw_ Session'vmStatus'params{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot Session'vmStatus'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Session'vmStatus'results 
type instance (R.ReprFor Session'vmStatus'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmStatus'results) where
    typeId  = 10964512409179261300
instance (C.TypedStruct Session'vmStatus'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'vmStatus'results) where
    type AllocHint Session'vmStatus'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmStatus'results (C.Parsed Session'vmStatus'results))
instance (C.AllocateList Session'vmStatus'results) where
    type ListAllocHint Session'vmStatus'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmStatus'results (C.Parsed Session'vmStatus'results))
data instance C.Parsed Session'vmStatus'results
    = Session'vmStatus'results 
        {status :: (RP.Parsed VmAgentStatus)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmStatus'results))
deriving instance (Std_.Eq (C.Parsed Session'vmStatus'results))
instance (C.Parse Session'vmStatus'results (C.Parsed Session'vmStatus'results)) where
    parse raw_ = (Session'vmStatus'results <$> (GH.parseField #status raw_))
instance (C.Marshal Session'vmStatus'results (C.Parsed Session'vmStatus'results)) where
    marshalInto raw_ Session'vmStatus'results{..} = (do
        (GH.encodeField #status status raw_)
        (Std_.pure ())
        )
instance (GH.HasField "status" GH.Slot Session'vmStatus'results VmAgentStatus) where
    fieldByLabel  = (GH.ptrField 0)
data Session'vmSetSpiceTicket'params 
type instance (R.ReprFor Session'vmSetSpiceTicket'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmSetSpiceTicket'params) where
    typeId  = 17809974164239182615
instance (C.TypedStruct Session'vmSetSpiceTicket'params) where
    numStructWords  = 2
    numStructPtrs  = 1
instance (C.Allocate Session'vmSetSpiceTicket'params) where
    type AllocHint Session'vmSetSpiceTicket'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmSetSpiceTicket'params (C.Parsed Session'vmSetSpiceTicket'params))
instance (C.AllocateList Session'vmSetSpiceTicket'params) where
    type ListAllocHint Session'vmSetSpiceTicket'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmSetSpiceTicket'params (C.Parsed Session'vmSetSpiceTicket'params))
data instance C.Parsed Session'vmSetSpiceTicket'params
    = Session'vmSetSpiceTicket'params 
        {vmId :: (RP.Parsed Std_.Int64)
        ,password :: (RP.Parsed Basics.Text)
        ,ttlSeconds :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmSetSpiceTicket'params))
deriving instance (Std_.Eq (C.Parsed Session'vmSetSpiceTicket'params))
instance (C.Parse Session'vmSetSpiceTicket'params (C.Parsed Session'vmSetSpiceTicket'params)) where
    parse raw_ = (Session'vmSetSpiceTicket'params <$> (GH.parseField #vmId raw_)
                                                  <*> (GH.parseField #password raw_)
                                                  <*> (GH.parseField #ttlSeconds raw_))
instance (C.Marshal Session'vmSetSpiceTicket'params (C.Parsed Session'vmSetSpiceTicket'params)) where
    marshalInto raw_ Session'vmSetSpiceTicket'params{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (GH.encodeField #password password raw_)
        (GH.encodeField #ttlSeconds ttlSeconds raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot Session'vmSetSpiceTicket'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "password" GH.Slot Session'vmSetSpiceTicket'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "ttlSeconds" GH.Slot Session'vmSetSpiceTicket'params Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
data Session'vmSetSpiceTicket'results 
type instance (R.ReprFor Session'vmSetSpiceTicket'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmSetSpiceTicket'results) where
    typeId  = 15442773020073118915
instance (C.TypedStruct Session'vmSetSpiceTicket'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'vmSetSpiceTicket'results) where
    type AllocHint Session'vmSetSpiceTicket'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmSetSpiceTicket'results (C.Parsed Session'vmSetSpiceTicket'results))
instance (C.AllocateList Session'vmSetSpiceTicket'results) where
    type ListAllocHint Session'vmSetSpiceTicket'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmSetSpiceTicket'results (C.Parsed Session'vmSetSpiceTicket'results))
data instance C.Parsed Session'vmSetSpiceTicket'results
    = Session'vmSetSpiceTicket'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmSetSpiceTicket'results))
deriving instance (Std_.Eq (C.Parsed Session'vmSetSpiceTicket'results))
instance (C.Parse Session'vmSetSpiceTicket'results (C.Parsed Session'vmSetSpiceTicket'results)) where
    parse raw_ = (Std_.pure Session'vmSetSpiceTicket'results)
instance (C.Marshal Session'vmSetSpiceTicket'results (C.Parsed Session'vmSetSpiceTicket'results)) where
    marshalInto _raw (Session'vmSetSpiceTicket'results) = (Std_.pure ())
data Session'subscribeVmStatus'params 
type instance (R.ReprFor Session'subscribeVmStatus'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'subscribeVmStatus'params) where
    typeId  = 13322787620260168359
instance (C.TypedStruct Session'subscribeVmStatus'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'subscribeVmStatus'params) where
    type AllocHint Session'subscribeVmStatus'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'subscribeVmStatus'params (C.Parsed Session'subscribeVmStatus'params))
instance (C.AllocateList Session'subscribeVmStatus'params) where
    type ListAllocHint Session'subscribeVmStatus'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'subscribeVmStatus'params (C.Parsed Session'subscribeVmStatus'params))
data instance C.Parsed Session'subscribeVmStatus'params
    = Session'subscribeVmStatus'params 
        {sink :: (RP.Parsed VmStatusSink)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'subscribeVmStatus'params))
deriving instance (Std_.Eq (C.Parsed Session'subscribeVmStatus'params))
instance (C.Parse Session'subscribeVmStatus'params (C.Parsed Session'subscribeVmStatus'params)) where
    parse raw_ = (Session'subscribeVmStatus'params <$> (GH.parseField #sink raw_))
instance (C.Marshal Session'subscribeVmStatus'params (C.Parsed Session'subscribeVmStatus'params)) where
    marshalInto raw_ Session'subscribeVmStatus'params{..} = (do
        (GH.encodeField #sink sink raw_)
        (Std_.pure ())
        )
instance (GH.HasField "sink" GH.Slot Session'subscribeVmStatus'params VmStatusSink) where
    fieldByLabel  = (GH.ptrField 0)
data Session'subscribeVmStatus'results 
type instance (R.ReprFor Session'subscribeVmStatus'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'subscribeVmStatus'results) where
    typeId  = 9283104170751054662
instance (C.TypedStruct Session'subscribeVmStatus'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'subscribeVmStatus'results) where
    type AllocHint Session'subscribeVmStatus'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'subscribeVmStatus'results (C.Parsed Session'subscribeVmStatus'results))
instance (C.AllocateList Session'subscribeVmStatus'results) where
    type ListAllocHint Session'subscribeVmStatus'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'subscribeVmStatus'results (C.Parsed Session'subscribeVmStatus'results))
data instance C.Parsed Session'subscribeVmStatus'results
    = Session'subscribeVmStatus'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'subscribeVmStatus'results))
deriving instance (Std_.Eq (C.Parsed Session'subscribeVmStatus'results))
instance (C.Parse Session'subscribeVmStatus'results (C.Parsed Session'subscribeVmStatus'results)) where
    parse raw_ = (Std_.pure Session'subscribeVmStatus'results)
instance (C.Marshal Session'subscribeVmStatus'results (C.Parsed Session'subscribeVmStatus'results)) where
    marshalInto _raw (Session'subscribeVmStatus'results) = (Std_.pure ())
data Session'openSerialConsole'params 
type instance (R.ReprFor Session'openSerialConsole'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'openSerialConsole'params) where
    typeId  = 13149968643096890336
instance (C.TypedStruct Session'openSerialConsole'params) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Session'openSerialConsole'params) where
    type AllocHint Session'openSerialConsole'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'openSerialConsole'params (C.Parsed Session'openSerialConsole'params))
instance (C.AllocateList Session'openSerialConsole'params) where
    type ListAllocHint Session'openSerialConsole'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'openSerialConsole'params (C.Parsed Session'openSerialConsole'params))
data instance C.Parsed Session'openSerialConsole'params
    = Session'openSerialConsole'params 
        {vmId :: (RP.Parsed Std_.Int64)
        ,sink :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.ByteSink)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'openSerialConsole'params))
deriving instance (Std_.Eq (C.Parsed Session'openSerialConsole'params))
instance (C.Parse Session'openSerialConsole'params (C.Parsed Session'openSerialConsole'params)) where
    parse raw_ = (Session'openSerialConsole'params <$> (GH.parseField #vmId raw_)
                                                   <*> (GH.parseField #sink raw_))
instance (C.Marshal Session'openSerialConsole'params (C.Parsed Session'openSerialConsole'params)) where
    marshalInto raw_ Session'openSerialConsole'params{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (GH.encodeField #sink sink raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot Session'openSerialConsole'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "sink" GH.Slot Session'openSerialConsole'params Capnp.Gen.ById.X9bd452a518ed3917.ByteSink) where
    fieldByLabel  = (GH.ptrField 0)
data Session'openSerialConsole'results 
type instance (R.ReprFor Session'openSerialConsole'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'openSerialConsole'results) where
    typeId  = 11705289286321711561
instance (C.TypedStruct Session'openSerialConsole'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'openSerialConsole'results) where
    type AllocHint Session'openSerialConsole'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'openSerialConsole'results (C.Parsed Session'openSerialConsole'results))
instance (C.AllocateList Session'openSerialConsole'results) where
    type ListAllocHint Session'openSerialConsole'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'openSerialConsole'results (C.Parsed Session'openSerialConsole'results))
data instance C.Parsed Session'openSerialConsole'results
    = Session'openSerialConsole'results 
        {input :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.ByteSink)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'openSerialConsole'results))
deriving instance (Std_.Eq (C.Parsed Session'openSerialConsole'results))
instance (C.Parse Session'openSerialConsole'results (C.Parsed Session'openSerialConsole'results)) where
    parse raw_ = (Session'openSerialConsole'results <$> (GH.parseField #input raw_))
instance (C.Marshal Session'openSerialConsole'results (C.Parsed Session'openSerialConsole'results)) where
    marshalInto raw_ Session'openSerialConsole'results{..} = (do
        (GH.encodeField #input input raw_)
        (Std_.pure ())
        )
instance (GH.HasField "input" GH.Slot Session'openSerialConsole'results Capnp.Gen.ById.X9bd452a518ed3917.ByteSink) where
    fieldByLabel  = (GH.ptrField 0)
data Session'openHmpMonitor'params 
type instance (R.ReprFor Session'openHmpMonitor'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'openHmpMonitor'params) where
    typeId  = 12373475575530691862
instance (C.TypedStruct Session'openHmpMonitor'params) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Session'openHmpMonitor'params) where
    type AllocHint Session'openHmpMonitor'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'openHmpMonitor'params (C.Parsed Session'openHmpMonitor'params))
instance (C.AllocateList Session'openHmpMonitor'params) where
    type ListAllocHint Session'openHmpMonitor'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'openHmpMonitor'params (C.Parsed Session'openHmpMonitor'params))
data instance C.Parsed Session'openHmpMonitor'params
    = Session'openHmpMonitor'params 
        {vmId :: (RP.Parsed Std_.Int64)
        ,sink :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.ByteSink)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'openHmpMonitor'params))
deriving instance (Std_.Eq (C.Parsed Session'openHmpMonitor'params))
instance (C.Parse Session'openHmpMonitor'params (C.Parsed Session'openHmpMonitor'params)) where
    parse raw_ = (Session'openHmpMonitor'params <$> (GH.parseField #vmId raw_)
                                                <*> (GH.parseField #sink raw_))
instance (C.Marshal Session'openHmpMonitor'params (C.Parsed Session'openHmpMonitor'params)) where
    marshalInto raw_ Session'openHmpMonitor'params{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (GH.encodeField #sink sink raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot Session'openHmpMonitor'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "sink" GH.Slot Session'openHmpMonitor'params Capnp.Gen.ById.X9bd452a518ed3917.ByteSink) where
    fieldByLabel  = (GH.ptrField 0)
data Session'openHmpMonitor'results 
type instance (R.ReprFor Session'openHmpMonitor'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'openHmpMonitor'results) where
    typeId  = 11436753603730193187
instance (C.TypedStruct Session'openHmpMonitor'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'openHmpMonitor'results) where
    type AllocHint Session'openHmpMonitor'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'openHmpMonitor'results (C.Parsed Session'openHmpMonitor'results))
instance (C.AllocateList Session'openHmpMonitor'results) where
    type ListAllocHint Session'openHmpMonitor'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'openHmpMonitor'results (C.Parsed Session'openHmpMonitor'results))
data instance C.Parsed Session'openHmpMonitor'results
    = Session'openHmpMonitor'results 
        {input :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.ByteSink)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'openHmpMonitor'results))
deriving instance (Std_.Eq (C.Parsed Session'openHmpMonitor'results))
instance (C.Parse Session'openHmpMonitor'results (C.Parsed Session'openHmpMonitor'results)) where
    parse raw_ = (Session'openHmpMonitor'results <$> (GH.parseField #input raw_))
instance (C.Marshal Session'openHmpMonitor'results (C.Parsed Session'openHmpMonitor'results)) where
    marshalInto raw_ Session'openHmpMonitor'results{..} = (do
        (GH.encodeField #input input raw_)
        (Std_.pure ())
        )
instance (GH.HasField "input" GH.Slot Session'openHmpMonitor'results Capnp.Gen.ById.X9bd452a518ed3917.ByteSink) where
    fieldByLabel  = (GH.ptrField 0)
data Session'flushSerialConsole'params 
type instance (R.ReprFor Session'flushSerialConsole'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'flushSerialConsole'params) where
    typeId  = 12309027465553179158
instance (C.TypedStruct Session'flushSerialConsole'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Session'flushSerialConsole'params) where
    type AllocHint Session'flushSerialConsole'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'flushSerialConsole'params (C.Parsed Session'flushSerialConsole'params))
instance (C.AllocateList Session'flushSerialConsole'params) where
    type ListAllocHint Session'flushSerialConsole'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'flushSerialConsole'params (C.Parsed Session'flushSerialConsole'params))
data instance C.Parsed Session'flushSerialConsole'params
    = Session'flushSerialConsole'params 
        {vmId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'flushSerialConsole'params))
deriving instance (Std_.Eq (C.Parsed Session'flushSerialConsole'params))
instance (C.Parse Session'flushSerialConsole'params (C.Parsed Session'flushSerialConsole'params)) where
    parse raw_ = (Session'flushSerialConsole'params <$> (GH.parseField #vmId raw_))
instance (C.Marshal Session'flushSerialConsole'params (C.Parsed Session'flushSerialConsole'params)) where
    marshalInto raw_ Session'flushSerialConsole'params{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot Session'flushSerialConsole'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Session'flushSerialConsole'results 
type instance (R.ReprFor Session'flushSerialConsole'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'flushSerialConsole'results) where
    typeId  = 14315634664360764488
instance (C.TypedStruct Session'flushSerialConsole'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'flushSerialConsole'results) where
    type AllocHint Session'flushSerialConsole'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'flushSerialConsole'results (C.Parsed Session'flushSerialConsole'results))
instance (C.AllocateList Session'flushSerialConsole'results) where
    type ListAllocHint Session'flushSerialConsole'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'flushSerialConsole'results (C.Parsed Session'flushSerialConsole'results))
data instance C.Parsed Session'flushSerialConsole'results
    = Session'flushSerialConsole'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'flushSerialConsole'results))
deriving instance (Std_.Eq (C.Parsed Session'flushSerialConsole'results))
instance (C.Parse Session'flushSerialConsole'results (C.Parsed Session'flushSerialConsole'results)) where
    parse raw_ = (Std_.pure Session'flushSerialConsole'results)
instance (C.Marshal Session'flushSerialConsole'results (C.Parsed Session'flushSerialConsole'results)) where
    marshalInto _raw (Session'flushSerialConsole'results) = (Std_.pure ())
data Session'flushHmpMonitor'params 
type instance (R.ReprFor Session'flushHmpMonitor'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'flushHmpMonitor'params) where
    typeId  = 17700805822514408571
instance (C.TypedStruct Session'flushHmpMonitor'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Session'flushHmpMonitor'params) where
    type AllocHint Session'flushHmpMonitor'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'flushHmpMonitor'params (C.Parsed Session'flushHmpMonitor'params))
instance (C.AllocateList Session'flushHmpMonitor'params) where
    type ListAllocHint Session'flushHmpMonitor'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'flushHmpMonitor'params (C.Parsed Session'flushHmpMonitor'params))
data instance C.Parsed Session'flushHmpMonitor'params
    = Session'flushHmpMonitor'params 
        {vmId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'flushHmpMonitor'params))
deriving instance (Std_.Eq (C.Parsed Session'flushHmpMonitor'params))
instance (C.Parse Session'flushHmpMonitor'params (C.Parsed Session'flushHmpMonitor'params)) where
    parse raw_ = (Session'flushHmpMonitor'params <$> (GH.parseField #vmId raw_))
instance (C.Marshal Session'flushHmpMonitor'params (C.Parsed Session'flushHmpMonitor'params)) where
    marshalInto raw_ Session'flushHmpMonitor'params{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot Session'flushHmpMonitor'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Session'flushHmpMonitor'results 
type instance (R.ReprFor Session'flushHmpMonitor'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'flushHmpMonitor'results) where
    typeId  = 17289518928549847130
instance (C.TypedStruct Session'flushHmpMonitor'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'flushHmpMonitor'results) where
    type AllocHint Session'flushHmpMonitor'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'flushHmpMonitor'results (C.Parsed Session'flushHmpMonitor'results))
instance (C.AllocateList Session'flushHmpMonitor'results) where
    type ListAllocHint Session'flushHmpMonitor'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'flushHmpMonitor'results (C.Parsed Session'flushHmpMonitor'results))
data instance C.Parsed Session'flushHmpMonitor'results
    = Session'flushHmpMonitor'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'flushHmpMonitor'results))
deriving instance (Std_.Eq (C.Parsed Session'flushHmpMonitor'results))
instance (C.Parse Session'flushHmpMonitor'results (C.Parsed Session'flushHmpMonitor'results)) where
    parse raw_ = (Std_.pure Session'flushHmpMonitor'results)
instance (C.Marshal Session'flushHmpMonitor'results (C.Parsed Session'flushHmpMonitor'results)) where
    marshalInto _raw (Session'flushHmpMonitor'results) = (Std_.pure ())
data Session'vmAttachDrive'params 
type instance (R.ReprFor Session'vmAttachDrive'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmAttachDrive'params) where
    typeId  = 13049054298676078877
instance (C.TypedStruct Session'vmAttachDrive'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'vmAttachDrive'params) where
    type AllocHint Session'vmAttachDrive'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmAttachDrive'params (C.Parsed Session'vmAttachDrive'params))
instance (C.AllocateList Session'vmAttachDrive'params) where
    type ListAllocHint Session'vmAttachDrive'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmAttachDrive'params (C.Parsed Session'vmAttachDrive'params))
data instance C.Parsed Session'vmAttachDrive'params
    = Session'vmAttachDrive'params 
        {req :: (RP.Parsed VmAttachDriveReq)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmAttachDrive'params))
deriving instance (Std_.Eq (C.Parsed Session'vmAttachDrive'params))
instance (C.Parse Session'vmAttachDrive'params (C.Parsed Session'vmAttachDrive'params)) where
    parse raw_ = (Session'vmAttachDrive'params <$> (GH.parseField #req raw_))
instance (C.Marshal Session'vmAttachDrive'params (C.Parsed Session'vmAttachDrive'params)) where
    marshalInto raw_ Session'vmAttachDrive'params{..} = (do
        (GH.encodeField #req req raw_)
        (Std_.pure ())
        )
instance (GH.HasField "req" GH.Slot Session'vmAttachDrive'params VmAttachDriveReq) where
    fieldByLabel  = (GH.ptrField 0)
data Session'vmAttachDrive'results 
type instance (R.ReprFor Session'vmAttachDrive'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmAttachDrive'results) where
    typeId  = 11477257483477198916
instance (C.TypedStruct Session'vmAttachDrive'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'vmAttachDrive'results) where
    type AllocHint Session'vmAttachDrive'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmAttachDrive'results (C.Parsed Session'vmAttachDrive'results))
instance (C.AllocateList Session'vmAttachDrive'results) where
    type ListAllocHint Session'vmAttachDrive'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmAttachDrive'results (C.Parsed Session'vmAttachDrive'results))
data instance C.Parsed Session'vmAttachDrive'results
    = Session'vmAttachDrive'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmAttachDrive'results))
deriving instance (Std_.Eq (C.Parsed Session'vmAttachDrive'results))
instance (C.Parse Session'vmAttachDrive'results (C.Parsed Session'vmAttachDrive'results)) where
    parse raw_ = (Std_.pure Session'vmAttachDrive'results)
instance (C.Marshal Session'vmAttachDrive'results (C.Parsed Session'vmAttachDrive'results)) where
    marshalInto _raw (Session'vmAttachDrive'results) = (Std_.pure ())
data Session'vmDetachDrive'params 
type instance (R.ReprFor Session'vmDetachDrive'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmDetachDrive'params) where
    typeId  = 16364905694916860660
instance (C.TypedStruct Session'vmDetachDrive'params) where
    numStructWords  = 2
    numStructPtrs  = 0
instance (C.Allocate Session'vmDetachDrive'params) where
    type AllocHint Session'vmDetachDrive'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmDetachDrive'params (C.Parsed Session'vmDetachDrive'params))
instance (C.AllocateList Session'vmDetachDrive'params) where
    type ListAllocHint Session'vmDetachDrive'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmDetachDrive'params (C.Parsed Session'vmDetachDrive'params))
data instance C.Parsed Session'vmDetachDrive'params
    = Session'vmDetachDrive'params 
        {vmId :: (RP.Parsed Std_.Int64)
        ,driveId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmDetachDrive'params))
deriving instance (Std_.Eq (C.Parsed Session'vmDetachDrive'params))
instance (C.Parse Session'vmDetachDrive'params (C.Parsed Session'vmDetachDrive'params)) where
    parse raw_ = (Session'vmDetachDrive'params <$> (GH.parseField #vmId raw_)
                                               <*> (GH.parseField #driveId raw_))
instance (C.Marshal Session'vmDetachDrive'params (C.Parsed Session'vmDetachDrive'params)) where
    marshalInto raw_ Session'vmDetachDrive'params{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (GH.encodeField #driveId driveId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot Session'vmDetachDrive'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "driveId" GH.Slot Session'vmDetachDrive'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
data Session'vmDetachDrive'results 
type instance (R.ReprFor Session'vmDetachDrive'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmDetachDrive'results) where
    typeId  = 15875586182582213085
instance (C.TypedStruct Session'vmDetachDrive'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'vmDetachDrive'results) where
    type AllocHint Session'vmDetachDrive'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmDetachDrive'results (C.Parsed Session'vmDetachDrive'results))
instance (C.AllocateList Session'vmDetachDrive'results) where
    type ListAllocHint Session'vmDetachDrive'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmDetachDrive'results (C.Parsed Session'vmDetachDrive'results))
data instance C.Parsed Session'vmDetachDrive'results
    = Session'vmDetachDrive'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmDetachDrive'results))
deriving instance (Std_.Eq (C.Parsed Session'vmDetachDrive'results))
instance (C.Parse Session'vmDetachDrive'results (C.Parsed Session'vmDetachDrive'results)) where
    parse raw_ = (Std_.pure Session'vmDetachDrive'results)
instance (C.Marshal Session'vmDetachDrive'results (C.Parsed Session'vmDetachDrive'results)) where
    marshalInto _raw (Session'vmDetachDrive'results) = (Std_.pure ())
data Session'probeVsockCid'params 
type instance (R.ReprFor Session'probeVsockCid'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'probeVsockCid'params) where
    typeId  = 9957990287174145727
instance (C.TypedStruct Session'probeVsockCid'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Session'probeVsockCid'params) where
    type AllocHint Session'probeVsockCid'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'probeVsockCid'params (C.Parsed Session'probeVsockCid'params))
instance (C.AllocateList Session'probeVsockCid'params) where
    type ListAllocHint Session'probeVsockCid'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'probeVsockCid'params (C.Parsed Session'probeVsockCid'params))
data instance C.Parsed Session'probeVsockCid'params
    = Session'probeVsockCid'params 
        {cid :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'probeVsockCid'params))
deriving instance (Std_.Eq (C.Parsed Session'probeVsockCid'params))
instance (C.Parse Session'probeVsockCid'params (C.Parsed Session'probeVsockCid'params)) where
    parse raw_ = (Session'probeVsockCid'params <$> (GH.parseField #cid raw_))
instance (C.Marshal Session'probeVsockCid'params (C.Parsed Session'probeVsockCid'params)) where
    marshalInto raw_ Session'probeVsockCid'params{..} = (do
        (GH.encodeField #cid cid raw_)
        (Std_.pure ())
        )
instance (GH.HasField "cid" GH.Slot Session'probeVsockCid'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Session'probeVsockCid'results 
type instance (R.ReprFor Session'probeVsockCid'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'probeVsockCid'results) where
    typeId  = 13444723105890588371
instance (C.TypedStruct Session'probeVsockCid'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Session'probeVsockCid'results) where
    type AllocHint Session'probeVsockCid'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'probeVsockCid'results (C.Parsed Session'probeVsockCid'results))
instance (C.AllocateList Session'probeVsockCid'results) where
    type ListAllocHint Session'probeVsockCid'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'probeVsockCid'results (C.Parsed Session'probeVsockCid'results))
data instance C.Parsed Session'probeVsockCid'results
    = Session'probeVsockCid'results 
        {free :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'probeVsockCid'results))
deriving instance (Std_.Eq (C.Parsed Session'probeVsockCid'results))
instance (C.Parse Session'probeVsockCid'results (C.Parsed Session'probeVsockCid'results)) where
    parse raw_ = (Session'probeVsockCid'results <$> (GH.parseField #free raw_))
instance (C.Marshal Session'probeVsockCid'results (C.Parsed Session'probeVsockCid'results)) where
    marshalInto raw_ Session'probeVsockCid'results{..} = (do
        (GH.encodeField #free free raw_)
        (Std_.pure ())
        )
instance (GH.HasField "free" GH.Slot Session'probeVsockCid'results Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
data Session'diskOpenRead'params 
type instance (R.ReprFor Session'diskOpenRead'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskOpenRead'params) where
    typeId  = 14382638796393382158
instance (C.TypedStruct Session'diskOpenRead'params) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Session'diskOpenRead'params) where
    type AllocHint Session'diskOpenRead'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskOpenRead'params (C.Parsed Session'diskOpenRead'params))
instance (C.AllocateList Session'diskOpenRead'params) where
    type ListAllocHint Session'diskOpenRead'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskOpenRead'params (C.Parsed Session'diskOpenRead'params))
data instance C.Parsed Session'diskOpenRead'params
    = Session'diskOpenRead'params 
        {path :: (RP.Parsed Basics.Text)
        ,ttlSec :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskOpenRead'params))
deriving instance (Std_.Eq (C.Parsed Session'diskOpenRead'params))
instance (C.Parse Session'diskOpenRead'params (C.Parsed Session'diskOpenRead'params)) where
    parse raw_ = (Session'diskOpenRead'params <$> (GH.parseField #path raw_)
                                              <*> (GH.parseField #ttlSec raw_))
instance (C.Marshal Session'diskOpenRead'params (C.Parsed Session'diskOpenRead'params)) where
    marshalInto raw_ Session'diskOpenRead'params{..} = (do
        (GH.encodeField #path path raw_)
        (GH.encodeField #ttlSec ttlSec raw_)
        (Std_.pure ())
        )
instance (GH.HasField "path" GH.Slot Session'diskOpenRead'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "ttlSec" GH.Slot Session'diskOpenRead'params Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
data Session'diskOpenRead'results 
type instance (R.ReprFor Session'diskOpenRead'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskOpenRead'results) where
    typeId  = 18250419826091380833
instance (C.TypedStruct Session'diskOpenRead'results) where
    numStructWords  = 1
    numStructPtrs  = 3
instance (C.Allocate Session'diskOpenRead'results) where
    type AllocHint Session'diskOpenRead'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskOpenRead'results (C.Parsed Session'diskOpenRead'results))
instance (C.AllocateList Session'diskOpenRead'results) where
    type ListAllocHint Session'diskOpenRead'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskOpenRead'results (C.Parsed Session'diskOpenRead'results))
data instance C.Parsed Session'diskOpenRead'results
    = Session'diskOpenRead'results 
        {reader :: (RP.Parsed DiskReader)
        ,token :: (RP.Parsed Basics.Text)
        ,sizeBytes :: (RP.Parsed Std_.Int64)
        ,md5 :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskOpenRead'results))
deriving instance (Std_.Eq (C.Parsed Session'diskOpenRead'results))
instance (C.Parse Session'diskOpenRead'results (C.Parsed Session'diskOpenRead'results)) where
    parse raw_ = (Session'diskOpenRead'results <$> (GH.parseField #reader raw_)
                                               <*> (GH.parseField #token raw_)
                                               <*> (GH.parseField #sizeBytes raw_)
                                               <*> (GH.parseField #md5 raw_))
instance (C.Marshal Session'diskOpenRead'results (C.Parsed Session'diskOpenRead'results)) where
    marshalInto raw_ Session'diskOpenRead'results{..} = (do
        (GH.encodeField #reader reader raw_)
        (GH.encodeField #token token raw_)
        (GH.encodeField #sizeBytes sizeBytes raw_)
        (GH.encodeField #md5 md5 raw_)
        (Std_.pure ())
        )
instance (GH.HasField "reader" GH.Slot Session'diskOpenRead'results DiskReader) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "token" GH.Slot Session'diskOpenRead'results Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "sizeBytes" GH.Slot Session'diskOpenRead'results Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "md5" GH.Slot Session'diskOpenRead'results Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
data Session'attachReader'params 
type instance (R.ReprFor Session'attachReader'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'attachReader'params) where
    typeId  = 16583164990814995810
instance (C.TypedStruct Session'attachReader'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'attachReader'params) where
    type AllocHint Session'attachReader'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'attachReader'params (C.Parsed Session'attachReader'params))
instance (C.AllocateList Session'attachReader'params) where
    type ListAllocHint Session'attachReader'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'attachReader'params (C.Parsed Session'attachReader'params))
data instance C.Parsed Session'attachReader'params
    = Session'attachReader'params 
        {token :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'attachReader'params))
deriving instance (Std_.Eq (C.Parsed Session'attachReader'params))
instance (C.Parse Session'attachReader'params (C.Parsed Session'attachReader'params)) where
    parse raw_ = (Session'attachReader'params <$> (GH.parseField #token raw_))
instance (C.Marshal Session'attachReader'params (C.Parsed Session'attachReader'params)) where
    marshalInto raw_ Session'attachReader'params{..} = (do
        (GH.encodeField #token token raw_)
        (Std_.pure ())
        )
instance (GH.HasField "token" GH.Slot Session'attachReader'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Session'attachReader'results 
type instance (R.ReprFor Session'attachReader'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'attachReader'results) where
    typeId  = 13063254323432150227
instance (C.TypedStruct Session'attachReader'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'attachReader'results) where
    type AllocHint Session'attachReader'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'attachReader'results (C.Parsed Session'attachReader'results))
instance (C.AllocateList Session'attachReader'results) where
    type ListAllocHint Session'attachReader'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'attachReader'results (C.Parsed Session'attachReader'results))
data instance C.Parsed Session'attachReader'results
    = Session'attachReader'results 
        {reader :: (RP.Parsed DiskReader)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'attachReader'results))
deriving instance (Std_.Eq (C.Parsed Session'attachReader'results))
instance (C.Parse Session'attachReader'results (C.Parsed Session'attachReader'results)) where
    parse raw_ = (Session'attachReader'results <$> (GH.parseField #reader raw_))
instance (C.Marshal Session'attachReader'results (C.Parsed Session'attachReader'results)) where
    marshalInto raw_ Session'attachReader'results{..} = (do
        (GH.encodeField #reader reader raw_)
        (Std_.pure ())
        )
instance (GH.HasField "reader" GH.Slot Session'attachReader'results DiskReader) where
    fieldByLabel  = (GH.ptrField 0)
data Session'diskImportFromPeer'params 
type instance (R.ReprFor Session'diskImportFromPeer'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskImportFromPeer'params) where
    typeId  = 14396209859029846008
instance (C.TypedStruct Session'diskImportFromPeer'params) where
    numStructWords  = 2
    numStructPtrs  = 4
instance (C.Allocate Session'diskImportFromPeer'params) where
    type AllocHint Session'diskImportFromPeer'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskImportFromPeer'params (C.Parsed Session'diskImportFromPeer'params))
instance (C.AllocateList Session'diskImportFromPeer'params) where
    type ListAllocHint Session'diskImportFromPeer'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskImportFromPeer'params (C.Parsed Session'diskImportFromPeer'params))
data instance C.Parsed Session'diskImportFromPeer'params
    = Session'diskImportFromPeer'params 
        {destPath :: (RP.Parsed Basics.Text)
        ,peerHost :: (RP.Parsed Basics.Text)
        ,peerPort :: (RP.Parsed Std_.Int32)
        ,token :: (RP.Parsed Basics.Text)
        ,expectedBytes :: (RP.Parsed Std_.Int64)
        ,expectedMd5 :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskImportFromPeer'params))
deriving instance (Std_.Eq (C.Parsed Session'diskImportFromPeer'params))
instance (C.Parse Session'diskImportFromPeer'params (C.Parsed Session'diskImportFromPeer'params)) where
    parse raw_ = (Session'diskImportFromPeer'params <$> (GH.parseField #destPath raw_)
                                                    <*> (GH.parseField #peerHost raw_)
                                                    <*> (GH.parseField #peerPort raw_)
                                                    <*> (GH.parseField #token raw_)
                                                    <*> (GH.parseField #expectedBytes raw_)
                                                    <*> (GH.parseField #expectedMd5 raw_))
instance (C.Marshal Session'diskImportFromPeer'params (C.Parsed Session'diskImportFromPeer'params)) where
    marshalInto raw_ Session'diskImportFromPeer'params{..} = (do
        (GH.encodeField #destPath destPath raw_)
        (GH.encodeField #peerHost peerHost raw_)
        (GH.encodeField #peerPort peerPort raw_)
        (GH.encodeField #token token raw_)
        (GH.encodeField #expectedBytes expectedBytes raw_)
        (GH.encodeField #expectedMd5 expectedMd5 raw_)
        (Std_.pure ())
        )
instance (GH.HasField "destPath" GH.Slot Session'diskImportFromPeer'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "peerHost" GH.Slot Session'diskImportFromPeer'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "peerPort" GH.Slot Session'diskImportFromPeer'params Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "token" GH.Slot Session'diskImportFromPeer'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "expectedBytes" GH.Slot Session'diskImportFromPeer'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "expectedMd5" GH.Slot Session'diskImportFromPeer'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 3)
data Session'diskImportFromPeer'results 
type instance (R.ReprFor Session'diskImportFromPeer'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'diskImportFromPeer'results) where
    typeId  = 18286574020088566998
instance (C.TypedStruct Session'diskImportFromPeer'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'diskImportFromPeer'results) where
    type AllocHint Session'diskImportFromPeer'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'diskImportFromPeer'results (C.Parsed Session'diskImportFromPeer'results))
instance (C.AllocateList Session'diskImportFromPeer'results) where
    type ListAllocHint Session'diskImportFromPeer'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'diskImportFromPeer'results (C.Parsed Session'diskImportFromPeer'results))
data instance C.Parsed Session'diskImportFromPeer'results
    = Session'diskImportFromPeer'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'diskImportFromPeer'results))
deriving instance (Std_.Eq (C.Parsed Session'diskImportFromPeer'results))
instance (C.Parse Session'diskImportFromPeer'results (C.Parsed Session'diskImportFromPeer'results)) where
    parse raw_ = (Std_.pure Session'diskImportFromPeer'results)
instance (C.Marshal Session'diskImportFromPeer'results (C.Parsed Session'diskImportFromPeer'results)) where
    marshalInto _raw (Session'diskImportFromPeer'results) = (Std_.pure ())
data Session'vmGuestExecStream'params 
type instance (R.ReprFor Session'vmGuestExecStream'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmGuestExecStream'params) where
    typeId  = 10495467010808434359
instance (C.TypedStruct Session'vmGuestExecStream'params) where
    numStructWords  = 0
    numStructPtrs  = 3
instance (C.Allocate Session'vmGuestExecStream'params) where
    type AllocHint Session'vmGuestExecStream'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmGuestExecStream'params (C.Parsed Session'vmGuestExecStream'params))
instance (C.AllocateList Session'vmGuestExecStream'params) where
    type ListAllocHint Session'vmGuestExecStream'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmGuestExecStream'params (C.Parsed Session'vmGuestExecStream'params))
data instance C.Parsed Session'vmGuestExecStream'params
    = Session'vmGuestExecStream'params 
        {req :: (RP.Parsed VmGuestExecReq)
        ,stdoutSink :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.ByteSink)
        ,stderrSink :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.ByteSink)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmGuestExecStream'params))
deriving instance (Std_.Eq (C.Parsed Session'vmGuestExecStream'params))
instance (C.Parse Session'vmGuestExecStream'params (C.Parsed Session'vmGuestExecStream'params)) where
    parse raw_ = (Session'vmGuestExecStream'params <$> (GH.parseField #req raw_)
                                                   <*> (GH.parseField #stdoutSink raw_)
                                                   <*> (GH.parseField #stderrSink raw_))
instance (C.Marshal Session'vmGuestExecStream'params (C.Parsed Session'vmGuestExecStream'params)) where
    marshalInto raw_ Session'vmGuestExecStream'params{..} = (do
        (GH.encodeField #req req raw_)
        (GH.encodeField #stdoutSink stdoutSink raw_)
        (GH.encodeField #stderrSink stderrSink raw_)
        (Std_.pure ())
        )
instance (GH.HasField "req" GH.Slot Session'vmGuestExecStream'params VmGuestExecReq) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "stdoutSink" GH.Slot Session'vmGuestExecStream'params Capnp.Gen.ById.X9bd452a518ed3917.ByteSink) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "stderrSink" GH.Slot Session'vmGuestExecStream'params Capnp.Gen.ById.X9bd452a518ed3917.ByteSink) where
    fieldByLabel  = (GH.ptrField 2)
data Session'vmGuestExecStream'results 
type instance (R.ReprFor Session'vmGuestExecStream'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmGuestExecStream'results) where
    typeId  = 17516551508749116903
instance (C.TypedStruct Session'vmGuestExecStream'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'vmGuestExecStream'results) where
    type AllocHint Session'vmGuestExecStream'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmGuestExecStream'results (C.Parsed Session'vmGuestExecStream'results))
instance (C.AllocateList Session'vmGuestExecStream'results) where
    type ListAllocHint Session'vmGuestExecStream'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmGuestExecStream'results (C.Parsed Session'vmGuestExecStream'results))
data instance C.Parsed Session'vmGuestExecStream'results
    = Session'vmGuestExecStream'results 
        {info :: (RP.Parsed VmGuestExecInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmGuestExecStream'results))
deriving instance (Std_.Eq (C.Parsed Session'vmGuestExecStream'results))
instance (C.Parse Session'vmGuestExecStream'results (C.Parsed Session'vmGuestExecStream'results)) where
    parse raw_ = (Session'vmGuestExecStream'results <$> (GH.parseField #info raw_))
instance (C.Marshal Session'vmGuestExecStream'results (C.Parsed Session'vmGuestExecStream'results)) where
    marshalInto raw_ Session'vmGuestExecStream'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot Session'vmGuestExecStream'results VmGuestExecInfo) where
    fieldByLabel  = (GH.ptrField 0)
data Session'vmSave'params 
type instance (R.ReprFor Session'vmSave'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmSave'params) where
    typeId  = 10510390357778605881
instance (C.TypedStruct Session'vmSave'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Session'vmSave'params) where
    type AllocHint Session'vmSave'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmSave'params (C.Parsed Session'vmSave'params))
instance (C.AllocateList Session'vmSave'params) where
    type ListAllocHint Session'vmSave'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmSave'params (C.Parsed Session'vmSave'params))
data instance C.Parsed Session'vmSave'params
    = Session'vmSave'params 
        {vmId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmSave'params))
deriving instance (Std_.Eq (C.Parsed Session'vmSave'params))
instance (C.Parse Session'vmSave'params (C.Parsed Session'vmSave'params)) where
    parse raw_ = (Session'vmSave'params <$> (GH.parseField #vmId raw_))
instance (C.Marshal Session'vmSave'params (C.Parsed Session'vmSave'params)) where
    marshalInto raw_ Session'vmSave'params{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot Session'vmSave'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Session'vmSave'results 
type instance (R.ReprFor Session'vmSave'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'vmSave'results) where
    typeId  = 18091016661478486259
instance (C.TypedStruct Session'vmSave'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'vmSave'results) where
    type AllocHint Session'vmSave'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'vmSave'results (C.Parsed Session'vmSave'results))
instance (C.AllocateList Session'vmSave'results) where
    type ListAllocHint Session'vmSave'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'vmSave'results (C.Parsed Session'vmSave'results))
data instance C.Parsed Session'vmSave'results
    = Session'vmSave'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'vmSave'results))
deriving instance (Std_.Eq (C.Parsed Session'vmSave'results))
instance (C.Parse Session'vmSave'results (C.Parsed Session'vmSave'results)) where
    parse raw_ = (Std_.pure Session'vmSave'results)
instance (C.Marshal Session'vmSave'results (C.Parsed Session'vmSave'results)) where
    marshalInto _raw (Session'vmSave'results) = (Std_.pure ())
data Session'deleteSavedState'params 
type instance (R.ReprFor Session'deleteSavedState'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'deleteSavedState'params) where
    typeId  = 13578646548577570901
instance (C.TypedStruct Session'deleteSavedState'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'deleteSavedState'params) where
    type AllocHint Session'deleteSavedState'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'deleteSavedState'params (C.Parsed Session'deleteSavedState'params))
instance (C.AllocateList Session'deleteSavedState'params) where
    type ListAllocHint Session'deleteSavedState'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'deleteSavedState'params (C.Parsed Session'deleteSavedState'params))
data instance C.Parsed Session'deleteSavedState'params
    = Session'deleteSavedState'params 
        {vmName :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'deleteSavedState'params))
deriving instance (Std_.Eq (C.Parsed Session'deleteSavedState'params))
instance (C.Parse Session'deleteSavedState'params (C.Parsed Session'deleteSavedState'params)) where
    parse raw_ = (Session'deleteSavedState'params <$> (GH.parseField #vmName raw_))
instance (C.Marshal Session'deleteSavedState'params (C.Parsed Session'deleteSavedState'params)) where
    marshalInto raw_ Session'deleteSavedState'params{..} = (do
        (GH.encodeField #vmName vmName raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmName" GH.Slot Session'deleteSavedState'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Session'deleteSavedState'results 
type instance (R.ReprFor Session'deleteSavedState'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'deleteSavedState'results) where
    typeId  = 11447949880474079985
instance (C.TypedStruct Session'deleteSavedState'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Session'deleteSavedState'results) where
    type AllocHint Session'deleteSavedState'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'deleteSavedState'results (C.Parsed Session'deleteSavedState'results))
instance (C.AllocateList Session'deleteSavedState'results) where
    type ListAllocHint Session'deleteSavedState'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'deleteSavedState'results (C.Parsed Session'deleteSavedState'results))
data instance C.Parsed Session'deleteSavedState'results
    = Session'deleteSavedState'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'deleteSavedState'results))
deriving instance (Std_.Eq (C.Parsed Session'deleteSavedState'results))
instance (C.Parse Session'deleteSavedState'results (C.Parsed Session'deleteSavedState'results)) where
    parse raw_ = (Std_.pure Session'deleteSavedState'results)
instance (C.Marshal Session'deleteSavedState'results (C.Parsed Session'deleteSavedState'results)) where
    marshalInto _raw (Session'deleteSavedState'results) = (Std_.pure ())
data Session'snapshotCreateLive'params 
type instance (R.ReprFor Session'snapshotCreateLive'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotCreateLive'params) where
    typeId  = 17354183927217357071
instance (C.TypedStruct Session'snapshotCreateLive'params) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate Session'snapshotCreateLive'params) where
    type AllocHint Session'snapshotCreateLive'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotCreateLive'params (C.Parsed Session'snapshotCreateLive'params))
instance (C.AllocateList Session'snapshotCreateLive'params) where
    type ListAllocHint Session'snapshotCreateLive'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotCreateLive'params (C.Parsed Session'snapshotCreateLive'params))
data instance C.Parsed Session'snapshotCreateLive'params
    = Session'snapshotCreateLive'params 
        {path :: (RP.Parsed Basics.Text)
        ,name :: (RP.Parsed Basics.Text)
        ,vmId :: (RP.Parsed Std_.Int64)
        ,quiesce :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.QuiesceMode)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotCreateLive'params))
deriving instance (Std_.Eq (C.Parsed Session'snapshotCreateLive'params))
instance (C.Parse Session'snapshotCreateLive'params (C.Parsed Session'snapshotCreateLive'params)) where
    parse raw_ = (Session'snapshotCreateLive'params <$> (GH.parseField #path raw_)
                                                    <*> (GH.parseField #name raw_)
                                                    <*> (GH.parseField #vmId raw_)
                                                    <*> (GH.parseField #quiesce raw_))
instance (C.Marshal Session'snapshotCreateLive'params (C.Parsed Session'snapshotCreateLive'params)) where
    marshalInto raw_ Session'snapshotCreateLive'params{..} = (do
        (GH.encodeField #path path raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #vmId vmId raw_)
        (GH.encodeField #quiesce quiesce raw_)
        (Std_.pure ())
        )
instance (GH.HasField "path" GH.Slot Session'snapshotCreateLive'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "name" GH.Slot Session'snapshotCreateLive'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "vmId" GH.Slot Session'snapshotCreateLive'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "quiesce" GH.Slot Session'snapshotCreateLive'params Capnp.Gen.ById.Xbf9b09f64c0dd40d.QuiesceMode) where
    fieldByLabel  = (GH.dataField 0 1 16 0)
data Session'snapshotCreateLive'results 
type instance (R.ReprFor Session'snapshotCreateLive'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotCreateLive'results) where
    typeId  = 14984007507268565335
instance (C.TypedStruct Session'snapshotCreateLive'results) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Session'snapshotCreateLive'results) where
    type AllocHint Session'snapshotCreateLive'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotCreateLive'results (C.Parsed Session'snapshotCreateLive'results))
instance (C.AllocateList Session'snapshotCreateLive'results) where
    type ListAllocHint Session'snapshotCreateLive'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotCreateLive'results (C.Parsed Session'snapshotCreateLive'results))
data instance C.Parsed Session'snapshotCreateLive'results
    = Session'snapshotCreateLive'results 
        {result :: (RP.Parsed DiskOpResult)
        ,quiesced :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotCreateLive'results))
deriving instance (Std_.Eq (C.Parsed Session'snapshotCreateLive'results))
instance (C.Parse Session'snapshotCreateLive'results (C.Parsed Session'snapshotCreateLive'results)) where
    parse raw_ = (Session'snapshotCreateLive'results <$> (GH.parseField #result raw_)
                                                     <*> (GH.parseField #quiesced raw_))
instance (C.Marshal Session'snapshotCreateLive'results (C.Parsed Session'snapshotCreateLive'results)) where
    marshalInto raw_ Session'snapshotCreateLive'results{..} = (do
        (GH.encodeField #result result raw_)
        (GH.encodeField #quiesced quiesced raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'snapshotCreateLive'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "quiesced" GH.Slot Session'snapshotCreateLive'results Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
data Session'snapshotDeleteLive'params 
type instance (R.ReprFor Session'snapshotDeleteLive'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotDeleteLive'params) where
    typeId  = 11242620735601425299
instance (C.TypedStruct Session'snapshotDeleteLive'params) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Session'snapshotDeleteLive'params) where
    type AllocHint Session'snapshotDeleteLive'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotDeleteLive'params (C.Parsed Session'snapshotDeleteLive'params))
instance (C.AllocateList Session'snapshotDeleteLive'params) where
    type ListAllocHint Session'snapshotDeleteLive'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotDeleteLive'params (C.Parsed Session'snapshotDeleteLive'params))
data instance C.Parsed Session'snapshotDeleteLive'params
    = Session'snapshotDeleteLive'params 
        {path :: (RP.Parsed Basics.Text)
        ,name :: (RP.Parsed Basics.Text)
        ,vmId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotDeleteLive'params))
deriving instance (Std_.Eq (C.Parsed Session'snapshotDeleteLive'params))
instance (C.Parse Session'snapshotDeleteLive'params (C.Parsed Session'snapshotDeleteLive'params)) where
    parse raw_ = (Session'snapshotDeleteLive'params <$> (GH.parseField #path raw_)
                                                    <*> (GH.parseField #name raw_)
                                                    <*> (GH.parseField #vmId raw_))
instance (C.Marshal Session'snapshotDeleteLive'params (C.Parsed Session'snapshotDeleteLive'params)) where
    marshalInto raw_ Session'snapshotDeleteLive'params{..} = (do
        (GH.encodeField #path path raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #vmId vmId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "path" GH.Slot Session'snapshotDeleteLive'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "name" GH.Slot Session'snapshotDeleteLive'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "vmId" GH.Slot Session'snapshotDeleteLive'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Session'snapshotDeleteLive'results 
type instance (R.ReprFor Session'snapshotDeleteLive'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotDeleteLive'results) where
    typeId  = 11567279993457323767
instance (C.TypedStruct Session'snapshotDeleteLive'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'snapshotDeleteLive'results) where
    type AllocHint Session'snapshotDeleteLive'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotDeleteLive'results (C.Parsed Session'snapshotDeleteLive'results))
instance (C.AllocateList Session'snapshotDeleteLive'results) where
    type ListAllocHint Session'snapshotDeleteLive'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotDeleteLive'results (C.Parsed Session'snapshotDeleteLive'results))
data instance C.Parsed Session'snapshotDeleteLive'results
    = Session'snapshotDeleteLive'results 
        {result :: (RP.Parsed DiskOpResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotDeleteLive'results))
deriving instance (Std_.Eq (C.Parsed Session'snapshotDeleteLive'results))
instance (C.Parse Session'snapshotDeleteLive'results (C.Parsed Session'snapshotDeleteLive'results)) where
    parse raw_ = (Session'snapshotDeleteLive'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'snapshotDeleteLive'results (C.Parsed Session'snapshotDeleteLive'results)) where
    marshalInto raw_ Session'snapshotDeleteLive'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'snapshotDeleteLive'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
data Session'snapshotCreateLiveMany'params 
type instance (R.ReprFor Session'snapshotCreateLiveMany'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotCreateLiveMany'params) where
    typeId  = 17352390649456236637
instance (C.TypedStruct Session'snapshotCreateLiveMany'params) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate Session'snapshotCreateLiveMany'params) where
    type AllocHint Session'snapshotCreateLiveMany'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotCreateLiveMany'params (C.Parsed Session'snapshotCreateLiveMany'params))
instance (C.AllocateList Session'snapshotCreateLiveMany'params) where
    type ListAllocHint Session'snapshotCreateLiveMany'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotCreateLiveMany'params (C.Parsed Session'snapshotCreateLiveMany'params))
data instance C.Parsed Session'snapshotCreateLiveMany'params
    = Session'snapshotCreateLiveMany'params 
        {paths :: (RP.Parsed (R.List Basics.Text))
        ,name :: (RP.Parsed Basics.Text)
        ,vmId :: (RP.Parsed Std_.Int64)
        ,quiesce :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.QuiesceMode)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotCreateLiveMany'params))
deriving instance (Std_.Eq (C.Parsed Session'snapshotCreateLiveMany'params))
instance (C.Parse Session'snapshotCreateLiveMany'params (C.Parsed Session'snapshotCreateLiveMany'params)) where
    parse raw_ = (Session'snapshotCreateLiveMany'params <$> (GH.parseField #paths raw_)
                                                        <*> (GH.parseField #name raw_)
                                                        <*> (GH.parseField #vmId raw_)
                                                        <*> (GH.parseField #quiesce raw_))
instance (C.Marshal Session'snapshotCreateLiveMany'params (C.Parsed Session'snapshotCreateLiveMany'params)) where
    marshalInto raw_ Session'snapshotCreateLiveMany'params{..} = (do
        (GH.encodeField #paths paths raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #vmId vmId raw_)
        (GH.encodeField #quiesce quiesce raw_)
        (Std_.pure ())
        )
instance (GH.HasField "paths" GH.Slot Session'snapshotCreateLiveMany'params (R.List Basics.Text)) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "name" GH.Slot Session'snapshotCreateLiveMany'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "vmId" GH.Slot Session'snapshotCreateLiveMany'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "quiesce" GH.Slot Session'snapshotCreateLiveMany'params Capnp.Gen.ById.Xbf9b09f64c0dd40d.QuiesceMode) where
    fieldByLabel  = (GH.dataField 0 1 16 0)
data Session'snapshotCreateLiveMany'results 
type instance (R.ReprFor Session'snapshotCreateLiveMany'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotCreateLiveMany'results) where
    typeId  = 15677871614244384781
instance (C.TypedStruct Session'snapshotCreateLiveMany'results) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Session'snapshotCreateLiveMany'results) where
    type AllocHint Session'snapshotCreateLiveMany'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotCreateLiveMany'results (C.Parsed Session'snapshotCreateLiveMany'results))
instance (C.AllocateList Session'snapshotCreateLiveMany'results) where
    type ListAllocHint Session'snapshotCreateLiveMany'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotCreateLiveMany'results (C.Parsed Session'snapshotCreateLiveMany'results))
data instance C.Parsed Session'snapshotCreateLiveMany'results
    = Session'snapshotCreateLiveMany'results 
        {result :: (RP.Parsed DiskOpResult)
        ,quiesced :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotCreateLiveMany'results))
deriving instance (Std_.Eq (C.Parsed Session'snapshotCreateLiveMany'results))
instance (C.Parse Session'snapshotCreateLiveMany'results (C.Parsed Session'snapshotCreateLiveMany'results)) where
    parse raw_ = (Session'snapshotCreateLiveMany'results <$> (GH.parseField #result raw_)
                                                         <*> (GH.parseField #quiesced raw_))
instance (C.Marshal Session'snapshotCreateLiveMany'results (C.Parsed Session'snapshotCreateLiveMany'results)) where
    marshalInto raw_ Session'snapshotCreateLiveMany'results{..} = (do
        (GH.encodeField #result result raw_)
        (GH.encodeField #quiesced quiesced raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'snapshotCreateLiveMany'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "quiesced" GH.Slot Session'snapshotCreateLiveMany'results Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
data Session'snapshotCreateWithVmstate'params 
type instance (R.ReprFor Session'snapshotCreateWithVmstate'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotCreateWithVmstate'params) where
    typeId  = 16101421658982316407
instance (C.TypedStruct Session'snapshotCreateWithVmstate'params) where
    numStructWords  = 1
    numStructPtrs  = 3
instance (C.Allocate Session'snapshotCreateWithVmstate'params) where
    type AllocHint Session'snapshotCreateWithVmstate'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotCreateWithVmstate'params (C.Parsed Session'snapshotCreateWithVmstate'params))
instance (C.AllocateList Session'snapshotCreateWithVmstate'params) where
    type ListAllocHint Session'snapshotCreateWithVmstate'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotCreateWithVmstate'params (C.Parsed Session'snapshotCreateWithVmstate'params))
data instance C.Parsed Session'snapshotCreateWithVmstate'params
    = Session'snapshotCreateWithVmstate'params 
        {vmstateDevicePath :: (RP.Parsed Basics.Text)
        ,devicePaths :: (RP.Parsed (R.List Basics.Text))
        ,name :: (RP.Parsed Basics.Text)
        ,vmId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotCreateWithVmstate'params))
deriving instance (Std_.Eq (C.Parsed Session'snapshotCreateWithVmstate'params))
instance (C.Parse Session'snapshotCreateWithVmstate'params (C.Parsed Session'snapshotCreateWithVmstate'params)) where
    parse raw_ = (Session'snapshotCreateWithVmstate'params <$> (GH.parseField #vmstateDevicePath raw_)
                                                           <*> (GH.parseField #devicePaths raw_)
                                                           <*> (GH.parseField #name raw_)
                                                           <*> (GH.parseField #vmId raw_))
instance (C.Marshal Session'snapshotCreateWithVmstate'params (C.Parsed Session'snapshotCreateWithVmstate'params)) where
    marshalInto raw_ Session'snapshotCreateWithVmstate'params{..} = (do
        (GH.encodeField #vmstateDevicePath vmstateDevicePath raw_)
        (GH.encodeField #devicePaths devicePaths raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #vmId vmId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmstateDevicePath" GH.Slot Session'snapshotCreateWithVmstate'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "devicePaths" GH.Slot Session'snapshotCreateWithVmstate'params (R.List Basics.Text)) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "name" GH.Slot Session'snapshotCreateWithVmstate'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "vmId" GH.Slot Session'snapshotCreateWithVmstate'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Session'snapshotCreateWithVmstate'results 
type instance (R.ReprFor Session'snapshotCreateWithVmstate'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotCreateWithVmstate'results) where
    typeId  = 15319697288655617572
instance (C.TypedStruct Session'snapshotCreateWithVmstate'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'snapshotCreateWithVmstate'results) where
    type AllocHint Session'snapshotCreateWithVmstate'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotCreateWithVmstate'results (C.Parsed Session'snapshotCreateWithVmstate'results))
instance (C.AllocateList Session'snapshotCreateWithVmstate'results) where
    type ListAllocHint Session'snapshotCreateWithVmstate'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotCreateWithVmstate'results (C.Parsed Session'snapshotCreateWithVmstate'results))
data instance C.Parsed Session'snapshotCreateWithVmstate'results
    = Session'snapshotCreateWithVmstate'results 
        {result :: (RP.Parsed DiskOpResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotCreateWithVmstate'results))
deriving instance (Std_.Eq (C.Parsed Session'snapshotCreateWithVmstate'results))
instance (C.Parse Session'snapshotCreateWithVmstate'results (C.Parsed Session'snapshotCreateWithVmstate'results)) where
    parse raw_ = (Session'snapshotCreateWithVmstate'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'snapshotCreateWithVmstate'results (C.Parsed Session'snapshotCreateWithVmstate'results)) where
    marshalInto raw_ Session'snapshotCreateWithVmstate'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'snapshotCreateWithVmstate'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
data Session'snapshotLoadWithVmstate'params 
type instance (R.ReprFor Session'snapshotLoadWithVmstate'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotLoadWithVmstate'params) where
    typeId  = 13379629769303057490
instance (C.TypedStruct Session'snapshotLoadWithVmstate'params) where
    numStructWords  = 1
    numStructPtrs  = 3
instance (C.Allocate Session'snapshotLoadWithVmstate'params) where
    type AllocHint Session'snapshotLoadWithVmstate'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotLoadWithVmstate'params (C.Parsed Session'snapshotLoadWithVmstate'params))
instance (C.AllocateList Session'snapshotLoadWithVmstate'params) where
    type ListAllocHint Session'snapshotLoadWithVmstate'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotLoadWithVmstate'params (C.Parsed Session'snapshotLoadWithVmstate'params))
data instance C.Parsed Session'snapshotLoadWithVmstate'params
    = Session'snapshotLoadWithVmstate'params 
        {vmstateDevicePath :: (RP.Parsed Basics.Text)
        ,devicePaths :: (RP.Parsed (R.List Basics.Text))
        ,name :: (RP.Parsed Basics.Text)
        ,vmId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotLoadWithVmstate'params))
deriving instance (Std_.Eq (C.Parsed Session'snapshotLoadWithVmstate'params))
instance (C.Parse Session'snapshotLoadWithVmstate'params (C.Parsed Session'snapshotLoadWithVmstate'params)) where
    parse raw_ = (Session'snapshotLoadWithVmstate'params <$> (GH.parseField #vmstateDevicePath raw_)
                                                         <*> (GH.parseField #devicePaths raw_)
                                                         <*> (GH.parseField #name raw_)
                                                         <*> (GH.parseField #vmId raw_))
instance (C.Marshal Session'snapshotLoadWithVmstate'params (C.Parsed Session'snapshotLoadWithVmstate'params)) where
    marshalInto raw_ Session'snapshotLoadWithVmstate'params{..} = (do
        (GH.encodeField #vmstateDevicePath vmstateDevicePath raw_)
        (GH.encodeField #devicePaths devicePaths raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #vmId vmId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmstateDevicePath" GH.Slot Session'snapshotLoadWithVmstate'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "devicePaths" GH.Slot Session'snapshotLoadWithVmstate'params (R.List Basics.Text)) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "name" GH.Slot Session'snapshotLoadWithVmstate'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "vmId" GH.Slot Session'snapshotLoadWithVmstate'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Session'snapshotLoadWithVmstate'results 
type instance (R.ReprFor Session'snapshotLoadWithVmstate'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotLoadWithVmstate'results) where
    typeId  = 10484588875035174898
instance (C.TypedStruct Session'snapshotLoadWithVmstate'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'snapshotLoadWithVmstate'results) where
    type AllocHint Session'snapshotLoadWithVmstate'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotLoadWithVmstate'results (C.Parsed Session'snapshotLoadWithVmstate'results))
instance (C.AllocateList Session'snapshotLoadWithVmstate'results) where
    type ListAllocHint Session'snapshotLoadWithVmstate'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotLoadWithVmstate'results (C.Parsed Session'snapshotLoadWithVmstate'results))
data instance C.Parsed Session'snapshotLoadWithVmstate'results
    = Session'snapshotLoadWithVmstate'results 
        {result :: (RP.Parsed DiskOpResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotLoadWithVmstate'results))
deriving instance (Std_.Eq (C.Parsed Session'snapshotLoadWithVmstate'results))
instance (C.Parse Session'snapshotLoadWithVmstate'results (C.Parsed Session'snapshotLoadWithVmstate'results)) where
    parse raw_ = (Session'snapshotLoadWithVmstate'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'snapshotLoadWithVmstate'results (C.Parsed Session'snapshotLoadWithVmstate'results)) where
    marshalInto raw_ Session'snapshotLoadWithVmstate'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'snapshotLoadWithVmstate'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
data Session'snapshotDeleteWithVmstate'params 
type instance (R.ReprFor Session'snapshotDeleteWithVmstate'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotDeleteWithVmstate'params) where
    typeId  = 14347331234765982838
instance (C.TypedStruct Session'snapshotDeleteWithVmstate'params) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Session'snapshotDeleteWithVmstate'params) where
    type AllocHint Session'snapshotDeleteWithVmstate'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotDeleteWithVmstate'params (C.Parsed Session'snapshotDeleteWithVmstate'params))
instance (C.AllocateList Session'snapshotDeleteWithVmstate'params) where
    type ListAllocHint Session'snapshotDeleteWithVmstate'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotDeleteWithVmstate'params (C.Parsed Session'snapshotDeleteWithVmstate'params))
data instance C.Parsed Session'snapshotDeleteWithVmstate'params
    = Session'snapshotDeleteWithVmstate'params 
        {devicePaths :: (RP.Parsed (R.List Basics.Text))
        ,name :: (RP.Parsed Basics.Text)
        ,vmId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotDeleteWithVmstate'params))
deriving instance (Std_.Eq (C.Parsed Session'snapshotDeleteWithVmstate'params))
instance (C.Parse Session'snapshotDeleteWithVmstate'params (C.Parsed Session'snapshotDeleteWithVmstate'params)) where
    parse raw_ = (Session'snapshotDeleteWithVmstate'params <$> (GH.parseField #devicePaths raw_)
                                                           <*> (GH.parseField #name raw_)
                                                           <*> (GH.parseField #vmId raw_))
instance (C.Marshal Session'snapshotDeleteWithVmstate'params (C.Parsed Session'snapshotDeleteWithVmstate'params)) where
    marshalInto raw_ Session'snapshotDeleteWithVmstate'params{..} = (do
        (GH.encodeField #devicePaths devicePaths raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #vmId vmId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "devicePaths" GH.Slot Session'snapshotDeleteWithVmstate'params (R.List Basics.Text)) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "name" GH.Slot Session'snapshotDeleteWithVmstate'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "vmId" GH.Slot Session'snapshotDeleteWithVmstate'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Session'snapshotDeleteWithVmstate'results 
type instance (R.ReprFor Session'snapshotDeleteWithVmstate'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'snapshotDeleteWithVmstate'results) where
    typeId  = 14767847684973433713
instance (C.TypedStruct Session'snapshotDeleteWithVmstate'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'snapshotDeleteWithVmstate'results) where
    type AllocHint Session'snapshotDeleteWithVmstate'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'snapshotDeleteWithVmstate'results (C.Parsed Session'snapshotDeleteWithVmstate'results))
instance (C.AllocateList Session'snapshotDeleteWithVmstate'results) where
    type ListAllocHint Session'snapshotDeleteWithVmstate'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'snapshotDeleteWithVmstate'results (C.Parsed Session'snapshotDeleteWithVmstate'results))
data instance C.Parsed Session'snapshotDeleteWithVmstate'results
    = Session'snapshotDeleteWithVmstate'results 
        {result :: (RP.Parsed DiskOpResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'snapshotDeleteWithVmstate'results))
deriving instance (Std_.Eq (C.Parsed Session'snapshotDeleteWithVmstate'results))
instance (C.Parse Session'snapshotDeleteWithVmstate'results (C.Parsed Session'snapshotDeleteWithVmstate'results)) where
    parse raw_ = (Session'snapshotDeleteWithVmstate'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'snapshotDeleteWithVmstate'results (C.Parsed Session'snapshotDeleteWithVmstate'results)) where
    marshalInto raw_ Session'snapshotDeleteWithVmstate'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'snapshotDeleteWithVmstate'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
data Session'guestSetTime'params 
type instance (R.ReprFor Session'guestSetTime'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'guestSetTime'params) where
    typeId  = 12950672055964986808
instance (C.TypedStruct Session'guestSetTime'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Session'guestSetTime'params) where
    type AllocHint Session'guestSetTime'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'guestSetTime'params (C.Parsed Session'guestSetTime'params))
instance (C.AllocateList Session'guestSetTime'params) where
    type ListAllocHint Session'guestSetTime'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'guestSetTime'params (C.Parsed Session'guestSetTime'params))
data instance C.Parsed Session'guestSetTime'params
    = Session'guestSetTime'params 
        {vmId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'guestSetTime'params))
deriving instance (Std_.Eq (C.Parsed Session'guestSetTime'params))
instance (C.Parse Session'guestSetTime'params (C.Parsed Session'guestSetTime'params)) where
    parse raw_ = (Session'guestSetTime'params <$> (GH.parseField #vmId raw_))
instance (C.Marshal Session'guestSetTime'params (C.Parsed Session'guestSetTime'params)) where
    marshalInto raw_ Session'guestSetTime'params{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot Session'guestSetTime'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Session'guestSetTime'results 
type instance (R.ReprFor Session'guestSetTime'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Session'guestSetTime'results) where
    typeId  = 11219506602945326464
instance (C.TypedStruct Session'guestSetTime'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Session'guestSetTime'results) where
    type AllocHint Session'guestSetTime'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Session'guestSetTime'results (C.Parsed Session'guestSetTime'results))
instance (C.AllocateList Session'guestSetTime'results) where
    type ListAllocHint Session'guestSetTime'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Session'guestSetTime'results (C.Parsed Session'guestSetTime'results))
data instance C.Parsed Session'guestSetTime'results
    = Session'guestSetTime'results 
        {result :: (RP.Parsed DiskOpResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Session'guestSetTime'results))
deriving instance (Std_.Eq (C.Parsed Session'guestSetTime'results))
instance (C.Parse Session'guestSetTime'results (C.Parsed Session'guestSetTime'results)) where
    parse raw_ = (Session'guestSetTime'results <$> (GH.parseField #result raw_))
instance (C.Marshal Session'guestSetTime'results (C.Parsed Session'guestSetTime'results)) where
    marshalInto raw_ Session'guestSetTime'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Session'guestSetTime'results DiskOpResult) where
    fieldByLabel  = (GH.ptrField 0)
data DiskReader 
type instance (R.ReprFor DiskReader) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId DiskReader) where
    typeId  = 18266150226689627376
instance (C.Parse DiskReader (GH.Client DiskReader)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export DiskReader) where
    type Server DiskReader = DiskReader'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(DiskReader)) [(GH.toUntypedMethodHandler ((diskReader'pipeInto) s_))
                                                                            ,(GH.toUntypedMethodHandler ((diskReader'cancel) s_))] [])
class (DiskReader'server_ s_) where
    {-# MINIMAL diskReader'pipeInto,diskReader'cancel #-}
    diskReader'pipeInto :: s_ -> (GH.MethodHandler DiskReader'pipeInto'params DiskReader'pipeInto'results)
    diskReader'pipeInto _ = GH.methodUnimplemented
    diskReader'cancel :: s_ -> (GH.MethodHandler DiskReader'cancel'params DiskReader'cancel'results)
    diskReader'cancel _ = GH.methodUnimplemented
instance (GH.HasMethod "pipeInto" DiskReader DiskReader'pipeInto'params DiskReader'pipeInto'results) where
    methodByLabel  = (GH.Method 18266150226689627376 0)
instance (GH.HasMethod "cancel" DiskReader DiskReader'cancel'params DiskReader'cancel'results) where
    methodByLabel  = (GH.Method 18266150226689627376 1)
data DiskReader'pipeInto'params 
type instance (R.ReprFor DiskReader'pipeInto'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskReader'pipeInto'params) where
    typeId  = 10662965049043475550
instance (C.TypedStruct DiskReader'pipeInto'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskReader'pipeInto'params) where
    type AllocHint DiskReader'pipeInto'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskReader'pipeInto'params (C.Parsed DiskReader'pipeInto'params))
instance (C.AllocateList DiskReader'pipeInto'params) where
    type ListAllocHint DiskReader'pipeInto'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskReader'pipeInto'params (C.Parsed DiskReader'pipeInto'params))
data instance C.Parsed DiskReader'pipeInto'params
    = DiskReader'pipeInto'params 
        {sink :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.ByteSink)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskReader'pipeInto'params))
deriving instance (Std_.Eq (C.Parsed DiskReader'pipeInto'params))
instance (C.Parse DiskReader'pipeInto'params (C.Parsed DiskReader'pipeInto'params)) where
    parse raw_ = (DiskReader'pipeInto'params <$> (GH.parseField #sink raw_))
instance (C.Marshal DiskReader'pipeInto'params (C.Parsed DiskReader'pipeInto'params)) where
    marshalInto raw_ DiskReader'pipeInto'params{..} = (do
        (GH.encodeField #sink sink raw_)
        (Std_.pure ())
        )
instance (GH.HasField "sink" GH.Slot DiskReader'pipeInto'params Capnp.Gen.ById.X9bd452a518ed3917.ByteSink) where
    fieldByLabel  = (GH.ptrField 0)
data DiskReader'pipeInto'results 
type instance (R.ReprFor DiskReader'pipeInto'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskReader'pipeInto'results) where
    typeId  = 14293781581944344404
instance (C.TypedStruct DiskReader'pipeInto'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate DiskReader'pipeInto'results) where
    type AllocHint DiskReader'pipeInto'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskReader'pipeInto'results (C.Parsed DiskReader'pipeInto'results))
instance (C.AllocateList DiskReader'pipeInto'results) where
    type ListAllocHint DiskReader'pipeInto'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskReader'pipeInto'results (C.Parsed DiskReader'pipeInto'results))
data instance C.Parsed DiskReader'pipeInto'results
    = DiskReader'pipeInto'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskReader'pipeInto'results))
deriving instance (Std_.Eq (C.Parsed DiskReader'pipeInto'results))
instance (C.Parse DiskReader'pipeInto'results (C.Parsed DiskReader'pipeInto'results)) where
    parse raw_ = (Std_.pure DiskReader'pipeInto'results)
instance (C.Marshal DiskReader'pipeInto'results (C.Parsed DiskReader'pipeInto'results)) where
    marshalInto _raw (DiskReader'pipeInto'results) = (Std_.pure ())
data DiskReader'cancel'params 
type instance (R.ReprFor DiskReader'cancel'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskReader'cancel'params) where
    typeId  = 16505186430357742365
instance (C.TypedStruct DiskReader'cancel'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate DiskReader'cancel'params) where
    type AllocHint DiskReader'cancel'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskReader'cancel'params (C.Parsed DiskReader'cancel'params))
instance (C.AllocateList DiskReader'cancel'params) where
    type ListAllocHint DiskReader'cancel'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskReader'cancel'params (C.Parsed DiskReader'cancel'params))
data instance C.Parsed DiskReader'cancel'params
    = DiskReader'cancel'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskReader'cancel'params))
deriving instance (Std_.Eq (C.Parsed DiskReader'cancel'params))
instance (C.Parse DiskReader'cancel'params (C.Parsed DiskReader'cancel'params)) where
    parse raw_ = (Std_.pure DiskReader'cancel'params)
instance (C.Marshal DiskReader'cancel'params (C.Parsed DiskReader'cancel'params)) where
    marshalInto _raw (DiskReader'cancel'params) = (Std_.pure ())
data DiskReader'cancel'results 
type instance (R.ReprFor DiskReader'cancel'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskReader'cancel'results) where
    typeId  = 14645444459278494587
instance (C.TypedStruct DiskReader'cancel'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate DiskReader'cancel'results) where
    type AllocHint DiskReader'cancel'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskReader'cancel'results (C.Parsed DiskReader'cancel'results))
instance (C.AllocateList DiskReader'cancel'results) where
    type ListAllocHint DiskReader'cancel'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskReader'cancel'results (C.Parsed DiskReader'cancel'results))
data instance C.Parsed DiskReader'cancel'results
    = DiskReader'cancel'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskReader'cancel'results))
deriving instance (Std_.Eq (C.Parsed DiskReader'cancel'results))
instance (C.Parse DiskReader'cancel'results (C.Parsed DiskReader'cancel'results)) where
    parse raw_ = (Std_.pure DiskReader'cancel'results)
instance (C.Marshal DiskReader'cancel'results (C.Parsed DiskReader'cancel'results)) where
    marshalInto _raw (DiskReader'cancel'results) = (Std_.pure ())
data VmStatusSink 
type instance (R.ReprFor VmStatusSink) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId VmStatusSink) where
    typeId  = 16931578229482041897
instance (C.Parse VmStatusSink (GH.Client VmStatusSink)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export VmStatusSink) where
    type Server VmStatusSink = VmStatusSink'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(VmStatusSink)) [(GH.toUntypedMethodHandler ((vmStatusSink'onSnapshot) s_))] [])
class (VmStatusSink'server_ s_) where
    {-# MINIMAL vmStatusSink'onSnapshot #-}
    vmStatusSink'onSnapshot :: s_ -> (GH.MethodHandler VmStatusSink'onSnapshot'params VmStatusSink'onSnapshot'results)
    vmStatusSink'onSnapshot _ = GH.methodUnimplemented
instance (GH.HasMethod "onSnapshot" VmStatusSink VmStatusSink'onSnapshot'params VmStatusSink'onSnapshot'results) where
    methodByLabel  = (GH.Method 16931578229482041897 0)
data VmStatusSink'onSnapshot'params 
type instance (R.ReprFor VmStatusSink'onSnapshot'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmStatusSink'onSnapshot'params) where
    typeId  = 11931450306569493951
instance (C.TypedStruct VmStatusSink'onSnapshot'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate VmStatusSink'onSnapshot'params) where
    type AllocHint VmStatusSink'onSnapshot'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmStatusSink'onSnapshot'params (C.Parsed VmStatusSink'onSnapshot'params))
instance (C.AllocateList VmStatusSink'onSnapshot'params) where
    type ListAllocHint VmStatusSink'onSnapshot'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmStatusSink'onSnapshot'params (C.Parsed VmStatusSink'onSnapshot'params))
data instance C.Parsed VmStatusSink'onSnapshot'params
    = VmStatusSink'onSnapshot'params 
        {snapshot :: (RP.Parsed VmStatusSnapshot)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmStatusSink'onSnapshot'params))
deriving instance (Std_.Eq (C.Parsed VmStatusSink'onSnapshot'params))
instance (C.Parse VmStatusSink'onSnapshot'params (C.Parsed VmStatusSink'onSnapshot'params)) where
    parse raw_ = (VmStatusSink'onSnapshot'params <$> (GH.parseField #snapshot raw_))
instance (C.Marshal VmStatusSink'onSnapshot'params (C.Parsed VmStatusSink'onSnapshot'params)) where
    marshalInto raw_ VmStatusSink'onSnapshot'params{..} = (do
        (GH.encodeField #snapshot snapshot raw_)
        (Std_.pure ())
        )
instance (GH.HasField "snapshot" GH.Slot VmStatusSink'onSnapshot'params VmStatusSnapshot) where
    fieldByLabel  = (GH.ptrField 0)
data VmStatusSink'onSnapshot'results 
type instance (R.ReprFor VmStatusSink'onSnapshot'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmStatusSink'onSnapshot'results) where
    typeId  = 9367394179805790926
instance (C.TypedStruct VmStatusSink'onSnapshot'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate VmStatusSink'onSnapshot'results) where
    type AllocHint VmStatusSink'onSnapshot'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmStatusSink'onSnapshot'results (C.Parsed VmStatusSink'onSnapshot'results))
instance (C.AllocateList VmStatusSink'onSnapshot'results) where
    type ListAllocHint VmStatusSink'onSnapshot'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmStatusSink'onSnapshot'results (C.Parsed VmStatusSink'onSnapshot'results))
data instance C.Parsed VmStatusSink'onSnapshot'results
    = VmStatusSink'onSnapshot'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmStatusSink'onSnapshot'results))
deriving instance (Std_.Eq (C.Parsed VmStatusSink'onSnapshot'results))
instance (C.Parse VmStatusSink'onSnapshot'results (C.Parsed VmStatusSink'onSnapshot'results)) where
    parse raw_ = (Std_.pure VmStatusSink'onSnapshot'results)
instance (C.Marshal VmStatusSink'onSnapshot'results (C.Parsed VmStatusSink'onSnapshot'results)) where
    marshalInto _raw (VmStatusSink'onSnapshot'results) = (Std_.pure ())
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
data VmSpec 
type instance (R.ReprFor VmSpec) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmSpec) where
    typeId  = 13869854766932503674
instance (C.TypedStruct VmSpec) where
    numStructWords  = 4
    numStructPtrs  = 6
instance (C.Allocate VmSpec) where
    type AllocHint VmSpec = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmSpec (C.Parsed VmSpec))
instance (C.AllocateList VmSpec) where
    type ListAllocHint VmSpec = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmSpec (C.Parsed VmSpec))
data instance C.Parsed VmSpec
    = VmSpec 
        {vmId :: (RP.Parsed Std_.Int64)
        ,name :: (RP.Parsed Basics.Text)
        ,cpuCount :: (RP.Parsed Std_.Int32)
        ,ramMb :: (RP.Parsed Std_.Int32)
        ,headless :: (RP.Parsed Std_.Bool)
        ,guestAgent :: (RP.Parsed Std_.Bool)
        ,vsockCid :: (RP.Parsed Std_.Word32)
        ,hasVsockCid :: (RP.Parsed Std_.Bool)
        ,spicePort :: (RP.Parsed Std_.Int32)
        ,hasSpicePort :: (RP.Parsed Std_.Bool)
        ,drives :: (RP.Parsed (R.List VmDriveSpec))
        ,netIfs :: (RP.Parsed (R.List VmNetIfSpec))
        ,sharedDirs :: (RP.Parsed (R.List VmSharedDirSpec))
        ,waitForGuestAgentMs :: (RP.Parsed Std_.Word32)
        ,rebootQuirk :: (RP.Parsed Std_.Bool)
        ,spiceBindAddr :: (RP.Parsed Basics.Text)
        ,loadFromSavedState :: (RP.Parsed Std_.Bool)
        ,cpuModel :: (RP.Parsed Basics.Text)
        ,startPaused :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmSpec))
deriving instance (Std_.Eq (C.Parsed VmSpec))
instance (C.Parse VmSpec (C.Parsed VmSpec)) where
    parse raw_ = (VmSpec <$> (GH.parseField #vmId raw_)
                         <*> (GH.parseField #name raw_)
                         <*> (GH.parseField #cpuCount raw_)
                         <*> (GH.parseField #ramMb raw_)
                         <*> (GH.parseField #headless raw_)
                         <*> (GH.parseField #guestAgent raw_)
                         <*> (GH.parseField #vsockCid raw_)
                         <*> (GH.parseField #hasVsockCid raw_)
                         <*> (GH.parseField #spicePort raw_)
                         <*> (GH.parseField #hasSpicePort raw_)
                         <*> (GH.parseField #drives raw_)
                         <*> (GH.parseField #netIfs raw_)
                         <*> (GH.parseField #sharedDirs raw_)
                         <*> (GH.parseField #waitForGuestAgentMs raw_)
                         <*> (GH.parseField #rebootQuirk raw_)
                         <*> (GH.parseField #spiceBindAddr raw_)
                         <*> (GH.parseField #loadFromSavedState raw_)
                         <*> (GH.parseField #cpuModel raw_)
                         <*> (GH.parseField #startPaused raw_))
instance (C.Marshal VmSpec (C.Parsed VmSpec)) where
    marshalInto raw_ VmSpec{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #cpuCount cpuCount raw_)
        (GH.encodeField #ramMb ramMb raw_)
        (GH.encodeField #headless headless raw_)
        (GH.encodeField #guestAgent guestAgent raw_)
        (GH.encodeField #vsockCid vsockCid raw_)
        (GH.encodeField #hasVsockCid hasVsockCid raw_)
        (GH.encodeField #spicePort spicePort raw_)
        (GH.encodeField #hasSpicePort hasSpicePort raw_)
        (GH.encodeField #drives drives raw_)
        (GH.encodeField #netIfs netIfs raw_)
        (GH.encodeField #sharedDirs sharedDirs raw_)
        (GH.encodeField #waitForGuestAgentMs waitForGuestAgentMs raw_)
        (GH.encodeField #rebootQuirk rebootQuirk raw_)
        (GH.encodeField #spiceBindAddr spiceBindAddr raw_)
        (GH.encodeField #loadFromSavedState loadFromSavedState raw_)
        (GH.encodeField #cpuModel cpuModel raw_)
        (GH.encodeField #startPaused startPaused raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot VmSpec Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "name" GH.Slot VmSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "cpuCount" GH.Slot VmSpec Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
instance (GH.HasField "ramMb" GH.Slot VmSpec Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 1 32 0)
instance (GH.HasField "headless" GH.Slot VmSpec Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 2 1 0)
instance (GH.HasField "guestAgent" GH.Slot VmSpec Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 2 1 0)
instance (GH.HasField "vsockCid" GH.Slot VmSpec Std_.Word32) where
    fieldByLabel  = (GH.dataField 32 2 32 0)
instance (GH.HasField "hasVsockCid" GH.Slot VmSpec Std_.Bool) where
    fieldByLabel  = (GH.dataField 2 2 1 0)
instance (GH.HasField "spicePort" GH.Slot VmSpec Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 3 32 0)
instance (GH.HasField "hasSpicePort" GH.Slot VmSpec Std_.Bool) where
    fieldByLabel  = (GH.dataField 3 2 1 0)
instance (GH.HasField "drives" GH.Slot VmSpec (R.List VmDriveSpec)) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "netIfs" GH.Slot VmSpec (R.List VmNetIfSpec)) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "sharedDirs" GH.Slot VmSpec (R.List VmSharedDirSpec)) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "waitForGuestAgentMs" GH.Slot VmSpec Std_.Word32) where
    fieldByLabel  = (GH.dataField 32 3 32 0)
instance (GH.HasField "rebootQuirk" GH.Slot VmSpec Std_.Bool) where
    fieldByLabel  = (GH.dataField 4 2 1 0)
instance (GH.HasField "spiceBindAddr" GH.Slot VmSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 4)
instance (GH.HasField "loadFromSavedState" GH.Slot VmSpec Std_.Bool) where
    fieldByLabel  = (GH.dataField 5 2 1 0)
instance (GH.HasField "cpuModel" GH.Slot VmSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 5)
instance (GH.HasField "startPaused" GH.Slot VmSpec Std_.Bool) where
    fieldByLabel  = (GH.dataField 6 2 1 0)
data VmDriveSpec 
type instance (R.ReprFor VmDriveSpec) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmDriveSpec) where
    typeId  = 13130167343872007904
instance (C.TypedStruct VmDriveSpec) where
    numStructWords  = 1
    numStructPtrs  = 5
instance (C.Allocate VmDriveSpec) where
    type AllocHint VmDriveSpec = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmDriveSpec (C.Parsed VmDriveSpec))
instance (C.AllocateList VmDriveSpec) where
    type ListAllocHint VmDriveSpec = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmDriveSpec (C.Parsed VmDriveSpec))
data instance C.Parsed VmDriveSpec
    = VmDriveSpec 
        {diskFilePath :: (RP.Parsed Basics.Text)
        ,format :: (RP.Parsed Basics.Text)
        ,ifKind :: (RP.Parsed Basics.Text)
        ,media :: (RP.Parsed Basics.Text)
        ,readOnly :: (RP.Parsed Std_.Bool)
        ,cache :: (RP.Parsed Basics.Text)
        ,discard :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmDriveSpec))
deriving instance (Std_.Eq (C.Parsed VmDriveSpec))
instance (C.Parse VmDriveSpec (C.Parsed VmDriveSpec)) where
    parse raw_ = (VmDriveSpec <$> (GH.parseField #diskFilePath raw_)
                              <*> (GH.parseField #format raw_)
                              <*> (GH.parseField #ifKind raw_)
                              <*> (GH.parseField #media raw_)
                              <*> (GH.parseField #readOnly raw_)
                              <*> (GH.parseField #cache raw_)
                              <*> (GH.parseField #discard raw_))
instance (C.Marshal VmDriveSpec (C.Parsed VmDriveSpec)) where
    marshalInto raw_ VmDriveSpec{..} = (do
        (GH.encodeField #diskFilePath diskFilePath raw_)
        (GH.encodeField #format format raw_)
        (GH.encodeField #ifKind ifKind raw_)
        (GH.encodeField #media media raw_)
        (GH.encodeField #readOnly readOnly raw_)
        (GH.encodeField #cache cache raw_)
        (GH.encodeField #discard discard raw_)
        (Std_.pure ())
        )
instance (GH.HasField "diskFilePath" GH.Slot VmDriveSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "format" GH.Slot VmDriveSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "ifKind" GH.Slot VmDriveSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "media" GH.Slot VmDriveSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "readOnly" GH.Slot VmDriveSpec Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (GH.HasField "cache" GH.Slot VmDriveSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 4)
instance (GH.HasField "discard" GH.Slot VmDriveSpec Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 0 1 0)
data VmNetIfSpec 
type instance (R.ReprFor VmNetIfSpec) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmNetIfSpec) where
    typeId  = 11985747815183304231
instance (C.TypedStruct VmNetIfSpec) where
    numStructWords  = 0
    numStructPtrs  = 3
instance (C.Allocate VmNetIfSpec) where
    type AllocHint VmNetIfSpec = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmNetIfSpec (C.Parsed VmNetIfSpec))
instance (C.AllocateList VmNetIfSpec) where
    type ListAllocHint VmNetIfSpec = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmNetIfSpec (C.Parsed VmNetIfSpec))
data instance C.Parsed VmNetIfSpec
    = VmNetIfSpec 
        {ifType :: (RP.Parsed Basics.Text)
        ,hostDevice :: (RP.Parsed Basics.Text)
        ,macAddress :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmNetIfSpec))
deriving instance (Std_.Eq (C.Parsed VmNetIfSpec))
instance (C.Parse VmNetIfSpec (C.Parsed VmNetIfSpec)) where
    parse raw_ = (VmNetIfSpec <$> (GH.parseField #ifType raw_)
                              <*> (GH.parseField #hostDevice raw_)
                              <*> (GH.parseField #macAddress raw_))
instance (C.Marshal VmNetIfSpec (C.Parsed VmNetIfSpec)) where
    marshalInto raw_ VmNetIfSpec{..} = (do
        (GH.encodeField #ifType ifType raw_)
        (GH.encodeField #hostDevice hostDevice raw_)
        (GH.encodeField #macAddress macAddress raw_)
        (Std_.pure ())
        )
instance (GH.HasField "ifType" GH.Slot VmNetIfSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "hostDevice" GH.Slot VmNetIfSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "macAddress" GH.Slot VmNetIfSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
data VmSharedDirSpec 
type instance (R.ReprFor VmSharedDirSpec) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmSharedDirSpec) where
    typeId  = 17871272060610999230
instance (C.TypedStruct VmSharedDirSpec) where
    numStructWords  = 1
    numStructPtrs  = 3
instance (C.Allocate VmSharedDirSpec) where
    type AllocHint VmSharedDirSpec = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmSharedDirSpec (C.Parsed VmSharedDirSpec))
instance (C.AllocateList VmSharedDirSpec) where
    type ListAllocHint VmSharedDirSpec = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmSharedDirSpec (C.Parsed VmSharedDirSpec))
data instance C.Parsed VmSharedDirSpec
    = VmSharedDirSpec 
        {hostPath :: (RP.Parsed Basics.Text)
        ,tag :: (RP.Parsed Basics.Text)
        ,cache :: (RP.Parsed Basics.Text)
        ,readOnly :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmSharedDirSpec))
deriving instance (Std_.Eq (C.Parsed VmSharedDirSpec))
instance (C.Parse VmSharedDirSpec (C.Parsed VmSharedDirSpec)) where
    parse raw_ = (VmSharedDirSpec <$> (GH.parseField #hostPath raw_)
                                  <*> (GH.parseField #tag raw_)
                                  <*> (GH.parseField #cache raw_)
                                  <*> (GH.parseField #readOnly raw_))
instance (C.Marshal VmSharedDirSpec (C.Parsed VmSharedDirSpec)) where
    marshalInto raw_ VmSharedDirSpec{..} = (do
        (GH.encodeField #hostPath hostPath raw_)
        (GH.encodeField #tag tag raw_)
        (GH.encodeField #cache cache raw_)
        (GH.encodeField #readOnly readOnly raw_)
        (Std_.pure ())
        )
instance (GH.HasField "hostPath" GH.Slot VmSharedDirSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "tag" GH.Slot VmSharedDirSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "cache" GH.Slot VmSharedDirSpec Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "readOnly" GH.Slot VmSharedDirSpec Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
data VmRuntimeInfo 
type instance (R.ReprFor VmRuntimeInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmRuntimeInfo) where
    typeId  = 16495572734220069740
instance (C.TypedStruct VmRuntimeInfo) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate VmRuntimeInfo) where
    type AllocHint VmRuntimeInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmRuntimeInfo (C.Parsed VmRuntimeInfo))
instance (C.AllocateList VmRuntimeInfo) where
    type ListAllocHint VmRuntimeInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmRuntimeInfo (C.Parsed VmRuntimeInfo))
data instance C.Parsed VmRuntimeInfo
    = VmRuntimeInfo 
        {qemuPid :: (RP.Parsed Std_.Int32)
        ,virtiofsdPids :: (RP.Parsed (R.List Std_.Int32))
        ,spicePort :: (RP.Parsed Std_.Int32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmRuntimeInfo))
deriving instance (Std_.Eq (C.Parsed VmRuntimeInfo))
instance (C.Parse VmRuntimeInfo (C.Parsed VmRuntimeInfo)) where
    parse raw_ = (VmRuntimeInfo <$> (GH.parseField #qemuPid raw_)
                                <*> (GH.parseField #virtiofsdPids raw_)
                                <*> (GH.parseField #spicePort raw_))
instance (C.Marshal VmRuntimeInfo (C.Parsed VmRuntimeInfo)) where
    marshalInto raw_ VmRuntimeInfo{..} = (do
        (GH.encodeField #qemuPid qemuPid raw_)
        (GH.encodeField #virtiofsdPids virtiofsdPids raw_)
        (GH.encodeField #spicePort spicePort raw_)
        (Std_.pure ())
        )
instance (GH.HasField "qemuPid" GH.Slot VmRuntimeInfo Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "virtiofsdPids" GH.Slot VmRuntimeInfo (R.List Std_.Int32)) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "spicePort" GH.Slot VmRuntimeInfo Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
data VmStopResult 
type instance (R.ReprFor VmStopResult) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmStopResult) where
    typeId  = 17427776312475992219
instance (C.TypedStruct VmStopResult) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate VmStopResult) where
    type AllocHint VmStopResult = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmStopResult (C.Parsed VmStopResult))
instance (C.AllocateList VmStopResult) where
    type ListAllocHint VmStopResult = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmStopResult (C.Parsed VmStopResult))
data instance C.Parsed VmStopResult
    = VmStopResult 
        {kind :: (RP.Parsed VmStopKind)
        ,message :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmStopResult))
deriving instance (Std_.Eq (C.Parsed VmStopResult))
instance (C.Parse VmStopResult (C.Parsed VmStopResult)) where
    parse raw_ = (VmStopResult <$> (GH.parseField #kind raw_)
                               <*> (GH.parseField #message raw_))
instance (C.Marshal VmStopResult (C.Parsed VmStopResult)) where
    marshalInto raw_ VmStopResult{..} = (do
        (GH.encodeField #kind kind raw_)
        (GH.encodeField #message message raw_)
        (Std_.pure ())
        )
instance (GH.HasField "kind" GH.Slot VmStopResult VmStopKind) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "message" GH.Slot VmStopResult Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data VmStopKind 
    = VmStopKind'stopped 
    | VmStopKind'alreadyStopped 
    | VmStopKind'timeout 
    | VmStopKind'failed 
    | VmStopKind'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor VmStopKind) = (R.Data R.Sz16)
instance (C.HasTypeId VmStopKind) where
    typeId  = 11795038726096318266
instance (Std_.Enum VmStopKind) where
    toEnum n_ = case n_ of
        0 ->
            VmStopKind'stopped
        1 ->
            VmStopKind'alreadyStopped
        2 ->
            VmStopKind'timeout
        3 ->
            VmStopKind'failed
        tag_ ->
            (VmStopKind'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (VmStopKind'stopped) ->
            0
        (VmStopKind'alreadyStopped) ->
            1
        (VmStopKind'timeout) ->
            2
        (VmStopKind'failed) ->
            3
        (VmStopKind'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord VmStopKind) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse VmStopKind VmStopKind) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList VmStopKind) where
    type ListAllocHint VmStopKind = Std_.Int
instance (C.EstimateListAlloc VmStopKind VmStopKind)
data VmAgentStatus 
type instance (R.ReprFor VmAgentStatus) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmAgentStatus) where
    typeId  = 13243773639774657792
instance (C.TypedStruct VmAgentStatus) where
    numStructWords  = 2
    numStructPtrs  = 0
instance (C.Allocate VmAgentStatus) where
    type AllocHint VmAgentStatus = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmAgentStatus (C.Parsed VmAgentStatus))
instance (C.AllocateList VmAgentStatus) where
    type ListAllocHint VmAgentStatus = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmAgentStatus (C.Parsed VmAgentStatus))
data instance C.Parsed VmAgentStatus
    = VmAgentStatus 
        {state :: (RP.Parsed VmAgentState)
        ,qemuPid :: (RP.Parsed Std_.Int32)
        ,lastExitCode :: (RP.Parsed Std_.Int32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmAgentStatus))
deriving instance (Std_.Eq (C.Parsed VmAgentStatus))
instance (C.Parse VmAgentStatus (C.Parsed VmAgentStatus)) where
    parse raw_ = (VmAgentStatus <$> (GH.parseField #state raw_)
                                <*> (GH.parseField #qemuPid raw_)
                                <*> (GH.parseField #lastExitCode raw_))
instance (C.Marshal VmAgentStatus (C.Parsed VmAgentStatus)) where
    marshalInto raw_ VmAgentStatus{..} = (do
        (GH.encodeField #state state raw_)
        (GH.encodeField #qemuPid qemuPid raw_)
        (GH.encodeField #lastExitCode lastExitCode raw_)
        (Std_.pure ())
        )
instance (GH.HasField "state" GH.Slot VmAgentStatus VmAgentState) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "qemuPid" GH.Slot VmAgentStatus Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
instance (GH.HasField "lastExitCode" GH.Slot VmAgentStatus Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
data VmAgentState 
    = VmAgentState'running 
    | VmAgentState'stopped 
    | VmAgentState'errored 
    | VmAgentState'unknown 
    | VmAgentState'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor VmAgentState) = (R.Data R.Sz16)
instance (C.HasTypeId VmAgentState) where
    typeId  = 12918804164212289735
instance (Std_.Enum VmAgentState) where
    toEnum n_ = case n_ of
        0 ->
            VmAgentState'running
        1 ->
            VmAgentState'stopped
        2 ->
            VmAgentState'errored
        3 ->
            VmAgentState'unknown
        tag_ ->
            (VmAgentState'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (VmAgentState'running) ->
            0
        (VmAgentState'stopped) ->
            1
        (VmAgentState'errored) ->
            2
        (VmAgentState'unknown) ->
            3
        (VmAgentState'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord VmAgentState) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse VmAgentState VmAgentState) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList VmAgentState) where
    type ListAllocHint VmAgentState = Std_.Int
instance (C.EstimateListAlloc VmAgentState VmAgentState)
data VmAttachDriveReq 
type instance (R.ReprFor VmAttachDriveReq) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmAttachDriveReq) where
    typeId  = 13876782660338277154
instance (C.TypedStruct VmAttachDriveReq) where
    numStructWords  = 3
    numStructPtrs  = 3
instance (C.Allocate VmAttachDriveReq) where
    type AllocHint VmAttachDriveReq = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmAttachDriveReq (C.Parsed VmAttachDriveReq))
instance (C.AllocateList VmAttachDriveReq) where
    type ListAllocHint VmAttachDriveReq = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmAttachDriveReq (C.Parsed VmAttachDriveReq))
data instance C.Parsed VmAttachDriveReq
    = VmAttachDriveReq 
        {vmId :: (RP.Parsed Std_.Int64)
        ,driveId :: (RP.Parsed Std_.Int64)
        ,filePath :: (RP.Parsed Basics.Text)
        ,format :: (RP.Parsed Basics.Text)
        ,ifKind :: (RP.Parsed Basics.Text)
        ,readOnly :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmAttachDriveReq))
deriving instance (Std_.Eq (C.Parsed VmAttachDriveReq))
instance (C.Parse VmAttachDriveReq (C.Parsed VmAttachDriveReq)) where
    parse raw_ = (VmAttachDriveReq <$> (GH.parseField #vmId raw_)
                                   <*> (GH.parseField #driveId raw_)
                                   <*> (GH.parseField #filePath raw_)
                                   <*> (GH.parseField #format raw_)
                                   <*> (GH.parseField #ifKind raw_)
                                   <*> (GH.parseField #readOnly raw_))
instance (C.Marshal VmAttachDriveReq (C.Parsed VmAttachDriveReq)) where
    marshalInto raw_ VmAttachDriveReq{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (GH.encodeField #driveId driveId raw_)
        (GH.encodeField #filePath filePath raw_)
        (GH.encodeField #format format raw_)
        (GH.encodeField #ifKind ifKind raw_)
        (GH.encodeField #readOnly readOnly raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot VmAttachDriveReq Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "driveId" GH.Slot VmAttachDriveReq Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "filePath" GH.Slot VmAttachDriveReq Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "format" GH.Slot VmAttachDriveReq Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "ifKind" GH.Slot VmAttachDriveReq Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "readOnly" GH.Slot VmAttachDriveReq Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 2 1 0)
data VmGuestExecReq 
type instance (R.ReprFor VmGuestExecReq) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmGuestExecReq) where
    typeId  = 11296655065212550067
instance (C.TypedStruct VmGuestExecReq) where
    numStructWords  = 2
    numStructPtrs  = 3
instance (C.Allocate VmGuestExecReq) where
    type AllocHint VmGuestExecReq = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmGuestExecReq (C.Parsed VmGuestExecReq))
instance (C.AllocateList VmGuestExecReq) where
    type ListAllocHint VmGuestExecReq = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmGuestExecReq (C.Parsed VmGuestExecReq))
data instance C.Parsed VmGuestExecReq
    = VmGuestExecReq 
        {vmId :: (RP.Parsed Std_.Int64)
        ,path :: (RP.Parsed Basics.Text)
        ,args :: (RP.Parsed (R.List Basics.Text))
        ,captureOutput :: (RP.Parsed Std_.Bool)
        ,inputData :: (RP.Parsed Basics.Data)
        ,timeoutSec :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmGuestExecReq))
deriving instance (Std_.Eq (C.Parsed VmGuestExecReq))
instance (C.Parse VmGuestExecReq (C.Parsed VmGuestExecReq)) where
    parse raw_ = (VmGuestExecReq <$> (GH.parseField #vmId raw_)
                                 <*> (GH.parseField #path raw_)
                                 <*> (GH.parseField #args raw_)
                                 <*> (GH.parseField #captureOutput raw_)
                                 <*> (GH.parseField #inputData raw_)
                                 <*> (GH.parseField #timeoutSec raw_))
instance (C.Marshal VmGuestExecReq (C.Parsed VmGuestExecReq)) where
    marshalInto raw_ VmGuestExecReq{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (GH.encodeField #path path raw_)
        (GH.encodeField #args args raw_)
        (GH.encodeField #captureOutput captureOutput raw_)
        (GH.encodeField #inputData inputData raw_)
        (GH.encodeField #timeoutSec timeoutSec raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot VmGuestExecReq Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "path" GH.Slot VmGuestExecReq Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "args" GH.Slot VmGuestExecReq (R.List Basics.Text)) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "captureOutput" GH.Slot VmGuestExecReq Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 1 1 0)
instance (GH.HasField "inputData" GH.Slot VmGuestExecReq Basics.Data) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "timeoutSec" GH.Slot VmGuestExecReq Std_.Word32) where
    fieldByLabel  = (GH.dataField 32 1 32 0)
data VmGuestExecInfo 
type instance (R.ReprFor VmGuestExecInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmGuestExecInfo) where
    typeId  = 9696248131774112025
instance (C.TypedStruct VmGuestExecInfo) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate VmGuestExecInfo) where
    type AllocHint VmGuestExecInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmGuestExecInfo (C.Parsed VmGuestExecInfo))
instance (C.AllocateList VmGuestExecInfo) where
    type ListAllocHint VmGuestExecInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmGuestExecInfo (C.Parsed VmGuestExecInfo))
data instance C.Parsed VmGuestExecInfo
    = VmGuestExecInfo 
        {exitCode :: (RP.Parsed Std_.Int32)
        ,hasExit :: (RP.Parsed Std_.Bool)
        ,signal :: (RP.Parsed Std_.Int32)
        ,stdout :: (RP.Parsed Basics.Data)
        ,stderr :: (RP.Parsed Basics.Data)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmGuestExecInfo))
deriving instance (Std_.Eq (C.Parsed VmGuestExecInfo))
instance (C.Parse VmGuestExecInfo (C.Parsed VmGuestExecInfo)) where
    parse raw_ = (VmGuestExecInfo <$> (GH.parseField #exitCode raw_)
                                  <*> (GH.parseField #hasExit raw_)
                                  <*> (GH.parseField #signal raw_)
                                  <*> (GH.parseField #stdout raw_)
                                  <*> (GH.parseField #stderr raw_))
instance (C.Marshal VmGuestExecInfo (C.Parsed VmGuestExecInfo)) where
    marshalInto raw_ VmGuestExecInfo{..} = (do
        (GH.encodeField #exitCode exitCode raw_)
        (GH.encodeField #hasExit hasExit raw_)
        (GH.encodeField #signal signal raw_)
        (GH.encodeField #stdout stdout raw_)
        (GH.encodeField #stderr stderr raw_)
        (Std_.pure ())
        )
instance (GH.HasField "exitCode" GH.Slot VmGuestExecInfo Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "hasExit" GH.Slot VmGuestExecInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 32 0 1 0)
instance (GH.HasField "signal" GH.Slot VmGuestExecInfo Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
instance (GH.HasField "stdout" GH.Slot VmGuestExecInfo Basics.Data) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "stderr" GH.Slot VmGuestExecInfo Basics.Data) where
    fieldByLabel  = (GH.ptrField 1)
data VmStatusSnapshot 
type instance (R.ReprFor VmStatusSnapshot) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmStatusSnapshot) where
    typeId  = 12776058269055557477
instance (C.TypedStruct VmStatusSnapshot) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate VmStatusSnapshot) where
    type AllocHint VmStatusSnapshot = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmStatusSnapshot (C.Parsed VmStatusSnapshot))
instance (C.AllocateList VmStatusSnapshot) where
    type ListAllocHint VmStatusSnapshot = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmStatusSnapshot (C.Parsed VmStatusSnapshot))
data instance C.Parsed VmStatusSnapshot
    = VmStatusSnapshot 
        {snapshotAtMillis :: (RP.Parsed Std_.Int64)
        ,entries :: (RP.Parsed (R.List VmStatusEntry))
        ,nodeStats :: (RP.Parsed NodeStats)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmStatusSnapshot))
deriving instance (Std_.Eq (C.Parsed VmStatusSnapshot))
instance (C.Parse VmStatusSnapshot (C.Parsed VmStatusSnapshot)) where
    parse raw_ = (VmStatusSnapshot <$> (GH.parseField #snapshotAtMillis raw_)
                                   <*> (GH.parseField #entries raw_)
                                   <*> (GH.parseField #nodeStats raw_))
instance (C.Marshal VmStatusSnapshot (C.Parsed VmStatusSnapshot)) where
    marshalInto raw_ VmStatusSnapshot{..} = (do
        (GH.encodeField #snapshotAtMillis snapshotAtMillis raw_)
        (GH.encodeField #entries entries raw_)
        (GH.encodeField #nodeStats nodeStats raw_)
        (Std_.pure ())
        )
instance (GH.HasField "snapshotAtMillis" GH.Slot VmStatusSnapshot Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "entries" GH.Slot VmStatusSnapshot (R.List VmStatusEntry)) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "nodeStats" GH.Slot VmStatusSnapshot NodeStats) where
    fieldByLabel  = (GH.ptrField 1)
data NodeStats 
type instance (R.ReprFor NodeStats) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeStats) where
    typeId  = 17976094670731628670
instance (C.TypedStruct NodeStats) where
    numStructWords  = 7
    numStructPtrs  = 2
instance (C.Allocate NodeStats) where
    type AllocHint NodeStats = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeStats (C.Parsed NodeStats))
instance (C.AllocateList NodeStats) where
    type ListAllocHint NodeStats = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeStats (C.Parsed NodeStats))
data instance C.Parsed NodeStats
    = NodeStats 
        {cpuCount :: (RP.Parsed Std_.Int32)
        ,ramMbTotal :: (RP.Parsed Std_.Int32)
        ,ramMbFree :: (RP.Parsed Std_.Int32)
        ,storageBytesTotal :: (RP.Parsed Std_.Int64)
        ,storageBytesFree :: (RP.Parsed Std_.Int64)
        ,loadAvg1 :: (RP.Parsed Std_.Double)
        ,loadAvg5 :: (RP.Parsed Std_.Double)
        ,loadAvg15 :: (RP.Parsed Std_.Double)
        ,kernelRelease :: (RP.Parsed Basics.Text)
        ,agentVersion :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeStats))
deriving instance (Std_.Eq (C.Parsed NodeStats))
instance (C.Parse NodeStats (C.Parsed NodeStats)) where
    parse raw_ = (NodeStats <$> (GH.parseField #cpuCount raw_)
                            <*> (GH.parseField #ramMbTotal raw_)
                            <*> (GH.parseField #ramMbFree raw_)
                            <*> (GH.parseField #storageBytesTotal raw_)
                            <*> (GH.parseField #storageBytesFree raw_)
                            <*> (GH.parseField #loadAvg1 raw_)
                            <*> (GH.parseField #loadAvg5 raw_)
                            <*> (GH.parseField #loadAvg15 raw_)
                            <*> (GH.parseField #kernelRelease raw_)
                            <*> (GH.parseField #agentVersion raw_))
instance (C.Marshal NodeStats (C.Parsed NodeStats)) where
    marshalInto raw_ NodeStats{..} = (do
        (GH.encodeField #cpuCount cpuCount raw_)
        (GH.encodeField #ramMbTotal ramMbTotal raw_)
        (GH.encodeField #ramMbFree ramMbFree raw_)
        (GH.encodeField #storageBytesTotal storageBytesTotal raw_)
        (GH.encodeField #storageBytesFree storageBytesFree raw_)
        (GH.encodeField #loadAvg1 loadAvg1 raw_)
        (GH.encodeField #loadAvg5 loadAvg5 raw_)
        (GH.encodeField #loadAvg15 loadAvg15 raw_)
        (GH.encodeField #kernelRelease kernelRelease raw_)
        (GH.encodeField #agentVersion agentVersion raw_)
        (Std_.pure ())
        )
instance (GH.HasField "cpuCount" GH.Slot NodeStats Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "ramMbTotal" GH.Slot NodeStats Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
instance (GH.HasField "ramMbFree" GH.Slot NodeStats Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
instance (GH.HasField "storageBytesTotal" GH.Slot NodeStats Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
instance (GH.HasField "storageBytesFree" GH.Slot NodeStats Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 3 64 0)
instance (GH.HasField "loadAvg1" GH.Slot NodeStats Std_.Double) where
    fieldByLabel  = (GH.dataField 0 4 64 0)
instance (GH.HasField "loadAvg5" GH.Slot NodeStats Std_.Double) where
    fieldByLabel  = (GH.dataField 0 5 64 0)
instance (GH.HasField "loadAvg15" GH.Slot NodeStats Std_.Double) where
    fieldByLabel  = (GH.dataField 0 6 64 0)
instance (GH.HasField "kernelRelease" GH.Slot NodeStats Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "agentVersion" GH.Slot NodeStats Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
data VmStatusEntry 
type instance (R.ReprFor VmStatusEntry) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmStatusEntry) where
    typeId  = 18089874872843444784
instance (C.TypedStruct VmStatusEntry) where
    numStructWords  = 4
    numStructPtrs  = 2
instance (C.Allocate VmStatusEntry) where
    type AllocHint VmStatusEntry = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmStatusEntry (C.Parsed VmStatusEntry))
instance (C.AllocateList VmStatusEntry) where
    type ListAllocHint VmStatusEntry = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmStatusEntry (C.Parsed VmStatusEntry))
data instance C.Parsed VmStatusEntry
    = VmStatusEntry 
        {vmId :: (RP.Parsed Std_.Int64)
        ,state :: (RP.Parsed VmAgentState)
        ,qemuPid :: (RP.Parsed Std_.Int32)
        ,lastExitCode :: (RP.Parsed Std_.Int32)
        ,guestAgentOk :: (RP.Parsed Std_.Bool)
        ,lastPingMillis :: (RP.Parsed Std_.Int64)
        ,netIfs :: (RP.Parsed (R.List GuestNetIf))
        ,stats :: (RP.Parsed Capnp.Gen.ById.Xa7366eabdb0b1db4.VmStats)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmStatusEntry))
deriving instance (Std_.Eq (C.Parsed VmStatusEntry))
instance (C.Parse VmStatusEntry (C.Parsed VmStatusEntry)) where
    parse raw_ = (VmStatusEntry <$> (GH.parseField #vmId raw_)
                                <*> (GH.parseField #state raw_)
                                <*> (GH.parseField #qemuPid raw_)
                                <*> (GH.parseField #lastExitCode raw_)
                                <*> (GH.parseField #guestAgentOk raw_)
                                <*> (GH.parseField #lastPingMillis raw_)
                                <*> (GH.parseField #netIfs raw_)
                                <*> (GH.parseField #stats raw_))
instance (C.Marshal VmStatusEntry (C.Parsed VmStatusEntry)) where
    marshalInto raw_ VmStatusEntry{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (GH.encodeField #state state raw_)
        (GH.encodeField #qemuPid qemuPid raw_)
        (GH.encodeField #lastExitCode lastExitCode raw_)
        (GH.encodeField #guestAgentOk guestAgentOk raw_)
        (GH.encodeField #lastPingMillis lastPingMillis raw_)
        (GH.encodeField #netIfs netIfs raw_)
        (GH.encodeField #stats stats raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot VmStatusEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "state" GH.Slot VmStatusEntry VmAgentState) where
    fieldByLabel  = (GH.dataField 0 1 16 0)
instance (GH.HasField "qemuPid" GH.Slot VmStatusEntry Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 1 32 0)
instance (GH.HasField "lastExitCode" GH.Slot VmStatusEntry Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 2 32 0)
instance (GH.HasField "guestAgentOk" GH.Slot VmStatusEntry Std_.Bool) where
    fieldByLabel  = (GH.dataField 16 1 1 0)
instance (GH.HasField "lastPingMillis" GH.Slot VmStatusEntry Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 3 64 0)
instance (GH.HasField "netIfs" GH.Slot VmStatusEntry (R.List GuestNetIf)) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "stats" GH.Slot VmStatusEntry Capnp.Gen.ById.Xa7366eabdb0b1db4.VmStats) where
    fieldByLabel  = (GH.ptrField 1)
data GuestNetIf 
type instance (R.ReprFor GuestNetIf) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId GuestNetIf) where
    typeId  = 15719728507930151912
instance (C.TypedStruct GuestNetIf) where
    numStructWords  = 0
    numStructPtrs  = 3
instance (C.Allocate GuestNetIf) where
    type AllocHint GuestNetIf = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc GuestNetIf (C.Parsed GuestNetIf))
instance (C.AllocateList GuestNetIf) where
    type ListAllocHint GuestNetIf = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc GuestNetIf (C.Parsed GuestNetIf))
data instance C.Parsed GuestNetIf
    = GuestNetIf 
        {name :: (RP.Parsed Basics.Text)
        ,hwAddress :: (RP.Parsed Basics.Text)
        ,ipAddresses :: (RP.Parsed (R.List GuestIpAddress))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed GuestNetIf))
deriving instance (Std_.Eq (C.Parsed GuestNetIf))
instance (C.Parse GuestNetIf (C.Parsed GuestNetIf)) where
    parse raw_ = (GuestNetIf <$> (GH.parseField #name raw_)
                             <*> (GH.parseField #hwAddress raw_)
                             <*> (GH.parseField #ipAddresses raw_))
instance (C.Marshal GuestNetIf (C.Parsed GuestNetIf)) where
    marshalInto raw_ GuestNetIf{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #hwAddress hwAddress raw_)
        (GH.encodeField #ipAddresses ipAddresses raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot GuestNetIf Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "hwAddress" GH.Slot GuestNetIf Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "ipAddresses" GH.Slot GuestNetIf (R.List GuestIpAddress)) where
    fieldByLabel  = (GH.ptrField 2)
data GuestIpAddress 
type instance (R.ReprFor GuestIpAddress) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId GuestIpAddress) where
    typeId  = 12848223563161887323
instance (C.TypedStruct GuestIpAddress) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate GuestIpAddress) where
    type AllocHint GuestIpAddress = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc GuestIpAddress (C.Parsed GuestIpAddress))
instance (C.AllocateList GuestIpAddress) where
    type ListAllocHint GuestIpAddress = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc GuestIpAddress (C.Parsed GuestIpAddress))
data instance C.Parsed GuestIpAddress
    = GuestIpAddress 
        {ipAddress :: (RP.Parsed Basics.Text)
        ,prefix :: (RP.Parsed Std_.Int32)
        ,ipAddrType :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed GuestIpAddress))
deriving instance (Std_.Eq (C.Parsed GuestIpAddress))
instance (C.Parse GuestIpAddress (C.Parsed GuestIpAddress)) where
    parse raw_ = (GuestIpAddress <$> (GH.parseField #ipAddress raw_)
                                 <*> (GH.parseField #prefix raw_)
                                 <*> (GH.parseField #ipAddrType raw_))
instance (C.Marshal GuestIpAddress (C.Parsed GuestIpAddress)) where
    marshalInto raw_ GuestIpAddress{..} = (do
        (GH.encodeField #ipAddress ipAddress raw_)
        (GH.encodeField #prefix prefix raw_)
        (GH.encodeField #ipAddrType ipAddrType raw_)
        (Std_.pure ())
        )
instance (GH.HasField "ipAddress" GH.Slot GuestIpAddress Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "prefix" GH.Slot GuestIpAddress Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "ipAddrType" GH.Slot GuestIpAddress Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)