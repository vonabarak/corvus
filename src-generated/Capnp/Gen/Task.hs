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
module Capnp.Gen.Task where
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP
import qualified Capnp.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers as GH
import qualified Capnp.Classes as C
import qualified GHC.Generics as Generics
import qualified Capnp.GenHelpers.Rpc as GH
import qualified Capnp.Gen.ById.X9bd452a518ed3917
import qualified Capnp.Gen.ById.Xbf9b09f64c0dd40d
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data TaskInfo 
type instance (R.ReprFor TaskInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TaskInfo) where
    typeId  = 12448351927280309598
instance (C.TypedStruct TaskInfo) where
    numStructWords  = 6
    numStructPtrs  = 4
instance (C.Allocate TaskInfo) where
    type AllocHint TaskInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TaskInfo (C.Parsed TaskInfo))
instance (C.AllocateList TaskInfo) where
    type ListAllocHint TaskInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TaskInfo (C.Parsed TaskInfo))
data instance C.Parsed TaskInfo
    = TaskInfo 
        {id :: (RP.Parsed Std_.Int64)
        ,parentId :: (RP.Parsed Std_.Int64)
        ,startedAt :: (RP.Parsed Std_.Int64)
        ,finishedAt :: (RP.Parsed Std_.Int64)
        ,subsystem :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.TaskSubsystem)
        ,entityId :: (RP.Parsed Std_.Int64)
        ,entityName :: (RP.Parsed Basics.Text)
        ,command :: (RP.Parsed Basics.Text)
        ,result :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.TaskResult)
        ,message :: (RP.Parsed Basics.Text)
        ,clientName :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TaskInfo))
deriving instance (Std_.Eq (C.Parsed TaskInfo))
instance (C.Parse TaskInfo (C.Parsed TaskInfo)) where
    parse raw_ = (TaskInfo <$> (GH.parseField #id raw_)
                           <*> (GH.parseField #parentId raw_)
                           <*> (GH.parseField #startedAt raw_)
                           <*> (GH.parseField #finishedAt raw_)
                           <*> (GH.parseField #subsystem raw_)
                           <*> (GH.parseField #entityId raw_)
                           <*> (GH.parseField #entityName raw_)
                           <*> (GH.parseField #command raw_)
                           <*> (GH.parseField #result raw_)
                           <*> (GH.parseField #message raw_)
                           <*> (GH.parseField #clientName raw_))
instance (C.Marshal TaskInfo (C.Parsed TaskInfo)) where
    marshalInto raw_ TaskInfo{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #parentId parentId raw_)
        (GH.encodeField #startedAt startedAt raw_)
        (GH.encodeField #finishedAt finishedAt raw_)
        (GH.encodeField #subsystem subsystem raw_)
        (GH.encodeField #entityId entityId raw_)
        (GH.encodeField #entityName entityName raw_)
        (GH.encodeField #command command raw_)
        (GH.encodeField #result result raw_)
        (GH.encodeField #message message raw_)
        (GH.encodeField #clientName clientName raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot TaskInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "parentId" GH.Slot TaskInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "startedAt" GH.Slot TaskInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
instance (GH.HasField "finishedAt" GH.Slot TaskInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 3 64 0)
instance (GH.HasField "subsystem" GH.Slot TaskInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.TaskSubsystem) where
    fieldByLabel  = (GH.dataField 0 4 16 0)
instance (GH.HasField "entityId" GH.Slot TaskInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 5 64 0)
instance (GH.HasField "entityName" GH.Slot TaskInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "command" GH.Slot TaskInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "result" GH.Slot TaskInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.TaskResult) where
    fieldByLabel  = (GH.dataField 16 4 16 0)
instance (GH.HasField "message" GH.Slot TaskInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "clientName" GH.Slot TaskInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 3)
data TaskListParams 
type instance (R.ReprFor TaskListParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TaskListParams) where
    typeId  = 12107488232528086742
instance (C.TypedStruct TaskListParams) where
    numStructWords  = 3
    numStructPtrs  = 0
instance (C.Allocate TaskListParams) where
    type AllocHint TaskListParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TaskListParams (C.Parsed TaskListParams))
instance (C.AllocateList TaskListParams) where
    type ListAllocHint TaskListParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TaskListParams (C.Parsed TaskListParams))
data instance C.Parsed TaskListParams
    = TaskListParams 
        {limit :: (RP.Parsed Std_.Int32)
        ,subsystem :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.TaskSubsystem)
        ,hasSubsystem :: (RP.Parsed Std_.Bool)
        ,entityId :: (RP.Parsed Std_.Int64)
        ,result :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.TaskResult)
        ,hasResult :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TaskListParams))
deriving instance (Std_.Eq (C.Parsed TaskListParams))
instance (C.Parse TaskListParams (C.Parsed TaskListParams)) where
    parse raw_ = (TaskListParams <$> (GH.parseField #limit raw_)
                                 <*> (GH.parseField #subsystem raw_)
                                 <*> (GH.parseField #hasSubsystem raw_)
                                 <*> (GH.parseField #entityId raw_)
                                 <*> (GH.parseField #result raw_)
                                 <*> (GH.parseField #hasResult raw_))
instance (C.Marshal TaskListParams (C.Parsed TaskListParams)) where
    marshalInto raw_ TaskListParams{..} = (do
        (GH.encodeField #limit limit raw_)
        (GH.encodeField #subsystem subsystem raw_)
        (GH.encodeField #hasSubsystem hasSubsystem raw_)
        (GH.encodeField #entityId entityId raw_)
        (GH.encodeField #result result raw_)
        (GH.encodeField #hasResult hasResult raw_)
        (Std_.pure ())
        )
instance (GH.HasField "limit" GH.Slot TaskListParams Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "subsystem" GH.Slot TaskListParams Capnp.Gen.ById.Xbf9b09f64c0dd40d.TaskSubsystem) where
    fieldByLabel  = (GH.dataField 32 0 16 0)
instance (GH.HasField "hasSubsystem" GH.Slot TaskListParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 48 0 1 0)
instance (GH.HasField "entityId" GH.Slot TaskListParams Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "result" GH.Slot TaskListParams Capnp.Gen.ById.Xbf9b09f64c0dd40d.TaskResult) where
    fieldByLabel  = (GH.dataField 0 2 16 0)
instance (GH.HasField "hasResult" GH.Slot TaskListParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 49 0 1 0)
data TaskManager 
type instance (R.ReprFor TaskManager) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId TaskManager) where
    typeId  = 15122551829697145524
instance (C.Parse TaskManager (GH.Client TaskManager)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export TaskManager) where
    type Server TaskManager = TaskManager'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(TaskManager)) [(GH.toUntypedMethodHandler ((taskManager'list) s_))
                                                                             ,(GH.toUntypedMethodHandler ((taskManager'get) s_))
                                                                             ,(GH.toUntypedMethodHandler ((taskManager'listChildren) s_))
                                                                             ,(GH.toUntypedMethodHandler ((taskManager'subscribe) s_))] [])
class (TaskManager'server_ s_) where
    {-# MINIMAL taskManager'list,taskManager'get,taskManager'listChildren,taskManager'subscribe #-}
    taskManager'list :: s_ -> (GH.MethodHandler TaskManager'list'params TaskManager'list'results)
    taskManager'list _ = GH.methodUnimplemented
    taskManager'get :: s_ -> (GH.MethodHandler TaskManager'get'params TaskManager'get'results)
    taskManager'get _ = GH.methodUnimplemented
    taskManager'listChildren :: s_ -> (GH.MethodHandler TaskManager'listChildren'params TaskManager'listChildren'results)
    taskManager'listChildren _ = GH.methodUnimplemented
    taskManager'subscribe :: s_ -> (GH.MethodHandler TaskManager'subscribe'params TaskManager'subscribe'results)
    taskManager'subscribe _ = GH.methodUnimplemented
instance (GH.HasMethod "list" TaskManager TaskManager'list'params TaskManager'list'results) where
    methodByLabel  = (GH.Method 15122551829697145524 0)
instance (GH.HasMethod "get" TaskManager TaskManager'get'params TaskManager'get'results) where
    methodByLabel  = (GH.Method 15122551829697145524 1)
instance (GH.HasMethod "listChildren" TaskManager TaskManager'listChildren'params TaskManager'listChildren'results) where
    methodByLabel  = (GH.Method 15122551829697145524 2)
instance (GH.HasMethod "subscribe" TaskManager TaskManager'subscribe'params TaskManager'subscribe'results) where
    methodByLabel  = (GH.Method 15122551829697145524 3)
data TaskManager'list'params 
type instance (R.ReprFor TaskManager'list'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TaskManager'list'params) where
    typeId  = 9263611476082562761
instance (C.TypedStruct TaskManager'list'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate TaskManager'list'params) where
    type AllocHint TaskManager'list'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TaskManager'list'params (C.Parsed TaskManager'list'params))
instance (C.AllocateList TaskManager'list'params) where
    type ListAllocHint TaskManager'list'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TaskManager'list'params (C.Parsed TaskManager'list'params))
data instance C.Parsed TaskManager'list'params
    = TaskManager'list'params 
        {params :: (RP.Parsed TaskListParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TaskManager'list'params))
deriving instance (Std_.Eq (C.Parsed TaskManager'list'params))
instance (C.Parse TaskManager'list'params (C.Parsed TaskManager'list'params)) where
    parse raw_ = (TaskManager'list'params <$> (GH.parseField #params raw_))
instance (C.Marshal TaskManager'list'params (C.Parsed TaskManager'list'params)) where
    marshalInto raw_ TaskManager'list'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot TaskManager'list'params TaskListParams) where
    fieldByLabel  = (GH.ptrField 0)
data TaskManager'list'results 
type instance (R.ReprFor TaskManager'list'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TaskManager'list'results) where
    typeId  = 12355368995453518004
instance (C.TypedStruct TaskManager'list'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate TaskManager'list'results) where
    type AllocHint TaskManager'list'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TaskManager'list'results (C.Parsed TaskManager'list'results))
instance (C.AllocateList TaskManager'list'results) where
    type ListAllocHint TaskManager'list'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TaskManager'list'results (C.Parsed TaskManager'list'results))
data instance C.Parsed TaskManager'list'results
    = TaskManager'list'results 
        {tasks :: (RP.Parsed (R.List TaskInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TaskManager'list'results))
deriving instance (Std_.Eq (C.Parsed TaskManager'list'results))
instance (C.Parse TaskManager'list'results (C.Parsed TaskManager'list'results)) where
    parse raw_ = (TaskManager'list'results <$> (GH.parseField #tasks raw_))
instance (C.Marshal TaskManager'list'results (C.Parsed TaskManager'list'results)) where
    marshalInto raw_ TaskManager'list'results{..} = (do
        (GH.encodeField #tasks tasks raw_)
        (Std_.pure ())
        )
instance (GH.HasField "tasks" GH.Slot TaskManager'list'results (R.List TaskInfo)) where
    fieldByLabel  = (GH.ptrField 0)
data TaskManager'get'params 
type instance (R.ReprFor TaskManager'get'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TaskManager'get'params) where
    typeId  = 17042536501845889037
instance (C.TypedStruct TaskManager'get'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate TaskManager'get'params) where
    type AllocHint TaskManager'get'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TaskManager'get'params (C.Parsed TaskManager'get'params))
instance (C.AllocateList TaskManager'get'params) where
    type ListAllocHint TaskManager'get'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TaskManager'get'params (C.Parsed TaskManager'get'params))
data instance C.Parsed TaskManager'get'params
    = TaskManager'get'params 
        {taskId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TaskManager'get'params))
deriving instance (Std_.Eq (C.Parsed TaskManager'get'params))
instance (C.Parse TaskManager'get'params (C.Parsed TaskManager'get'params)) where
    parse raw_ = (TaskManager'get'params <$> (GH.parseField #taskId raw_))
instance (C.Marshal TaskManager'get'params (C.Parsed TaskManager'get'params)) where
    marshalInto raw_ TaskManager'get'params{..} = (do
        (GH.encodeField #taskId taskId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "taskId" GH.Slot TaskManager'get'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data TaskManager'get'results 
type instance (R.ReprFor TaskManager'get'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TaskManager'get'results) where
    typeId  = 12587289212802709768
instance (C.TypedStruct TaskManager'get'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate TaskManager'get'results) where
    type AllocHint TaskManager'get'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TaskManager'get'results (C.Parsed TaskManager'get'results))
instance (C.AllocateList TaskManager'get'results) where
    type ListAllocHint TaskManager'get'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TaskManager'get'results (C.Parsed TaskManager'get'results))
data instance C.Parsed TaskManager'get'results
    = TaskManager'get'results 
        {task :: (RP.Parsed Task)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TaskManager'get'results))
deriving instance (Std_.Eq (C.Parsed TaskManager'get'results))
instance (C.Parse TaskManager'get'results (C.Parsed TaskManager'get'results)) where
    parse raw_ = (TaskManager'get'results <$> (GH.parseField #task raw_))
instance (C.Marshal TaskManager'get'results (C.Parsed TaskManager'get'results)) where
    marshalInto raw_ TaskManager'get'results{..} = (do
        (GH.encodeField #task task raw_)
        (Std_.pure ())
        )
instance (GH.HasField "task" GH.Slot TaskManager'get'results Task) where
    fieldByLabel  = (GH.ptrField 0)
data TaskManager'listChildren'params 
type instance (R.ReprFor TaskManager'listChildren'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TaskManager'listChildren'params) where
    typeId  = 10260744308741782733
instance (C.TypedStruct TaskManager'listChildren'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate TaskManager'listChildren'params) where
    type AllocHint TaskManager'listChildren'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TaskManager'listChildren'params (C.Parsed TaskManager'listChildren'params))
instance (C.AllocateList TaskManager'listChildren'params) where
    type ListAllocHint TaskManager'listChildren'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TaskManager'listChildren'params (C.Parsed TaskManager'listChildren'params))
data instance C.Parsed TaskManager'listChildren'params
    = TaskManager'listChildren'params 
        {parentId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TaskManager'listChildren'params))
deriving instance (Std_.Eq (C.Parsed TaskManager'listChildren'params))
instance (C.Parse TaskManager'listChildren'params (C.Parsed TaskManager'listChildren'params)) where
    parse raw_ = (TaskManager'listChildren'params <$> (GH.parseField #parentId raw_))
instance (C.Marshal TaskManager'listChildren'params (C.Parsed TaskManager'listChildren'params)) where
    marshalInto raw_ TaskManager'listChildren'params{..} = (do
        (GH.encodeField #parentId parentId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "parentId" GH.Slot TaskManager'listChildren'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data TaskManager'listChildren'results 
type instance (R.ReprFor TaskManager'listChildren'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TaskManager'listChildren'results) where
    typeId  = 18220865692108873772
instance (C.TypedStruct TaskManager'listChildren'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate TaskManager'listChildren'results) where
    type AllocHint TaskManager'listChildren'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TaskManager'listChildren'results (C.Parsed TaskManager'listChildren'results))
instance (C.AllocateList TaskManager'listChildren'results) where
    type ListAllocHint TaskManager'listChildren'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TaskManager'listChildren'results (C.Parsed TaskManager'listChildren'results))
data instance C.Parsed TaskManager'listChildren'results
    = TaskManager'listChildren'results 
        {tasks :: (RP.Parsed (R.List TaskInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TaskManager'listChildren'results))
deriving instance (Std_.Eq (C.Parsed TaskManager'listChildren'results))
instance (C.Parse TaskManager'listChildren'results (C.Parsed TaskManager'listChildren'results)) where
    parse raw_ = (TaskManager'listChildren'results <$> (GH.parseField #tasks raw_))
instance (C.Marshal TaskManager'listChildren'results (C.Parsed TaskManager'listChildren'results)) where
    marshalInto raw_ TaskManager'listChildren'results{..} = (do
        (GH.encodeField #tasks tasks raw_)
        (Std_.pure ())
        )
instance (GH.HasField "tasks" GH.Slot TaskManager'listChildren'results (R.List TaskInfo)) where
    fieldByLabel  = (GH.ptrField 0)
data TaskManager'subscribe'params 
type instance (R.ReprFor TaskManager'subscribe'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TaskManager'subscribe'params) where
    typeId  = 15462404650246442084
instance (C.TypedStruct TaskManager'subscribe'params) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate TaskManager'subscribe'params) where
    type AllocHint TaskManager'subscribe'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TaskManager'subscribe'params (C.Parsed TaskManager'subscribe'params))
instance (C.AllocateList TaskManager'subscribe'params) where
    type ListAllocHint TaskManager'subscribe'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TaskManager'subscribe'params (C.Parsed TaskManager'subscribe'params))
data instance C.Parsed TaskManager'subscribe'params
    = TaskManager'subscribe'params 
        {taskId :: (RP.Parsed Std_.Int64)
        ,sink :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.TaskProgressSink)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TaskManager'subscribe'params))
deriving instance (Std_.Eq (C.Parsed TaskManager'subscribe'params))
instance (C.Parse TaskManager'subscribe'params (C.Parsed TaskManager'subscribe'params)) where
    parse raw_ = (TaskManager'subscribe'params <$> (GH.parseField #taskId raw_)
                                               <*> (GH.parseField #sink raw_))
instance (C.Marshal TaskManager'subscribe'params (C.Parsed TaskManager'subscribe'params)) where
    marshalInto raw_ TaskManager'subscribe'params{..} = (do
        (GH.encodeField #taskId taskId raw_)
        (GH.encodeField #sink sink raw_)
        (Std_.pure ())
        )
instance (GH.HasField "taskId" GH.Slot TaskManager'subscribe'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "sink" GH.Slot TaskManager'subscribe'params Capnp.Gen.ById.X9bd452a518ed3917.TaskProgressSink) where
    fieldByLabel  = (GH.ptrField 0)
data TaskManager'subscribe'results 
type instance (R.ReprFor TaskManager'subscribe'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TaskManager'subscribe'results) where
    typeId  = 17937177260830842392
instance (C.TypedStruct TaskManager'subscribe'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate TaskManager'subscribe'results) where
    type AllocHint TaskManager'subscribe'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TaskManager'subscribe'results (C.Parsed TaskManager'subscribe'results))
instance (C.AllocateList TaskManager'subscribe'results) where
    type ListAllocHint TaskManager'subscribe'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TaskManager'subscribe'results (C.Parsed TaskManager'subscribe'results))
data instance C.Parsed TaskManager'subscribe'results
    = TaskManager'subscribe'results 
        {handle :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.Handle)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TaskManager'subscribe'results))
deriving instance (Std_.Eq (C.Parsed TaskManager'subscribe'results))
instance (C.Parse TaskManager'subscribe'results (C.Parsed TaskManager'subscribe'results)) where
    parse raw_ = (TaskManager'subscribe'results <$> (GH.parseField #handle raw_))
instance (C.Marshal TaskManager'subscribe'results (C.Parsed TaskManager'subscribe'results)) where
    marshalInto raw_ TaskManager'subscribe'results{..} = (do
        (GH.encodeField #handle handle raw_)
        (Std_.pure ())
        )
instance (GH.HasField "handle" GH.Slot TaskManager'subscribe'results Capnp.Gen.ById.X9bd452a518ed3917.Handle) where
    fieldByLabel  = (GH.ptrField 0)
data Task 
type instance (R.ReprFor Task) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Task) where
    typeId  = 9829389839671654244
instance (C.Parse Task (GH.Client Task)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Task) where
    type Server Task = Task'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Task)) [(GH.toUntypedMethodHandler ((task'show) s_))] [])
class (Task'server_ s_) where
    {-# MINIMAL task'show #-}
    task'show :: s_ -> (GH.MethodHandler Task'show'params Task'show'results)
    task'show _ = GH.methodUnimplemented
instance (GH.HasMethod "show" Task Task'show'params Task'show'results) where
    methodByLabel  = (GH.Method 9829389839671654244 0)
data Task'show'params 
type instance (R.ReprFor Task'show'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Task'show'params) where
    typeId  = 14142766387597901081
instance (C.TypedStruct Task'show'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Task'show'params) where
    type AllocHint Task'show'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Task'show'params (C.Parsed Task'show'params))
instance (C.AllocateList Task'show'params) where
    type ListAllocHint Task'show'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Task'show'params (C.Parsed Task'show'params))
data instance C.Parsed Task'show'params
    = Task'show'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Task'show'params))
deriving instance (Std_.Eq (C.Parsed Task'show'params))
instance (C.Parse Task'show'params (C.Parsed Task'show'params)) where
    parse raw_ = (Std_.pure Task'show'params)
instance (C.Marshal Task'show'params (C.Parsed Task'show'params)) where
    marshalInto _raw (Task'show'params) = (Std_.pure ())
data Task'show'results 
type instance (R.ReprFor Task'show'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Task'show'results) where
    typeId  = 16653870563493179242
instance (C.TypedStruct Task'show'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Task'show'results) where
    type AllocHint Task'show'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Task'show'results (C.Parsed Task'show'results))
instance (C.AllocateList Task'show'results) where
    type ListAllocHint Task'show'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Task'show'results (C.Parsed Task'show'results))
data instance C.Parsed Task'show'results
    = Task'show'results 
        {info :: (RP.Parsed TaskInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Task'show'results))
deriving instance (Std_.Eq (C.Parsed Task'show'results))
instance (C.Parse Task'show'results (C.Parsed Task'show'results)) where
    parse raw_ = (Task'show'results <$> (GH.parseField #info raw_))
instance (C.Marshal Task'show'results (C.Parsed Task'show'results)) where
    marshalInto raw_ Task'show'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot Task'show'results TaskInfo) where
    fieldByLabel  = (GH.ptrField 0)