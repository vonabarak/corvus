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
module Capnp.Gen.Streams where
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP
import qualified Capnp.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers as GH
import qualified Capnp.Classes as C
import qualified GHC.Generics as Generics
import qualified Capnp.GenHelpers.Rpc as GH
import qualified Capnp.Gen.ById.Xbf9b09f64c0dd40d
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data ByteSink 
type instance (R.ReprFor ByteSink) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId ByteSink) where
    typeId  = 16961475304222214800
instance (C.Parse ByteSink (GH.Client ByteSink)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export ByteSink) where
    type Server ByteSink = ByteSink'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(ByteSink)) [(GH.toUntypedMethodHandler ((byteSink'write) s_))
                                                                          ,(GH.toUntypedMethodHandler ((byteSink'end) s_))] [])
class (ByteSink'server_ s_) where
    {-# MINIMAL byteSink'write,byteSink'end #-}
    byteSink'write :: s_ -> (GH.MethodHandler ByteSink'write'params ByteSink'write'results)
    byteSink'write _ = GH.methodUnimplemented
    byteSink'end :: s_ -> (GH.MethodHandler ByteSink'end'params ByteSink'end'results)
    byteSink'end _ = GH.methodUnimplemented
instance (GH.HasMethod "write" ByteSink ByteSink'write'params ByteSink'write'results) where
    methodByLabel  = (GH.Method 16961475304222214800 0)
instance (GH.HasMethod "end" ByteSink ByteSink'end'params ByteSink'end'results) where
    methodByLabel  = (GH.Method 16961475304222214800 1)
data ByteSink'write'params 
type instance (R.ReprFor ByteSink'write'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId ByteSink'write'params) where
    typeId  = 13088654581470093873
instance (C.TypedStruct ByteSink'write'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate ByteSink'write'params) where
    type AllocHint ByteSink'write'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc ByteSink'write'params (C.Parsed ByteSink'write'params))
instance (C.AllocateList ByteSink'write'params) where
    type ListAllocHint ByteSink'write'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc ByteSink'write'params (C.Parsed ByteSink'write'params))
data instance C.Parsed ByteSink'write'params
    = ByteSink'write'params 
        {chunk :: (RP.Parsed Basics.Data)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed ByteSink'write'params))
deriving instance (Std_.Eq (C.Parsed ByteSink'write'params))
instance (C.Parse ByteSink'write'params (C.Parsed ByteSink'write'params)) where
    parse raw_ = (ByteSink'write'params <$> (GH.parseField #chunk raw_))
instance (C.Marshal ByteSink'write'params (C.Parsed ByteSink'write'params)) where
    marshalInto raw_ ByteSink'write'params{..} = (do
        (GH.encodeField #chunk chunk raw_)
        (Std_.pure ())
        )
instance (GH.HasField "chunk" GH.Slot ByteSink'write'params Basics.Data) where
    fieldByLabel  = (GH.ptrField 0)
data ByteSink'write'results 
type instance (R.ReprFor ByteSink'write'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId ByteSink'write'results) where
    typeId  = 13658629455305381735
instance (C.TypedStruct ByteSink'write'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate ByteSink'write'results) where
    type AllocHint ByteSink'write'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc ByteSink'write'results (C.Parsed ByteSink'write'results))
instance (C.AllocateList ByteSink'write'results) where
    type ListAllocHint ByteSink'write'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc ByteSink'write'results (C.Parsed ByteSink'write'results))
data instance C.Parsed ByteSink'write'results
    = ByteSink'write'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed ByteSink'write'results))
deriving instance (Std_.Eq (C.Parsed ByteSink'write'results))
instance (C.Parse ByteSink'write'results (C.Parsed ByteSink'write'results)) where
    parse raw_ = (Std_.pure ByteSink'write'results)
instance (C.Marshal ByteSink'write'results (C.Parsed ByteSink'write'results)) where
    marshalInto _raw (ByteSink'write'results) = (Std_.pure ())
data ByteSink'end'params 
type instance (R.ReprFor ByteSink'end'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId ByteSink'end'params) where
    typeId  = 13161865942622842752
instance (C.TypedStruct ByteSink'end'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate ByteSink'end'params) where
    type AllocHint ByteSink'end'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc ByteSink'end'params (C.Parsed ByteSink'end'params))
instance (C.AllocateList ByteSink'end'params) where
    type ListAllocHint ByteSink'end'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc ByteSink'end'params (C.Parsed ByteSink'end'params))
data instance C.Parsed ByteSink'end'params
    = ByteSink'end'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed ByteSink'end'params))
deriving instance (Std_.Eq (C.Parsed ByteSink'end'params))
instance (C.Parse ByteSink'end'params (C.Parsed ByteSink'end'params)) where
    parse raw_ = (Std_.pure ByteSink'end'params)
instance (C.Marshal ByteSink'end'params (C.Parsed ByteSink'end'params)) where
    marshalInto _raw (ByteSink'end'params) = (Std_.pure ())
data ByteSink'end'results 
type instance (R.ReprFor ByteSink'end'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId ByteSink'end'results) where
    typeId  = 10827911077671108647
instance (C.TypedStruct ByteSink'end'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate ByteSink'end'results) where
    type AllocHint ByteSink'end'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc ByteSink'end'results (C.Parsed ByteSink'end'results))
instance (C.AllocateList ByteSink'end'results) where
    type ListAllocHint ByteSink'end'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc ByteSink'end'results (C.Parsed ByteSink'end'results))
data instance C.Parsed ByteSink'end'results
    = ByteSink'end'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed ByteSink'end'results))
deriving instance (Std_.Eq (C.Parsed ByteSink'end'results))
instance (C.Parse ByteSink'end'results (C.Parsed ByteSink'end'results)) where
    parse raw_ = (Std_.pure ByteSink'end'results)
instance (C.Marshal ByteSink'end'results (C.Parsed ByteSink'end'results)) where
    marshalInto _raw (ByteSink'end'results) = (Std_.pure ())
data BuildEvent 
type instance (R.ReprFor BuildEvent) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId BuildEvent) where
    typeId  = 12632443230413456067
instance (C.TypedStruct BuildEvent) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate BuildEvent) where
    type AllocHint BuildEvent = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc BuildEvent (C.Parsed BuildEvent))
instance (C.AllocateList BuildEvent) where
    type ListAllocHint BuildEvent = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc BuildEvent (C.Parsed BuildEvent))
data instance C.Parsed BuildEvent
    = BuildEvent 
        {union' :: (C.Parsed (GH.Which BuildEvent))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed BuildEvent))
deriving instance (Std_.Eq (C.Parsed BuildEvent))
instance (C.Parse BuildEvent (C.Parsed BuildEvent)) where
    parse raw_ = (BuildEvent <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal BuildEvent (C.Parsed BuildEvent)) where
    marshalInto raw_ BuildEvent{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion BuildEvent) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich BuildEvent mut_
        = RW_BuildEvent'logLine (R.Raw Basics.Text mut_)
        | RW_BuildEvent'stepStart (R.Raw BuildEvent'stepStart mut_)
        | RW_BuildEvent'stepOutput (R.Raw BuildEvent'stepOutput mut_)
        | RW_BuildEvent'stepEnd (R.Raw BuildEvent'stepEnd mut_)
        | RW_BuildEvent'buildEnd (R.Raw BuildEvent'buildEnd mut_)
        | RW_BuildEvent'pipelineEnd (R.Raw BuildEvent'pipelineEnd mut_)
        | RW_BuildEvent'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_BuildEvent'logLine <$> (GH.readVariant #logLine struct_))
        1 ->
            (RW_BuildEvent'stepStart <$> (GH.readVariant #stepStart struct_))
        2 ->
            (RW_BuildEvent'stepOutput <$> (GH.readVariant #stepOutput struct_))
        3 ->
            (RW_BuildEvent'stepEnd <$> (GH.readVariant #stepEnd struct_))
        4 ->
            (RW_BuildEvent'buildEnd <$> (GH.readVariant #buildEnd struct_))
        5 ->
            (RW_BuildEvent'pipelineEnd <$> (GH.readVariant #pipelineEnd struct_))
        _ ->
            (Std_.pure (RW_BuildEvent'unknown' tag_))
    data Which BuildEvent
instance (GH.HasVariant "logLine" GH.Slot BuildEvent Basics.Text) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 0)
instance (GH.HasVariant "stepStart" GH.Group BuildEvent BuildEvent'stepStart) where
    variantByLabel  = (GH.Variant GH.groupField 1)
instance (GH.HasVariant "stepOutput" GH.Group BuildEvent BuildEvent'stepOutput) where
    variantByLabel  = (GH.Variant GH.groupField 2)
instance (GH.HasVariant "stepEnd" GH.Group BuildEvent BuildEvent'stepEnd) where
    variantByLabel  = (GH.Variant GH.groupField 3)
instance (GH.HasVariant "buildEnd" GH.Group BuildEvent BuildEvent'buildEnd) where
    variantByLabel  = (GH.Variant GH.groupField 4)
instance (GH.HasVariant "pipelineEnd" GH.Group BuildEvent BuildEvent'pipelineEnd) where
    variantByLabel  = (GH.Variant GH.groupField 5)
data instance C.Parsed (GH.Which BuildEvent)
    = BuildEvent'logLine (RP.Parsed Basics.Text)
    | BuildEvent'stepStart (RP.Parsed BuildEvent'stepStart)
    | BuildEvent'stepOutput (RP.Parsed BuildEvent'stepOutput)
    | BuildEvent'stepEnd (RP.Parsed BuildEvent'stepEnd)
    | BuildEvent'buildEnd (RP.Parsed BuildEvent'buildEnd)
    | BuildEvent'pipelineEnd (RP.Parsed BuildEvent'pipelineEnd)
    | BuildEvent'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which BuildEvent)))
deriving instance (Std_.Eq (C.Parsed (GH.Which BuildEvent)))
instance (C.Parse (GH.Which BuildEvent) (C.Parsed (GH.Which BuildEvent))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_BuildEvent'logLine rawArg_) ->
                (BuildEvent'logLine <$> (C.parse rawArg_))
            (RW_BuildEvent'stepStart rawArg_) ->
                (BuildEvent'stepStart <$> (C.parse rawArg_))
            (RW_BuildEvent'stepOutput rawArg_) ->
                (BuildEvent'stepOutput <$> (C.parse rawArg_))
            (RW_BuildEvent'stepEnd rawArg_) ->
                (BuildEvent'stepEnd <$> (C.parse rawArg_))
            (RW_BuildEvent'buildEnd rawArg_) ->
                (BuildEvent'buildEnd <$> (C.parse rawArg_))
            (RW_BuildEvent'pipelineEnd rawArg_) ->
                (BuildEvent'pipelineEnd <$> (C.parse rawArg_))
            (RW_BuildEvent'unknown' tag_) ->
                (Std_.pure (BuildEvent'unknown' tag_))
        )
instance (C.Marshal (GH.Which BuildEvent) (C.Parsed (GH.Which BuildEvent))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (BuildEvent'logLine arg_) ->
            (GH.encodeVariant #logLine arg_ (GH.unionStruct raw_))
        (BuildEvent'stepStart arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #stepStart (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (BuildEvent'stepOutput arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #stepOutput (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (BuildEvent'stepEnd arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #stepEnd (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (BuildEvent'buildEnd arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #buildEnd (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (BuildEvent'pipelineEnd arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #pipelineEnd (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (BuildEvent'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data BuildEvent'stepStart 
type instance (R.ReprFor BuildEvent'stepStart) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId BuildEvent'stepStart) where
    typeId  = 11119697918761859952
instance (C.TypedStruct BuildEvent'stepStart) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate BuildEvent'stepStart) where
    type AllocHint BuildEvent'stepStart = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc BuildEvent'stepStart (C.Parsed BuildEvent'stepStart))
instance (C.AllocateList BuildEvent'stepStart) where
    type ListAllocHint BuildEvent'stepStart = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc BuildEvent'stepStart (C.Parsed BuildEvent'stepStart))
data instance C.Parsed BuildEvent'stepStart
    = BuildEvent'stepStart' 
        {stepIndex :: (RP.Parsed Std_.Int32)
        ,name :: (RP.Parsed Basics.Text)
        ,command :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed BuildEvent'stepStart))
deriving instance (Std_.Eq (C.Parsed BuildEvent'stepStart))
instance (C.Parse BuildEvent'stepStart (C.Parsed BuildEvent'stepStart)) where
    parse raw_ = (BuildEvent'stepStart' <$> (GH.parseField #stepIndex raw_)
                                        <*> (GH.parseField #name raw_)
                                        <*> (GH.parseField #command raw_))
instance (C.Marshal BuildEvent'stepStart (C.Parsed BuildEvent'stepStart)) where
    marshalInto raw_ BuildEvent'stepStart'{..} = (do
        (GH.encodeField #stepIndex stepIndex raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #command command raw_)
        (Std_.pure ())
        )
instance (GH.HasField "stepIndex" GH.Slot BuildEvent'stepStart Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
instance (GH.HasField "name" GH.Slot BuildEvent'stepStart Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "command" GH.Slot BuildEvent'stepStart Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
data BuildEvent'stepOutput 
type instance (R.ReprFor BuildEvent'stepOutput) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId BuildEvent'stepOutput) where
    typeId  = 11959638801161250796
instance (C.TypedStruct BuildEvent'stepOutput) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate BuildEvent'stepOutput) where
    type AllocHint BuildEvent'stepOutput = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc BuildEvent'stepOutput (C.Parsed BuildEvent'stepOutput))
instance (C.AllocateList BuildEvent'stepOutput) where
    type ListAllocHint BuildEvent'stepOutput = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc BuildEvent'stepOutput (C.Parsed BuildEvent'stepOutput))
data instance C.Parsed BuildEvent'stepOutput
    = BuildEvent'stepOutput' 
        {stepIndex :: (RP.Parsed Std_.Int32)
        ,line :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed BuildEvent'stepOutput))
deriving instance (Std_.Eq (C.Parsed BuildEvent'stepOutput))
instance (C.Parse BuildEvent'stepOutput (C.Parsed BuildEvent'stepOutput)) where
    parse raw_ = (BuildEvent'stepOutput' <$> (GH.parseField #stepIndex raw_)
                                         <*> (GH.parseField #line raw_))
instance (C.Marshal BuildEvent'stepOutput (C.Parsed BuildEvent'stepOutput)) where
    marshalInto raw_ BuildEvent'stepOutput'{..} = (do
        (GH.encodeField #stepIndex stepIndex raw_)
        (GH.encodeField #line line raw_)
        (Std_.pure ())
        )
instance (GH.HasField "stepIndex" GH.Slot BuildEvent'stepOutput Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
instance (GH.HasField "line" GH.Slot BuildEvent'stepOutput Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data BuildEvent'stepEnd 
type instance (R.ReprFor BuildEvent'stepEnd) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId BuildEvent'stepEnd) where
    typeId  = 10325751565404853604
instance (C.TypedStruct BuildEvent'stepEnd) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate BuildEvent'stepEnd) where
    type AllocHint BuildEvent'stepEnd = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc BuildEvent'stepEnd (C.Parsed BuildEvent'stepEnd))
instance (C.AllocateList BuildEvent'stepEnd) where
    type ListAllocHint BuildEvent'stepEnd = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc BuildEvent'stepEnd (C.Parsed BuildEvent'stepEnd))
data instance C.Parsed BuildEvent'stepEnd
    = BuildEvent'stepEnd' 
        {stepIndex :: (RP.Parsed Std_.Int32)
        ,result :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.TaskResult)
        ,message :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed BuildEvent'stepEnd))
deriving instance (Std_.Eq (C.Parsed BuildEvent'stepEnd))
instance (C.Parse BuildEvent'stepEnd (C.Parsed BuildEvent'stepEnd)) where
    parse raw_ = (BuildEvent'stepEnd' <$> (GH.parseField #stepIndex raw_)
                                      <*> (GH.parseField #result raw_)
                                      <*> (GH.parseField #message raw_))
instance (C.Marshal BuildEvent'stepEnd (C.Parsed BuildEvent'stepEnd)) where
    marshalInto raw_ BuildEvent'stepEnd'{..} = (do
        (GH.encodeField #stepIndex stepIndex raw_)
        (GH.encodeField #result result raw_)
        (GH.encodeField #message message raw_)
        (Std_.pure ())
        )
instance (GH.HasField "stepIndex" GH.Slot BuildEvent'stepEnd Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
instance (GH.HasField "result" GH.Slot BuildEvent'stepEnd Capnp.Gen.ById.Xbf9b09f64c0dd40d.TaskResult) where
    fieldByLabel  = (GH.dataField 16 0 16 0)
instance (GH.HasField "message" GH.Slot BuildEvent'stepEnd Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data BuildEvent'buildEnd 
type instance (R.ReprFor BuildEvent'buildEnd) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId BuildEvent'buildEnd) where
    typeId  = 17801622825246187390
instance (C.TypedStruct BuildEvent'buildEnd) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate BuildEvent'buildEnd) where
    type AllocHint BuildEvent'buildEnd = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc BuildEvent'buildEnd (C.Parsed BuildEvent'buildEnd))
instance (C.AllocateList BuildEvent'buildEnd) where
    type ListAllocHint BuildEvent'buildEnd = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc BuildEvent'buildEnd (C.Parsed BuildEvent'buildEnd))
data instance C.Parsed BuildEvent'buildEnd
    = BuildEvent'buildEnd' 
        {success :: (RP.Parsed Std_.Bool)
        ,errorMessage :: (RP.Parsed Basics.Text)
        ,artifactDiskId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed BuildEvent'buildEnd))
deriving instance (Std_.Eq (C.Parsed BuildEvent'buildEnd))
instance (C.Parse BuildEvent'buildEnd (C.Parsed BuildEvent'buildEnd)) where
    parse raw_ = (BuildEvent'buildEnd' <$> (GH.parseField #success raw_)
                                       <*> (GH.parseField #errorMessage raw_)
                                       <*> (GH.parseField #artifactDiskId raw_))
instance (C.Marshal BuildEvent'buildEnd (C.Parsed BuildEvent'buildEnd)) where
    marshalInto raw_ BuildEvent'buildEnd'{..} = (do
        (GH.encodeField #success success raw_)
        (GH.encodeField #errorMessage errorMessage raw_)
        (GH.encodeField #artifactDiskId artifactDiskId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "success" GH.Slot BuildEvent'buildEnd Std_.Bool) where
    fieldByLabel  = (GH.dataField 16 0 1 0)
instance (GH.HasField "errorMessage" GH.Slot BuildEvent'buildEnd Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "artifactDiskId" GH.Slot BuildEvent'buildEnd Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
data BuildEvent'pipelineEnd 
type instance (R.ReprFor BuildEvent'pipelineEnd) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId BuildEvent'pipelineEnd) where
    typeId  = 15731921494424439453
instance (C.TypedStruct BuildEvent'pipelineEnd) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate BuildEvent'pipelineEnd) where
    type AllocHint BuildEvent'pipelineEnd = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc BuildEvent'pipelineEnd (C.Parsed BuildEvent'pipelineEnd))
instance (C.AllocateList BuildEvent'pipelineEnd) where
    type ListAllocHint BuildEvent'pipelineEnd = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc BuildEvent'pipelineEnd (C.Parsed BuildEvent'pipelineEnd))
data instance C.Parsed BuildEvent'pipelineEnd
    = BuildEvent'pipelineEnd' 
        {builds :: (RP.Parsed (R.List BuildOneResult))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed BuildEvent'pipelineEnd))
deriving instance (Std_.Eq (C.Parsed BuildEvent'pipelineEnd))
instance (C.Parse BuildEvent'pipelineEnd (C.Parsed BuildEvent'pipelineEnd)) where
    parse raw_ = (BuildEvent'pipelineEnd' <$> (GH.parseField #builds raw_))
instance (C.Marshal BuildEvent'pipelineEnd (C.Parsed BuildEvent'pipelineEnd)) where
    marshalInto raw_ BuildEvent'pipelineEnd'{..} = (do
        (GH.encodeField #builds builds raw_)
        (Std_.pure ())
        )
instance (GH.HasField "builds" GH.Slot BuildEvent'pipelineEnd (R.List BuildOneResult)) where
    fieldByLabel  = (GH.ptrField 0)
data BuildOneResult 
type instance (R.ReprFor BuildOneResult) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId BuildOneResult) where
    typeId  = 11447095582140246299
instance (C.TypedStruct BuildOneResult) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate BuildOneResult) where
    type AllocHint BuildOneResult = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc BuildOneResult (C.Parsed BuildOneResult))
instance (C.AllocateList BuildOneResult) where
    type ListAllocHint BuildOneResult = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc BuildOneResult (C.Parsed BuildOneResult))
data instance C.Parsed BuildOneResult
    = BuildOneResult 
        {name :: (RP.Parsed Basics.Text)
        ,artifactDiskId :: (RP.Parsed Std_.Int64)
        ,errorMessage :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed BuildOneResult))
deriving instance (Std_.Eq (C.Parsed BuildOneResult))
instance (C.Parse BuildOneResult (C.Parsed BuildOneResult)) where
    parse raw_ = (BuildOneResult <$> (GH.parseField #name raw_)
                                 <*> (GH.parseField #artifactDiskId raw_)
                                 <*> (GH.parseField #errorMessage raw_))
instance (C.Marshal BuildOneResult (C.Parsed BuildOneResult)) where
    marshalInto raw_ BuildOneResult{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #artifactDiskId artifactDiskId raw_)
        (GH.encodeField #errorMessage errorMessage raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot BuildOneResult Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "artifactDiskId" GH.Slot BuildOneResult Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "errorMessage" GH.Slot BuildOneResult Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
data BuildEventSink 
type instance (R.ReprFor BuildEventSink) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId BuildEventSink) where
    typeId  = 16712026953253763169
instance (C.Parse BuildEventSink (GH.Client BuildEventSink)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export BuildEventSink) where
    type Server BuildEventSink = BuildEventSink'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(BuildEventSink)) [(GH.toUntypedMethodHandler ((buildEventSink'push) s_))
                                                                                ,(GH.toUntypedMethodHandler ((buildEventSink'end) s_))] [])
class (BuildEventSink'server_ s_) where
    {-# MINIMAL buildEventSink'push,buildEventSink'end #-}
    buildEventSink'push :: s_ -> (GH.MethodHandler BuildEventSink'push'params BuildEventSink'push'results)
    buildEventSink'push _ = GH.methodUnimplemented
    buildEventSink'end :: s_ -> (GH.MethodHandler BuildEventSink'end'params BuildEventSink'end'results)
    buildEventSink'end _ = GH.methodUnimplemented
instance (GH.HasMethod "push" BuildEventSink BuildEventSink'push'params BuildEventSink'push'results) where
    methodByLabel  = (GH.Method 16712026953253763169 0)
instance (GH.HasMethod "end" BuildEventSink BuildEventSink'end'params BuildEventSink'end'results) where
    methodByLabel  = (GH.Method 16712026953253763169 1)
data BuildEventSink'push'params 
type instance (R.ReprFor BuildEventSink'push'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId BuildEventSink'push'params) where
    typeId  = 17133132847911929902
instance (C.TypedStruct BuildEventSink'push'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate BuildEventSink'push'params) where
    type AllocHint BuildEventSink'push'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc BuildEventSink'push'params (C.Parsed BuildEventSink'push'params))
instance (C.AllocateList BuildEventSink'push'params) where
    type ListAllocHint BuildEventSink'push'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc BuildEventSink'push'params (C.Parsed BuildEventSink'push'params))
data instance C.Parsed BuildEventSink'push'params
    = BuildEventSink'push'params 
        {event :: (RP.Parsed BuildEvent)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed BuildEventSink'push'params))
deriving instance (Std_.Eq (C.Parsed BuildEventSink'push'params))
instance (C.Parse BuildEventSink'push'params (C.Parsed BuildEventSink'push'params)) where
    parse raw_ = (BuildEventSink'push'params <$> (GH.parseField #event raw_))
instance (C.Marshal BuildEventSink'push'params (C.Parsed BuildEventSink'push'params)) where
    marshalInto raw_ BuildEventSink'push'params{..} = (do
        (GH.encodeField #event event raw_)
        (Std_.pure ())
        )
instance (GH.HasField "event" GH.Slot BuildEventSink'push'params BuildEvent) where
    fieldByLabel  = (GH.ptrField 0)
data BuildEventSink'push'results 
type instance (R.ReprFor BuildEventSink'push'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId BuildEventSink'push'results) where
    typeId  = 16379409240599575284
instance (C.TypedStruct BuildEventSink'push'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate BuildEventSink'push'results) where
    type AllocHint BuildEventSink'push'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc BuildEventSink'push'results (C.Parsed BuildEventSink'push'results))
instance (C.AllocateList BuildEventSink'push'results) where
    type ListAllocHint BuildEventSink'push'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc BuildEventSink'push'results (C.Parsed BuildEventSink'push'results))
data instance C.Parsed BuildEventSink'push'results
    = BuildEventSink'push'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed BuildEventSink'push'results))
deriving instance (Std_.Eq (C.Parsed BuildEventSink'push'results))
instance (C.Parse BuildEventSink'push'results (C.Parsed BuildEventSink'push'results)) where
    parse raw_ = (Std_.pure BuildEventSink'push'results)
instance (C.Marshal BuildEventSink'push'results (C.Parsed BuildEventSink'push'results)) where
    marshalInto _raw (BuildEventSink'push'results) = (Std_.pure ())
data BuildEventSink'end'params 
type instance (R.ReprFor BuildEventSink'end'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId BuildEventSink'end'params) where
    typeId  = 11429780874616529355
instance (C.TypedStruct BuildEventSink'end'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate BuildEventSink'end'params) where
    type AllocHint BuildEventSink'end'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc BuildEventSink'end'params (C.Parsed BuildEventSink'end'params))
instance (C.AllocateList BuildEventSink'end'params) where
    type ListAllocHint BuildEventSink'end'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc BuildEventSink'end'params (C.Parsed BuildEventSink'end'params))
data instance C.Parsed BuildEventSink'end'params
    = BuildEventSink'end'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed BuildEventSink'end'params))
deriving instance (Std_.Eq (C.Parsed BuildEventSink'end'params))
instance (C.Parse BuildEventSink'end'params (C.Parsed BuildEventSink'end'params)) where
    parse raw_ = (Std_.pure BuildEventSink'end'params)
instance (C.Marshal BuildEventSink'end'params (C.Parsed BuildEventSink'end'params)) where
    marshalInto _raw (BuildEventSink'end'params) = (Std_.pure ())
data BuildEventSink'end'results 
type instance (R.ReprFor BuildEventSink'end'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId BuildEventSink'end'results) where
    typeId  = 12287790370474743643
instance (C.TypedStruct BuildEventSink'end'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate BuildEventSink'end'results) where
    type AllocHint BuildEventSink'end'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc BuildEventSink'end'results (C.Parsed BuildEventSink'end'results))
instance (C.AllocateList BuildEventSink'end'results) where
    type ListAllocHint BuildEventSink'end'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc BuildEventSink'end'results (C.Parsed BuildEventSink'end'results))
data instance C.Parsed BuildEventSink'end'results
    = BuildEventSink'end'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed BuildEventSink'end'results))
deriving instance (Std_.Eq (C.Parsed BuildEventSink'end'results))
instance (C.Parse BuildEventSink'end'results (C.Parsed BuildEventSink'end'results)) where
    parse raw_ = (Std_.pure BuildEventSink'end'results)
instance (C.Marshal BuildEventSink'end'results (C.Parsed BuildEventSink'end'results)) where
    marshalInto _raw (BuildEventSink'end'results) = (Std_.pure ())
data GuestAgentStatus 
type instance (R.ReprFor GuestAgentStatus) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId GuestAgentStatus) where
    typeId  = 9806760370689256619
instance (C.TypedStruct GuestAgentStatus) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate GuestAgentStatus) where
    type AllocHint GuestAgentStatus = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc GuestAgentStatus (C.Parsed GuestAgentStatus))
instance (C.AllocateList GuestAgentStatus) where
    type ListAllocHint GuestAgentStatus = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc GuestAgentStatus (C.Parsed GuestAgentStatus))
data instance C.Parsed GuestAgentStatus
    = GuestAgentStatus 
        {vmId :: (RP.Parsed Std_.Int64)
        ,lastHealthcheck :: (RP.Parsed Std_.Int64)
        ,enabled :: (RP.Parsed Std_.Bool)
        ,reachable :: (RP.Parsed Std_.Bool)
        ,message :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed GuestAgentStatus))
deriving instance (Std_.Eq (C.Parsed GuestAgentStatus))
instance (C.Parse GuestAgentStatus (C.Parsed GuestAgentStatus)) where
    parse raw_ = (GuestAgentStatus <$> (GH.parseField #vmId raw_)
                                   <*> (GH.parseField #lastHealthcheck raw_)
                                   <*> (GH.parseField #enabled raw_)
                                   <*> (GH.parseField #reachable raw_)
                                   <*> (GH.parseField #message raw_))
instance (C.Marshal GuestAgentStatus (C.Parsed GuestAgentStatus)) where
    marshalInto raw_ GuestAgentStatus{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (GH.encodeField #lastHealthcheck lastHealthcheck raw_)
        (GH.encodeField #enabled enabled raw_)
        (GH.encodeField #reachable reachable raw_)
        (GH.encodeField #message message raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot GuestAgentStatus Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "lastHealthcheck" GH.Slot GuestAgentStatus Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "enabled" GH.Slot GuestAgentStatus Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 2 1 0)
instance (GH.HasField "reachable" GH.Slot GuestAgentStatus Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 2 1 0)
instance (GH.HasField "message" GH.Slot GuestAgentStatus Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data GuestAgentStatusSink 
type instance (R.ReprFor GuestAgentStatusSink) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId GuestAgentStatusSink) where
    typeId  = 18387021947746468511
instance (C.Parse GuestAgentStatusSink (GH.Client GuestAgentStatusSink)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export GuestAgentStatusSink) where
    type Server GuestAgentStatusSink = GuestAgentStatusSink'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(GuestAgentStatusSink)) [(GH.toUntypedMethodHandler ((guestAgentStatusSink'push) s_))] [])
class (GuestAgentStatusSink'server_ s_) where
    {-# MINIMAL guestAgentStatusSink'push #-}
    guestAgentStatusSink'push :: s_ -> (GH.MethodHandler GuestAgentStatusSink'push'params GuestAgentStatusSink'push'results)
    guestAgentStatusSink'push _ = GH.methodUnimplemented
instance (GH.HasMethod "push" GuestAgentStatusSink GuestAgentStatusSink'push'params GuestAgentStatusSink'push'results) where
    methodByLabel  = (GH.Method 18387021947746468511 0)
data GuestAgentStatusSink'push'params 
type instance (R.ReprFor GuestAgentStatusSink'push'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId GuestAgentStatusSink'push'params) where
    typeId  = 10073535227956786748
instance (C.TypedStruct GuestAgentStatusSink'push'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate GuestAgentStatusSink'push'params) where
    type AllocHint GuestAgentStatusSink'push'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc GuestAgentStatusSink'push'params (C.Parsed GuestAgentStatusSink'push'params))
instance (C.AllocateList GuestAgentStatusSink'push'params) where
    type ListAllocHint GuestAgentStatusSink'push'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc GuestAgentStatusSink'push'params (C.Parsed GuestAgentStatusSink'push'params))
data instance C.Parsed GuestAgentStatusSink'push'params
    = GuestAgentStatusSink'push'params 
        {status :: (RP.Parsed GuestAgentStatus)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed GuestAgentStatusSink'push'params))
deriving instance (Std_.Eq (C.Parsed GuestAgentStatusSink'push'params))
instance (C.Parse GuestAgentStatusSink'push'params (C.Parsed GuestAgentStatusSink'push'params)) where
    parse raw_ = (GuestAgentStatusSink'push'params <$> (GH.parseField #status raw_))
instance (C.Marshal GuestAgentStatusSink'push'params (C.Parsed GuestAgentStatusSink'push'params)) where
    marshalInto raw_ GuestAgentStatusSink'push'params{..} = (do
        (GH.encodeField #status status raw_)
        (Std_.pure ())
        )
instance (GH.HasField "status" GH.Slot GuestAgentStatusSink'push'params GuestAgentStatus) where
    fieldByLabel  = (GH.ptrField 0)
data GuestAgentStatusSink'push'results 
type instance (R.ReprFor GuestAgentStatusSink'push'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId GuestAgentStatusSink'push'results) where
    typeId  = 16640327112236440375
instance (C.TypedStruct GuestAgentStatusSink'push'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate GuestAgentStatusSink'push'results) where
    type AllocHint GuestAgentStatusSink'push'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc GuestAgentStatusSink'push'results (C.Parsed GuestAgentStatusSink'push'results))
instance (C.AllocateList GuestAgentStatusSink'push'results) where
    type ListAllocHint GuestAgentStatusSink'push'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc GuestAgentStatusSink'push'results (C.Parsed GuestAgentStatusSink'push'results))
data instance C.Parsed GuestAgentStatusSink'push'results
    = GuestAgentStatusSink'push'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed GuestAgentStatusSink'push'results))
deriving instance (Std_.Eq (C.Parsed GuestAgentStatusSink'push'results))
instance (C.Parse GuestAgentStatusSink'push'results (C.Parsed GuestAgentStatusSink'push'results)) where
    parse raw_ = (Std_.pure GuestAgentStatusSink'push'results)
instance (C.Marshal GuestAgentStatusSink'push'results (C.Parsed GuestAgentStatusSink'push'results)) where
    marshalInto _raw (GuestAgentStatusSink'push'results) = (Std_.pure ())
data TaskProgressEvent 
type instance (R.ReprFor TaskProgressEvent) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TaskProgressEvent) where
    typeId  = 17149390811314179134
instance (C.TypedStruct TaskProgressEvent) where
    numStructWords  = 4
    numStructPtrs  = 1
instance (C.Allocate TaskProgressEvent) where
    type AllocHint TaskProgressEvent = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TaskProgressEvent (C.Parsed TaskProgressEvent))
instance (C.AllocateList TaskProgressEvent) where
    type ListAllocHint TaskProgressEvent = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TaskProgressEvent (C.Parsed TaskProgressEvent))
data instance C.Parsed TaskProgressEvent
    = TaskProgressEvent 
        {taskId :: (RP.Parsed Std_.Int64)
        ,union' :: (C.Parsed (GH.Which TaskProgressEvent))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TaskProgressEvent))
deriving instance (Std_.Eq (C.Parsed TaskProgressEvent))
instance (C.Parse TaskProgressEvent (C.Parsed TaskProgressEvent)) where
    parse raw_ = (TaskProgressEvent <$> (GH.parseField #taskId raw_)
                                    <*> (C.parse (GH.structUnion raw_)))
instance (C.Marshal TaskProgressEvent (C.Parsed TaskProgressEvent)) where
    marshalInto raw_ TaskProgressEvent{..} = (do
        (GH.encodeField #taskId taskId raw_)
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion TaskProgressEvent) where
    unionField  = (GH.dataField 16 1 16 0)
    data RawWhich TaskProgressEvent mut_
        = RW_TaskProgressEvent'started (R.Raw TaskProgressEvent'started mut_)
        | RW_TaskProgressEvent'progress (R.Raw TaskProgressEvent'progress mut_)
        | RW_TaskProgressEvent'finished (R.Raw TaskProgressEvent'finished mut_)
        | RW_TaskProgressEvent'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_TaskProgressEvent'started <$> (GH.readVariant #started struct_))
        1 ->
            (RW_TaskProgressEvent'progress <$> (GH.readVariant #progress struct_))
        2 ->
            (RW_TaskProgressEvent'finished <$> (GH.readVariant #finished struct_))
        _ ->
            (Std_.pure (RW_TaskProgressEvent'unknown' tag_))
    data Which TaskProgressEvent
instance (GH.HasVariant "started" GH.Group TaskProgressEvent TaskProgressEvent'started) where
    variantByLabel  = (GH.Variant GH.groupField 0)
instance (GH.HasVariant "progress" GH.Group TaskProgressEvent TaskProgressEvent'progress) where
    variantByLabel  = (GH.Variant GH.groupField 1)
instance (GH.HasVariant "finished" GH.Group TaskProgressEvent TaskProgressEvent'finished) where
    variantByLabel  = (GH.Variant GH.groupField 2)
data instance C.Parsed (GH.Which TaskProgressEvent)
    = TaskProgressEvent'started (RP.Parsed TaskProgressEvent'started)
    | TaskProgressEvent'progress (RP.Parsed TaskProgressEvent'progress)
    | TaskProgressEvent'finished (RP.Parsed TaskProgressEvent'finished)
    | TaskProgressEvent'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which TaskProgressEvent)))
deriving instance (Std_.Eq (C.Parsed (GH.Which TaskProgressEvent)))
instance (C.Parse (GH.Which TaskProgressEvent) (C.Parsed (GH.Which TaskProgressEvent))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_TaskProgressEvent'started rawArg_) ->
                (TaskProgressEvent'started <$> (C.parse rawArg_))
            (RW_TaskProgressEvent'progress rawArg_) ->
                (TaskProgressEvent'progress <$> (C.parse rawArg_))
            (RW_TaskProgressEvent'finished rawArg_) ->
                (TaskProgressEvent'finished <$> (C.parse rawArg_))
            (RW_TaskProgressEvent'unknown' tag_) ->
                (Std_.pure (TaskProgressEvent'unknown' tag_))
        )
instance (C.Marshal (GH.Which TaskProgressEvent) (C.Parsed (GH.Which TaskProgressEvent))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (TaskProgressEvent'started arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #started (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (TaskProgressEvent'progress arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #progress (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (TaskProgressEvent'finished arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #finished (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (TaskProgressEvent'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
instance (GH.HasField "taskId" GH.Slot TaskProgressEvent Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data TaskProgressEvent'started 
type instance (R.ReprFor TaskProgressEvent'started) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TaskProgressEvent'started) where
    typeId  = 17663129747473275575
instance (C.TypedStruct TaskProgressEvent'started) where
    numStructWords  = 4
    numStructPtrs  = 1
instance (C.Allocate TaskProgressEvent'started) where
    type AllocHint TaskProgressEvent'started = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TaskProgressEvent'started (C.Parsed TaskProgressEvent'started))
instance (C.AllocateList TaskProgressEvent'started) where
    type ListAllocHint TaskProgressEvent'started = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TaskProgressEvent'started (C.Parsed TaskProgressEvent'started))
data instance C.Parsed TaskProgressEvent'started
    = TaskProgressEvent'started' 
        {command :: (RP.Parsed Basics.Text)
        ,subsystem :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.TaskSubsystem)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TaskProgressEvent'started))
deriving instance (Std_.Eq (C.Parsed TaskProgressEvent'started))
instance (C.Parse TaskProgressEvent'started (C.Parsed TaskProgressEvent'started)) where
    parse raw_ = (TaskProgressEvent'started' <$> (GH.parseField #command raw_)
                                             <*> (GH.parseField #subsystem raw_))
instance (C.Marshal TaskProgressEvent'started (C.Parsed TaskProgressEvent'started)) where
    marshalInto raw_ TaskProgressEvent'started'{..} = (do
        (GH.encodeField #command command raw_)
        (GH.encodeField #subsystem subsystem raw_)
        (Std_.pure ())
        )
instance (GH.HasField "command" GH.Slot TaskProgressEvent'started Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "subsystem" GH.Slot TaskProgressEvent'started Capnp.Gen.ById.Xbf9b09f64c0dd40d.TaskSubsystem) where
    fieldByLabel  = (GH.dataField 0 1 16 0)
data TaskProgressEvent'progress 
type instance (R.ReprFor TaskProgressEvent'progress) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TaskProgressEvent'progress) where
    typeId  = 13460493468814335971
instance (C.TypedStruct TaskProgressEvent'progress) where
    numStructWords  = 4
    numStructPtrs  = 1
instance (C.Allocate TaskProgressEvent'progress) where
    type AllocHint TaskProgressEvent'progress = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TaskProgressEvent'progress (C.Parsed TaskProgressEvent'progress))
instance (C.AllocateList TaskProgressEvent'progress) where
    type ListAllocHint TaskProgressEvent'progress = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TaskProgressEvent'progress (C.Parsed TaskProgressEvent'progress))
data instance C.Parsed TaskProgressEvent'progress
    = TaskProgressEvent'progress' 
        {completed :: (RP.Parsed Std_.Int64)
        ,total :: (RP.Parsed Std_.Int64)
        ,label :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TaskProgressEvent'progress))
deriving instance (Std_.Eq (C.Parsed TaskProgressEvent'progress))
instance (C.Parse TaskProgressEvent'progress (C.Parsed TaskProgressEvent'progress)) where
    parse raw_ = (TaskProgressEvent'progress' <$> (GH.parseField #completed raw_)
                                              <*> (GH.parseField #total raw_)
                                              <*> (GH.parseField #label raw_))
instance (C.Marshal TaskProgressEvent'progress (C.Parsed TaskProgressEvent'progress)) where
    marshalInto raw_ TaskProgressEvent'progress'{..} = (do
        (GH.encodeField #completed completed raw_)
        (GH.encodeField #total total raw_)
        (GH.encodeField #label label raw_)
        (Std_.pure ())
        )
instance (GH.HasField "completed" GH.Slot TaskProgressEvent'progress Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
instance (GH.HasField "total" GH.Slot TaskProgressEvent'progress Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 3 64 0)
instance (GH.HasField "label" GH.Slot TaskProgressEvent'progress Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data TaskProgressEvent'finished 
type instance (R.ReprFor TaskProgressEvent'finished) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TaskProgressEvent'finished) where
    typeId  = 17063630004492208097
instance (C.TypedStruct TaskProgressEvent'finished) where
    numStructWords  = 4
    numStructPtrs  = 1
instance (C.Allocate TaskProgressEvent'finished) where
    type AllocHint TaskProgressEvent'finished = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TaskProgressEvent'finished (C.Parsed TaskProgressEvent'finished))
instance (C.AllocateList TaskProgressEvent'finished) where
    type ListAllocHint TaskProgressEvent'finished = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TaskProgressEvent'finished (C.Parsed TaskProgressEvent'finished))
data instance C.Parsed TaskProgressEvent'finished
    = TaskProgressEvent'finished' 
        {result :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.TaskResult)
        ,message :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TaskProgressEvent'finished))
deriving instance (Std_.Eq (C.Parsed TaskProgressEvent'finished))
instance (C.Parse TaskProgressEvent'finished (C.Parsed TaskProgressEvent'finished)) where
    parse raw_ = (TaskProgressEvent'finished' <$> (GH.parseField #result raw_)
                                              <*> (GH.parseField #message raw_))
instance (C.Marshal TaskProgressEvent'finished (C.Parsed TaskProgressEvent'finished)) where
    marshalInto raw_ TaskProgressEvent'finished'{..} = (do
        (GH.encodeField #result result raw_)
        (GH.encodeField #message message raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot TaskProgressEvent'finished Capnp.Gen.ById.Xbf9b09f64c0dd40d.TaskResult) where
    fieldByLabel  = (GH.dataField 0 1 16 0)
instance (GH.HasField "message" GH.Slot TaskProgressEvent'finished Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data TaskProgressSink 
type instance (R.ReprFor TaskProgressSink) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId TaskProgressSink) where
    typeId  = 12651927043253757543
instance (C.Parse TaskProgressSink (GH.Client TaskProgressSink)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export TaskProgressSink) where
    type Server TaskProgressSink = TaskProgressSink'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(TaskProgressSink)) [(GH.toUntypedMethodHandler ((taskProgressSink'push) s_))] [])
class (TaskProgressSink'server_ s_) where
    {-# MINIMAL taskProgressSink'push #-}
    taskProgressSink'push :: s_ -> (GH.MethodHandler TaskProgressSink'push'params TaskProgressSink'push'results)
    taskProgressSink'push _ = GH.methodUnimplemented
instance (GH.HasMethod "push" TaskProgressSink TaskProgressSink'push'params TaskProgressSink'push'results) where
    methodByLabel  = (GH.Method 12651927043253757543 0)
data TaskProgressSink'push'params 
type instance (R.ReprFor TaskProgressSink'push'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TaskProgressSink'push'params) where
    typeId  = 16257940435731255139
instance (C.TypedStruct TaskProgressSink'push'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate TaskProgressSink'push'params) where
    type AllocHint TaskProgressSink'push'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TaskProgressSink'push'params (C.Parsed TaskProgressSink'push'params))
instance (C.AllocateList TaskProgressSink'push'params) where
    type ListAllocHint TaskProgressSink'push'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TaskProgressSink'push'params (C.Parsed TaskProgressSink'push'params))
data instance C.Parsed TaskProgressSink'push'params
    = TaskProgressSink'push'params 
        {event :: (RP.Parsed TaskProgressEvent)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TaskProgressSink'push'params))
deriving instance (Std_.Eq (C.Parsed TaskProgressSink'push'params))
instance (C.Parse TaskProgressSink'push'params (C.Parsed TaskProgressSink'push'params)) where
    parse raw_ = (TaskProgressSink'push'params <$> (GH.parseField #event raw_))
instance (C.Marshal TaskProgressSink'push'params (C.Parsed TaskProgressSink'push'params)) where
    marshalInto raw_ TaskProgressSink'push'params{..} = (do
        (GH.encodeField #event event raw_)
        (Std_.pure ())
        )
instance (GH.HasField "event" GH.Slot TaskProgressSink'push'params TaskProgressEvent) where
    fieldByLabel  = (GH.ptrField 0)
data TaskProgressSink'push'results 
type instance (R.ReprFor TaskProgressSink'push'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TaskProgressSink'push'results) where
    typeId  = 13247952275294732796
instance (C.TypedStruct TaskProgressSink'push'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate TaskProgressSink'push'results) where
    type AllocHint TaskProgressSink'push'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TaskProgressSink'push'results (C.Parsed TaskProgressSink'push'results))
instance (C.AllocateList TaskProgressSink'push'results) where
    type ListAllocHint TaskProgressSink'push'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TaskProgressSink'push'results (C.Parsed TaskProgressSink'push'results))
data instance C.Parsed TaskProgressSink'push'results
    = TaskProgressSink'push'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TaskProgressSink'push'results))
deriving instance (Std_.Eq (C.Parsed TaskProgressSink'push'results))
instance (C.Parse TaskProgressSink'push'results (C.Parsed TaskProgressSink'push'results)) where
    parse raw_ = (Std_.pure TaskProgressSink'push'results)
instance (C.Marshal TaskProgressSink'push'results (C.Parsed TaskProgressSink'push'results)) where
    marshalInto _raw (TaskProgressSink'push'results) = (Std_.pure ())
data Handle 
type instance (R.ReprFor Handle) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Handle) where
    typeId  = 16791630405107916798
instance (C.Parse Handle (GH.Client Handle)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Handle) where
    type Server Handle = Handle'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Handle)) [] [])
class (Handle'server_ s_) where
    {-# MINIMAL  #-}