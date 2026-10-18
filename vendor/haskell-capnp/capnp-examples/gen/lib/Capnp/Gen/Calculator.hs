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
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
module Capnp.Gen.Calculator where
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
data Calculator 
type instance (R.ReprFor Calculator) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Calculator) where
    typeId  = 10923537602090224694
instance (C.Parse Calculator (GH.Client Calculator)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Calculator) where
    type Server Calculator = Calculator'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Calculator)) [(GH.toUntypedMethodHandler ((calculator'evaluate) s_))
                                                                            ,(GH.toUntypedMethodHandler ((calculator'defFunction) s_))
                                                                            ,(GH.toUntypedMethodHandler ((calculator'getOperator) s_))] [])
class (Calculator'server_ s_) where
    {-# MINIMAL calculator'evaluate,calculator'defFunction,calculator'getOperator #-}
    calculator'evaluate :: s_ -> (GH.MethodHandler Calculator'evaluate'params Calculator'evaluate'results)
    calculator'evaluate _ = GH.methodUnimplemented
    calculator'defFunction :: s_ -> (GH.MethodHandler Calculator'defFunction'params Calculator'defFunction'results)
    calculator'defFunction _ = GH.methodUnimplemented
    calculator'getOperator :: s_ -> (GH.MethodHandler Calculator'getOperator'params Calculator'getOperator'results)
    calculator'getOperator _ = GH.methodUnimplemented
instance (GH.HasMethod "evaluate" Calculator Calculator'evaluate'params Calculator'evaluate'results) where
    methodByLabel  = (GH.Method 10923537602090224694 0)
instance (GH.HasMethod "defFunction" Calculator Calculator'defFunction'params Calculator'defFunction'results) where
    methodByLabel  = (GH.Method 10923537602090224694 1)
instance (GH.HasMethod "getOperator" Calculator Calculator'getOperator'params Calculator'getOperator'results) where
    methodByLabel  = (GH.Method 10923537602090224694 2)
data Calculator'evaluate'params 
type instance (R.ReprFor Calculator'evaluate'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Calculator'evaluate'params) where
    typeId  = 13478898619544909524
instance (C.TypedStruct Calculator'evaluate'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Calculator'evaluate'params) where
    type AllocHint Calculator'evaluate'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Calculator'evaluate'params (C.Parsed Calculator'evaluate'params))
instance (C.AllocateList Calculator'evaluate'params) where
    type ListAllocHint Calculator'evaluate'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Calculator'evaluate'params (C.Parsed Calculator'evaluate'params))
data instance C.Parsed Calculator'evaluate'params
    = Calculator'evaluate'params 
        {expression :: (RP.Parsed Expression)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Calculator'evaluate'params))
deriving instance (Std_.Eq (C.Parsed Calculator'evaluate'params))
instance (C.Parse Calculator'evaluate'params (C.Parsed Calculator'evaluate'params)) where
    parse raw_ = (Calculator'evaluate'params <$> (GH.parseField #expression raw_))
instance (C.Marshal Calculator'evaluate'params (C.Parsed Calculator'evaluate'params)) where
    marshalInto raw_ Calculator'evaluate'params{..} = (do
        (GH.encodeField #expression expression raw_)
        (Std_.pure ())
        )
instance (GH.HasField "expression" GH.Slot Calculator'evaluate'params Expression) where
    fieldByLabel  = (GH.ptrField 0)
data Calculator'evaluate'results 
type instance (R.ReprFor Calculator'evaluate'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Calculator'evaluate'results) where
    typeId  = 9345430975918089745
instance (C.TypedStruct Calculator'evaluate'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Calculator'evaluate'results) where
    type AllocHint Calculator'evaluate'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Calculator'evaluate'results (C.Parsed Calculator'evaluate'results))
instance (C.AllocateList Calculator'evaluate'results) where
    type ListAllocHint Calculator'evaluate'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Calculator'evaluate'results (C.Parsed Calculator'evaluate'results))
data instance C.Parsed Calculator'evaluate'results
    = Calculator'evaluate'results 
        {value :: (RP.Parsed Value)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Calculator'evaluate'results))
deriving instance (Std_.Eq (C.Parsed Calculator'evaluate'results))
instance (C.Parse Calculator'evaluate'results (C.Parsed Calculator'evaluate'results)) where
    parse raw_ = (Calculator'evaluate'results <$> (GH.parseField #value raw_))
instance (C.Marshal Calculator'evaluate'results (C.Parsed Calculator'evaluate'results)) where
    marshalInto raw_ Calculator'evaluate'results{..} = (do
        (GH.encodeField #value value raw_)
        (Std_.pure ())
        )
instance (GH.HasField "value" GH.Slot Calculator'evaluate'results Value) where
    fieldByLabel  = (GH.ptrField 0)
data Calculator'defFunction'params 
type instance (R.ReprFor Calculator'defFunction'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Calculator'defFunction'params) where
    typeId  = 17476144387247758473
instance (C.TypedStruct Calculator'defFunction'params) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Calculator'defFunction'params) where
    type AllocHint Calculator'defFunction'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Calculator'defFunction'params (C.Parsed Calculator'defFunction'params))
instance (C.AllocateList Calculator'defFunction'params) where
    type ListAllocHint Calculator'defFunction'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Calculator'defFunction'params (C.Parsed Calculator'defFunction'params))
data instance C.Parsed Calculator'defFunction'params
    = Calculator'defFunction'params 
        {paramCount :: (RP.Parsed Std_.Int32)
        ,body :: (RP.Parsed Expression)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Calculator'defFunction'params))
deriving instance (Std_.Eq (C.Parsed Calculator'defFunction'params))
instance (C.Parse Calculator'defFunction'params (C.Parsed Calculator'defFunction'params)) where
    parse raw_ = (Calculator'defFunction'params <$> (GH.parseField #paramCount raw_)
                                                <*> (GH.parseField #body raw_))
instance (C.Marshal Calculator'defFunction'params (C.Parsed Calculator'defFunction'params)) where
    marshalInto raw_ Calculator'defFunction'params{..} = (do
        (GH.encodeField #paramCount paramCount raw_)
        (GH.encodeField #body body raw_)
        (Std_.pure ())
        )
instance (GH.HasField "paramCount" GH.Slot Calculator'defFunction'params Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "body" GH.Slot Calculator'defFunction'params Expression) where
    fieldByLabel  = (GH.ptrField 0)
data Calculator'defFunction'results 
type instance (R.ReprFor Calculator'defFunction'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Calculator'defFunction'results) where
    typeId  = 10170522573213587144
instance (C.TypedStruct Calculator'defFunction'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Calculator'defFunction'results) where
    type AllocHint Calculator'defFunction'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Calculator'defFunction'results (C.Parsed Calculator'defFunction'results))
instance (C.AllocateList Calculator'defFunction'results) where
    type ListAllocHint Calculator'defFunction'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Calculator'defFunction'results (C.Parsed Calculator'defFunction'results))
data instance C.Parsed Calculator'defFunction'results
    = Calculator'defFunction'results 
        {func :: (RP.Parsed Function)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Calculator'defFunction'results))
deriving instance (Std_.Eq (C.Parsed Calculator'defFunction'results))
instance (C.Parse Calculator'defFunction'results (C.Parsed Calculator'defFunction'results)) where
    parse raw_ = (Calculator'defFunction'results <$> (GH.parseField #func raw_))
instance (C.Marshal Calculator'defFunction'results (C.Parsed Calculator'defFunction'results)) where
    marshalInto raw_ Calculator'defFunction'results{..} = (do
        (GH.encodeField #func func raw_)
        (Std_.pure ())
        )
instance (GH.HasField "func" GH.Slot Calculator'defFunction'results Function) where
    fieldByLabel  = (GH.ptrField 0)
data Calculator'getOperator'params 
type instance (R.ReprFor Calculator'getOperator'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Calculator'getOperator'params) where
    typeId  = 9983863225544066352
instance (C.TypedStruct Calculator'getOperator'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Calculator'getOperator'params) where
    type AllocHint Calculator'getOperator'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Calculator'getOperator'params (C.Parsed Calculator'getOperator'params))
instance (C.AllocateList Calculator'getOperator'params) where
    type ListAllocHint Calculator'getOperator'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Calculator'getOperator'params (C.Parsed Calculator'getOperator'params))
data instance C.Parsed Calculator'getOperator'params
    = Calculator'getOperator'params 
        {op :: (RP.Parsed Operator)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Calculator'getOperator'params))
deriving instance (Std_.Eq (C.Parsed Calculator'getOperator'params))
instance (C.Parse Calculator'getOperator'params (C.Parsed Calculator'getOperator'params)) where
    parse raw_ = (Calculator'getOperator'params <$> (GH.parseField #op raw_))
instance (C.Marshal Calculator'getOperator'params (C.Parsed Calculator'getOperator'params)) where
    marshalInto raw_ Calculator'getOperator'params{..} = (do
        (GH.encodeField #op op raw_)
        (Std_.pure ())
        )
instance (GH.HasField "op" GH.Slot Calculator'getOperator'params Operator) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
data Calculator'getOperator'results 
type instance (R.ReprFor Calculator'getOperator'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Calculator'getOperator'results) where
    typeId  = 15100494197045627936
instance (C.TypedStruct Calculator'getOperator'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Calculator'getOperator'results) where
    type AllocHint Calculator'getOperator'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Calculator'getOperator'results (C.Parsed Calculator'getOperator'results))
instance (C.AllocateList Calculator'getOperator'results) where
    type ListAllocHint Calculator'getOperator'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Calculator'getOperator'results (C.Parsed Calculator'getOperator'results))
data instance C.Parsed Calculator'getOperator'results
    = Calculator'getOperator'results 
        {func :: (RP.Parsed Function)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Calculator'getOperator'results))
deriving instance (Std_.Eq (C.Parsed Calculator'getOperator'results))
instance (C.Parse Calculator'getOperator'results (C.Parsed Calculator'getOperator'results)) where
    parse raw_ = (Calculator'getOperator'results <$> (GH.parseField #func raw_))
instance (C.Marshal Calculator'getOperator'results (C.Parsed Calculator'getOperator'results)) where
    marshalInto raw_ Calculator'getOperator'results{..} = (do
        (GH.encodeField #func func raw_)
        (Std_.pure ())
        )
instance (GH.HasField "func" GH.Slot Calculator'getOperator'results Function) where
    fieldByLabel  = (GH.ptrField 0)
data Expression 
type instance (R.ReprFor Expression) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Expression) where
    typeId  = 15292209801435843861
instance (C.TypedStruct Expression) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate Expression) where
    type AllocHint Expression = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Expression (C.Parsed Expression))
instance (C.AllocateList Expression) where
    type ListAllocHint Expression = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Expression (C.Parsed Expression))
data instance C.Parsed Expression
    = Expression 
        {union' :: (C.Parsed (GH.Which Expression))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Expression))
deriving instance (Std_.Eq (C.Parsed Expression))
instance (C.Parse Expression (C.Parsed Expression)) where
    parse raw_ = (Expression <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Expression (C.Parsed Expression)) where
    marshalInto raw_ Expression{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Expression) where
    unionField  = (GH.dataField 0 1 16 0)
    data RawWhich Expression mut_
        = RW_Expression'literal (R.Raw Std_.Double mut_)
        | RW_Expression'previousResult (R.Raw Value mut_)
        | RW_Expression'parameter (R.Raw Std_.Word32 mut_)
        | RW_Expression'call (R.Raw Expression'call mut_)
        | RW_Expression'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Expression'literal <$> (GH.readVariant #literal struct_))
        1 ->
            (RW_Expression'previousResult <$> (GH.readVariant #previousResult struct_))
        2 ->
            (RW_Expression'parameter <$> (GH.readVariant #parameter struct_))
        3 ->
            (RW_Expression'call <$> (GH.readVariant #call struct_))
        _ ->
            (Std_.pure (RW_Expression'unknown' tag_))
    data Which Expression
instance (GH.HasVariant "literal" GH.Slot Expression Std_.Double) where
    variantByLabel  = (GH.Variant (GH.dataField 0 0 64 0) 0)
instance (GH.HasVariant "previousResult" GH.Slot Expression Value) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 1)
instance (GH.HasVariant "parameter" GH.Slot Expression Std_.Word32) where
    variantByLabel  = (GH.Variant (GH.dataField 0 0 32 0) 2)
instance (GH.HasVariant "call" GH.Group Expression Expression'call) where
    variantByLabel  = (GH.Variant GH.groupField 3)
data instance C.Parsed (GH.Which Expression)
    = Expression'literal (RP.Parsed Std_.Double)
    | Expression'previousResult (RP.Parsed Value)
    | Expression'parameter (RP.Parsed Std_.Word32)
    | Expression'call (RP.Parsed Expression'call)
    | Expression'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Expression)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Expression)))
instance (C.Parse (GH.Which Expression) (C.Parsed (GH.Which Expression))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Expression'literal rawArg_) ->
                (Expression'literal <$> (C.parse rawArg_))
            (RW_Expression'previousResult rawArg_) ->
                (Expression'previousResult <$> (C.parse rawArg_))
            (RW_Expression'parameter rawArg_) ->
                (Expression'parameter <$> (C.parse rawArg_))
            (RW_Expression'call rawArg_) ->
                (Expression'call <$> (C.parse rawArg_))
            (RW_Expression'unknown' tag_) ->
                (Std_.pure (Expression'unknown' tag_))
        )
instance (C.Marshal (GH.Which Expression) (C.Parsed (GH.Which Expression))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Expression'literal arg_) ->
            (GH.encodeVariant #literal arg_ (GH.unionStruct raw_))
        (Expression'previousResult arg_) ->
            (GH.encodeVariant #previousResult arg_ (GH.unionStruct raw_))
        (Expression'parameter arg_) ->
            (GH.encodeVariant #parameter arg_ (GH.unionStruct raw_))
        (Expression'call arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #call (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Expression'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data Expression'call 
type instance (R.ReprFor Expression'call) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Expression'call) where
    typeId  = 15678475764784139622
instance (C.TypedStruct Expression'call) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate Expression'call) where
    type AllocHint Expression'call = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Expression'call (C.Parsed Expression'call))
instance (C.AllocateList Expression'call) where
    type ListAllocHint Expression'call = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Expression'call (C.Parsed Expression'call))
data instance C.Parsed Expression'call
    = Expression'call' 
        {function :: (RP.Parsed Function)
        ,params :: (RP.Parsed (R.List Expression))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Expression'call))
deriving instance (Std_.Eq (C.Parsed Expression'call))
instance (C.Parse Expression'call (C.Parsed Expression'call)) where
    parse raw_ = (Expression'call' <$> (GH.parseField #function raw_)
                                   <*> (GH.parseField #params raw_))
instance (C.Marshal Expression'call (C.Parsed Expression'call)) where
    marshalInto raw_ Expression'call'{..} = (do
        (GH.encodeField #function function raw_)
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "function" GH.Slot Expression'call Function) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "params" GH.Slot Expression'call (R.List Expression)) where
    fieldByLabel  = (GH.ptrField 1)
data Value 
type instance (R.ReprFor Value) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Value) where
    typeId  = 14116142932258867410
instance (C.Parse Value (GH.Client Value)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Value) where
    type Server Value = Value'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Value)) [(GH.toUntypedMethodHandler ((value'read) s_))] [])
class (Value'server_ s_) where
    {-# MINIMAL value'read #-}
    value'read :: s_ -> (GH.MethodHandler Value'read'params Value'read'results)
    value'read _ = GH.methodUnimplemented
instance (GH.HasMethod "read" Value Value'read'params Value'read'results) where
    methodByLabel  = (GH.Method 14116142932258867410 0)
data Value'read'params 
type instance (R.ReprFor Value'read'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Value'read'params) where
    typeId  = 15227555948799047000
instance (C.TypedStruct Value'read'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Value'read'params) where
    type AllocHint Value'read'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Value'read'params (C.Parsed Value'read'params))
instance (C.AllocateList Value'read'params) where
    type ListAllocHint Value'read'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Value'read'params (C.Parsed Value'read'params))
data instance C.Parsed Value'read'params
    = Value'read'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Value'read'params))
deriving instance (Std_.Eq (C.Parsed Value'read'params))
instance (C.Parse Value'read'params (C.Parsed Value'read'params)) where
    parse raw_ = (Std_.pure Value'read'params)
instance (C.Marshal Value'read'params (C.Parsed Value'read'params)) where
    marshalInto _raw (Value'read'params) = (Std_.pure ())
data Value'read'results 
type instance (R.ReprFor Value'read'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Value'read'results) where
    typeId  = 16626840274624768034
instance (C.TypedStruct Value'read'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Value'read'results) where
    type AllocHint Value'read'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Value'read'results (C.Parsed Value'read'results))
instance (C.AllocateList Value'read'results) where
    type ListAllocHint Value'read'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Value'read'results (C.Parsed Value'read'results))
data instance C.Parsed Value'read'results
    = Value'read'results 
        {value :: (RP.Parsed Std_.Double)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Value'read'results))
deriving instance (Std_.Eq (C.Parsed Value'read'results))
instance (C.Parse Value'read'results (C.Parsed Value'read'results)) where
    parse raw_ = (Value'read'results <$> (GH.parseField #value raw_))
instance (C.Marshal Value'read'results (C.Parsed Value'read'results)) where
    marshalInto raw_ Value'read'results{..} = (do
        (GH.encodeField #value value raw_)
        (Std_.pure ())
        )
instance (GH.HasField "value" GH.Slot Value'read'results Std_.Double) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Function 
type instance (R.ReprFor Function) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Function) where
    typeId  = 17143016017778443156
instance (C.Parse Function (GH.Client Function)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Function) where
    type Server Function = Function'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Function)) [(GH.toUntypedMethodHandler ((function'call) s_))] [])
class (Function'server_ s_) where
    {-# MINIMAL function'call #-}
    function'call :: s_ -> (GH.MethodHandler Function'call'params Function'call'results)
    function'call _ = GH.methodUnimplemented
instance (GH.HasMethod "call" Function Function'call'params Function'call'results) where
    methodByLabel  = (GH.Method 17143016017778443156 0)
data Function'call'params 
type instance (R.ReprFor Function'call'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Function'call'params) where
    typeId  = 12795114529121400599
instance (C.TypedStruct Function'call'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Function'call'params) where
    type AllocHint Function'call'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Function'call'params (C.Parsed Function'call'params))
instance (C.AllocateList Function'call'params) where
    type ListAllocHint Function'call'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Function'call'params (C.Parsed Function'call'params))
data instance C.Parsed Function'call'params
    = Function'call'params 
        {params :: (RP.Parsed (R.List Std_.Double))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Function'call'params))
deriving instance (Std_.Eq (C.Parsed Function'call'params))
instance (C.Parse Function'call'params (C.Parsed Function'call'params)) where
    parse raw_ = (Function'call'params <$> (GH.parseField #params raw_))
instance (C.Marshal Function'call'params (C.Parsed Function'call'params)) where
    marshalInto raw_ Function'call'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot Function'call'params (R.List Std_.Double)) where
    fieldByLabel  = (GH.ptrField 0)
data Function'call'results 
type instance (R.ReprFor Function'call'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Function'call'results) where
    typeId  = 13898297529173597869
instance (C.TypedStruct Function'call'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Function'call'results) where
    type AllocHint Function'call'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Function'call'results (C.Parsed Function'call'results))
instance (C.AllocateList Function'call'results) where
    type ListAllocHint Function'call'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Function'call'results (C.Parsed Function'call'results))
data instance C.Parsed Function'call'results
    = Function'call'results 
        {value :: (RP.Parsed Std_.Double)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Function'call'results))
deriving instance (Std_.Eq (C.Parsed Function'call'results))
instance (C.Parse Function'call'results (C.Parsed Function'call'results)) where
    parse raw_ = (Function'call'results <$> (GH.parseField #value raw_))
instance (C.Marshal Function'call'results (C.Parsed Function'call'results)) where
    marshalInto raw_ Function'call'results{..} = (do
        (GH.encodeField #value value raw_)
        (Std_.pure ())
        )
instance (GH.HasField "value" GH.Slot Function'call'results Std_.Double) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Operator 
    = Operator'add 
    | Operator'subtract 
    | Operator'multiply 
    | Operator'divide 
    | Operator'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor Operator) = (R.Data R.Sz16)
instance (C.HasTypeId Operator) where
    typeId  = 9769222902479511526
instance (Std_.Enum Operator) where
    toEnum n_ = case n_ of
        0 ->
            Operator'add
        1 ->
            Operator'subtract
        2 ->
            Operator'multiply
        3 ->
            Operator'divide
        tag_ ->
            (Operator'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (Operator'add) ->
            0
        (Operator'subtract) ->
            1
        (Operator'multiply) ->
            2
        (Operator'divide) ->
            3
        (Operator'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord Operator) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse Operator Operator) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList Operator) where
    type ListAllocHint Operator = Std_.Int
instance (C.EstimateListAlloc Operator Operator)