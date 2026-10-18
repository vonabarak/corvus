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
module Capnp.Gen.Generics where
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
data Maybe t
type instance (R.ReprFor (Maybe t)) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId (Maybe t)) where
    typeId  = 12834113532530355529
instance ((GH.TypeParam t)) => (C.TypedStruct (Maybe t)) where
    numStructWords  = 1
    numStructPtrs  = 1
instance ((GH.TypeParam t)) => (C.Allocate (Maybe t)) where
    type AllocHint (Maybe t) = ()
    new _ = C.newTypedStruct
instance ((GH.TypeParam t)) => (C.EstimateAlloc (Maybe t) (C.Parsed (Maybe t)))
instance ((GH.TypeParam t)) => (C.AllocateList (Maybe t)) where
    type ListAllocHint (Maybe t) = Std_.Int
    newList  = C.newTypedStructList
instance ((GH.TypeParam t)) => (C.EstimateListAlloc (Maybe t) (C.Parsed (Maybe t)))
data instance C.Parsed (Maybe t)
    = Maybe 
        {union' :: (C.Parsed (GH.Which (Maybe t)))}
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed t))) => (Std_.Show (C.Parsed (Maybe t)))
deriving instance ((Std_.Eq (RP.Parsed t))) => (Std_.Eq (C.Parsed (Maybe t)))
instance ((GH.TypeParam t)) => (C.Parse (Maybe t) (C.Parsed (Maybe t))) where
    parse raw_ = (Maybe <$> (C.parse (GH.structUnion raw_)))
instance ((GH.TypeParam t)) => (C.Marshal (Maybe t) (C.Parsed (Maybe t))) where
    marshalInto raw_ Maybe{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance ((GH.TypeParam t)) => (GH.HasUnion (Maybe t)) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich (Maybe t) mut_
        = RW_Maybe'nothing (R.Raw () mut_)
        | RW_Maybe'just (R.Raw t mut_)
        | RW_Maybe'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Maybe'nothing <$> (GH.readVariant #nothing struct_))
        1 ->
            (RW_Maybe'just <$> (GH.readVariant #just struct_))
        _ ->
            (Std_.pure (RW_Maybe'unknown' tag_))
    data Which (Maybe t)
instance ((GH.TypeParam t)) => (GH.HasVariant "nothing" GH.Slot (Maybe t) ()) where
    variantByLabel  = (GH.Variant GH.voidField 0)
instance ((GH.TypeParam t)) => (GH.HasVariant "just" GH.Slot (Maybe t) t) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 1)
data instance C.Parsed (GH.Which (Maybe t))
    = Maybe'nothing 
    | Maybe'just (RP.Parsed t)
    | Maybe'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed t))) => (Std_.Show (C.Parsed (GH.Which (Maybe t))))
deriving instance ((Std_.Eq (RP.Parsed t))) => (Std_.Eq (C.Parsed (GH.Which (Maybe t))))
instance ((GH.TypeParam t)) => (C.Parse (GH.Which (Maybe t)) (C.Parsed (GH.Which (Maybe t)))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Maybe'nothing _) ->
                (Std_.pure Maybe'nothing)
            (RW_Maybe'just rawArg_) ->
                (Maybe'just <$> (C.parse rawArg_))
            (RW_Maybe'unknown' tag_) ->
                (Std_.pure (Maybe'unknown' tag_))
        )
instance ((GH.TypeParam t)) => (C.Marshal (GH.Which (Maybe t)) (C.Parsed (GH.Which (Maybe t)))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Maybe'nothing) ->
            (GH.encodeVariant #nothing () (GH.unionStruct raw_))
        (Maybe'just arg_) ->
            (GH.encodeVariant #just arg_ (GH.unionStruct raw_))
        (Maybe'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data Either a b
type instance (R.ReprFor (Either a b)) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId (Either a b)) where
    typeId  = 16016919828037144358
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (C.TypedStruct (Either a b)) where
    numStructWords  = 1
    numStructPtrs  = 1
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (C.Allocate (Either a b)) where
    type AllocHint (Either a b) = ()
    new _ = C.newTypedStruct
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (C.EstimateAlloc (Either a b) (C.Parsed (Either a b)))
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (C.AllocateList (Either a b)) where
    type ListAllocHint (Either a b) = Std_.Int
    newList  = C.newTypedStructList
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (C.EstimateListAlloc (Either a b) (C.Parsed (Either a b)))
data instance C.Parsed (Either a b)
    = Either 
        {union' :: (C.Parsed (GH.Which (Either a b)))}
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed a))
                  ,(Std_.Show (RP.Parsed b))) => (Std_.Show (C.Parsed (Either a b)))
deriving instance ((Std_.Eq (RP.Parsed a))
                  ,(Std_.Eq (RP.Parsed b))) => (Std_.Eq (C.Parsed (Either a b)))
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (C.Parse (Either a b) (C.Parsed (Either a b))) where
    parse raw_ = (Either <$> (C.parse (GH.structUnion raw_)))
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (C.Marshal (Either a b) (C.Parsed (Either a b))) where
    marshalInto raw_ Either{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (GH.HasUnion (Either a b)) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich (Either a b) mut_
        = RW_Either'left (R.Raw a mut_)
        | RW_Either'right (R.Raw b mut_)
        | RW_Either'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Either'left <$> (GH.readVariant #left struct_))
        1 ->
            (RW_Either'right <$> (GH.readVariant #right struct_))
        _ ->
            (Std_.pure (RW_Either'unknown' tag_))
    data Which (Either a b)
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (GH.HasVariant "left" GH.Slot (Either a b) a) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 0)
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (GH.HasVariant "right" GH.Slot (Either a b) b) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 1)
data instance C.Parsed (GH.Which (Either a b))
    = Either'left (RP.Parsed a)
    | Either'right (RP.Parsed b)
    | Either'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed a))
                  ,(Std_.Show (RP.Parsed b))) => (Std_.Show (C.Parsed (GH.Which (Either a b))))
deriving instance ((Std_.Eq (RP.Parsed a))
                  ,(Std_.Eq (RP.Parsed b))) => (Std_.Eq (C.Parsed (GH.Which (Either a b))))
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (C.Parse (GH.Which (Either a b)) (C.Parsed (GH.Which (Either a b)))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Either'left rawArg_) ->
                (Either'left <$> (C.parse rawArg_))
            (RW_Either'right rawArg_) ->
                (Either'right <$> (C.parse rawArg_))
            (RW_Either'unknown' tag_) ->
                (Std_.pure (Either'unknown' tag_))
        )
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (C.Marshal (GH.Which (Either a b)) (C.Parsed (GH.Which (Either a b)))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Either'left arg_) ->
            (GH.encodeVariant #left arg_ (GH.unionStruct raw_))
        (Either'right arg_) ->
            (GH.encodeVariant #right arg_ (GH.unionStruct raw_))
        (Either'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data Pair a b
type instance (R.ReprFor (Pair a b)) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId (Pair a b)) where
    typeId  = 18425972019411955385
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (C.TypedStruct (Pair a b)) where
    numStructWords  = 0
    numStructPtrs  = 2
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (C.Allocate (Pair a b)) where
    type AllocHint (Pair a b) = ()
    new _ = C.newTypedStruct
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (C.EstimateAlloc (Pair a b) (C.Parsed (Pair a b)))
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (C.AllocateList (Pair a b)) where
    type ListAllocHint (Pair a b) = Std_.Int
    newList  = C.newTypedStructList
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (C.EstimateListAlloc (Pair a b) (C.Parsed (Pair a b)))
data instance C.Parsed (Pair a b)
    = Pair 
        {fst :: (RP.Parsed a)
        ,snd :: (RP.Parsed b)}
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed a))
                  ,(Std_.Show (RP.Parsed b))) => (Std_.Show (C.Parsed (Pair a b)))
deriving instance ((Std_.Eq (RP.Parsed a))
                  ,(Std_.Eq (RP.Parsed b))) => (Std_.Eq (C.Parsed (Pair a b)))
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (C.Parse (Pair a b) (C.Parsed (Pair a b))) where
    parse raw_ = (Pair <$> (GH.parseField #fst raw_)
                       <*> (GH.parseField #snd raw_))
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (C.Marshal (Pair a b) (C.Parsed (Pair a b))) where
    marshalInto raw_ Pair{..} = (do
        (GH.encodeField #fst fst raw_)
        (GH.encodeField #snd snd raw_)
        (Std_.pure ())
        )
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (GH.HasField "fst" GH.Slot (Pair a b) a) where
    fieldByLabel  = (GH.ptrField 0)
instance ((GH.TypeParam a)
         ,(GH.TypeParam b)) => (GH.HasField "snd" GH.Slot (Pair a b) b) where
    fieldByLabel  = (GH.ptrField 1)
data Nested t
type instance (R.ReprFor (Nested t)) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId (Nested t)) where
    typeId  = 17472990467641505320
instance ((GH.TypeParam t)) => (C.TypedStruct (Nested t)) where
    numStructWords  = 0
    numStructPtrs  = 0
instance ((GH.TypeParam t)) => (C.Allocate (Nested t)) where
    type AllocHint (Nested t) = ()
    new _ = C.newTypedStruct
instance ((GH.TypeParam t)) => (C.EstimateAlloc (Nested t) (C.Parsed (Nested t)))
instance ((GH.TypeParam t)) => (C.AllocateList (Nested t)) where
    type ListAllocHint (Nested t) = Std_.Int
    newList  = C.newTypedStructList
instance ((GH.TypeParam t)) => (C.EstimateListAlloc (Nested t) (C.Parsed (Nested t)))
data instance C.Parsed (Nested t)
    = Nested 
        {}
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed t))) => (Std_.Show (C.Parsed (Nested t)))
deriving instance ((Std_.Eq (RP.Parsed t))) => (Std_.Eq (C.Parsed (Nested t)))
instance ((GH.TypeParam t)) => (C.Parse (Nested t) (C.Parsed (Nested t))) where
    parse raw_ = (Std_.pure Nested)
instance ((GH.TypeParam t)) => (C.Marshal (Nested t) (C.Parsed (Nested t))) where
    marshalInto _raw (Nested) = (Std_.pure ())
data Nested'SomeStruct t
type instance (R.ReprFor (Nested'SomeStruct t)) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId (Nested'SomeStruct t)) where
    typeId  = 10581732449597785397
instance ((GH.TypeParam t)) => (C.TypedStruct (Nested'SomeStruct t)) where
    numStructWords  = 0
    numStructPtrs  = 1
instance ((GH.TypeParam t)) => (C.Allocate (Nested'SomeStruct t)) where
    type AllocHint (Nested'SomeStruct t) = ()
    new _ = C.newTypedStruct
instance ((GH.TypeParam t)) => (C.EstimateAlloc (Nested'SomeStruct t) (C.Parsed (Nested'SomeStruct t)))
instance ((GH.TypeParam t)) => (C.AllocateList (Nested'SomeStruct t)) where
    type ListAllocHint (Nested'SomeStruct t) = Std_.Int
    newList  = C.newTypedStructList
instance ((GH.TypeParam t)) => (C.EstimateListAlloc (Nested'SomeStruct t) (C.Parsed (Nested'SomeStruct t)))
data instance C.Parsed (Nested'SomeStruct t)
    = Nested'SomeStruct 
        {value :: (RP.Parsed t)}
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed t))) => (Std_.Show (C.Parsed (Nested'SomeStruct t)))
deriving instance ((Std_.Eq (RP.Parsed t))) => (Std_.Eq (C.Parsed (Nested'SomeStruct t)))
instance ((GH.TypeParam t)) => (C.Parse (Nested'SomeStruct t) (C.Parsed (Nested'SomeStruct t))) where
    parse raw_ = (Nested'SomeStruct <$> (GH.parseField #value raw_))
instance ((GH.TypeParam t)) => (C.Marshal (Nested'SomeStruct t) (C.Parsed (Nested'SomeStruct t))) where
    marshalInto raw_ Nested'SomeStruct{..} = (do
        (GH.encodeField #value value raw_)
        (Std_.pure ())
        )
instance ((GH.TypeParam t)) => (GH.HasField "value" GH.Slot (Nested'SomeStruct t) t) where
    fieldByLabel  = (GH.ptrField 0)
data Nested'SomeInterface t
type instance (R.ReprFor (Nested'SomeInterface t)) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId (Nested'SomeInterface t)) where
    typeId  = 17400383877992806407
instance ((GH.TypeParam t)) => (C.Parse (Nested'SomeInterface t) (GH.Client (Nested'SomeInterface t))) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance ((GH.TypeParam t)) => (GH.Export (Nested'SomeInterface t)) where
    type Server (Nested'SomeInterface t) = (Nested'SomeInterface'server_ t)
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @((Nested'SomeInterface t))) [(GH.toUntypedMethodHandler ((nested'SomeInterface'method @(t)) s_))] [])
class (Nested'SomeInterface'server_ t s_) where
    {-# MINIMAL nested'SomeInterface'method #-}
    nested'SomeInterface'method :: s_ -> (GH.MethodHandler (Nested'SomeInterface'method'params t) (Nested'SomeInterface'method'results t))
    nested'SomeInterface'method _ = GH.methodUnimplemented
instance ((GH.TypeParam t)) => (GH.HasMethod "method" (Nested'SomeInterface t) (Nested'SomeInterface'method'params t) (Nested'SomeInterface'method'results t)) where
    methodByLabel  = (GH.Method 17400383877992806407 0)
data Nested'SomeInterface'method'params t
type instance (R.ReprFor (Nested'SomeInterface'method'params t)) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId (Nested'SomeInterface'method'params t)) where
    typeId  = 13278839458779198088
instance ((GH.TypeParam t)) => (C.TypedStruct (Nested'SomeInterface'method'params t)) where
    numStructWords  = 0
    numStructPtrs  = 1
instance ((GH.TypeParam t)) => (C.Allocate (Nested'SomeInterface'method'params t)) where
    type AllocHint (Nested'SomeInterface'method'params t) = ()
    new _ = C.newTypedStruct
instance ((GH.TypeParam t)) => (C.EstimateAlloc (Nested'SomeInterface'method'params t) (C.Parsed (Nested'SomeInterface'method'params t)))
instance ((GH.TypeParam t)) => (C.AllocateList (Nested'SomeInterface'method'params t)) where
    type ListAllocHint (Nested'SomeInterface'method'params t) = Std_.Int
    newList  = C.newTypedStructList
instance ((GH.TypeParam t)) => (C.EstimateListAlloc (Nested'SomeInterface'method'params t) (C.Parsed (Nested'SomeInterface'method'params t)))
data instance C.Parsed (Nested'SomeInterface'method'params t)
    = Nested'SomeInterface'method'params 
        {arg :: (RP.Parsed t)}
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed t))) => (Std_.Show (C.Parsed (Nested'SomeInterface'method'params t)))
deriving instance ((Std_.Eq (RP.Parsed t))) => (Std_.Eq (C.Parsed (Nested'SomeInterface'method'params t)))
instance ((GH.TypeParam t)) => (C.Parse (Nested'SomeInterface'method'params t) (C.Parsed (Nested'SomeInterface'method'params t))) where
    parse raw_ = (Nested'SomeInterface'method'params <$> (GH.parseField #arg raw_))
instance ((GH.TypeParam t)) => (C.Marshal (Nested'SomeInterface'method'params t) (C.Parsed (Nested'SomeInterface'method'params t))) where
    marshalInto raw_ Nested'SomeInterface'method'params{..} = (do
        (GH.encodeField #arg arg raw_)
        (Std_.pure ())
        )
instance ((GH.TypeParam t)) => (GH.HasField "arg" GH.Slot (Nested'SomeInterface'method'params t) t) where
    fieldByLabel  = (GH.ptrField 0)
data Nested'SomeInterface'method'results t
type instance (R.ReprFor (Nested'SomeInterface'method'results t)) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId (Nested'SomeInterface'method'results t)) where
    typeId  = 9305012847082487733
instance ((GH.TypeParam t)) => (C.TypedStruct (Nested'SomeInterface'method'results t)) where
    numStructWords  = 0
    numStructPtrs  = 1
instance ((GH.TypeParam t)) => (C.Allocate (Nested'SomeInterface'method'results t)) where
    type AllocHint (Nested'SomeInterface'method'results t) = ()
    new _ = C.newTypedStruct
instance ((GH.TypeParam t)) => (C.EstimateAlloc (Nested'SomeInterface'method'results t) (C.Parsed (Nested'SomeInterface'method'results t)))
instance ((GH.TypeParam t)) => (C.AllocateList (Nested'SomeInterface'method'results t)) where
    type ListAllocHint (Nested'SomeInterface'method'results t) = Std_.Int
    newList  = C.newTypedStructList
instance ((GH.TypeParam t)) => (C.EstimateListAlloc (Nested'SomeInterface'method'results t) (C.Parsed (Nested'SomeInterface'method'results t)))
data instance C.Parsed (Nested'SomeInterface'method'results t)
    = Nested'SomeInterface'method'results 
        {result :: (RP.Parsed t)}
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed t))) => (Std_.Show (C.Parsed (Nested'SomeInterface'method'results t)))
deriving instance ((Std_.Eq (RP.Parsed t))) => (Std_.Eq (C.Parsed (Nested'SomeInterface'method'results t)))
instance ((GH.TypeParam t)) => (C.Parse (Nested'SomeInterface'method'results t) (C.Parsed (Nested'SomeInterface'method'results t))) where
    parse raw_ = (Nested'SomeInterface'method'results <$> (GH.parseField #result raw_))
instance ((GH.TypeParam t)) => (C.Marshal (Nested'SomeInterface'method'results t) (C.Parsed (Nested'SomeInterface'method'results t))) where
    marshalInto raw_ Nested'SomeInterface'method'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance ((GH.TypeParam t)) => (GH.HasField "result" GH.Slot (Nested'SomeInterface'method'results t) t) where
    fieldByLabel  = (GH.ptrField 0)
data Specialized t
type instance (R.ReprFor (Specialized t)) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId (Specialized t)) where
    typeId  = 11111346635683596512
instance ((GH.TypeParam t)) => (C.TypedStruct (Specialized t)) where
    numStructWords  = 0
    numStructPtrs  = 2
instance ((GH.TypeParam t)) => (C.Allocate (Specialized t)) where
    type AllocHint (Specialized t) = ()
    new _ = C.newTypedStruct
instance ((GH.TypeParam t)) => (C.EstimateAlloc (Specialized t) (C.Parsed (Specialized t)))
instance ((GH.TypeParam t)) => (C.AllocateList (Specialized t)) where
    type ListAllocHint (Specialized t) = Std_.Int
    newList  = C.newTypedStructList
instance ((GH.TypeParam t)) => (C.EstimateListAlloc (Specialized t) (C.Parsed (Specialized t)))
data instance C.Parsed (Specialized t)
    = Specialized 
        {either :: (RP.Parsed (Either Basics.Text t))
        ,nestedStruct :: (RP.Parsed (Nested'SomeStruct Basics.Data))}
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed t))) => (Std_.Show (C.Parsed (Specialized t)))
deriving instance ((Std_.Eq (RP.Parsed t))) => (Std_.Eq (C.Parsed (Specialized t)))
instance ((GH.TypeParam t)) => (C.Parse (Specialized t) (C.Parsed (Specialized t))) where
    parse raw_ = (Specialized <$> (GH.parseField #either raw_)
                              <*> (GH.parseField #nestedStruct raw_))
instance ((GH.TypeParam t)) => (C.Marshal (Specialized t) (C.Parsed (Specialized t))) where
    marshalInto raw_ Specialized{..} = (do
        (GH.encodeField #either either raw_)
        (GH.encodeField #nestedStruct nestedStruct raw_)
        (Std_.pure ())
        )
instance ((GH.TypeParam t)) => (GH.HasField "either" GH.Slot (Specialized t) (Either Basics.Text t)) where
    fieldByLabel  = (GH.ptrField 0)
instance ((GH.TypeParam t)) => (GH.HasField "nestedStruct" GH.Slot (Specialized t) (Nested'SomeStruct Basics.Data)) where
    fieldByLabel  = (GH.ptrField 1)
data HasGroup t
type instance (R.ReprFor (HasGroup t)) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId (HasGroup t)) where
    typeId  = 10809380793126616697
instance ((GH.TypeParam t)) => (C.TypedStruct (HasGroup t)) where
    numStructWords  = 0
    numStructPtrs  = 1
instance ((GH.TypeParam t)) => (C.Allocate (HasGroup t)) where
    type AllocHint (HasGroup t) = ()
    new _ = C.newTypedStruct
instance ((GH.TypeParam t)) => (C.EstimateAlloc (HasGroup t) (C.Parsed (HasGroup t)))
instance ((GH.TypeParam t)) => (C.AllocateList (HasGroup t)) where
    type ListAllocHint (HasGroup t) = Std_.Int
    newList  = C.newTypedStructList
instance ((GH.TypeParam t)) => (C.EstimateListAlloc (HasGroup t) (C.Parsed (HasGroup t)))
data instance C.Parsed (HasGroup t)
    = HasGroup 
        {theGroup :: (RP.Parsed (HasGroup'theGroup t))}
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed t))) => (Std_.Show (C.Parsed (HasGroup t)))
deriving instance ((Std_.Eq (RP.Parsed t))) => (Std_.Eq (C.Parsed (HasGroup t)))
instance ((GH.TypeParam t)) => (C.Parse (HasGroup t) (C.Parsed (HasGroup t))) where
    parse raw_ = (HasGroup <$> (GH.parseField #theGroup raw_))
instance ((GH.TypeParam t)) => (C.Marshal (HasGroup t) (C.Parsed (HasGroup t))) where
    marshalInto raw_ HasGroup{..} = (do
        (do
            group_ <- (GH.readField #theGroup raw_)
            (C.marshalInto group_ theGroup)
            )
        (Std_.pure ())
        )
instance ((GH.TypeParam t)) => (GH.HasField "theGroup" GH.Group (HasGroup t) (HasGroup'theGroup t)) where
    fieldByLabel  = GH.groupField
data HasGroup'theGroup t
type instance (R.ReprFor (HasGroup'theGroup t)) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId (HasGroup'theGroup t)) where
    typeId  = 13022770763187353254
instance ((GH.TypeParam t)) => (C.TypedStruct (HasGroup'theGroup t)) where
    numStructWords  = 0
    numStructPtrs  = 1
instance ((GH.TypeParam t)) => (C.Allocate (HasGroup'theGroup t)) where
    type AllocHint (HasGroup'theGroup t) = ()
    new _ = C.newTypedStruct
instance ((GH.TypeParam t)) => (C.EstimateAlloc (HasGroup'theGroup t) (C.Parsed (HasGroup'theGroup t)))
instance ((GH.TypeParam t)) => (C.AllocateList (HasGroup'theGroup t)) where
    type ListAllocHint (HasGroup'theGroup t) = Std_.Int
    newList  = C.newTypedStructList
instance ((GH.TypeParam t)) => (C.EstimateListAlloc (HasGroup'theGroup t) (C.Parsed (HasGroup'theGroup t)))
data instance C.Parsed (HasGroup'theGroup t)
    = HasGroup'theGroup' 
        {value :: (RP.Parsed t)}
    deriving(Generics.Generic)
deriving instance ((Std_.Show (RP.Parsed t))) => (Std_.Show (C.Parsed (HasGroup'theGroup t)))
deriving instance ((Std_.Eq (RP.Parsed t))) => (Std_.Eq (C.Parsed (HasGroup'theGroup t)))
instance ((GH.TypeParam t)) => (C.Parse (HasGroup'theGroup t) (C.Parsed (HasGroup'theGroup t))) where
    parse raw_ = (HasGroup'theGroup' <$> (GH.parseField #value raw_))
instance ((GH.TypeParam t)) => (C.Marshal (HasGroup'theGroup t) (C.Parsed (HasGroup'theGroup t))) where
    marshalInto raw_ HasGroup'theGroup'{..} = (do
        (GH.encodeField #value value raw_)
        (Std_.pure ())
        )
instance ((GH.TypeParam t)) => (GH.HasField "value" GH.Slot (HasGroup'theGroup t) t) where
    fieldByLabel  = (GH.ptrField 0)