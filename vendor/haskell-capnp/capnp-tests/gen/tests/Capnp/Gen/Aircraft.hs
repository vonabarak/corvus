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
module Capnp.Gen.Aircraft where
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
constDate :: (R.Raw Zdate GH.Const)
constDate  = (GH.getPtrConst ("\NUL\NUL\NUL\NUL\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\223\a\b\ESC\NUL\NUL\NUL\NUL" :: GH.ByteString))
constList :: (R.Raw (R.List Zdate) GH.Const)
constList  = (GH.getPtrConst ("\NUL\NUL\NUL\NUL\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NUL\SOH\NUL\NUL\NUL\ETB\NUL\NUL\NUL\b\NUL\NUL\NUL\SOH\NUL\NUL\NUL\223\a\b\ESC\NUL\NUL\NUL\NUL\223\a\b\FS\NUL\NUL\NUL\NUL" :: GH.ByteString))
constEnum :: Airport
constEnum  = (C.fromWord 1)
data Zdate 
type instance (R.ReprFor Zdate) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Zdate) where
    typeId  = 16019495995647153309
instance (C.TypedStruct Zdate) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Zdate) where
    type AllocHint Zdate = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Zdate (C.Parsed Zdate))
instance (C.AllocateList Zdate) where
    type ListAllocHint Zdate = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Zdate (C.Parsed Zdate))
data instance C.Parsed Zdate
    = Zdate 
        {year :: (RP.Parsed Std_.Int16)
        ,month :: (RP.Parsed Std_.Word8)
        ,day :: (RP.Parsed Std_.Word8)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Zdate))
deriving instance (Std_.Eq (C.Parsed Zdate))
instance (C.Parse Zdate (C.Parsed Zdate)) where
    parse raw_ = (Zdate <$> (GH.parseField #year raw_)
                        <*> (GH.parseField #month raw_)
                        <*> (GH.parseField #day raw_))
instance (C.Marshal Zdate (C.Parsed Zdate)) where
    marshalInto raw_ Zdate{..} = (do
        (GH.encodeField #year year raw_)
        (GH.encodeField #month month raw_)
        (GH.encodeField #day day raw_)
        (Std_.pure ())
        )
instance (GH.HasField "year" GH.Slot Zdate Std_.Int16) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "month" GH.Slot Zdate Std_.Word8) where
    fieldByLabel  = (GH.dataField 16 0 8 0)
instance (GH.HasField "day" GH.Slot Zdate Std_.Word8) where
    fieldByLabel  = (GH.dataField 24 0 8 0)
data Zdata 
type instance (R.ReprFor Zdata) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Zdata) where
    typeId  = 14400934881325616034
instance (C.TypedStruct Zdata) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Zdata) where
    type AllocHint Zdata = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Zdata (C.Parsed Zdata))
instance (C.AllocateList Zdata) where
    type ListAllocHint Zdata = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Zdata (C.Parsed Zdata))
data instance C.Parsed Zdata
    = Zdata 
        {data_ :: (RP.Parsed Basics.Data)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Zdata))
deriving instance (Std_.Eq (C.Parsed Zdata))
instance (C.Parse Zdata (C.Parsed Zdata)) where
    parse raw_ = (Zdata <$> (GH.parseField #data_ raw_))
instance (C.Marshal Zdata (C.Parsed Zdata)) where
    marshalInto raw_ Zdata{..} = (do
        (GH.encodeField #data_ data_ raw_)
        (Std_.pure ())
        )
instance (GH.HasField "data_" GH.Slot Zdata Basics.Data) where
    fieldByLabel  = (GH.ptrField 0)
data Airport 
    = Airport'none 
    | Airport'jfk 
    | Airport'lax 
    | Airport'sfo 
    | Airport'luv 
    | Airport'dfw 
    | Airport'test 
    | Airport'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor Airport) = (R.Data R.Sz16)
instance (C.HasTypeId Airport) where
    typeId  = 16527513525367090977
instance (Std_.Enum Airport) where
    toEnum n_ = case n_ of
        0 ->
            Airport'none
        1 ->
            Airport'jfk
        2 ->
            Airport'lax
        3 ->
            Airport'sfo
        4 ->
            Airport'luv
        5 ->
            Airport'dfw
        6 ->
            Airport'test
        tag_ ->
            (Airport'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (Airport'none) ->
            0
        (Airport'jfk) ->
            1
        (Airport'lax) ->
            2
        (Airport'sfo) ->
            3
        (Airport'luv) ->
            4
        (Airport'dfw) ->
            5
        (Airport'test) ->
            6
        (Airport'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord Airport) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse Airport Airport) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList Airport) where
    type ListAllocHint Airport = Std_.Int
instance (C.EstimateListAlloc Airport Airport)
data PlaneBase 
type instance (R.ReprFor PlaneBase) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId PlaneBase) where
    typeId  = 15617585680788961169
instance (C.TypedStruct PlaneBase) where
    numStructWords  = 4
    numStructPtrs  = 2
instance (C.Allocate PlaneBase) where
    type AllocHint PlaneBase = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc PlaneBase (C.Parsed PlaneBase))
instance (C.AllocateList PlaneBase) where
    type ListAllocHint PlaneBase = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc PlaneBase (C.Parsed PlaneBase))
data instance C.Parsed PlaneBase
    = PlaneBase 
        {name :: (RP.Parsed Basics.Text)
        ,homes :: (RP.Parsed (R.List Airport))
        ,rating :: (RP.Parsed Std_.Int64)
        ,canFly :: (RP.Parsed Std_.Bool)
        ,capacity :: (RP.Parsed Std_.Int64)
        ,maxSpeed :: (RP.Parsed Std_.Double)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed PlaneBase))
deriving instance (Std_.Eq (C.Parsed PlaneBase))
instance (C.Parse PlaneBase (C.Parsed PlaneBase)) where
    parse raw_ = (PlaneBase <$> (GH.parseField #name raw_)
                            <*> (GH.parseField #homes raw_)
                            <*> (GH.parseField #rating raw_)
                            <*> (GH.parseField #canFly raw_)
                            <*> (GH.parseField #capacity raw_)
                            <*> (GH.parseField #maxSpeed raw_))
instance (C.Marshal PlaneBase (C.Parsed PlaneBase)) where
    marshalInto raw_ PlaneBase{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #homes homes raw_)
        (GH.encodeField #rating rating raw_)
        (GH.encodeField #canFly canFly raw_)
        (GH.encodeField #capacity capacity raw_)
        (GH.encodeField #maxSpeed maxSpeed raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot PlaneBase Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "homes" GH.Slot PlaneBase (R.List Airport)) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "rating" GH.Slot PlaneBase Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "canFly" GH.Slot PlaneBase Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 1 1 0)
instance (GH.HasField "capacity" GH.Slot PlaneBase Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
instance (GH.HasField "maxSpeed" GH.Slot PlaneBase Std_.Double) where
    fieldByLabel  = (GH.dataField 0 3 64 0)
data B737 
type instance (R.ReprFor B737) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId B737) where
    typeId  = 14750329894210119392
instance (C.TypedStruct B737) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate B737) where
    type AllocHint B737 = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc B737 (C.Parsed B737))
instance (C.AllocateList B737) where
    type ListAllocHint B737 = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc B737 (C.Parsed B737))
data instance C.Parsed B737
    = B737 
        {base :: (RP.Parsed PlaneBase)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed B737))
deriving instance (Std_.Eq (C.Parsed B737))
instance (C.Parse B737 (C.Parsed B737)) where
    parse raw_ = (B737 <$> (GH.parseField #base raw_))
instance (C.Marshal B737 (C.Parsed B737)) where
    marshalInto raw_ B737{..} = (do
        (GH.encodeField #base base raw_)
        (Std_.pure ())
        )
instance (GH.HasField "base" GH.Slot B737 PlaneBase) where
    fieldByLabel  = (GH.ptrField 0)
data A320 
type instance (R.ReprFor A320) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId A320) where
    typeId  = 15676010542212434829
instance (C.TypedStruct A320) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate A320) where
    type AllocHint A320 = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc A320 (C.Parsed A320))
instance (C.AllocateList A320) where
    type ListAllocHint A320 = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc A320 (C.Parsed A320))
data instance C.Parsed A320
    = A320 
        {base :: (RP.Parsed PlaneBase)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed A320))
deriving instance (Std_.Eq (C.Parsed A320))
instance (C.Parse A320 (C.Parsed A320)) where
    parse raw_ = (A320 <$> (GH.parseField #base raw_))
instance (C.Marshal A320 (C.Parsed A320)) where
    marshalInto raw_ A320{..} = (do
        (GH.encodeField #base base raw_)
        (Std_.pure ())
        )
instance (GH.HasField "base" GH.Slot A320 PlaneBase) where
    fieldByLabel  = (GH.ptrField 0)
data F16 
type instance (R.ReprFor F16) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId F16) where
    typeId  = 16269793260987437921
instance (C.TypedStruct F16) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate F16) where
    type AllocHint F16 = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc F16 (C.Parsed F16))
instance (C.AllocateList F16) where
    type ListAllocHint F16 = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc F16 (C.Parsed F16))
data instance C.Parsed F16
    = F16 
        {base :: (RP.Parsed PlaneBase)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed F16))
deriving instance (Std_.Eq (C.Parsed F16))
instance (C.Parse F16 (C.Parsed F16)) where
    parse raw_ = (F16 <$> (GH.parseField #base raw_))
instance (C.Marshal F16 (C.Parsed F16)) where
    marshalInto raw_ F16{..} = (do
        (GH.encodeField #base base raw_)
        (Std_.pure ())
        )
instance (GH.HasField "base" GH.Slot F16 PlaneBase) where
    fieldByLabel  = (GH.ptrField 0)
data Regression 
type instance (R.ReprFor Regression) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Regression) where
    typeId  = 12821810113427682943
instance (C.TypedStruct Regression) where
    numStructWords  = 3
    numStructPtrs  = 3
instance (C.Allocate Regression) where
    type AllocHint Regression = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Regression (C.Parsed Regression))
instance (C.AllocateList Regression) where
    type ListAllocHint Regression = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Regression (C.Parsed Regression))
data instance C.Parsed Regression
    = Regression 
        {base :: (RP.Parsed PlaneBase)
        ,b0 :: (RP.Parsed Std_.Double)
        ,beta :: (RP.Parsed (R.List Std_.Double))
        ,planes :: (RP.Parsed (R.List Aircraft))
        ,ymu :: (RP.Parsed Std_.Double)
        ,ysd :: (RP.Parsed Std_.Double)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Regression))
deriving instance (Std_.Eq (C.Parsed Regression))
instance (C.Parse Regression (C.Parsed Regression)) where
    parse raw_ = (Regression <$> (GH.parseField #base raw_)
                             <*> (GH.parseField #b0 raw_)
                             <*> (GH.parseField #beta raw_)
                             <*> (GH.parseField #planes raw_)
                             <*> (GH.parseField #ymu raw_)
                             <*> (GH.parseField #ysd raw_))
instance (C.Marshal Regression (C.Parsed Regression)) where
    marshalInto raw_ Regression{..} = (do
        (GH.encodeField #base base raw_)
        (GH.encodeField #b0 b0 raw_)
        (GH.encodeField #beta beta raw_)
        (GH.encodeField #planes planes raw_)
        (GH.encodeField #ymu ymu raw_)
        (GH.encodeField #ysd ysd raw_)
        (Std_.pure ())
        )
instance (GH.HasField "base" GH.Slot Regression PlaneBase) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "b0" GH.Slot Regression Std_.Double) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "beta" GH.Slot Regression (R.List Std_.Double)) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "planes" GH.Slot Regression (R.List Aircraft)) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "ymu" GH.Slot Regression Std_.Double) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "ysd" GH.Slot Regression Std_.Double) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
data Aircraft 
type instance (R.ReprFor Aircraft) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Aircraft) where
    typeId  = 16523162426109446065
instance (C.TypedStruct Aircraft) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate Aircraft) where
    type AllocHint Aircraft = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Aircraft (C.Parsed Aircraft))
instance (C.AllocateList Aircraft) where
    type ListAllocHint Aircraft = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Aircraft (C.Parsed Aircraft))
data instance C.Parsed Aircraft
    = Aircraft 
        {union' :: (C.Parsed (GH.Which Aircraft))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Aircraft))
deriving instance (Std_.Eq (C.Parsed Aircraft))
instance (C.Parse Aircraft (C.Parsed Aircraft)) where
    parse raw_ = (Aircraft <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Aircraft (C.Parsed Aircraft)) where
    marshalInto raw_ Aircraft{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Aircraft) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich Aircraft mut_
        = RW_Aircraft'void (R.Raw () mut_)
        | RW_Aircraft'b737 (R.Raw B737 mut_)
        | RW_Aircraft'a320 (R.Raw A320 mut_)
        | RW_Aircraft'f16 (R.Raw F16 mut_)
        | RW_Aircraft'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Aircraft'void <$> (GH.readVariant #void struct_))
        1 ->
            (RW_Aircraft'b737 <$> (GH.readVariant #b737 struct_))
        2 ->
            (RW_Aircraft'a320 <$> (GH.readVariant #a320 struct_))
        3 ->
            (RW_Aircraft'f16 <$> (GH.readVariant #f16 struct_))
        _ ->
            (Std_.pure (RW_Aircraft'unknown' tag_))
    data Which Aircraft
instance (GH.HasVariant "void" GH.Slot Aircraft ()) where
    variantByLabel  = (GH.Variant GH.voidField 0)
instance (GH.HasVariant "b737" GH.Slot Aircraft B737) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 1)
instance (GH.HasVariant "a320" GH.Slot Aircraft A320) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 2)
instance (GH.HasVariant "f16" GH.Slot Aircraft F16) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 3)
data instance C.Parsed (GH.Which Aircraft)
    = Aircraft'void 
    | Aircraft'b737 (RP.Parsed B737)
    | Aircraft'a320 (RP.Parsed A320)
    | Aircraft'f16 (RP.Parsed F16)
    | Aircraft'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Aircraft)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Aircraft)))
instance (C.Parse (GH.Which Aircraft) (C.Parsed (GH.Which Aircraft))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Aircraft'void _) ->
                (Std_.pure Aircraft'void)
            (RW_Aircraft'b737 rawArg_) ->
                (Aircraft'b737 <$> (C.parse rawArg_))
            (RW_Aircraft'a320 rawArg_) ->
                (Aircraft'a320 <$> (C.parse rawArg_))
            (RW_Aircraft'f16 rawArg_) ->
                (Aircraft'f16 <$> (C.parse rawArg_))
            (RW_Aircraft'unknown' tag_) ->
                (Std_.pure (Aircraft'unknown' tag_))
        )
instance (C.Marshal (GH.Which Aircraft) (C.Parsed (GH.Which Aircraft))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Aircraft'void) ->
            (GH.encodeVariant #void () (GH.unionStruct raw_))
        (Aircraft'b737 arg_) ->
            (GH.encodeVariant #b737 arg_ (GH.unionStruct raw_))
        (Aircraft'a320 arg_) ->
            (GH.encodeVariant #a320 arg_ (GH.unionStruct raw_))
        (Aircraft'f16 arg_) ->
            (GH.encodeVariant #f16 arg_ (GH.unionStruct raw_))
        (Aircraft'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data Z 
type instance (R.ReprFor Z) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Z) where
    typeId  = 16872429889743397081
instance (C.TypedStruct Z) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Z) where
    type AllocHint Z = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Z (C.Parsed Z))
instance (C.AllocateList Z) where
    type ListAllocHint Z = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Z (C.Parsed Z))
data instance C.Parsed Z
    = Z 
        {union' :: (C.Parsed (GH.Which Z))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Z))
deriving instance (Std_.Eq (C.Parsed Z))
instance (C.Parse Z (C.Parsed Z)) where
    parse raw_ = (Z <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal Z (C.Parsed Z)) where
    marshalInto raw_ Z{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion Z) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich Z mut_
        = RW_Z'void (R.Raw () mut_)
        | RW_Z'zz (R.Raw Z mut_)
        | RW_Z'f64 (R.Raw Std_.Double mut_)
        | RW_Z'f32 (R.Raw Std_.Float mut_)
        | RW_Z'i64 (R.Raw Std_.Int64 mut_)
        | RW_Z'i32 (R.Raw Std_.Int32 mut_)
        | RW_Z'i16 (R.Raw Std_.Int16 mut_)
        | RW_Z'i8 (R.Raw Std_.Int8 mut_)
        | RW_Z'u64 (R.Raw Std_.Word64 mut_)
        | RW_Z'u32 (R.Raw Std_.Word32 mut_)
        | RW_Z'u16 (R.Raw Std_.Word16 mut_)
        | RW_Z'u8 (R.Raw Std_.Word8 mut_)
        | RW_Z'bool (R.Raw Std_.Bool mut_)
        | RW_Z'text (R.Raw Basics.Text mut_)
        | RW_Z'blob (R.Raw Basics.Data mut_)
        | RW_Z'f64vec (R.Raw (R.List Std_.Double) mut_)
        | RW_Z'f32vec (R.Raw (R.List Std_.Float) mut_)
        | RW_Z'i64vec (R.Raw (R.List Std_.Int64) mut_)
        | RW_Z'i32vec (R.Raw (R.List Std_.Int32) mut_)
        | RW_Z'i16vec (R.Raw (R.List Std_.Int16) mut_)
        | RW_Z'i8vec (R.Raw (R.List Std_.Int8) mut_)
        | RW_Z'u64vec (R.Raw (R.List Std_.Word64) mut_)
        | RW_Z'u32vec (R.Raw (R.List Std_.Word32) mut_)
        | RW_Z'u16vec (R.Raw (R.List Std_.Word16) mut_)
        | RW_Z'u8vec (R.Raw (R.List Std_.Word8) mut_)
        | RW_Z'zvec (R.Raw (R.List Z) mut_)
        | RW_Z'zvecvec (R.Raw (R.List (R.List Z)) mut_)
        | RW_Z'zdate (R.Raw Zdate mut_)
        | RW_Z'zdata (R.Raw Zdata mut_)
        | RW_Z'aircraftvec (R.Raw (R.List Aircraft) mut_)
        | RW_Z'aircraft (R.Raw Aircraft mut_)
        | RW_Z'regression (R.Raw Regression mut_)
        | RW_Z'planebase (R.Raw PlaneBase mut_)
        | RW_Z'airport (R.Raw Airport mut_)
        | RW_Z'b737 (R.Raw B737 mut_)
        | RW_Z'a320 (R.Raw A320 mut_)
        | RW_Z'f16 (R.Raw F16 mut_)
        | RW_Z'zdatevec (R.Raw (R.List Zdate) mut_)
        | RW_Z'zdatavec (R.Raw (R.List Zdata) mut_)
        | RW_Z'boolvec (R.Raw (R.List Std_.Bool) mut_)
        | RW_Z'datavec (R.Raw (R.List Basics.Data) mut_)
        | RW_Z'textvec (R.Raw (R.List Basics.Text) mut_)
        | RW_Z'grp (R.Raw Z'grp mut_)
        | RW_Z'echo (R.Raw Echo mut_)
        | RW_Z'echoBases (R.Raw EchoBases mut_)
        | RW_Z'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_Z'void <$> (GH.readVariant #void struct_))
        1 ->
            (RW_Z'zz <$> (GH.readVariant #zz struct_))
        2 ->
            (RW_Z'f64 <$> (GH.readVariant #f64 struct_))
        3 ->
            (RW_Z'f32 <$> (GH.readVariant #f32 struct_))
        4 ->
            (RW_Z'i64 <$> (GH.readVariant #i64 struct_))
        5 ->
            (RW_Z'i32 <$> (GH.readVariant #i32 struct_))
        6 ->
            (RW_Z'i16 <$> (GH.readVariant #i16 struct_))
        7 ->
            (RW_Z'i8 <$> (GH.readVariant #i8 struct_))
        8 ->
            (RW_Z'u64 <$> (GH.readVariant #u64 struct_))
        9 ->
            (RW_Z'u32 <$> (GH.readVariant #u32 struct_))
        10 ->
            (RW_Z'u16 <$> (GH.readVariant #u16 struct_))
        11 ->
            (RW_Z'u8 <$> (GH.readVariant #u8 struct_))
        12 ->
            (RW_Z'bool <$> (GH.readVariant #bool struct_))
        13 ->
            (RW_Z'text <$> (GH.readVariant #text struct_))
        14 ->
            (RW_Z'blob <$> (GH.readVariant #blob struct_))
        15 ->
            (RW_Z'f64vec <$> (GH.readVariant #f64vec struct_))
        16 ->
            (RW_Z'f32vec <$> (GH.readVariant #f32vec struct_))
        17 ->
            (RW_Z'i64vec <$> (GH.readVariant #i64vec struct_))
        18 ->
            (RW_Z'i32vec <$> (GH.readVariant #i32vec struct_))
        19 ->
            (RW_Z'i16vec <$> (GH.readVariant #i16vec struct_))
        20 ->
            (RW_Z'i8vec <$> (GH.readVariant #i8vec struct_))
        21 ->
            (RW_Z'u64vec <$> (GH.readVariant #u64vec struct_))
        22 ->
            (RW_Z'u32vec <$> (GH.readVariant #u32vec struct_))
        23 ->
            (RW_Z'u16vec <$> (GH.readVariant #u16vec struct_))
        24 ->
            (RW_Z'u8vec <$> (GH.readVariant #u8vec struct_))
        25 ->
            (RW_Z'zvec <$> (GH.readVariant #zvec struct_))
        26 ->
            (RW_Z'zvecvec <$> (GH.readVariant #zvecvec struct_))
        27 ->
            (RW_Z'zdate <$> (GH.readVariant #zdate struct_))
        28 ->
            (RW_Z'zdata <$> (GH.readVariant #zdata struct_))
        29 ->
            (RW_Z'aircraftvec <$> (GH.readVariant #aircraftvec struct_))
        30 ->
            (RW_Z'aircraft <$> (GH.readVariant #aircraft struct_))
        31 ->
            (RW_Z'regression <$> (GH.readVariant #regression struct_))
        32 ->
            (RW_Z'planebase <$> (GH.readVariant #planebase struct_))
        33 ->
            (RW_Z'airport <$> (GH.readVariant #airport struct_))
        34 ->
            (RW_Z'b737 <$> (GH.readVariant #b737 struct_))
        35 ->
            (RW_Z'a320 <$> (GH.readVariant #a320 struct_))
        36 ->
            (RW_Z'f16 <$> (GH.readVariant #f16 struct_))
        37 ->
            (RW_Z'zdatevec <$> (GH.readVariant #zdatevec struct_))
        38 ->
            (RW_Z'zdatavec <$> (GH.readVariant #zdatavec struct_))
        39 ->
            (RW_Z'boolvec <$> (GH.readVariant #boolvec struct_))
        40 ->
            (RW_Z'datavec <$> (GH.readVariant #datavec struct_))
        41 ->
            (RW_Z'textvec <$> (GH.readVariant #textvec struct_))
        42 ->
            (RW_Z'grp <$> (GH.readVariant #grp struct_))
        43 ->
            (RW_Z'echo <$> (GH.readVariant #echo struct_))
        44 ->
            (RW_Z'echoBases <$> (GH.readVariant #echoBases struct_))
        _ ->
            (Std_.pure (RW_Z'unknown' tag_))
    data Which Z
instance (GH.HasVariant "void" GH.Slot Z ()) where
    variantByLabel  = (GH.Variant GH.voidField 0)
instance (GH.HasVariant "zz" GH.Slot Z Z) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 1)
instance (GH.HasVariant "f64" GH.Slot Z Std_.Double) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 64 0) 2)
instance (GH.HasVariant "f32" GH.Slot Z Std_.Float) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 32 0) 3)
instance (GH.HasVariant "i64" GH.Slot Z Std_.Int64) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 64 0) 4)
instance (GH.HasVariant "i32" GH.Slot Z Std_.Int32) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 32 0) 5)
instance (GH.HasVariant "i16" GH.Slot Z Std_.Int16) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 16 0) 6)
instance (GH.HasVariant "i8" GH.Slot Z Std_.Int8) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 8 0) 7)
instance (GH.HasVariant "u64" GH.Slot Z Std_.Word64) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 64 0) 8)
instance (GH.HasVariant "u32" GH.Slot Z Std_.Word32) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 32 0) 9)
instance (GH.HasVariant "u16" GH.Slot Z Std_.Word16) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 16 0) 10)
instance (GH.HasVariant "u8" GH.Slot Z Std_.Word8) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 8 0) 11)
instance (GH.HasVariant "bool" GH.Slot Z Std_.Bool) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 1 0) 12)
instance (GH.HasVariant "text" GH.Slot Z Basics.Text) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 13)
instance (GH.HasVariant "blob" GH.Slot Z Basics.Data) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 14)
instance (GH.HasVariant "f64vec" GH.Slot Z (R.List Std_.Double)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 15)
instance (GH.HasVariant "f32vec" GH.Slot Z (R.List Std_.Float)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 16)
instance (GH.HasVariant "i64vec" GH.Slot Z (R.List Std_.Int64)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 17)
instance (GH.HasVariant "i32vec" GH.Slot Z (R.List Std_.Int32)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 18)
instance (GH.HasVariant "i16vec" GH.Slot Z (R.List Std_.Int16)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 19)
instance (GH.HasVariant "i8vec" GH.Slot Z (R.List Std_.Int8)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 20)
instance (GH.HasVariant "u64vec" GH.Slot Z (R.List Std_.Word64)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 21)
instance (GH.HasVariant "u32vec" GH.Slot Z (R.List Std_.Word32)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 22)
instance (GH.HasVariant "u16vec" GH.Slot Z (R.List Std_.Word16)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 23)
instance (GH.HasVariant "u8vec" GH.Slot Z (R.List Std_.Word8)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 24)
instance (GH.HasVariant "zvec" GH.Slot Z (R.List Z)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 25)
instance (GH.HasVariant "zvecvec" GH.Slot Z (R.List (R.List Z))) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 26)
instance (GH.HasVariant "zdate" GH.Slot Z Zdate) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 27)
instance (GH.HasVariant "zdata" GH.Slot Z Zdata) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 28)
instance (GH.HasVariant "aircraftvec" GH.Slot Z (R.List Aircraft)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 29)
instance (GH.HasVariant "aircraft" GH.Slot Z Aircraft) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 30)
instance (GH.HasVariant "regression" GH.Slot Z Regression) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 31)
instance (GH.HasVariant "planebase" GH.Slot Z PlaneBase) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 32)
instance (GH.HasVariant "airport" GH.Slot Z Airport) where
    variantByLabel  = (GH.Variant (GH.dataField 0 1 16 0) 33)
instance (GH.HasVariant "b737" GH.Slot Z B737) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 34)
instance (GH.HasVariant "a320" GH.Slot Z A320) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 35)
instance (GH.HasVariant "f16" GH.Slot Z F16) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 36)
instance (GH.HasVariant "zdatevec" GH.Slot Z (R.List Zdate)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 37)
instance (GH.HasVariant "zdatavec" GH.Slot Z (R.List Zdata)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 38)
instance (GH.HasVariant "boolvec" GH.Slot Z (R.List Std_.Bool)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 39)
instance (GH.HasVariant "datavec" GH.Slot Z (R.List Basics.Data)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 40)
instance (GH.HasVariant "textvec" GH.Slot Z (R.List Basics.Text)) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 41)
instance (GH.HasVariant "grp" GH.Group Z Z'grp) where
    variantByLabel  = (GH.Variant GH.groupField 42)
instance (GH.HasVariant "echo" GH.Slot Z Echo) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 43)
instance (GH.HasVariant "echoBases" GH.Slot Z EchoBases) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 44)
data instance C.Parsed (GH.Which Z)
    = Z'void 
    | Z'zz (RP.Parsed Z)
    | Z'f64 (RP.Parsed Std_.Double)
    | Z'f32 (RP.Parsed Std_.Float)
    | Z'i64 (RP.Parsed Std_.Int64)
    | Z'i32 (RP.Parsed Std_.Int32)
    | Z'i16 (RP.Parsed Std_.Int16)
    | Z'i8 (RP.Parsed Std_.Int8)
    | Z'u64 (RP.Parsed Std_.Word64)
    | Z'u32 (RP.Parsed Std_.Word32)
    | Z'u16 (RP.Parsed Std_.Word16)
    | Z'u8 (RP.Parsed Std_.Word8)
    | Z'bool (RP.Parsed Std_.Bool)
    | Z'text (RP.Parsed Basics.Text)
    | Z'blob (RP.Parsed Basics.Data)
    | Z'f64vec (RP.Parsed (R.List Std_.Double))
    | Z'f32vec (RP.Parsed (R.List Std_.Float))
    | Z'i64vec (RP.Parsed (R.List Std_.Int64))
    | Z'i32vec (RP.Parsed (R.List Std_.Int32))
    | Z'i16vec (RP.Parsed (R.List Std_.Int16))
    | Z'i8vec (RP.Parsed (R.List Std_.Int8))
    | Z'u64vec (RP.Parsed (R.List Std_.Word64))
    | Z'u32vec (RP.Parsed (R.List Std_.Word32))
    | Z'u16vec (RP.Parsed (R.List Std_.Word16))
    | Z'u8vec (RP.Parsed (R.List Std_.Word8))
    | Z'zvec (RP.Parsed (R.List Z))
    | Z'zvecvec (RP.Parsed (R.List (R.List Z)))
    | Z'zdate (RP.Parsed Zdate)
    | Z'zdata (RP.Parsed Zdata)
    | Z'aircraftvec (RP.Parsed (R.List Aircraft))
    | Z'aircraft (RP.Parsed Aircraft)
    | Z'regression (RP.Parsed Regression)
    | Z'planebase (RP.Parsed PlaneBase)
    | Z'airport (RP.Parsed Airport)
    | Z'b737 (RP.Parsed B737)
    | Z'a320 (RP.Parsed A320)
    | Z'f16 (RP.Parsed F16)
    | Z'zdatevec (RP.Parsed (R.List Zdate))
    | Z'zdatavec (RP.Parsed (R.List Zdata))
    | Z'boolvec (RP.Parsed (R.List Std_.Bool))
    | Z'datavec (RP.Parsed (R.List Basics.Data))
    | Z'textvec (RP.Parsed (R.List Basics.Text))
    | Z'grp (RP.Parsed Z'grp)
    | Z'echo (RP.Parsed Echo)
    | Z'echoBases (RP.Parsed EchoBases)
    | Z'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which Z)))
deriving instance (Std_.Eq (C.Parsed (GH.Which Z)))
instance (C.Parse (GH.Which Z) (C.Parsed (GH.Which Z))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_Z'void _) ->
                (Std_.pure Z'void)
            (RW_Z'zz rawArg_) ->
                (Z'zz <$> (C.parse rawArg_))
            (RW_Z'f64 rawArg_) ->
                (Z'f64 <$> (C.parse rawArg_))
            (RW_Z'f32 rawArg_) ->
                (Z'f32 <$> (C.parse rawArg_))
            (RW_Z'i64 rawArg_) ->
                (Z'i64 <$> (C.parse rawArg_))
            (RW_Z'i32 rawArg_) ->
                (Z'i32 <$> (C.parse rawArg_))
            (RW_Z'i16 rawArg_) ->
                (Z'i16 <$> (C.parse rawArg_))
            (RW_Z'i8 rawArg_) ->
                (Z'i8 <$> (C.parse rawArg_))
            (RW_Z'u64 rawArg_) ->
                (Z'u64 <$> (C.parse rawArg_))
            (RW_Z'u32 rawArg_) ->
                (Z'u32 <$> (C.parse rawArg_))
            (RW_Z'u16 rawArg_) ->
                (Z'u16 <$> (C.parse rawArg_))
            (RW_Z'u8 rawArg_) ->
                (Z'u8 <$> (C.parse rawArg_))
            (RW_Z'bool rawArg_) ->
                (Z'bool <$> (C.parse rawArg_))
            (RW_Z'text rawArg_) ->
                (Z'text <$> (C.parse rawArg_))
            (RW_Z'blob rawArg_) ->
                (Z'blob <$> (C.parse rawArg_))
            (RW_Z'f64vec rawArg_) ->
                (Z'f64vec <$> (C.parse rawArg_))
            (RW_Z'f32vec rawArg_) ->
                (Z'f32vec <$> (C.parse rawArg_))
            (RW_Z'i64vec rawArg_) ->
                (Z'i64vec <$> (C.parse rawArg_))
            (RW_Z'i32vec rawArg_) ->
                (Z'i32vec <$> (C.parse rawArg_))
            (RW_Z'i16vec rawArg_) ->
                (Z'i16vec <$> (C.parse rawArg_))
            (RW_Z'i8vec rawArg_) ->
                (Z'i8vec <$> (C.parse rawArg_))
            (RW_Z'u64vec rawArg_) ->
                (Z'u64vec <$> (C.parse rawArg_))
            (RW_Z'u32vec rawArg_) ->
                (Z'u32vec <$> (C.parse rawArg_))
            (RW_Z'u16vec rawArg_) ->
                (Z'u16vec <$> (C.parse rawArg_))
            (RW_Z'u8vec rawArg_) ->
                (Z'u8vec <$> (C.parse rawArg_))
            (RW_Z'zvec rawArg_) ->
                (Z'zvec <$> (C.parse rawArg_))
            (RW_Z'zvecvec rawArg_) ->
                (Z'zvecvec <$> (C.parse rawArg_))
            (RW_Z'zdate rawArg_) ->
                (Z'zdate <$> (C.parse rawArg_))
            (RW_Z'zdata rawArg_) ->
                (Z'zdata <$> (C.parse rawArg_))
            (RW_Z'aircraftvec rawArg_) ->
                (Z'aircraftvec <$> (C.parse rawArg_))
            (RW_Z'aircraft rawArg_) ->
                (Z'aircraft <$> (C.parse rawArg_))
            (RW_Z'regression rawArg_) ->
                (Z'regression <$> (C.parse rawArg_))
            (RW_Z'planebase rawArg_) ->
                (Z'planebase <$> (C.parse rawArg_))
            (RW_Z'airport rawArg_) ->
                (Z'airport <$> (C.parse rawArg_))
            (RW_Z'b737 rawArg_) ->
                (Z'b737 <$> (C.parse rawArg_))
            (RW_Z'a320 rawArg_) ->
                (Z'a320 <$> (C.parse rawArg_))
            (RW_Z'f16 rawArg_) ->
                (Z'f16 <$> (C.parse rawArg_))
            (RW_Z'zdatevec rawArg_) ->
                (Z'zdatevec <$> (C.parse rawArg_))
            (RW_Z'zdatavec rawArg_) ->
                (Z'zdatavec <$> (C.parse rawArg_))
            (RW_Z'boolvec rawArg_) ->
                (Z'boolvec <$> (C.parse rawArg_))
            (RW_Z'datavec rawArg_) ->
                (Z'datavec <$> (C.parse rawArg_))
            (RW_Z'textvec rawArg_) ->
                (Z'textvec <$> (C.parse rawArg_))
            (RW_Z'grp rawArg_) ->
                (Z'grp <$> (C.parse rawArg_))
            (RW_Z'echo rawArg_) ->
                (Z'echo <$> (C.parse rawArg_))
            (RW_Z'echoBases rawArg_) ->
                (Z'echoBases <$> (C.parse rawArg_))
            (RW_Z'unknown' tag_) ->
                (Std_.pure (Z'unknown' tag_))
        )
instance (C.Marshal (GH.Which Z) (C.Parsed (GH.Which Z))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (Z'void) ->
            (GH.encodeVariant #void () (GH.unionStruct raw_))
        (Z'zz arg_) ->
            (GH.encodeVariant #zz arg_ (GH.unionStruct raw_))
        (Z'f64 arg_) ->
            (GH.encodeVariant #f64 arg_ (GH.unionStruct raw_))
        (Z'f32 arg_) ->
            (GH.encodeVariant #f32 arg_ (GH.unionStruct raw_))
        (Z'i64 arg_) ->
            (GH.encodeVariant #i64 arg_ (GH.unionStruct raw_))
        (Z'i32 arg_) ->
            (GH.encodeVariant #i32 arg_ (GH.unionStruct raw_))
        (Z'i16 arg_) ->
            (GH.encodeVariant #i16 arg_ (GH.unionStruct raw_))
        (Z'i8 arg_) ->
            (GH.encodeVariant #i8 arg_ (GH.unionStruct raw_))
        (Z'u64 arg_) ->
            (GH.encodeVariant #u64 arg_ (GH.unionStruct raw_))
        (Z'u32 arg_) ->
            (GH.encodeVariant #u32 arg_ (GH.unionStruct raw_))
        (Z'u16 arg_) ->
            (GH.encodeVariant #u16 arg_ (GH.unionStruct raw_))
        (Z'u8 arg_) ->
            (GH.encodeVariant #u8 arg_ (GH.unionStruct raw_))
        (Z'bool arg_) ->
            (GH.encodeVariant #bool arg_ (GH.unionStruct raw_))
        (Z'text arg_) ->
            (GH.encodeVariant #text arg_ (GH.unionStruct raw_))
        (Z'blob arg_) ->
            (GH.encodeVariant #blob arg_ (GH.unionStruct raw_))
        (Z'f64vec arg_) ->
            (GH.encodeVariant #f64vec arg_ (GH.unionStruct raw_))
        (Z'f32vec arg_) ->
            (GH.encodeVariant #f32vec arg_ (GH.unionStruct raw_))
        (Z'i64vec arg_) ->
            (GH.encodeVariant #i64vec arg_ (GH.unionStruct raw_))
        (Z'i32vec arg_) ->
            (GH.encodeVariant #i32vec arg_ (GH.unionStruct raw_))
        (Z'i16vec arg_) ->
            (GH.encodeVariant #i16vec arg_ (GH.unionStruct raw_))
        (Z'i8vec arg_) ->
            (GH.encodeVariant #i8vec arg_ (GH.unionStruct raw_))
        (Z'u64vec arg_) ->
            (GH.encodeVariant #u64vec arg_ (GH.unionStruct raw_))
        (Z'u32vec arg_) ->
            (GH.encodeVariant #u32vec arg_ (GH.unionStruct raw_))
        (Z'u16vec arg_) ->
            (GH.encodeVariant #u16vec arg_ (GH.unionStruct raw_))
        (Z'u8vec arg_) ->
            (GH.encodeVariant #u8vec arg_ (GH.unionStruct raw_))
        (Z'zvec arg_) ->
            (GH.encodeVariant #zvec arg_ (GH.unionStruct raw_))
        (Z'zvecvec arg_) ->
            (GH.encodeVariant #zvecvec arg_ (GH.unionStruct raw_))
        (Z'zdate arg_) ->
            (GH.encodeVariant #zdate arg_ (GH.unionStruct raw_))
        (Z'zdata arg_) ->
            (GH.encodeVariant #zdata arg_ (GH.unionStruct raw_))
        (Z'aircraftvec arg_) ->
            (GH.encodeVariant #aircraftvec arg_ (GH.unionStruct raw_))
        (Z'aircraft arg_) ->
            (GH.encodeVariant #aircraft arg_ (GH.unionStruct raw_))
        (Z'regression arg_) ->
            (GH.encodeVariant #regression arg_ (GH.unionStruct raw_))
        (Z'planebase arg_) ->
            (GH.encodeVariant #planebase arg_ (GH.unionStruct raw_))
        (Z'airport arg_) ->
            (GH.encodeVariant #airport arg_ (GH.unionStruct raw_))
        (Z'b737 arg_) ->
            (GH.encodeVariant #b737 arg_ (GH.unionStruct raw_))
        (Z'a320 arg_) ->
            (GH.encodeVariant #a320 arg_ (GH.unionStruct raw_))
        (Z'f16 arg_) ->
            (GH.encodeVariant #f16 arg_ (GH.unionStruct raw_))
        (Z'zdatevec arg_) ->
            (GH.encodeVariant #zdatevec arg_ (GH.unionStruct raw_))
        (Z'zdatavec arg_) ->
            (GH.encodeVariant #zdatavec arg_ (GH.unionStruct raw_))
        (Z'boolvec arg_) ->
            (GH.encodeVariant #boolvec arg_ (GH.unionStruct raw_))
        (Z'datavec arg_) ->
            (GH.encodeVariant #datavec arg_ (GH.unionStruct raw_))
        (Z'textvec arg_) ->
            (GH.encodeVariant #textvec arg_ (GH.unionStruct raw_))
        (Z'grp arg_) ->
            (do
                rawGroup_ <- (GH.initVariant #grp (GH.unionStruct raw_))
                (C.marshalInto rawGroup_ arg_)
                )
        (Z'echo arg_) ->
            (GH.encodeVariant #echo arg_ (GH.unionStruct raw_))
        (Z'echoBases arg_) ->
            (GH.encodeVariant #echoBases arg_ (GH.unionStruct raw_))
        (Z'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data Z'grp 
type instance (R.ReprFor Z'grp) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Z'grp) where
    typeId  = 13198763830743312036
instance (C.TypedStruct Z'grp) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate Z'grp) where
    type AllocHint Z'grp = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Z'grp (C.Parsed Z'grp))
instance (C.AllocateList Z'grp) where
    type ListAllocHint Z'grp = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Z'grp (C.Parsed Z'grp))
data instance C.Parsed Z'grp
    = Z'grp' 
        {first :: (RP.Parsed Std_.Word64)
        ,second :: (RP.Parsed Std_.Word64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Z'grp))
deriving instance (Std_.Eq (C.Parsed Z'grp))
instance (C.Parse Z'grp (C.Parsed Z'grp)) where
    parse raw_ = (Z'grp' <$> (GH.parseField #first raw_)
                         <*> (GH.parseField #second raw_))
instance (C.Marshal Z'grp (C.Parsed Z'grp)) where
    marshalInto raw_ Z'grp'{..} = (do
        (GH.encodeField #first first raw_)
        (GH.encodeField #second second raw_)
        (Std_.pure ())
        )
instance (GH.HasField "first" GH.Slot Z'grp Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "second" GH.Slot Z'grp Std_.Word64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
data Counter 
type instance (R.ReprFor Counter) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Counter) where
    typeId  = 9748248141862325085
instance (C.TypedStruct Counter) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate Counter) where
    type AllocHint Counter = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Counter (C.Parsed Counter))
instance (C.AllocateList Counter) where
    type ListAllocHint Counter = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Counter (C.Parsed Counter))
data instance C.Parsed Counter
    = Counter 
        {size :: (RP.Parsed Std_.Int64)
        ,words :: (RP.Parsed Basics.Text)
        ,wordlist :: (RP.Parsed (R.List Basics.Text))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Counter))
deriving instance (Std_.Eq (C.Parsed Counter))
instance (C.Parse Counter (C.Parsed Counter)) where
    parse raw_ = (Counter <$> (GH.parseField #size raw_)
                          <*> (GH.parseField #words raw_)
                          <*> (GH.parseField #wordlist raw_))
instance (C.Marshal Counter (C.Parsed Counter)) where
    marshalInto raw_ Counter{..} = (do
        (GH.encodeField #size size raw_)
        (GH.encodeField #words words raw_)
        (GH.encodeField #wordlist wordlist raw_)
        (Std_.pure ())
        )
instance (GH.HasField "size" GH.Slot Counter Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "words" GH.Slot Counter Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "wordlist" GH.Slot Counter (R.List Basics.Text)) where
    fieldByLabel  = (GH.ptrField 1)
data Bag 
type instance (R.ReprFor Bag) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Bag) where
    typeId  = 15435801458704439998
instance (C.TypedStruct Bag) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Bag) where
    type AllocHint Bag = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Bag (C.Parsed Bag))
instance (C.AllocateList Bag) where
    type ListAllocHint Bag = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Bag (C.Parsed Bag))
data instance C.Parsed Bag
    = Bag 
        {counter :: (RP.Parsed Counter)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Bag))
deriving instance (Std_.Eq (C.Parsed Bag))
instance (C.Parse Bag (C.Parsed Bag)) where
    parse raw_ = (Bag <$> (GH.parseField #counter raw_))
instance (C.Marshal Bag (C.Parsed Bag)) where
    marshalInto raw_ Bag{..} = (do
        (GH.encodeField #counter counter raw_)
        (Std_.pure ())
        )
instance (GH.HasField "counter" GH.Slot Bag Counter) where
    fieldByLabel  = (GH.ptrField 0)
data Zserver 
type instance (R.ReprFor Zserver) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Zserver) where
    typeId  = 14718909161889449112
instance (C.TypedStruct Zserver) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Zserver) where
    type AllocHint Zserver = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Zserver (C.Parsed Zserver))
instance (C.AllocateList Zserver) where
    type ListAllocHint Zserver = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Zserver (C.Parsed Zserver))
data instance C.Parsed Zserver
    = Zserver 
        {waitingjobs :: (RP.Parsed (R.List Zjob))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Zserver))
deriving instance (Std_.Eq (C.Parsed Zserver))
instance (C.Parse Zserver (C.Parsed Zserver)) where
    parse raw_ = (Zserver <$> (GH.parseField #waitingjobs raw_))
instance (C.Marshal Zserver (C.Parsed Zserver)) where
    marshalInto raw_ Zserver{..} = (do
        (GH.encodeField #waitingjobs waitingjobs raw_)
        (Std_.pure ())
        )
instance (GH.HasField "waitingjobs" GH.Slot Zserver (R.List Zjob)) where
    fieldByLabel  = (GH.ptrField 0)
data Zjob 
type instance (R.ReprFor Zjob) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Zjob) where
    typeId  = 15983628460635158035
instance (C.TypedStruct Zjob) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Zjob) where
    type AllocHint Zjob = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Zjob (C.Parsed Zjob))
instance (C.AllocateList Zjob) where
    type ListAllocHint Zjob = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Zjob (C.Parsed Zjob))
data instance C.Parsed Zjob
    = Zjob 
        {cmd :: (RP.Parsed Basics.Text)
        ,args :: (RP.Parsed (R.List Basics.Text))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Zjob))
deriving instance (Std_.Eq (C.Parsed Zjob))
instance (C.Parse Zjob (C.Parsed Zjob)) where
    parse raw_ = (Zjob <$> (GH.parseField #cmd raw_)
                       <*> (GH.parseField #args raw_))
instance (C.Marshal Zjob (C.Parsed Zjob)) where
    marshalInto raw_ Zjob{..} = (do
        (GH.encodeField #cmd cmd raw_)
        (GH.encodeField #args args raw_)
        (Std_.pure ())
        )
instance (GH.HasField "cmd" GH.Slot Zjob Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "args" GH.Slot Zjob (R.List Basics.Text)) where
    fieldByLabel  = (GH.ptrField 1)
data VerEmpty 
type instance (R.ReprFor VerEmpty) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VerEmpty) where
    typeId  = 10649211371004916479
instance (C.TypedStruct VerEmpty) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate VerEmpty) where
    type AllocHint VerEmpty = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VerEmpty (C.Parsed VerEmpty))
instance (C.AllocateList VerEmpty) where
    type ListAllocHint VerEmpty = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VerEmpty (C.Parsed VerEmpty))
data instance C.Parsed VerEmpty
    = VerEmpty 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VerEmpty))
deriving instance (Std_.Eq (C.Parsed VerEmpty))
instance (C.Parse VerEmpty (C.Parsed VerEmpty)) where
    parse raw_ = (Std_.pure VerEmpty)
instance (C.Marshal VerEmpty (C.Parsed VerEmpty)) where
    marshalInto _raw (VerEmpty) = (Std_.pure ())
data VerOneData 
type instance (R.ReprFor VerOneData) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VerOneData) where
    typeId  = 18204521836387912926
instance (C.TypedStruct VerOneData) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate VerOneData) where
    type AllocHint VerOneData = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VerOneData (C.Parsed VerOneData))
instance (C.AllocateList VerOneData) where
    type ListAllocHint VerOneData = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VerOneData (C.Parsed VerOneData))
data instance C.Parsed VerOneData
    = VerOneData 
        {val :: (RP.Parsed Std_.Int16)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VerOneData))
deriving instance (Std_.Eq (C.Parsed VerOneData))
instance (C.Parse VerOneData (C.Parsed VerOneData)) where
    parse raw_ = (VerOneData <$> (GH.parseField #val raw_))
instance (C.Marshal VerOneData (C.Parsed VerOneData)) where
    marshalInto raw_ VerOneData{..} = (do
        (GH.encodeField #val val raw_)
        (Std_.pure ())
        )
instance (GH.HasField "val" GH.Slot VerOneData Std_.Int16) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
data VerTwoData 
type instance (R.ReprFor VerTwoData) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VerTwoData) where
    typeId  = 17799875294539507453
instance (C.TypedStruct VerTwoData) where
    numStructWords  = 2
    numStructPtrs  = 0
instance (C.Allocate VerTwoData) where
    type AllocHint VerTwoData = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VerTwoData (C.Parsed VerTwoData))
instance (C.AllocateList VerTwoData) where
    type ListAllocHint VerTwoData = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VerTwoData (C.Parsed VerTwoData))
data instance C.Parsed VerTwoData
    = VerTwoData 
        {val :: (RP.Parsed Std_.Int16)
        ,duo :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VerTwoData))
deriving instance (Std_.Eq (C.Parsed VerTwoData))
instance (C.Parse VerTwoData (C.Parsed VerTwoData)) where
    parse raw_ = (VerTwoData <$> (GH.parseField #val raw_)
                             <*> (GH.parseField #duo raw_))
instance (C.Marshal VerTwoData (C.Parsed VerTwoData)) where
    marshalInto raw_ VerTwoData{..} = (do
        (GH.encodeField #val val raw_)
        (GH.encodeField #duo duo raw_)
        (Std_.pure ())
        )
instance (GH.HasField "val" GH.Slot VerTwoData Std_.Int16) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "duo" GH.Slot VerTwoData Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
data VerOnePtr 
type instance (R.ReprFor VerOnePtr) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VerOnePtr) where
    typeId  = 10718424143143379341
instance (C.TypedStruct VerOnePtr) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate VerOnePtr) where
    type AllocHint VerOnePtr = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VerOnePtr (C.Parsed VerOnePtr))
instance (C.AllocateList VerOnePtr) where
    type ListAllocHint VerOnePtr = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VerOnePtr (C.Parsed VerOnePtr))
data instance C.Parsed VerOnePtr
    = VerOnePtr 
        {ptr :: (RP.Parsed VerOneData)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VerOnePtr))
deriving instance (Std_.Eq (C.Parsed VerOnePtr))
instance (C.Parse VerOnePtr (C.Parsed VerOnePtr)) where
    parse raw_ = (VerOnePtr <$> (GH.parseField #ptr raw_))
instance (C.Marshal VerOnePtr (C.Parsed VerOnePtr)) where
    marshalInto raw_ VerOnePtr{..} = (do
        (GH.encodeField #ptr ptr raw_)
        (Std_.pure ())
        )
instance (GH.HasField "ptr" GH.Slot VerOnePtr VerOneData) where
    fieldByLabel  = (GH.ptrField 0)
data VerTwoPtr 
type instance (R.ReprFor VerTwoPtr) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VerTwoPtr) where
    typeId  = 14509379619124759853
instance (C.TypedStruct VerTwoPtr) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate VerTwoPtr) where
    type AllocHint VerTwoPtr = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VerTwoPtr (C.Parsed VerTwoPtr))
instance (C.AllocateList VerTwoPtr) where
    type ListAllocHint VerTwoPtr = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VerTwoPtr (C.Parsed VerTwoPtr))
data instance C.Parsed VerTwoPtr
    = VerTwoPtr 
        {ptr1 :: (RP.Parsed VerOneData)
        ,ptr2 :: (RP.Parsed VerOneData)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VerTwoPtr))
deriving instance (Std_.Eq (C.Parsed VerTwoPtr))
instance (C.Parse VerTwoPtr (C.Parsed VerTwoPtr)) where
    parse raw_ = (VerTwoPtr <$> (GH.parseField #ptr1 raw_)
                            <*> (GH.parseField #ptr2 raw_))
instance (C.Marshal VerTwoPtr (C.Parsed VerTwoPtr)) where
    marshalInto raw_ VerTwoPtr{..} = (do
        (GH.encodeField #ptr1 ptr1 raw_)
        (GH.encodeField #ptr2 ptr2 raw_)
        (Std_.pure ())
        )
instance (GH.HasField "ptr1" GH.Slot VerTwoPtr VerOneData) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "ptr2" GH.Slot VerTwoPtr VerOneData) where
    fieldByLabel  = (GH.ptrField 1)
data VerTwoDataTwoPtr 
type instance (R.ReprFor VerTwoDataTwoPtr) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VerTwoDataTwoPtr) where
    typeId  = 13123175871726013043
instance (C.TypedStruct VerTwoDataTwoPtr) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate VerTwoDataTwoPtr) where
    type AllocHint VerTwoDataTwoPtr = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VerTwoDataTwoPtr (C.Parsed VerTwoDataTwoPtr))
instance (C.AllocateList VerTwoDataTwoPtr) where
    type ListAllocHint VerTwoDataTwoPtr = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VerTwoDataTwoPtr (C.Parsed VerTwoDataTwoPtr))
data instance C.Parsed VerTwoDataTwoPtr
    = VerTwoDataTwoPtr 
        {val :: (RP.Parsed Std_.Int16)
        ,duo :: (RP.Parsed Std_.Int64)
        ,ptr1 :: (RP.Parsed VerOneData)
        ,ptr2 :: (RP.Parsed VerOneData)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VerTwoDataTwoPtr))
deriving instance (Std_.Eq (C.Parsed VerTwoDataTwoPtr))
instance (C.Parse VerTwoDataTwoPtr (C.Parsed VerTwoDataTwoPtr)) where
    parse raw_ = (VerTwoDataTwoPtr <$> (GH.parseField #val raw_)
                                   <*> (GH.parseField #duo raw_)
                                   <*> (GH.parseField #ptr1 raw_)
                                   <*> (GH.parseField #ptr2 raw_))
instance (C.Marshal VerTwoDataTwoPtr (C.Parsed VerTwoDataTwoPtr)) where
    marshalInto raw_ VerTwoDataTwoPtr{..} = (do
        (GH.encodeField #val val raw_)
        (GH.encodeField #duo duo raw_)
        (GH.encodeField #ptr1 ptr1 raw_)
        (GH.encodeField #ptr2 ptr2 raw_)
        (Std_.pure ())
        )
instance (GH.HasField "val" GH.Slot VerTwoDataTwoPtr Std_.Int16) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "duo" GH.Slot VerTwoDataTwoPtr Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "ptr1" GH.Slot VerTwoDataTwoPtr VerOneData) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "ptr2" GH.Slot VerTwoDataTwoPtr VerOneData) where
    fieldByLabel  = (GH.ptrField 1)
data HoldsVerEmptyList 
type instance (R.ReprFor HoldsVerEmptyList) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId HoldsVerEmptyList) where
    typeId  = 16041492281108738195
instance (C.TypedStruct HoldsVerEmptyList) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate HoldsVerEmptyList) where
    type AllocHint HoldsVerEmptyList = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc HoldsVerEmptyList (C.Parsed HoldsVerEmptyList))
instance (C.AllocateList HoldsVerEmptyList) where
    type ListAllocHint HoldsVerEmptyList = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc HoldsVerEmptyList (C.Parsed HoldsVerEmptyList))
data instance C.Parsed HoldsVerEmptyList
    = HoldsVerEmptyList 
        {mylist :: (RP.Parsed (R.List VerEmpty))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed HoldsVerEmptyList))
deriving instance (Std_.Eq (C.Parsed HoldsVerEmptyList))
instance (C.Parse HoldsVerEmptyList (C.Parsed HoldsVerEmptyList)) where
    parse raw_ = (HoldsVerEmptyList <$> (GH.parseField #mylist raw_))
instance (C.Marshal HoldsVerEmptyList (C.Parsed HoldsVerEmptyList)) where
    marshalInto raw_ HoldsVerEmptyList{..} = (do
        (GH.encodeField #mylist mylist raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mylist" GH.Slot HoldsVerEmptyList (R.List VerEmpty)) where
    fieldByLabel  = (GH.ptrField 0)
data HoldsVerOneDataList 
type instance (R.ReprFor HoldsVerOneDataList) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId HoldsVerOneDataList) where
    typeId  = 12380489118307417585
instance (C.TypedStruct HoldsVerOneDataList) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate HoldsVerOneDataList) where
    type AllocHint HoldsVerOneDataList = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc HoldsVerOneDataList (C.Parsed HoldsVerOneDataList))
instance (C.AllocateList HoldsVerOneDataList) where
    type ListAllocHint HoldsVerOneDataList = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc HoldsVerOneDataList (C.Parsed HoldsVerOneDataList))
data instance C.Parsed HoldsVerOneDataList
    = HoldsVerOneDataList 
        {mylist :: (RP.Parsed (R.List VerOneData))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed HoldsVerOneDataList))
deriving instance (Std_.Eq (C.Parsed HoldsVerOneDataList))
instance (C.Parse HoldsVerOneDataList (C.Parsed HoldsVerOneDataList)) where
    parse raw_ = (HoldsVerOneDataList <$> (GH.parseField #mylist raw_))
instance (C.Marshal HoldsVerOneDataList (C.Parsed HoldsVerOneDataList)) where
    marshalInto raw_ HoldsVerOneDataList{..} = (do
        (GH.encodeField #mylist mylist raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mylist" GH.Slot HoldsVerOneDataList (R.List VerOneData)) where
    fieldByLabel  = (GH.ptrField 0)
data HoldsVerTwoDataList 
type instance (R.ReprFor HoldsVerTwoDataList) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId HoldsVerTwoDataList) where
    typeId  = 14689746238557910970
instance (C.TypedStruct HoldsVerTwoDataList) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate HoldsVerTwoDataList) where
    type AllocHint HoldsVerTwoDataList = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc HoldsVerTwoDataList (C.Parsed HoldsVerTwoDataList))
instance (C.AllocateList HoldsVerTwoDataList) where
    type ListAllocHint HoldsVerTwoDataList = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc HoldsVerTwoDataList (C.Parsed HoldsVerTwoDataList))
data instance C.Parsed HoldsVerTwoDataList
    = HoldsVerTwoDataList 
        {mylist :: (RP.Parsed (R.List VerTwoData))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed HoldsVerTwoDataList))
deriving instance (Std_.Eq (C.Parsed HoldsVerTwoDataList))
instance (C.Parse HoldsVerTwoDataList (C.Parsed HoldsVerTwoDataList)) where
    parse raw_ = (HoldsVerTwoDataList <$> (GH.parseField #mylist raw_))
instance (C.Marshal HoldsVerTwoDataList (C.Parsed HoldsVerTwoDataList)) where
    marshalInto raw_ HoldsVerTwoDataList{..} = (do
        (GH.encodeField #mylist mylist raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mylist" GH.Slot HoldsVerTwoDataList (R.List VerTwoData)) where
    fieldByLabel  = (GH.ptrField 0)
data HoldsVerOnePtrList 
type instance (R.ReprFor HoldsVerOnePtrList) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId HoldsVerOnePtrList) where
    typeId  = 16503619627606104568
instance (C.TypedStruct HoldsVerOnePtrList) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate HoldsVerOnePtrList) where
    type AllocHint HoldsVerOnePtrList = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc HoldsVerOnePtrList (C.Parsed HoldsVerOnePtrList))
instance (C.AllocateList HoldsVerOnePtrList) where
    type ListAllocHint HoldsVerOnePtrList = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc HoldsVerOnePtrList (C.Parsed HoldsVerOnePtrList))
data instance C.Parsed HoldsVerOnePtrList
    = HoldsVerOnePtrList 
        {mylist :: (RP.Parsed (R.List VerOnePtr))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed HoldsVerOnePtrList))
deriving instance (Std_.Eq (C.Parsed HoldsVerOnePtrList))
instance (C.Parse HoldsVerOnePtrList (C.Parsed HoldsVerOnePtrList)) where
    parse raw_ = (HoldsVerOnePtrList <$> (GH.parseField #mylist raw_))
instance (C.Marshal HoldsVerOnePtrList (C.Parsed HoldsVerOnePtrList)) where
    marshalInto raw_ HoldsVerOnePtrList{..} = (do
        (GH.encodeField #mylist mylist raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mylist" GH.Slot HoldsVerOnePtrList (R.List VerOnePtr)) where
    fieldByLabel  = (GH.ptrField 0)
data HoldsVerTwoPtrList 
type instance (R.ReprFor HoldsVerTwoPtrList) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId HoldsVerTwoPtrList) where
    typeId  = 14959808741027971272
instance (C.TypedStruct HoldsVerTwoPtrList) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate HoldsVerTwoPtrList) where
    type AllocHint HoldsVerTwoPtrList = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc HoldsVerTwoPtrList (C.Parsed HoldsVerTwoPtrList))
instance (C.AllocateList HoldsVerTwoPtrList) where
    type ListAllocHint HoldsVerTwoPtrList = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc HoldsVerTwoPtrList (C.Parsed HoldsVerTwoPtrList))
data instance C.Parsed HoldsVerTwoPtrList
    = HoldsVerTwoPtrList 
        {mylist :: (RP.Parsed (R.List VerTwoPtr))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed HoldsVerTwoPtrList))
deriving instance (Std_.Eq (C.Parsed HoldsVerTwoPtrList))
instance (C.Parse HoldsVerTwoPtrList (C.Parsed HoldsVerTwoPtrList)) where
    parse raw_ = (HoldsVerTwoPtrList <$> (GH.parseField #mylist raw_))
instance (C.Marshal HoldsVerTwoPtrList (C.Parsed HoldsVerTwoPtrList)) where
    marshalInto raw_ HoldsVerTwoPtrList{..} = (do
        (GH.encodeField #mylist mylist raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mylist" GH.Slot HoldsVerTwoPtrList (R.List VerTwoPtr)) where
    fieldByLabel  = (GH.ptrField 0)
data HoldsVerTwoTwoList 
type instance (R.ReprFor HoldsVerTwoTwoList) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId HoldsVerTwoTwoList) where
    typeId  = 10790341304104545899
instance (C.TypedStruct HoldsVerTwoTwoList) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate HoldsVerTwoTwoList) where
    type AllocHint HoldsVerTwoTwoList = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc HoldsVerTwoTwoList (C.Parsed HoldsVerTwoTwoList))
instance (C.AllocateList HoldsVerTwoTwoList) where
    type ListAllocHint HoldsVerTwoTwoList = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc HoldsVerTwoTwoList (C.Parsed HoldsVerTwoTwoList))
data instance C.Parsed HoldsVerTwoTwoList
    = HoldsVerTwoTwoList 
        {mylist :: (RP.Parsed (R.List VerTwoDataTwoPtr))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed HoldsVerTwoTwoList))
deriving instance (Std_.Eq (C.Parsed HoldsVerTwoTwoList))
instance (C.Parse HoldsVerTwoTwoList (C.Parsed HoldsVerTwoTwoList)) where
    parse raw_ = (HoldsVerTwoTwoList <$> (GH.parseField #mylist raw_))
instance (C.Marshal HoldsVerTwoTwoList (C.Parsed HoldsVerTwoTwoList)) where
    marshalInto raw_ HoldsVerTwoTwoList{..} = (do
        (GH.encodeField #mylist mylist raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mylist" GH.Slot HoldsVerTwoTwoList (R.List VerTwoDataTwoPtr)) where
    fieldByLabel  = (GH.ptrField 0)
data HoldsVerTwoTwoPlus 
type instance (R.ReprFor HoldsVerTwoTwoPlus) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId HoldsVerTwoTwoPlus) where
    typeId  = 9782732235957253080
instance (C.TypedStruct HoldsVerTwoTwoPlus) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate HoldsVerTwoTwoPlus) where
    type AllocHint HoldsVerTwoTwoPlus = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc HoldsVerTwoTwoPlus (C.Parsed HoldsVerTwoTwoPlus))
instance (C.AllocateList HoldsVerTwoTwoPlus) where
    type ListAllocHint HoldsVerTwoTwoPlus = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc HoldsVerTwoTwoPlus (C.Parsed HoldsVerTwoTwoPlus))
data instance C.Parsed HoldsVerTwoTwoPlus
    = HoldsVerTwoTwoPlus 
        {mylist :: (RP.Parsed (R.List VerTwoTwoPlus))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed HoldsVerTwoTwoPlus))
deriving instance (Std_.Eq (C.Parsed HoldsVerTwoTwoPlus))
instance (C.Parse HoldsVerTwoTwoPlus (C.Parsed HoldsVerTwoTwoPlus)) where
    parse raw_ = (HoldsVerTwoTwoPlus <$> (GH.parseField #mylist raw_))
instance (C.Marshal HoldsVerTwoTwoPlus (C.Parsed HoldsVerTwoTwoPlus)) where
    marshalInto raw_ HoldsVerTwoTwoPlus{..} = (do
        (GH.encodeField #mylist mylist raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mylist" GH.Slot HoldsVerTwoTwoPlus (R.List VerTwoTwoPlus)) where
    fieldByLabel  = (GH.ptrField 0)
data VerTwoTwoPlus 
type instance (R.ReprFor VerTwoTwoPlus) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VerTwoTwoPlus) where
    typeId  = 14863196959570808905
instance (C.TypedStruct VerTwoTwoPlus) where
    numStructWords  = 3
    numStructPtrs  = 3
instance (C.Allocate VerTwoTwoPlus) where
    type AllocHint VerTwoTwoPlus = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VerTwoTwoPlus (C.Parsed VerTwoTwoPlus))
instance (C.AllocateList VerTwoTwoPlus) where
    type ListAllocHint VerTwoTwoPlus = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VerTwoTwoPlus (C.Parsed VerTwoTwoPlus))
data instance C.Parsed VerTwoTwoPlus
    = VerTwoTwoPlus 
        {val :: (RP.Parsed Std_.Int16)
        ,duo :: (RP.Parsed Std_.Int64)
        ,ptr1 :: (RP.Parsed VerTwoDataTwoPtr)
        ,ptr2 :: (RP.Parsed VerTwoDataTwoPtr)
        ,tre :: (RP.Parsed Std_.Int64)
        ,lst3 :: (RP.Parsed (R.List Std_.Int64))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VerTwoTwoPlus))
deriving instance (Std_.Eq (C.Parsed VerTwoTwoPlus))
instance (C.Parse VerTwoTwoPlus (C.Parsed VerTwoTwoPlus)) where
    parse raw_ = (VerTwoTwoPlus <$> (GH.parseField #val raw_)
                                <*> (GH.parseField #duo raw_)
                                <*> (GH.parseField #ptr1 raw_)
                                <*> (GH.parseField #ptr2 raw_)
                                <*> (GH.parseField #tre raw_)
                                <*> (GH.parseField #lst3 raw_))
instance (C.Marshal VerTwoTwoPlus (C.Parsed VerTwoTwoPlus)) where
    marshalInto raw_ VerTwoTwoPlus{..} = (do
        (GH.encodeField #val val raw_)
        (GH.encodeField #duo duo raw_)
        (GH.encodeField #ptr1 ptr1 raw_)
        (GH.encodeField #ptr2 ptr2 raw_)
        (GH.encodeField #tre tre raw_)
        (GH.encodeField #lst3 lst3 raw_)
        (Std_.pure ())
        )
instance (GH.HasField "val" GH.Slot VerTwoTwoPlus Std_.Int16) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "duo" GH.Slot VerTwoTwoPlus Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "ptr1" GH.Slot VerTwoTwoPlus VerTwoDataTwoPtr) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "ptr2" GH.Slot VerTwoTwoPlus VerTwoDataTwoPtr) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "tre" GH.Slot VerTwoTwoPlus Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
instance (GH.HasField "lst3" GH.Slot VerTwoTwoPlus (R.List Std_.Int64)) where
    fieldByLabel  = (GH.ptrField 2)
data HoldsText 
type instance (R.ReprFor HoldsText) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId HoldsText) where
    typeId  = 16537639514277480156
instance (C.TypedStruct HoldsText) where
    numStructWords  = 0
    numStructPtrs  = 3
instance (C.Allocate HoldsText) where
    type AllocHint HoldsText = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc HoldsText (C.Parsed HoldsText))
instance (C.AllocateList HoldsText) where
    type ListAllocHint HoldsText = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc HoldsText (C.Parsed HoldsText))
data instance C.Parsed HoldsText
    = HoldsText 
        {txt :: (RP.Parsed Basics.Text)
        ,lst :: (RP.Parsed (R.List Basics.Text))
        ,lstlst :: (RP.Parsed (R.List (R.List Basics.Text)))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed HoldsText))
deriving instance (Std_.Eq (C.Parsed HoldsText))
instance (C.Parse HoldsText (C.Parsed HoldsText)) where
    parse raw_ = (HoldsText <$> (GH.parseField #txt raw_)
                            <*> (GH.parseField #lst raw_)
                            <*> (GH.parseField #lstlst raw_))
instance (C.Marshal HoldsText (C.Parsed HoldsText)) where
    marshalInto raw_ HoldsText{..} = (do
        (GH.encodeField #txt txt raw_)
        (GH.encodeField #lst lst raw_)
        (GH.encodeField #lstlst lstlst raw_)
        (Std_.pure ())
        )
instance (GH.HasField "txt" GH.Slot HoldsText Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "lst" GH.Slot HoldsText (R.List Basics.Text)) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "lstlst" GH.Slot HoldsText (R.List (R.List Basics.Text))) where
    fieldByLabel  = (GH.ptrField 2)
data WrapEmpty 
type instance (R.ReprFor WrapEmpty) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId WrapEmpty) where
    typeId  = 11147985329045285977
instance (C.TypedStruct WrapEmpty) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate WrapEmpty) where
    type AllocHint WrapEmpty = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc WrapEmpty (C.Parsed WrapEmpty))
instance (C.AllocateList WrapEmpty) where
    type ListAllocHint WrapEmpty = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc WrapEmpty (C.Parsed WrapEmpty))
data instance C.Parsed WrapEmpty
    = WrapEmpty 
        {mightNotBeReallyEmpty :: (RP.Parsed VerEmpty)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed WrapEmpty))
deriving instance (Std_.Eq (C.Parsed WrapEmpty))
instance (C.Parse WrapEmpty (C.Parsed WrapEmpty)) where
    parse raw_ = (WrapEmpty <$> (GH.parseField #mightNotBeReallyEmpty raw_))
instance (C.Marshal WrapEmpty (C.Parsed WrapEmpty)) where
    marshalInto raw_ WrapEmpty{..} = (do
        (GH.encodeField #mightNotBeReallyEmpty mightNotBeReallyEmpty raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mightNotBeReallyEmpty" GH.Slot WrapEmpty VerEmpty) where
    fieldByLabel  = (GH.ptrField 0)
data Wrap2x2 
type instance (R.ReprFor Wrap2x2) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Wrap2x2) where
    typeId  = 16258788317804871341
instance (C.TypedStruct Wrap2x2) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Wrap2x2) where
    type AllocHint Wrap2x2 = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Wrap2x2 (C.Parsed Wrap2x2))
instance (C.AllocateList Wrap2x2) where
    type ListAllocHint Wrap2x2 = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Wrap2x2 (C.Parsed Wrap2x2))
data instance C.Parsed Wrap2x2
    = Wrap2x2 
        {mightNotBeReallyEmpty :: (RP.Parsed VerTwoDataTwoPtr)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Wrap2x2))
deriving instance (Std_.Eq (C.Parsed Wrap2x2))
instance (C.Parse Wrap2x2 (C.Parsed Wrap2x2)) where
    parse raw_ = (Wrap2x2 <$> (GH.parseField #mightNotBeReallyEmpty raw_))
instance (C.Marshal Wrap2x2 (C.Parsed Wrap2x2)) where
    marshalInto raw_ Wrap2x2{..} = (do
        (GH.encodeField #mightNotBeReallyEmpty mightNotBeReallyEmpty raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mightNotBeReallyEmpty" GH.Slot Wrap2x2 VerTwoDataTwoPtr) where
    fieldByLabel  = (GH.ptrField 0)
data Wrap2x2plus 
type instance (R.ReprFor Wrap2x2plus) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Wrap2x2plus) where
    typeId  = 16610659964001347673
instance (C.TypedStruct Wrap2x2plus) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Wrap2x2plus) where
    type AllocHint Wrap2x2plus = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Wrap2x2plus (C.Parsed Wrap2x2plus))
instance (C.AllocateList Wrap2x2plus) where
    type ListAllocHint Wrap2x2plus = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Wrap2x2plus (C.Parsed Wrap2x2plus))
data instance C.Parsed Wrap2x2plus
    = Wrap2x2plus 
        {mightNotBeReallyEmpty :: (RP.Parsed VerTwoTwoPlus)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Wrap2x2plus))
deriving instance (Std_.Eq (C.Parsed Wrap2x2plus))
instance (C.Parse Wrap2x2plus (C.Parsed Wrap2x2plus)) where
    parse raw_ = (Wrap2x2plus <$> (GH.parseField #mightNotBeReallyEmpty raw_))
instance (C.Marshal Wrap2x2plus (C.Parsed Wrap2x2plus)) where
    marshalInto raw_ Wrap2x2plus{..} = (do
        (GH.encodeField #mightNotBeReallyEmpty mightNotBeReallyEmpty raw_)
        (Std_.pure ())
        )
instance (GH.HasField "mightNotBeReallyEmpty" GH.Slot Wrap2x2plus VerTwoTwoPlus) where
    fieldByLabel  = (GH.ptrField 0)
data VoidUnion 
type instance (R.ReprFor VoidUnion) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VoidUnion) where
    typeId  = 9809347628687718458
instance (C.TypedStruct VoidUnion) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate VoidUnion) where
    type AllocHint VoidUnion = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VoidUnion (C.Parsed VoidUnion))
instance (C.AllocateList VoidUnion) where
    type ListAllocHint VoidUnion = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VoidUnion (C.Parsed VoidUnion))
data instance C.Parsed VoidUnion
    = VoidUnion 
        {union' :: (C.Parsed (GH.Which VoidUnion))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VoidUnion))
deriving instance (Std_.Eq (C.Parsed VoidUnion))
instance (C.Parse VoidUnion (C.Parsed VoidUnion)) where
    parse raw_ = (VoidUnion <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal VoidUnion (C.Parsed VoidUnion)) where
    marshalInto raw_ VoidUnion{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion VoidUnion) where
    unionField  = (GH.dataField 0 0 16 0)
    data RawWhich VoidUnion mut_
        = RW_VoidUnion'a (R.Raw () mut_)
        | RW_VoidUnion'b (R.Raw () mut_)
        | RW_VoidUnion'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_VoidUnion'a <$> (GH.readVariant #a struct_))
        1 ->
            (RW_VoidUnion'b <$> (GH.readVariant #b struct_))
        _ ->
            (Std_.pure (RW_VoidUnion'unknown' tag_))
    data Which VoidUnion
instance (GH.HasVariant "a" GH.Slot VoidUnion ()) where
    variantByLabel  = (GH.Variant GH.voidField 0)
instance (GH.HasVariant "b" GH.Slot VoidUnion ()) where
    variantByLabel  = (GH.Variant GH.voidField 1)
data instance C.Parsed (GH.Which VoidUnion)
    = VoidUnion'a 
    | VoidUnion'b 
    | VoidUnion'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which VoidUnion)))
deriving instance (Std_.Eq (C.Parsed (GH.Which VoidUnion)))
instance (C.Parse (GH.Which VoidUnion) (C.Parsed (GH.Which VoidUnion))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_VoidUnion'a _) ->
                (Std_.pure VoidUnion'a)
            (RW_VoidUnion'b _) ->
                (Std_.pure VoidUnion'b)
            (RW_VoidUnion'unknown' tag_) ->
                (Std_.pure (VoidUnion'unknown' tag_))
        )
instance (C.Marshal (GH.Which VoidUnion) (C.Parsed (GH.Which VoidUnion))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (VoidUnion'a) ->
            (GH.encodeVariant #a () (GH.unionStruct raw_))
        (VoidUnion'b) ->
            (GH.encodeVariant #b () (GH.unionStruct raw_))
        (VoidUnion'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data Nester1Capn 
type instance (R.ReprFor Nester1Capn) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Nester1Capn) where
    typeId  = 17388306941580478492
instance (C.TypedStruct Nester1Capn) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Nester1Capn) where
    type AllocHint Nester1Capn = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Nester1Capn (C.Parsed Nester1Capn))
instance (C.AllocateList Nester1Capn) where
    type ListAllocHint Nester1Capn = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Nester1Capn (C.Parsed Nester1Capn))
data instance C.Parsed Nester1Capn
    = Nester1Capn 
        {strs :: (RP.Parsed (R.List Basics.Text))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Nester1Capn))
deriving instance (Std_.Eq (C.Parsed Nester1Capn))
instance (C.Parse Nester1Capn (C.Parsed Nester1Capn)) where
    parse raw_ = (Nester1Capn <$> (GH.parseField #strs raw_))
instance (C.Marshal Nester1Capn (C.Parsed Nester1Capn)) where
    marshalInto raw_ Nester1Capn{..} = (do
        (GH.encodeField #strs strs raw_)
        (Std_.pure ())
        )
instance (GH.HasField "strs" GH.Slot Nester1Capn (R.List Basics.Text)) where
    fieldByLabel  = (GH.ptrField 0)
data RWTestCapn 
type instance (R.ReprFor RWTestCapn) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId RWTestCapn) where
    typeId  = 17870076700317718634
instance (C.TypedStruct RWTestCapn) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate RWTestCapn) where
    type AllocHint RWTestCapn = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc RWTestCapn (C.Parsed RWTestCapn))
instance (C.AllocateList RWTestCapn) where
    type ListAllocHint RWTestCapn = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc RWTestCapn (C.Parsed RWTestCapn))
data instance C.Parsed RWTestCapn
    = RWTestCapn 
        {nestMatrix :: (RP.Parsed (R.List (R.List Nester1Capn)))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed RWTestCapn))
deriving instance (Std_.Eq (C.Parsed RWTestCapn))
instance (C.Parse RWTestCapn (C.Parsed RWTestCapn)) where
    parse raw_ = (RWTestCapn <$> (GH.parseField #nestMatrix raw_))
instance (C.Marshal RWTestCapn (C.Parsed RWTestCapn)) where
    marshalInto raw_ RWTestCapn{..} = (do
        (GH.encodeField #nestMatrix nestMatrix raw_)
        (Std_.pure ())
        )
instance (GH.HasField "nestMatrix" GH.Slot RWTestCapn (R.List (R.List Nester1Capn))) where
    fieldByLabel  = (GH.ptrField 0)
data ListStructCapn 
type instance (R.ReprFor ListStructCapn) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId ListStructCapn) where
    typeId  = 12802613814325702673
instance (C.TypedStruct ListStructCapn) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate ListStructCapn) where
    type AllocHint ListStructCapn = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc ListStructCapn (C.Parsed ListStructCapn))
instance (C.AllocateList ListStructCapn) where
    type ListAllocHint ListStructCapn = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc ListStructCapn (C.Parsed ListStructCapn))
data instance C.Parsed ListStructCapn
    = ListStructCapn 
        {vec :: (RP.Parsed (R.List Nester1Capn))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed ListStructCapn))
deriving instance (Std_.Eq (C.Parsed ListStructCapn))
instance (C.Parse ListStructCapn (C.Parsed ListStructCapn)) where
    parse raw_ = (ListStructCapn <$> (GH.parseField #vec raw_))
instance (C.Marshal ListStructCapn (C.Parsed ListStructCapn)) where
    marshalInto raw_ ListStructCapn{..} = (do
        (GH.encodeField #vec vec raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vec" GH.Slot ListStructCapn (R.List Nester1Capn)) where
    fieldByLabel  = (GH.ptrField 0)
data Echo 
type instance (R.ReprFor Echo) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Echo) where
    typeId  = 10255578992688506164
instance (C.Parse Echo (GH.Client Echo)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Echo) where
    type Server Echo = Echo'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Echo)) [(GH.toUntypedMethodHandler ((echo'echo) s_))] [])
class (Echo'server_ s_) where
    {-# MINIMAL echo'echo #-}
    echo'echo :: s_ -> (GH.MethodHandler Echo'echo'params Echo'echo'results)
    echo'echo _ = GH.methodUnimplemented
instance (GH.HasMethod "echo" Echo Echo'echo'params Echo'echo'results) where
    methodByLabel  = (GH.Method 10255578992688506164 0)
data Echo'echo'params 
type instance (R.ReprFor Echo'echo'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Echo'echo'params) where
    typeId  = 9950245657029374882
instance (C.TypedStruct Echo'echo'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Echo'echo'params) where
    type AllocHint Echo'echo'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Echo'echo'params (C.Parsed Echo'echo'params))
instance (C.AllocateList Echo'echo'params) where
    type ListAllocHint Echo'echo'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Echo'echo'params (C.Parsed Echo'echo'params))
data instance C.Parsed Echo'echo'params
    = Echo'echo'params 
        {in_ :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Echo'echo'params))
deriving instance (Std_.Eq (C.Parsed Echo'echo'params))
instance (C.Parse Echo'echo'params (C.Parsed Echo'echo'params)) where
    parse raw_ = (Echo'echo'params <$> (GH.parseField #in_ raw_))
instance (C.Marshal Echo'echo'params (C.Parsed Echo'echo'params)) where
    marshalInto raw_ Echo'echo'params{..} = (do
        (GH.encodeField #in_ in_ raw_)
        (Std_.pure ())
        )
instance (GH.HasField "in_" GH.Slot Echo'echo'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Echo'echo'results 
type instance (R.ReprFor Echo'echo'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Echo'echo'results) where
    typeId  = 11184644773809847197
instance (C.TypedStruct Echo'echo'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Echo'echo'results) where
    type AllocHint Echo'echo'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Echo'echo'results (C.Parsed Echo'echo'results))
instance (C.AllocateList Echo'echo'results) where
    type ListAllocHint Echo'echo'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Echo'echo'results (C.Parsed Echo'echo'results))
data instance C.Parsed Echo'echo'results
    = Echo'echo'results 
        {out :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Echo'echo'results))
deriving instance (Std_.Eq (C.Parsed Echo'echo'results))
instance (C.Parse Echo'echo'results (C.Parsed Echo'echo'results)) where
    parse raw_ = (Echo'echo'results <$> (GH.parseField #out raw_))
instance (C.Marshal Echo'echo'results (C.Parsed Echo'echo'results)) where
    marshalInto raw_ Echo'echo'results{..} = (do
        (GH.encodeField #out out raw_)
        (Std_.pure ())
        )
instance (GH.HasField "out" GH.Slot Echo'echo'results Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Hoth 
type instance (R.ReprFor Hoth) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Hoth) where
    typeId  = 12504202882178935737
instance (C.TypedStruct Hoth) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Hoth) where
    type AllocHint Hoth = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Hoth (C.Parsed Hoth))
instance (C.AllocateList Hoth) where
    type ListAllocHint Hoth = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Hoth (C.Parsed Hoth))
data instance C.Parsed Hoth
    = Hoth 
        {base :: (RP.Parsed EchoBase)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Hoth))
deriving instance (Std_.Eq (C.Parsed Hoth))
instance (C.Parse Hoth (C.Parsed Hoth)) where
    parse raw_ = (Hoth <$> (GH.parseField #base raw_))
instance (C.Marshal Hoth (C.Parsed Hoth)) where
    marshalInto raw_ Hoth{..} = (do
        (GH.encodeField #base base raw_)
        (Std_.pure ())
        )
instance (GH.HasField "base" GH.Slot Hoth EchoBase) where
    fieldByLabel  = (GH.ptrField 0)
data EchoBase 
type instance (R.ReprFor EchoBase) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId EchoBase) where
    typeId  = 12159459504633104486
instance (C.TypedStruct EchoBase) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate EchoBase) where
    type AllocHint EchoBase = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc EchoBase (C.Parsed EchoBase))
instance (C.AllocateList EchoBase) where
    type ListAllocHint EchoBase = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc EchoBase (C.Parsed EchoBase))
data instance C.Parsed EchoBase
    = EchoBase 
        {echo :: (RP.Parsed Echo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed EchoBase))
deriving instance (Std_.Eq (C.Parsed EchoBase))
instance (C.Parse EchoBase (C.Parsed EchoBase)) where
    parse raw_ = (EchoBase <$> (GH.parseField #echo raw_))
instance (C.Marshal EchoBase (C.Parsed EchoBase)) where
    marshalInto raw_ EchoBase{..} = (do
        (GH.encodeField #echo echo raw_)
        (Std_.pure ())
        )
instance (GH.HasField "echo" GH.Slot EchoBase Echo) where
    fieldByLabel  = (GH.ptrField 0)
data EchoBases 
type instance (R.ReprFor EchoBases) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId EchoBases) where
    typeId  = 13848178635387355324
instance (C.TypedStruct EchoBases) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate EchoBases) where
    type AllocHint EchoBases = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc EchoBases (C.Parsed EchoBases))
instance (C.AllocateList EchoBases) where
    type ListAllocHint EchoBases = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc EchoBases (C.Parsed EchoBases))
data instance C.Parsed EchoBases
    = EchoBases 
        {bases :: (RP.Parsed (R.List EchoBase))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed EchoBases))
deriving instance (Std_.Eq (C.Parsed EchoBases))
instance (C.Parse EchoBases (C.Parsed EchoBases)) where
    parse raw_ = (EchoBases <$> (GH.parseField #bases raw_))
instance (C.Marshal EchoBases (C.Parsed EchoBases)) where
    marshalInto raw_ EchoBases{..} = (do
        (GH.encodeField #bases bases raw_)
        (Std_.pure ())
        )
instance (GH.HasField "bases" GH.Slot EchoBases (R.List EchoBase)) where
    fieldByLabel  = (GH.ptrField 0)
data StackingRoot 
type instance (R.ReprFor StackingRoot) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId StackingRoot) where
    typeId  = 10353348115798411408
instance (C.TypedStruct StackingRoot) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate StackingRoot) where
    type AllocHint StackingRoot = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc StackingRoot (C.Parsed StackingRoot))
instance (C.AllocateList StackingRoot) where
    type ListAllocHint StackingRoot = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc StackingRoot (C.Parsed StackingRoot))
data instance C.Parsed StackingRoot
    = StackingRoot 
        {aWithDefault :: (RP.Parsed StackingA)
        ,a :: (RP.Parsed StackingA)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed StackingRoot))
deriving instance (Std_.Eq (C.Parsed StackingRoot))
instance (C.Parse StackingRoot (C.Parsed StackingRoot)) where
    parse raw_ = (StackingRoot <$> (GH.parseField #aWithDefault raw_)
                               <*> (GH.parseField #a raw_))
instance (C.Marshal StackingRoot (C.Parsed StackingRoot)) where
    marshalInto raw_ StackingRoot{..} = (do
        (GH.encodeField #aWithDefault aWithDefault raw_)
        (GH.encodeField #a a raw_)
        (Std_.pure ())
        )
instance (GH.HasField "aWithDefault" GH.Slot StackingRoot StackingA) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "a" GH.Slot StackingRoot StackingA) where
    fieldByLabel  = (GH.ptrField 1)
data StackingA 
type instance (R.ReprFor StackingA) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId StackingA) where
    typeId  = 11326609135883271029
instance (C.TypedStruct StackingA) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate StackingA) where
    type AllocHint StackingA = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc StackingA (C.Parsed StackingA))
instance (C.AllocateList StackingA) where
    type ListAllocHint StackingA = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc StackingA (C.Parsed StackingA))
data instance C.Parsed StackingA
    = StackingA 
        {num :: (RP.Parsed Std_.Int32)
        ,b :: (RP.Parsed StackingB)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed StackingA))
deriving instance (Std_.Eq (C.Parsed StackingA))
instance (C.Parse StackingA (C.Parsed StackingA)) where
    parse raw_ = (StackingA <$> (GH.parseField #num raw_)
                            <*> (GH.parseField #b raw_))
instance (C.Marshal StackingA (C.Parsed StackingA)) where
    marshalInto raw_ StackingA{..} = (do
        (GH.encodeField #num num raw_)
        (GH.encodeField #b b raw_)
        (Std_.pure ())
        )
instance (GH.HasField "num" GH.Slot StackingA Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "b" GH.Slot StackingA StackingB) where
    fieldByLabel  = (GH.ptrField 0)
data StackingB 
type instance (R.ReprFor StackingB) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId StackingB) where
    typeId  = 9594210030877276357
instance (C.TypedStruct StackingB) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate StackingB) where
    type AllocHint StackingB = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc StackingB (C.Parsed StackingB))
instance (C.AllocateList StackingB) where
    type ListAllocHint StackingB = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc StackingB (C.Parsed StackingB))
data instance C.Parsed StackingB
    = StackingB 
        {num :: (RP.Parsed Std_.Int32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed StackingB))
deriving instance (Std_.Eq (C.Parsed StackingB))
instance (C.Parse StackingB (C.Parsed StackingB)) where
    parse raw_ = (StackingB <$> (GH.parseField #num raw_))
instance (C.Marshal StackingB (C.Parsed StackingB)) where
    marshalInto raw_ StackingB{..} = (do
        (GH.encodeField #num num raw_)
        (Std_.pure ())
        )
instance (GH.HasField "num" GH.Slot StackingB Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
data CallSequence 
type instance (R.ReprFor CallSequence) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId CallSequence) where
    typeId  = 12371070827563042848
instance (C.Parse CallSequence (GH.Client CallSequence)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export CallSequence) where
    type Server CallSequence = CallSequence'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(CallSequence)) [(GH.toUntypedMethodHandler ((callSequence'getNumber) s_))] [])
class (CallSequence'server_ s_) where
    {-# MINIMAL callSequence'getNumber #-}
    callSequence'getNumber :: s_ -> (GH.MethodHandler CallSequence'getNumber'params CallSequence'getNumber'results)
    callSequence'getNumber _ = GH.methodUnimplemented
instance (GH.HasMethod "getNumber" CallSequence CallSequence'getNumber'params CallSequence'getNumber'results) where
    methodByLabel  = (GH.Method 12371070827563042848 0)
data CallSequence'getNumber'params 
type instance (R.ReprFor CallSequence'getNumber'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CallSequence'getNumber'params) where
    typeId  = 17692253647948355992
instance (C.TypedStruct CallSequence'getNumber'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate CallSequence'getNumber'params) where
    type AllocHint CallSequence'getNumber'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CallSequence'getNumber'params (C.Parsed CallSequence'getNumber'params))
instance (C.AllocateList CallSequence'getNumber'params) where
    type ListAllocHint CallSequence'getNumber'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CallSequence'getNumber'params (C.Parsed CallSequence'getNumber'params))
data instance C.Parsed CallSequence'getNumber'params
    = CallSequence'getNumber'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CallSequence'getNumber'params))
deriving instance (Std_.Eq (C.Parsed CallSequence'getNumber'params))
instance (C.Parse CallSequence'getNumber'params (C.Parsed CallSequence'getNumber'params)) where
    parse raw_ = (Std_.pure CallSequence'getNumber'params)
instance (C.Marshal CallSequence'getNumber'params (C.Parsed CallSequence'getNumber'params)) where
    marshalInto _raw (CallSequence'getNumber'params) = (Std_.pure ())
data CallSequence'getNumber'results 
type instance (R.ReprFor CallSequence'getNumber'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CallSequence'getNumber'results) where
    typeId  = 11846148517662891671
instance (C.TypedStruct CallSequence'getNumber'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate CallSequence'getNumber'results) where
    type AllocHint CallSequence'getNumber'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CallSequence'getNumber'results (C.Parsed CallSequence'getNumber'results))
instance (C.AllocateList CallSequence'getNumber'results) where
    type ListAllocHint CallSequence'getNumber'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CallSequence'getNumber'results (C.Parsed CallSequence'getNumber'results))
data instance C.Parsed CallSequence'getNumber'results
    = CallSequence'getNumber'results 
        {n :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CallSequence'getNumber'results))
deriving instance (Std_.Eq (C.Parsed CallSequence'getNumber'results))
instance (C.Parse CallSequence'getNumber'results (C.Parsed CallSequence'getNumber'results)) where
    parse raw_ = (CallSequence'getNumber'results <$> (GH.parseField #n raw_))
instance (C.Marshal CallSequence'getNumber'results (C.Parsed CallSequence'getNumber'results)) where
    marshalInto raw_ CallSequence'getNumber'results{..} = (do
        (GH.encodeField #n n raw_)
        (Std_.pure ())
        )
instance (GH.HasField "n" GH.Slot CallSequence'getNumber'results Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
data CounterFactory 
type instance (R.ReprFor CounterFactory) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId CounterFactory) where
    typeId  = 15610220054254702620
instance (C.Parse CounterFactory (GH.Client CounterFactory)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export CounterFactory) where
    type Server CounterFactory = CounterFactory'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(CounterFactory)) [(GH.toUntypedMethodHandler ((counterFactory'newCounter) s_))] [])
class (CounterFactory'server_ s_) where
    {-# MINIMAL counterFactory'newCounter #-}
    counterFactory'newCounter :: s_ -> (GH.MethodHandler CounterFactory'newCounter'params CounterFactory'newCounter'results)
    counterFactory'newCounter _ = GH.methodUnimplemented
instance (GH.HasMethod "newCounter" CounterFactory CounterFactory'newCounter'params CounterFactory'newCounter'results) where
    methodByLabel  = (GH.Method 15610220054254702620 0)
data CounterFactory'newCounter'params 
type instance (R.ReprFor CounterFactory'newCounter'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CounterFactory'newCounter'params) where
    typeId  = 17606593106159030387
instance (C.TypedStruct CounterFactory'newCounter'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate CounterFactory'newCounter'params) where
    type AllocHint CounterFactory'newCounter'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CounterFactory'newCounter'params (C.Parsed CounterFactory'newCounter'params))
instance (C.AllocateList CounterFactory'newCounter'params) where
    type ListAllocHint CounterFactory'newCounter'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CounterFactory'newCounter'params (C.Parsed CounterFactory'newCounter'params))
data instance C.Parsed CounterFactory'newCounter'params
    = CounterFactory'newCounter'params 
        {start :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CounterFactory'newCounter'params))
deriving instance (Std_.Eq (C.Parsed CounterFactory'newCounter'params))
instance (C.Parse CounterFactory'newCounter'params (C.Parsed CounterFactory'newCounter'params)) where
    parse raw_ = (CounterFactory'newCounter'params <$> (GH.parseField #start raw_))
instance (C.Marshal CounterFactory'newCounter'params (C.Parsed CounterFactory'newCounter'params)) where
    marshalInto raw_ CounterFactory'newCounter'params{..} = (do
        (GH.encodeField #start start raw_)
        (Std_.pure ())
        )
instance (GH.HasField "start" GH.Slot CounterFactory'newCounter'params Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
data CounterFactory'newCounter'results 
type instance (R.ReprFor CounterFactory'newCounter'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CounterFactory'newCounter'results) where
    typeId  = 14063453475556553460
instance (C.TypedStruct CounterFactory'newCounter'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate CounterFactory'newCounter'results) where
    type AllocHint CounterFactory'newCounter'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CounterFactory'newCounter'results (C.Parsed CounterFactory'newCounter'results))
instance (C.AllocateList CounterFactory'newCounter'results) where
    type ListAllocHint CounterFactory'newCounter'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CounterFactory'newCounter'results (C.Parsed CounterFactory'newCounter'results))
data instance C.Parsed CounterFactory'newCounter'results
    = CounterFactory'newCounter'results 
        {counter :: (RP.Parsed CallSequence)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CounterFactory'newCounter'results))
deriving instance (Std_.Eq (C.Parsed CounterFactory'newCounter'results))
instance (C.Parse CounterFactory'newCounter'results (C.Parsed CounterFactory'newCounter'results)) where
    parse raw_ = (CounterFactory'newCounter'results <$> (GH.parseField #counter raw_))
instance (C.Marshal CounterFactory'newCounter'results (C.Parsed CounterFactory'newCounter'results)) where
    marshalInto raw_ CounterFactory'newCounter'results{..} = (do
        (GH.encodeField #counter counter raw_)
        (Std_.pure ())
        )
instance (GH.HasField "counter" GH.Slot CounterFactory'newCounter'results CallSequence) where
    fieldByLabel  = (GH.ptrField 0)
data CounterAcceptor 
type instance (R.ReprFor CounterAcceptor) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId CounterAcceptor) where
    typeId  = 14317498215560924065
instance (C.Parse CounterAcceptor (GH.Client CounterAcceptor)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export CounterAcceptor) where
    type Server CounterAcceptor = CounterAcceptor'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(CounterAcceptor)) [(GH.toUntypedMethodHandler ((counterAcceptor'accept) s_))] [])
class (CounterAcceptor'server_ s_) where
    {-# MINIMAL counterAcceptor'accept #-}
    counterAcceptor'accept :: s_ -> (GH.MethodHandler CounterAcceptor'accept'params CounterAcceptor'accept'results)
    counterAcceptor'accept _ = GH.methodUnimplemented
instance (GH.HasMethod "accept" CounterAcceptor CounterAcceptor'accept'params CounterAcceptor'accept'results) where
    methodByLabel  = (GH.Method 14317498215560924065 0)
data CounterAcceptor'accept'params 
type instance (R.ReprFor CounterAcceptor'accept'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CounterAcceptor'accept'params) where
    typeId  = 9397955985493553485
instance (C.TypedStruct CounterAcceptor'accept'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate CounterAcceptor'accept'params) where
    type AllocHint CounterAcceptor'accept'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CounterAcceptor'accept'params (C.Parsed CounterAcceptor'accept'params))
instance (C.AllocateList CounterAcceptor'accept'params) where
    type ListAllocHint CounterAcceptor'accept'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CounterAcceptor'accept'params (C.Parsed CounterAcceptor'accept'params))
data instance C.Parsed CounterAcceptor'accept'params
    = CounterAcceptor'accept'params 
        {counter :: (RP.Parsed CallSequence)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CounterAcceptor'accept'params))
deriving instance (Std_.Eq (C.Parsed CounterAcceptor'accept'params))
instance (C.Parse CounterAcceptor'accept'params (C.Parsed CounterAcceptor'accept'params)) where
    parse raw_ = (CounterAcceptor'accept'params <$> (GH.parseField #counter raw_))
instance (C.Marshal CounterAcceptor'accept'params (C.Parsed CounterAcceptor'accept'params)) where
    marshalInto raw_ CounterAcceptor'accept'params{..} = (do
        (GH.encodeField #counter counter raw_)
        (Std_.pure ())
        )
instance (GH.HasField "counter" GH.Slot CounterAcceptor'accept'params CallSequence) where
    fieldByLabel  = (GH.ptrField 0)
data CounterAcceptor'accept'results 
type instance (R.ReprFor CounterAcceptor'accept'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId CounterAcceptor'accept'results) where
    typeId  = 12945664473818918456
instance (C.TypedStruct CounterAcceptor'accept'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate CounterAcceptor'accept'results) where
    type AllocHint CounterAcceptor'accept'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc CounterAcceptor'accept'results (C.Parsed CounterAcceptor'accept'results))
instance (C.AllocateList CounterAcceptor'accept'results) where
    type ListAllocHint CounterAcceptor'accept'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc CounterAcceptor'accept'results (C.Parsed CounterAcceptor'accept'results))
data instance C.Parsed CounterAcceptor'accept'results
    = CounterAcceptor'accept'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed CounterAcceptor'accept'results))
deriving instance (Std_.Eq (C.Parsed CounterAcceptor'accept'results))
instance (C.Parse CounterAcceptor'accept'results (C.Parsed CounterAcceptor'accept'results)) where
    parse raw_ = (Std_.pure CounterAcceptor'accept'results)
instance (C.Marshal CounterAcceptor'accept'results (C.Parsed CounterAcceptor'accept'results)) where
    marshalInto _raw (CounterAcceptor'accept'results) = (Std_.pure ())
data Top 
type instance (R.ReprFor Top) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Top) where
    typeId  = 17861645508359101525
instance (C.Parse Top (GH.Client Top)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Top) where
    type Server Top = Top'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Top)) [(GH.toUntypedMethodHandler ((top'top) s_))] [])
class (Top'server_ s_) where
    {-# MINIMAL top'top #-}
    top'top :: s_ -> (GH.MethodHandler Top'top'params Top'top'results)
    top'top _ = GH.methodUnimplemented
instance (GH.HasMethod "top" Top Top'top'params Top'top'results) where
    methodByLabel  = (GH.Method 17861645508359101525 0)
data Top'top'params 
type instance (R.ReprFor Top'top'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Top'top'params) where
    typeId  = 9393200598485985531
instance (C.TypedStruct Top'top'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Top'top'params) where
    type AllocHint Top'top'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Top'top'params (C.Parsed Top'top'params))
instance (C.AllocateList Top'top'params) where
    type ListAllocHint Top'top'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Top'top'params (C.Parsed Top'top'params))
data instance C.Parsed Top'top'params
    = Top'top'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Top'top'params))
deriving instance (Std_.Eq (C.Parsed Top'top'params))
instance (C.Parse Top'top'params (C.Parsed Top'top'params)) where
    parse raw_ = (Std_.pure Top'top'params)
instance (C.Marshal Top'top'params (C.Parsed Top'top'params)) where
    marshalInto _raw (Top'top'params) = (Std_.pure ())
data Top'top'results 
type instance (R.ReprFor Top'top'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Top'top'results) where
    typeId  = 15495666596821516773
instance (C.TypedStruct Top'top'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Top'top'results) where
    type AllocHint Top'top'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Top'top'results (C.Parsed Top'top'results))
instance (C.AllocateList Top'top'results) where
    type ListAllocHint Top'top'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Top'top'results (C.Parsed Top'top'results))
data instance C.Parsed Top'top'results
    = Top'top'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Top'top'results))
deriving instance (Std_.Eq (C.Parsed Top'top'results))
instance (C.Parse Top'top'results (C.Parsed Top'top'results)) where
    parse raw_ = (Std_.pure Top'top'results)
instance (C.Marshal Top'top'results (C.Parsed Top'top'results)) where
    marshalInto _raw (Top'top'results) = (Std_.pure ())
data Left 
type instance (R.ReprFor Left) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Left) where
    typeId  = 17679152961798450446
instance (C.Parse Left (GH.Client Left)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Left) where
    type Server Left = Left'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Left)) [(GH.toUntypedMethodHandler ((left'left) s_))] [(GH.methodHandlerTree (GH.Proxy @(Top)) s_)])
class ((GH.Server Top s_)) => (Left'server_ s_) where
    {-# MINIMAL left'left #-}
    left'left :: s_ -> (GH.MethodHandler Left'left'params Left'left'results)
    left'left _ = GH.methodUnimplemented
instance (C.Super Top Left)
instance (GH.HasMethod "left" Left Left'left'params Left'left'results) where
    methodByLabel  = (GH.Method 17679152961798450446 0)
data Left'left'params 
type instance (R.ReprFor Left'left'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Left'left'params) where
    typeId  = 15491006379377963583
instance (C.TypedStruct Left'left'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Left'left'params) where
    type AllocHint Left'left'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Left'left'params (C.Parsed Left'left'params))
instance (C.AllocateList Left'left'params) where
    type ListAllocHint Left'left'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Left'left'params (C.Parsed Left'left'params))
data instance C.Parsed Left'left'params
    = Left'left'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Left'left'params))
deriving instance (Std_.Eq (C.Parsed Left'left'params))
instance (C.Parse Left'left'params (C.Parsed Left'left'params)) where
    parse raw_ = (Std_.pure Left'left'params)
instance (C.Marshal Left'left'params (C.Parsed Left'left'params)) where
    marshalInto _raw (Left'left'params) = (Std_.pure ())
data Left'left'results 
type instance (R.ReprFor Left'left'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Left'left'results) where
    typeId  = 14681384339713198083
instance (C.TypedStruct Left'left'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Left'left'results) where
    type AllocHint Left'left'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Left'left'results (C.Parsed Left'left'results))
instance (C.AllocateList Left'left'results) where
    type ListAllocHint Left'left'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Left'left'results (C.Parsed Left'left'results))
data instance C.Parsed Left'left'results
    = Left'left'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Left'left'results))
deriving instance (Std_.Eq (C.Parsed Left'left'results))
instance (C.Parse Left'left'results (C.Parsed Left'left'results)) where
    parse raw_ = (Std_.pure Left'left'results)
instance (C.Marshal Left'left'results (C.Parsed Left'left'results)) where
    marshalInto _raw (Left'left'results) = (Std_.pure ())
data Right 
type instance (R.ReprFor Right) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Right) where
    typeId  = 14712639595305348988
instance (C.Parse Right (GH.Client Right)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Right) where
    type Server Right = Right'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Right)) [(GH.toUntypedMethodHandler ((right'right) s_))] [(GH.methodHandlerTree (GH.Proxy @(Top)) s_)])
class ((GH.Server Top s_)) => (Right'server_ s_) where
    {-# MINIMAL right'right #-}
    right'right :: s_ -> (GH.MethodHandler Right'right'params Right'right'results)
    right'right _ = GH.methodUnimplemented
instance (C.Super Top Right)
instance (GH.HasMethod "right" Right Right'right'params Right'right'results) where
    methodByLabel  = (GH.Method 14712639595305348988 0)
data Right'right'params 
type instance (R.ReprFor Right'right'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Right'right'params) where
    typeId  = 17692925949004434588
instance (C.TypedStruct Right'right'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Right'right'params) where
    type AllocHint Right'right'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Right'right'params (C.Parsed Right'right'params))
instance (C.AllocateList Right'right'params) where
    type ListAllocHint Right'right'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Right'right'params (C.Parsed Right'right'params))
data instance C.Parsed Right'right'params
    = Right'right'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Right'right'params))
deriving instance (Std_.Eq (C.Parsed Right'right'params))
instance (C.Parse Right'right'params (C.Parsed Right'right'params)) where
    parse raw_ = (Std_.pure Right'right'params)
instance (C.Marshal Right'right'params (C.Parsed Right'right'params)) where
    marshalInto _raw (Right'right'params) = (Std_.pure ())
data Right'right'results 
type instance (R.ReprFor Right'right'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Right'right'results) where
    typeId  = 12438774687365689496
instance (C.TypedStruct Right'right'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Right'right'results) where
    type AllocHint Right'right'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Right'right'results (C.Parsed Right'right'results))
instance (C.AllocateList Right'right'results) where
    type ListAllocHint Right'right'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Right'right'results (C.Parsed Right'right'results))
data instance C.Parsed Right'right'results
    = Right'right'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Right'right'results))
deriving instance (Std_.Eq (C.Parsed Right'right'results))
instance (C.Parse Right'right'results (C.Parsed Right'right'results)) where
    parse raw_ = (Std_.pure Right'right'results)
instance (C.Marshal Right'right'results (C.Parsed Right'right'results)) where
    marshalInto _raw (Right'right'results) = (Std_.pure ())
data Bottom 
type instance (R.ReprFor Bottom) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Bottom) where
    typeId  = 18402872613340521775
instance (C.Parse Bottom (GH.Client Bottom)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Bottom) where
    type Server Bottom = Bottom'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Bottom)) [(GH.toUntypedMethodHandler ((bottom'bottom) s_))] [(GH.methodHandlerTree (GH.Proxy @(Left)) s_)
                                                                                                                           ,(GH.methodHandlerTree (GH.Proxy @(Right)) s_)])
class ((GH.Server Left s_)
      ,(GH.Server Right s_)) => (Bottom'server_ s_) where
    {-# MINIMAL bottom'bottom #-}
    bottom'bottom :: s_ -> (GH.MethodHandler Bottom'bottom'params Bottom'bottom'results)
    bottom'bottom _ = GH.methodUnimplemented
instance (C.Super Left Bottom)
instance (C.Super Right Bottom)
instance (GH.HasMethod "bottom" Bottom Bottom'bottom'params Bottom'bottom'results) where
    methodByLabel  = (GH.Method 18402872613340521775 0)
data Bottom'bottom'params 
type instance (R.ReprFor Bottom'bottom'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Bottom'bottom'params) where
    typeId  = 12919405706404437540
instance (C.TypedStruct Bottom'bottom'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Bottom'bottom'params) where
    type AllocHint Bottom'bottom'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Bottom'bottom'params (C.Parsed Bottom'bottom'params))
instance (C.AllocateList Bottom'bottom'params) where
    type ListAllocHint Bottom'bottom'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Bottom'bottom'params (C.Parsed Bottom'bottom'params))
data instance C.Parsed Bottom'bottom'params
    = Bottom'bottom'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Bottom'bottom'params))
deriving instance (Std_.Eq (C.Parsed Bottom'bottom'params))
instance (C.Parse Bottom'bottom'params (C.Parsed Bottom'bottom'params)) where
    parse raw_ = (Std_.pure Bottom'bottom'params)
instance (C.Marshal Bottom'bottom'params (C.Parsed Bottom'bottom'params)) where
    marshalInto _raw (Bottom'bottom'params) = (Std_.pure ())
data Bottom'bottom'results 
type instance (R.ReprFor Bottom'bottom'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Bottom'bottom'results) where
    typeId  = 10197200620860118778
instance (C.TypedStruct Bottom'bottom'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Bottom'bottom'results) where
    type AllocHint Bottom'bottom'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Bottom'bottom'results (C.Parsed Bottom'bottom'results))
instance (C.AllocateList Bottom'bottom'results) where
    type ListAllocHint Bottom'bottom'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Bottom'bottom'results (C.Parsed Bottom'bottom'results))
data instance C.Parsed Bottom'bottom'results
    = Bottom'bottom'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Bottom'bottom'results))
deriving instance (Std_.Eq (C.Parsed Bottom'bottom'results))
instance (C.Parse Bottom'bottom'results (C.Parsed Bottom'bottom'results)) where
    parse raw_ = (Std_.pure Bottom'bottom'results)
instance (C.Marshal Bottom'bottom'results (C.Parsed Bottom'bottom'results)) where
    marshalInto _raw (Bottom'bottom'results) = (Std_.pure ())
data Defaults 
type instance (R.ReprFor Defaults) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Defaults) where
    typeId  = 10944742465095042957
instance (C.TypedStruct Defaults) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate Defaults) where
    type AllocHint Defaults = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Defaults (C.Parsed Defaults))
instance (C.AllocateList Defaults) where
    type ListAllocHint Defaults = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Defaults (C.Parsed Defaults))
data instance C.Parsed Defaults
    = Defaults 
        {text :: (RP.Parsed Basics.Text)
        ,data_ :: (RP.Parsed Basics.Data)
        ,float :: (RP.Parsed Std_.Float)
        ,int :: (RP.Parsed Std_.Int32)
        ,uint :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Defaults))
deriving instance (Std_.Eq (C.Parsed Defaults))
instance (C.Parse Defaults (C.Parsed Defaults)) where
    parse raw_ = (Defaults <$> (GH.parseField #text raw_)
                           <*> (GH.parseField #data_ raw_)
                           <*> (GH.parseField #float raw_)
                           <*> (GH.parseField #int raw_)
                           <*> (GH.parseField #uint raw_))
instance (C.Marshal Defaults (C.Parsed Defaults)) where
    marshalInto raw_ Defaults{..} = (do
        (GH.encodeField #text text raw_)
        (GH.encodeField #data_ data_ raw_)
        (GH.encodeField #float float raw_)
        (GH.encodeField #int int raw_)
        (GH.encodeField #uint uint raw_)
        (Std_.pure ())
        )
instance (GH.HasField "text" GH.Slot Defaults Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "data_" GH.Slot Defaults Basics.Data) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "float" GH.Slot Defaults Std_.Float) where
    fieldByLabel  = (GH.dataField 0 0 32 1078523331)
instance (GH.HasField "int" GH.Slot Defaults Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 0 32 18446744073709551493)
instance (GH.HasField "uint" GH.Slot Defaults Std_.Word32) where
    fieldByLabel  = (GH.dataField 0 1 32 42)
data BenchmarkA 
type instance (R.ReprFor BenchmarkA) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId BenchmarkA) where
    typeId  = 16008637057130021148
instance (C.TypedStruct BenchmarkA) where
    numStructWords  = 3
    numStructPtrs  = 2
instance (C.Allocate BenchmarkA) where
    type AllocHint BenchmarkA = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc BenchmarkA (C.Parsed BenchmarkA))
instance (C.AllocateList BenchmarkA) where
    type ListAllocHint BenchmarkA = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc BenchmarkA (C.Parsed BenchmarkA))
data instance C.Parsed BenchmarkA
    = BenchmarkA 
        {name :: (RP.Parsed Basics.Text)
        ,birthDay :: (RP.Parsed Std_.Int64)
        ,phone :: (RP.Parsed Basics.Text)
        ,siblings :: (RP.Parsed Std_.Int32)
        ,spouse :: (RP.Parsed Std_.Bool)
        ,money :: (RP.Parsed Std_.Double)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed BenchmarkA))
deriving instance (Std_.Eq (C.Parsed BenchmarkA))
instance (C.Parse BenchmarkA (C.Parsed BenchmarkA)) where
    parse raw_ = (BenchmarkA <$> (GH.parseField #name raw_)
                             <*> (GH.parseField #birthDay raw_)
                             <*> (GH.parseField #phone raw_)
                             <*> (GH.parseField #siblings raw_)
                             <*> (GH.parseField #spouse raw_)
                             <*> (GH.parseField #money raw_))
instance (C.Marshal BenchmarkA (C.Parsed BenchmarkA)) where
    marshalInto raw_ BenchmarkA{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #birthDay birthDay raw_)
        (GH.encodeField #phone phone raw_)
        (GH.encodeField #siblings siblings raw_)
        (GH.encodeField #spouse spouse raw_)
        (GH.encodeField #money money raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot BenchmarkA Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "birthDay" GH.Slot BenchmarkA Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "phone" GH.Slot BenchmarkA Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "siblings" GH.Slot BenchmarkA Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
instance (GH.HasField "spouse" GH.Slot BenchmarkA Std_.Bool) where
    fieldByLabel  = (GH.dataField 32 1 1 0)
instance (GH.HasField "money" GH.Slot BenchmarkA Std_.Double) where
    fieldByLabel  = (GH.dataField 0 2 64 0)