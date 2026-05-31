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
module Capnp.Gen.Common where
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP
import qualified Capnp.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers as GH
import qualified Capnp.Classes as C
import qualified GHC.Generics as Generics
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data EntityRef 
type instance (R.ReprFor EntityRef) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId EntityRef) where
    typeId  = 11224312134546276084
instance (C.TypedStruct EntityRef) where
    numStructWords  = 2
    numStructPtrs  = 1
instance (C.Allocate EntityRef) where
    type AllocHint EntityRef = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc EntityRef (C.Parsed EntityRef))
instance (C.AllocateList EntityRef) where
    type ListAllocHint EntityRef = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc EntityRef (C.Parsed EntityRef))
data instance C.Parsed EntityRef
    = EntityRef 
        {union' :: (C.Parsed (GH.Which EntityRef))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed EntityRef))
deriving instance (Std_.Eq (C.Parsed EntityRef))
instance (C.Parse EntityRef (C.Parsed EntityRef)) where
    parse raw_ = (EntityRef <$> (C.parse (GH.structUnion raw_)))
instance (C.Marshal EntityRef (C.Parsed EntityRef)) where
    marshalInto raw_ EntityRef{..} = (do
        (C.marshalInto (GH.structUnion raw_) union')
        )
instance (GH.HasUnion EntityRef) where
    unionField  = (GH.dataField 0 1 16 0)
    data RawWhich EntityRef mut_
        = RW_EntityRef'id (R.Raw Std_.Int64 mut_)
        | RW_EntityRef'name (R.Raw Basics.Text mut_)
        | RW_EntityRef'unknown' Std_.Word16
    internalWhich tag_ struct_ = case tag_ of
        0 ->
            (RW_EntityRef'id <$> (GH.readVariant #id struct_))
        1 ->
            (RW_EntityRef'name <$> (GH.readVariant #name struct_))
        _ ->
            (Std_.pure (RW_EntityRef'unknown' tag_))
    data Which EntityRef
instance (GH.HasVariant "id" GH.Slot EntityRef Std_.Int64) where
    variantByLabel  = (GH.Variant (GH.dataField 0 0 64 0) 0)
instance (GH.HasVariant "name" GH.Slot EntityRef Basics.Text) where
    variantByLabel  = (GH.Variant (GH.ptrField 0) 1)
data instance C.Parsed (GH.Which EntityRef)
    = EntityRef'id (RP.Parsed Std_.Int64)
    | EntityRef'name (RP.Parsed Basics.Text)
    | EntityRef'unknown' Std_.Word16
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed (GH.Which EntityRef)))
deriving instance (Std_.Eq (C.Parsed (GH.Which EntityRef)))
instance (C.Parse (GH.Which EntityRef) (C.Parsed (GH.Which EntityRef))) where
    parse raw_ = (do
        rawWhich_ <- (GH.unionWhich raw_)
        case rawWhich_ of
            (RW_EntityRef'id rawArg_) ->
                (EntityRef'id <$> (C.parse rawArg_))
            (RW_EntityRef'name rawArg_) ->
                (EntityRef'name <$> (C.parse rawArg_))
            (RW_EntityRef'unknown' tag_) ->
                (Std_.pure (EntityRef'unknown' tag_))
        )
instance (C.Marshal (GH.Which EntityRef) (C.Parsed (GH.Which EntityRef))) where
    marshalInto raw_ parsed_ = case parsed_ of
        (EntityRef'id arg_) ->
            (GH.encodeVariant #id arg_ (GH.unionStruct raw_))
        (EntityRef'name arg_) ->
            (GH.encodeVariant #name arg_ (GH.unionStruct raw_))
        (EntityRef'unknown' tag_) ->
            (GH.encodeField GH.unionField tag_ (GH.unionStruct raw_))
data NamedRef 
type instance (R.ReprFor NamedRef) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NamedRef) where
    typeId  = 9929435162299174186
instance (C.TypedStruct NamedRef) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate NamedRef) where
    type AllocHint NamedRef = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NamedRef (C.Parsed NamedRef))
instance (C.AllocateList NamedRef) where
    type ListAllocHint NamedRef = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NamedRef (C.Parsed NamedRef))
data instance C.Parsed NamedRef
    = NamedRef 
        {id :: (RP.Parsed Std_.Int64)
        ,name :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NamedRef))
deriving instance (Std_.Eq (C.Parsed NamedRef))
instance (C.Parse NamedRef (C.Parsed NamedRef)) where
    parse raw_ = (NamedRef <$> (GH.parseField #id raw_)
                           <*> (GH.parseField #name raw_))
instance (C.Marshal NamedRef (C.Parsed NamedRef)) where
    marshalInto raw_ NamedRef{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #name name raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot NamedRef Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "name" GH.Slot NamedRef Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data StatusInfo 
type instance (R.ReprFor StatusInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId StatusInfo) where
    typeId  = 15446564585122045252
instance (C.TypedStruct StatusInfo) where
    numStructWords  = 2
    numStructPtrs  = 1
instance (C.Allocate StatusInfo) where
    type AllocHint StatusInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc StatusInfo (C.Parsed StatusInfo))
instance (C.AllocateList StatusInfo) where
    type ListAllocHint StatusInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc StatusInfo (C.Parsed StatusInfo))
data instance C.Parsed StatusInfo
    = StatusInfo 
        {uptimeSeconds :: (RP.Parsed Std_.Int64)
        ,connections :: (RP.Parsed Std_.Int32)
        ,version :: (RP.Parsed Basics.Text)
        ,protocolVersion :: (RP.Parsed Std_.Word32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed StatusInfo))
deriving instance (Std_.Eq (C.Parsed StatusInfo))
instance (C.Parse StatusInfo (C.Parsed StatusInfo)) where
    parse raw_ = (StatusInfo <$> (GH.parseField #uptimeSeconds raw_)
                             <*> (GH.parseField #connections raw_)
                             <*> (GH.parseField #version raw_)
                             <*> (GH.parseField #protocolVersion raw_))
instance (C.Marshal StatusInfo (C.Parsed StatusInfo)) where
    marshalInto raw_ StatusInfo{..} = (do
        (GH.encodeField #uptimeSeconds uptimeSeconds raw_)
        (GH.encodeField #connections connections raw_)
        (GH.encodeField #version version raw_)
        (GH.encodeField #protocolVersion protocolVersion raw_)
        (Std_.pure ())
        )
instance (GH.HasField "uptimeSeconds" GH.Slot StatusInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "connections" GH.Slot StatusInfo Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
instance (GH.HasField "version" GH.Slot StatusInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "protocolVersion" GH.Slot StatusInfo Std_.Word32) where
    fieldByLabel  = (GH.dataField 32 1 32 0)
data ViewGrant 
type instance (R.ReprFor ViewGrant) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId ViewGrant) where
    typeId  = 12119798709984641960
instance (C.TypedStruct ViewGrant) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate ViewGrant) where
    type AllocHint ViewGrant = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc ViewGrant (C.Parsed ViewGrant))
instance (C.AllocateList ViewGrant) where
    type ListAllocHint ViewGrant = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc ViewGrant (C.Parsed ViewGrant))
data instance C.Parsed ViewGrant
    = ViewGrant 
        {host :: (RP.Parsed Basics.Text)
        ,port :: (RP.Parsed Std_.Int32)
        ,password :: (RP.Parsed Basics.Text)
        ,ttlSeconds :: (RP.Parsed Std_.Int32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed ViewGrant))
deriving instance (Std_.Eq (C.Parsed ViewGrant))
instance (C.Parse ViewGrant (C.Parsed ViewGrant)) where
    parse raw_ = (ViewGrant <$> (GH.parseField #host raw_)
                            <*> (GH.parseField #port raw_)
                            <*> (GH.parseField #password raw_)
                            <*> (GH.parseField #ttlSeconds raw_))
instance (C.Marshal ViewGrant (C.Parsed ViewGrant)) where
    marshalInto raw_ ViewGrant{..} = (do
        (GH.encodeField #host host raw_)
        (GH.encodeField #port port raw_)
        (GH.encodeField #password password raw_)
        (GH.encodeField #ttlSeconds ttlSeconds raw_)
        (Std_.pure ())
        )
instance (GH.HasField "host" GH.Slot ViewGrant Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "port" GH.Slot ViewGrant Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "password" GH.Slot ViewGrant Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "ttlSeconds" GH.Slot ViewGrant Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)