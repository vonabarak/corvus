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
module Capnp.Gen.Disk where
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP
import qualified Capnp.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers as GH
import qualified Capnp.Classes as C
import qualified GHC.Generics as Generics
import qualified Capnp.GenHelpers.Rpc as GH
import qualified Capnp.Gen.ById.X9b1373e2334a09e9
import qualified Capnp.Gen.ById.Xbf9b09f64c0dd40d
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data DiskImageInfo 
type instance (R.ReprFor DiskImageInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskImageInfo) where
    typeId  = 14053829785942251641
instance (C.TypedStruct DiskImageInfo) where
    numStructWords  = 5
    numStructPtrs  = 4
instance (C.Allocate DiskImageInfo) where
    type AllocHint DiskImageInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskImageInfo (C.Parsed DiskImageInfo))
instance (C.AllocateList DiskImageInfo) where
    type ListAllocHint DiskImageInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskImageInfo (C.Parsed DiskImageInfo))
data instance C.Parsed DiskImageInfo
    = DiskImageInfo 
        {id :: (RP.Parsed Std_.Int64)
        ,name :: (RP.Parsed Basics.Text)
        ,placements :: (RP.Parsed (R.List DiskImagePlacement))
        ,format :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveFormat)
        ,sizeMb :: (RP.Parsed Std_.Int64)
        ,createdAt :: (RP.Parsed Std_.Int64)
        ,attachedTo :: (RP.Parsed (R.List DiskAttachment))
        ,backingImageId :: (RP.Parsed Std_.Int64)
        ,backingImageName :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskImageInfo))
deriving instance (Std_.Eq (C.Parsed DiskImageInfo))
instance (C.Parse DiskImageInfo (C.Parsed DiskImageInfo)) where
    parse raw_ = (DiskImageInfo <$> (GH.parseField #id raw_)
                                <*> (GH.parseField #name raw_)
                                <*> (GH.parseField #placements raw_)
                                <*> (GH.parseField #format raw_)
                                <*> (GH.parseField #sizeMb raw_)
                                <*> (GH.parseField #createdAt raw_)
                                <*> (GH.parseField #attachedTo raw_)
                                <*> (GH.parseField #backingImageId raw_)
                                <*> (GH.parseField #backingImageName raw_))
instance (C.Marshal DiskImageInfo (C.Parsed DiskImageInfo)) where
    marshalInto raw_ DiskImageInfo{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #placements placements raw_)
        (GH.encodeField #format format raw_)
        (GH.encodeField #sizeMb sizeMb raw_)
        (GH.encodeField #createdAt createdAt raw_)
        (GH.encodeField #attachedTo attachedTo raw_)
        (GH.encodeField #backingImageId backingImageId raw_)
        (GH.encodeField #backingImageName backingImageName raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot DiskImageInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "name" GH.Slot DiskImageInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "placements" GH.Slot DiskImageInfo (R.List DiskImagePlacement)) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "format" GH.Slot DiskImageInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveFormat) where
    fieldByLabel  = (GH.dataField 0 1 16 0)
instance (GH.HasField "sizeMb" GH.Slot DiskImageInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
instance (GH.HasField "createdAt" GH.Slot DiskImageInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 3 64 0)
instance (GH.HasField "attachedTo" GH.Slot DiskImageInfo (R.List DiskAttachment)) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "backingImageId" GH.Slot DiskImageInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 4 64 0)
instance (GH.HasField "backingImageName" GH.Slot DiskImageInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 3)
data DiskImagePlacement 
type instance (R.ReprFor DiskImagePlacement) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskImagePlacement) where
    typeId  = 17704711645275117274
instance (C.TypedStruct DiskImagePlacement) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate DiskImagePlacement) where
    type AllocHint DiskImagePlacement = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskImagePlacement (C.Parsed DiskImagePlacement))
instance (C.AllocateList DiskImagePlacement) where
    type ListAllocHint DiskImagePlacement = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskImagePlacement (C.Parsed DiskImagePlacement))
data instance C.Parsed DiskImagePlacement
    = DiskImagePlacement 
        {nodeId :: (RP.Parsed Std_.Int64)
        ,nodeName :: (RP.Parsed Basics.Text)
        ,filePath :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskImagePlacement))
deriving instance (Std_.Eq (C.Parsed DiskImagePlacement))
instance (C.Parse DiskImagePlacement (C.Parsed DiskImagePlacement)) where
    parse raw_ = (DiskImagePlacement <$> (GH.parseField #nodeId raw_)
                                     <*> (GH.parseField #nodeName raw_)
                                     <*> (GH.parseField #filePath raw_))
instance (C.Marshal DiskImagePlacement (C.Parsed DiskImagePlacement)) where
    marshalInto raw_ DiskImagePlacement{..} = (do
        (GH.encodeField #nodeId nodeId raw_)
        (GH.encodeField #nodeName nodeName raw_)
        (GH.encodeField #filePath filePath raw_)
        (Std_.pure ())
        )
instance (GH.HasField "nodeId" GH.Slot DiskImagePlacement Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "nodeName" GH.Slot DiskImagePlacement Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "filePath" GH.Slot DiskImagePlacement Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
data DiskAttachment 
type instance (R.ReprFor DiskAttachment) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskAttachment) where
    typeId  = 16768680145487968521
instance (C.TypedStruct DiskAttachment) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate DiskAttachment) where
    type AllocHint DiskAttachment = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskAttachment (C.Parsed DiskAttachment))
instance (C.AllocateList DiskAttachment) where
    type ListAllocHint DiskAttachment = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskAttachment (C.Parsed DiskAttachment))
data instance C.Parsed DiskAttachment
    = DiskAttachment 
        {vmId :: (RP.Parsed Std_.Int64)
        ,vmName :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskAttachment))
deriving instance (Std_.Eq (C.Parsed DiskAttachment))
instance (C.Parse DiskAttachment (C.Parsed DiskAttachment)) where
    parse raw_ = (DiskAttachment <$> (GH.parseField #vmId raw_)
                                 <*> (GH.parseField #vmName raw_))
instance (C.Marshal DiskAttachment (C.Parsed DiskAttachment)) where
    marshalInto raw_ DiskAttachment{..} = (do
        (GH.encodeField #vmId vmId raw_)
        (GH.encodeField #vmName vmName raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vmId" GH.Slot DiskAttachment Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "vmName" GH.Slot DiskAttachment Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data SnapshotInfo 
type instance (R.ReprFor SnapshotInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId SnapshotInfo) where
    typeId  = 12234667223317650273
instance (C.TypedStruct SnapshotInfo) where
    numStructWords  = 3
    numStructPtrs  = 1
instance (C.Allocate SnapshotInfo) where
    type AllocHint SnapshotInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc SnapshotInfo (C.Parsed SnapshotInfo))
instance (C.AllocateList SnapshotInfo) where
    type ListAllocHint SnapshotInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc SnapshotInfo (C.Parsed SnapshotInfo))
data instance C.Parsed SnapshotInfo
    = SnapshotInfo 
        {id :: (RP.Parsed Std_.Int64)
        ,name :: (RP.Parsed Basics.Text)
        ,createdAt :: (RP.Parsed Std_.Int64)
        ,sizeMb :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed SnapshotInfo))
deriving instance (Std_.Eq (C.Parsed SnapshotInfo))
instance (C.Parse SnapshotInfo (C.Parsed SnapshotInfo)) where
    parse raw_ = (SnapshotInfo <$> (GH.parseField #id raw_)
                               <*> (GH.parseField #name raw_)
                               <*> (GH.parseField #createdAt raw_)
                               <*> (GH.parseField #sizeMb raw_))
instance (C.Marshal SnapshotInfo (C.Parsed SnapshotInfo)) where
    marshalInto raw_ SnapshotInfo{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #createdAt createdAt raw_)
        (GH.encodeField #sizeMb sizeMb raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot SnapshotInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "name" GH.Slot SnapshotInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "createdAt" GH.Slot SnapshotInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "sizeMb" GH.Slot SnapshotInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
data DiskCreateParams 
type instance (R.ReprFor DiskCreateParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskCreateParams) where
    typeId  = 16178903223577522352
instance (C.TypedStruct DiskCreateParams) where
    numStructWords  = 2
    numStructPtrs  = 1
instance (C.Allocate DiskCreateParams) where
    type AllocHint DiskCreateParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskCreateParams (C.Parsed DiskCreateParams))
instance (C.AllocateList DiskCreateParams) where
    type ListAllocHint DiskCreateParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskCreateParams (C.Parsed DiskCreateParams))
data instance C.Parsed DiskCreateParams
    = DiskCreateParams 
        {name :: (RP.Parsed Basics.Text)
        ,sizeMb :: (RP.Parsed Std_.Int64)
        ,format :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveFormat)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskCreateParams))
deriving instance (Std_.Eq (C.Parsed DiskCreateParams))
instance (C.Parse DiskCreateParams (C.Parsed DiskCreateParams)) where
    parse raw_ = (DiskCreateParams <$> (GH.parseField #name raw_)
                                   <*> (GH.parseField #sizeMb raw_)
                                   <*> (GH.parseField #format raw_))
instance (C.Marshal DiskCreateParams (C.Parsed DiskCreateParams)) where
    marshalInto raw_ DiskCreateParams{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #sizeMb sizeMb raw_)
        (GH.encodeField #format format raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot DiskCreateParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "sizeMb" GH.Slot DiskCreateParams Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "format" GH.Slot DiskCreateParams Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveFormat) where
    fieldByLabel  = (GH.dataField 0 1 16 0)
data DiskRegisterParams 
type instance (R.ReprFor DiskRegisterParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskRegisterParams) where
    typeId  = 17240446815621875179
instance (C.TypedStruct DiskRegisterParams) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate DiskRegisterParams) where
    type AllocHint DiskRegisterParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskRegisterParams (C.Parsed DiskRegisterParams))
instance (C.AllocateList DiskRegisterParams) where
    type ListAllocHint DiskRegisterParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskRegisterParams (C.Parsed DiskRegisterParams))
data instance C.Parsed DiskRegisterParams
    = DiskRegisterParams 
        {name :: (RP.Parsed Basics.Text)
        ,filePath :: (RP.Parsed Basics.Text)
        ,format :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveFormat)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskRegisterParams))
deriving instance (Std_.Eq (C.Parsed DiskRegisterParams))
instance (C.Parse DiskRegisterParams (C.Parsed DiskRegisterParams)) where
    parse raw_ = (DiskRegisterParams <$> (GH.parseField #name raw_)
                                     <*> (GH.parseField #filePath raw_)
                                     <*> (GH.parseField #format raw_))
instance (C.Marshal DiskRegisterParams (C.Parsed DiskRegisterParams)) where
    marshalInto raw_ DiskRegisterParams{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #filePath filePath raw_)
        (GH.encodeField #format format raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot DiskRegisterParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "filePath" GH.Slot DiskRegisterParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "format" GH.Slot DiskRegisterParams Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveFormat) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
data DiskCreateOverlayParams 
type instance (R.ReprFor DiskCreateOverlayParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskCreateOverlayParams) where
    typeId  = 17207359398972913415
instance (C.TypedStruct DiskCreateOverlayParams) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate DiskCreateOverlayParams) where
    type AllocHint DiskCreateOverlayParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskCreateOverlayParams (C.Parsed DiskCreateOverlayParams))
instance (C.AllocateList DiskCreateOverlayParams) where
    type ListAllocHint DiskCreateOverlayParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskCreateOverlayParams (C.Parsed DiskCreateOverlayParams))
data instance C.Parsed DiskCreateOverlayParams
    = DiskCreateOverlayParams 
        {name :: (RP.Parsed Basics.Text)
        ,backingDiskRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskCreateOverlayParams))
deriving instance (Std_.Eq (C.Parsed DiskCreateOverlayParams))
instance (C.Parse DiskCreateOverlayParams (C.Parsed DiskCreateOverlayParams)) where
    parse raw_ = (DiskCreateOverlayParams <$> (GH.parseField #name raw_)
                                          <*> (GH.parseField #backingDiskRef raw_))
instance (C.Marshal DiskCreateOverlayParams (C.Parsed DiskCreateOverlayParams)) where
    marshalInto raw_ DiskCreateOverlayParams{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #backingDiskRef backingDiskRef raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot DiskCreateOverlayParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "backingDiskRef" GH.Slot DiskCreateOverlayParams Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 1)
data DiskCloneParams 
type instance (R.ReprFor DiskCloneParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskCloneParams) where
    typeId  = 9630073188204636874
instance (C.TypedStruct DiskCloneParams) where
    numStructWords  = 0
    numStructPtrs  = 3
instance (C.Allocate DiskCloneParams) where
    type AllocHint DiskCloneParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskCloneParams (C.Parsed DiskCloneParams))
instance (C.AllocateList DiskCloneParams) where
    type ListAllocHint DiskCloneParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskCloneParams (C.Parsed DiskCloneParams))
data instance C.Parsed DiskCloneParams
    = DiskCloneParams 
        {sourceRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)
        ,newName :: (RP.Parsed Basics.Text)
        ,path :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskCloneParams))
deriving instance (Std_.Eq (C.Parsed DiskCloneParams))
instance (C.Parse DiskCloneParams (C.Parsed DiskCloneParams)) where
    parse raw_ = (DiskCloneParams <$> (GH.parseField #sourceRef raw_)
                                  <*> (GH.parseField #newName raw_)
                                  <*> (GH.parseField #path raw_))
instance (C.Marshal DiskCloneParams (C.Parsed DiskCloneParams)) where
    marshalInto raw_ DiskCloneParams{..} = (do
        (GH.encodeField #sourceRef sourceRef raw_)
        (GH.encodeField #newName newName raw_)
        (GH.encodeField #path path raw_)
        (Std_.pure ())
        )
instance (GH.HasField "sourceRef" GH.Slot DiskCloneParams Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "newName" GH.Slot DiskCloneParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "path" GH.Slot DiskCloneParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
data DiskRebaseParams 
type instance (R.ReprFor DiskRebaseParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskRebaseParams) where
    typeId  = 16985596159810866101
instance (C.TypedStruct DiskRebaseParams) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate DiskRebaseParams) where
    type AllocHint DiskRebaseParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskRebaseParams (C.Parsed DiskRebaseParams))
instance (C.AllocateList DiskRebaseParams) where
    type ListAllocHint DiskRebaseParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskRebaseParams (C.Parsed DiskRebaseParams))
data instance C.Parsed DiskRebaseParams
    = DiskRebaseParams 
        {diskRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)
        ,newBackingDiskRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskRebaseParams))
deriving instance (Std_.Eq (C.Parsed DiskRebaseParams))
instance (C.Parse DiskRebaseParams (C.Parsed DiskRebaseParams)) where
    parse raw_ = (DiskRebaseParams <$> (GH.parseField #diskRef raw_)
                                   <*> (GH.parseField #newBackingDiskRef raw_))
instance (C.Marshal DiskRebaseParams (C.Parsed DiskRebaseParams)) where
    marshalInto raw_ DiskRebaseParams{..} = (do
        (GH.encodeField #diskRef diskRef raw_)
        (GH.encodeField #newBackingDiskRef newBackingDiskRef raw_)
        (Std_.pure ())
        )
instance (GH.HasField "diskRef" GH.Slot DiskRebaseParams Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "newBackingDiskRef" GH.Slot DiskRebaseParams Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 1)
data DiskImportUrlParams 
type instance (R.ReprFor DiskImportUrlParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskImportUrlParams) where
    typeId  = 13524245572784434207
instance (C.TypedStruct DiskImportUrlParams) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate DiskImportUrlParams) where
    type AllocHint DiskImportUrlParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskImportUrlParams (C.Parsed DiskImportUrlParams))
instance (C.AllocateList DiskImportUrlParams) where
    type ListAllocHint DiskImportUrlParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskImportUrlParams (C.Parsed DiskImportUrlParams))
data instance C.Parsed DiskImportUrlParams
    = DiskImportUrlParams 
        {name :: (RP.Parsed Basics.Text)
        ,url :: (RP.Parsed Basics.Text)
        ,format :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveFormat)
        ,sizeMb :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskImportUrlParams))
deriving instance (Std_.Eq (C.Parsed DiskImportUrlParams))
instance (C.Parse DiskImportUrlParams (C.Parsed DiskImportUrlParams)) where
    parse raw_ = (DiskImportUrlParams <$> (GH.parseField #name raw_)
                                      <*> (GH.parseField #url raw_)
                                      <*> (GH.parseField #format raw_)
                                      <*> (GH.parseField #sizeMb raw_))
instance (C.Marshal DiskImportUrlParams (C.Parsed DiskImportUrlParams)) where
    marshalInto raw_ DiskImportUrlParams{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #url url raw_)
        (GH.encodeField #format format raw_)
        (GH.encodeField #sizeMb sizeMb raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot DiskImportUrlParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "url" GH.Slot DiskImportUrlParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "format" GH.Slot DiskImportUrlParams Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveFormat) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "sizeMb" GH.Slot DiskImportUrlParams Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
data DiskImportParams 
type instance (R.ReprFor DiskImportParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskImportParams) where
    typeId  = 16137633644853007213
instance (C.TypedStruct DiskImportParams) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate DiskImportParams) where
    type AllocHint DiskImportParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskImportParams (C.Parsed DiskImportParams))
instance (C.AllocateList DiskImportParams) where
    type ListAllocHint DiskImportParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskImportParams (C.Parsed DiskImportParams))
data instance C.Parsed DiskImportParams
    = DiskImportParams 
        {name :: (RP.Parsed Basics.Text)
        ,srcPath :: (RP.Parsed Basics.Text)
        ,format :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveFormat)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskImportParams))
deriving instance (Std_.Eq (C.Parsed DiskImportParams))
instance (C.Parse DiskImportParams (C.Parsed DiskImportParams)) where
    parse raw_ = (DiskImportParams <$> (GH.parseField #name raw_)
                                   <*> (GH.parseField #srcPath raw_)
                                   <*> (GH.parseField #format raw_))
instance (C.Marshal DiskImportParams (C.Parsed DiskImportParams)) where
    marshalInto raw_ DiskImportParams{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #srcPath srcPath raw_)
        (GH.encodeField #format format raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot DiskImportParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "srcPath" GH.Slot DiskImportParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "format" GH.Slot DiskImportParams Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveFormat) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
data DiskCopyParams 
type instance (R.ReprFor DiskCopyParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskCopyParams) where
    typeId  = 10673387965178120780
instance (C.TypedStruct DiskCopyParams) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate DiskCopyParams) where
    type AllocHint DiskCopyParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskCopyParams (C.Parsed DiskCopyParams))
instance (C.AllocateList DiskCopyParams) where
    type ListAllocHint DiskCopyParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskCopyParams (C.Parsed DiskCopyParams))
data instance C.Parsed DiskCopyParams
    = DiskCopyParams 
        {diskRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)
        ,toNodeRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskCopyParams))
deriving instance (Std_.Eq (C.Parsed DiskCopyParams))
instance (C.Parse DiskCopyParams (C.Parsed DiskCopyParams)) where
    parse raw_ = (DiskCopyParams <$> (GH.parseField #diskRef raw_)
                                 <*> (GH.parseField #toNodeRef raw_))
instance (C.Marshal DiskCopyParams (C.Parsed DiskCopyParams)) where
    marshalInto raw_ DiskCopyParams{..} = (do
        (GH.encodeField #diskRef diskRef raw_)
        (GH.encodeField #toNodeRef toNodeRef raw_)
        (Std_.pure ())
        )
instance (GH.HasField "diskRef" GH.Slot DiskCopyParams Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "toNodeRef" GH.Slot DiskCopyParams Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 1)
data DiskMoveParams 
type instance (R.ReprFor DiskMoveParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskMoveParams) where
    typeId  = 12946892671933591168
instance (C.TypedStruct DiskMoveParams) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate DiskMoveParams) where
    type AllocHint DiskMoveParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskMoveParams (C.Parsed DiskMoveParams))
instance (C.AllocateList DiskMoveParams) where
    type ListAllocHint DiskMoveParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskMoveParams (C.Parsed DiskMoveParams))
data instance C.Parsed DiskMoveParams
    = DiskMoveParams 
        {diskRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)
        ,toNodeRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskMoveParams))
deriving instance (Std_.Eq (C.Parsed DiskMoveParams))
instance (C.Parse DiskMoveParams (C.Parsed DiskMoveParams)) where
    parse raw_ = (DiskMoveParams <$> (GH.parseField #diskRef raw_)
                                 <*> (GH.parseField #toNodeRef raw_))
instance (C.Marshal DiskMoveParams (C.Parsed DiskMoveParams)) where
    marshalInto raw_ DiskMoveParams{..} = (do
        (GH.encodeField #diskRef diskRef raw_)
        (GH.encodeField #toNodeRef toNodeRef raw_)
        (Std_.pure ())
        )
instance (GH.HasField "diskRef" GH.Slot DiskMoveParams Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "toNodeRef" GH.Slot DiskMoveParams Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 1)
data DiskManager 
type instance (R.ReprFor DiskManager) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId DiskManager) where
    typeId  = 14751763957337118315
instance (C.Parse DiskManager (GH.Client DiskManager)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export DiskManager) where
    type Server DiskManager = DiskManager'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(DiskManager)) [(GH.toUntypedMethodHandler ((diskManager'list) s_))
                                                                             ,(GH.toUntypedMethodHandler ((diskManager'get) s_))
                                                                             ,(GH.toUntypedMethodHandler ((diskManager'create) s_))
                                                                             ,(GH.toUntypedMethodHandler ((diskManager'register) s_))
                                                                             ,(GH.toUntypedMethodHandler ((diskManager'createOverlay) s_))
                                                                             ,(GH.toUntypedMethodHandler ((diskManager'clone) s_))
                                                                             ,(GH.toUntypedMethodHandler ((diskManager'rebase) s_))
                                                                             ,(GH.toUntypedMethodHandler ((diskManager'importUrl) s_))
                                                                             ,(GH.toUntypedMethodHandler ((diskManager'import_) s_))
                                                                             ,(GH.toUntypedMethodHandler ((diskManager'flatten) s_))
                                                                             ,(GH.toUntypedMethodHandler ((diskManager'copy) s_))
                                                                             ,(GH.toUntypedMethodHandler ((diskManager'move) s_))] [])
class (DiskManager'server_ s_) where
    {-# MINIMAL diskManager'list,diskManager'get,diskManager'create,diskManager'register,diskManager'createOverlay,diskManager'clone,diskManager'rebase,diskManager'importUrl,diskManager'import_,diskManager'flatten,diskManager'copy,diskManager'move #-}
    diskManager'list :: s_ -> (GH.MethodHandler DiskManager'list'params DiskManager'list'results)
    diskManager'list _ = GH.methodUnimplemented
    diskManager'get :: s_ -> (GH.MethodHandler DiskManager'get'params DiskManager'get'results)
    diskManager'get _ = GH.methodUnimplemented
    diskManager'create :: s_ -> (GH.MethodHandler DiskManager'create'params DiskManager'create'results)
    diskManager'create _ = GH.methodUnimplemented
    diskManager'register :: s_ -> (GH.MethodHandler DiskManager'register'params DiskManager'register'results)
    diskManager'register _ = GH.methodUnimplemented
    diskManager'createOverlay :: s_ -> (GH.MethodHandler DiskManager'createOverlay'params DiskManager'createOverlay'results)
    diskManager'createOverlay _ = GH.methodUnimplemented
    diskManager'clone :: s_ -> (GH.MethodHandler DiskManager'clone'params DiskManager'clone'results)
    diskManager'clone _ = GH.methodUnimplemented
    diskManager'rebase :: s_ -> (GH.MethodHandler DiskManager'rebase'params DiskManager'rebase'results)
    diskManager'rebase _ = GH.methodUnimplemented
    diskManager'importUrl :: s_ -> (GH.MethodHandler DiskManager'importUrl'params DiskManager'importUrl'results)
    diskManager'importUrl _ = GH.methodUnimplemented
    diskManager'import_ :: s_ -> (GH.MethodHandler DiskManager'import'params DiskManager'import'results)
    diskManager'import_ _ = GH.methodUnimplemented
    diskManager'flatten :: s_ -> (GH.MethodHandler DiskManager'flatten'params DiskManager'flatten'results)
    diskManager'flatten _ = GH.methodUnimplemented
    diskManager'copy :: s_ -> (GH.MethodHandler DiskManager'copy'params DiskManager'copy'results)
    diskManager'copy _ = GH.methodUnimplemented
    diskManager'move :: s_ -> (GH.MethodHandler DiskManager'move'params DiskManager'move'results)
    diskManager'move _ = GH.methodUnimplemented
instance (GH.HasMethod "list" DiskManager DiskManager'list'params DiskManager'list'results) where
    methodByLabel  = (GH.Method 14751763957337118315 0)
instance (GH.HasMethod "get" DiskManager DiskManager'get'params DiskManager'get'results) where
    methodByLabel  = (GH.Method 14751763957337118315 1)
instance (GH.HasMethod "create" DiskManager DiskManager'create'params DiskManager'create'results) where
    methodByLabel  = (GH.Method 14751763957337118315 2)
instance (GH.HasMethod "register" DiskManager DiskManager'register'params DiskManager'register'results) where
    methodByLabel  = (GH.Method 14751763957337118315 3)
instance (GH.HasMethod "createOverlay" DiskManager DiskManager'createOverlay'params DiskManager'createOverlay'results) where
    methodByLabel  = (GH.Method 14751763957337118315 4)
instance (GH.HasMethod "clone" DiskManager DiskManager'clone'params DiskManager'clone'results) where
    methodByLabel  = (GH.Method 14751763957337118315 5)
instance (GH.HasMethod "rebase" DiskManager DiskManager'rebase'params DiskManager'rebase'results) where
    methodByLabel  = (GH.Method 14751763957337118315 6)
instance (GH.HasMethod "importUrl" DiskManager DiskManager'importUrl'params DiskManager'importUrl'results) where
    methodByLabel  = (GH.Method 14751763957337118315 7)
instance (GH.HasMethod "import_" DiskManager DiskManager'import'params DiskManager'import'results) where
    methodByLabel  = (GH.Method 14751763957337118315 8)
instance (GH.HasMethod "flatten" DiskManager DiskManager'flatten'params DiskManager'flatten'results) where
    methodByLabel  = (GH.Method 14751763957337118315 9)
instance (GH.HasMethod "copy" DiskManager DiskManager'copy'params DiskManager'copy'results) where
    methodByLabel  = (GH.Method 14751763957337118315 10)
instance (GH.HasMethod "move" DiskManager DiskManager'move'params DiskManager'move'results) where
    methodByLabel  = (GH.Method 14751763957337118315 11)
data DiskManager'list'params 
type instance (R.ReprFor DiskManager'list'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'list'params) where
    typeId  = 11274078221496753728
instance (C.TypedStruct DiskManager'list'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate DiskManager'list'params) where
    type AllocHint DiskManager'list'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'list'params (C.Parsed DiskManager'list'params))
instance (C.AllocateList DiskManager'list'params) where
    type ListAllocHint DiskManager'list'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'list'params (C.Parsed DiskManager'list'params))
data instance C.Parsed DiskManager'list'params
    = DiskManager'list'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'list'params))
deriving instance (Std_.Eq (C.Parsed DiskManager'list'params))
instance (C.Parse DiskManager'list'params (C.Parsed DiskManager'list'params)) where
    parse raw_ = (Std_.pure DiskManager'list'params)
instance (C.Marshal DiskManager'list'params (C.Parsed DiskManager'list'params)) where
    marshalInto _raw (DiskManager'list'params) = (Std_.pure ())
data DiskManager'list'results 
type instance (R.ReprFor DiskManager'list'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'list'results) where
    typeId  = 17921627094505845108
instance (C.TypedStruct DiskManager'list'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'list'results) where
    type AllocHint DiskManager'list'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'list'results (C.Parsed DiskManager'list'results))
instance (C.AllocateList DiskManager'list'results) where
    type ListAllocHint DiskManager'list'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'list'results (C.Parsed DiskManager'list'results))
data instance C.Parsed DiskManager'list'results
    = DiskManager'list'results 
        {disks :: (RP.Parsed (R.List DiskImageInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'list'results))
deriving instance (Std_.Eq (C.Parsed DiskManager'list'results))
instance (C.Parse DiskManager'list'results (C.Parsed DiskManager'list'results)) where
    parse raw_ = (DiskManager'list'results <$> (GH.parseField #disks raw_))
instance (C.Marshal DiskManager'list'results (C.Parsed DiskManager'list'results)) where
    marshalInto raw_ DiskManager'list'results{..} = (do
        (GH.encodeField #disks disks raw_)
        (Std_.pure ())
        )
instance (GH.HasField "disks" GH.Slot DiskManager'list'results (R.List DiskImageInfo)) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'get'params 
type instance (R.ReprFor DiskManager'get'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'get'params) where
    typeId  = 10187011872311452638
instance (C.TypedStruct DiskManager'get'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'get'params) where
    type AllocHint DiskManager'get'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'get'params (C.Parsed DiskManager'get'params))
instance (C.AllocateList DiskManager'get'params) where
    type ListAllocHint DiskManager'get'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'get'params (C.Parsed DiskManager'get'params))
data instance C.Parsed DiskManager'get'params
    = DiskManager'get'params 
        {ref :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'get'params))
deriving instance (Std_.Eq (C.Parsed DiskManager'get'params))
instance (C.Parse DiskManager'get'params (C.Parsed DiskManager'get'params)) where
    parse raw_ = (DiskManager'get'params <$> (GH.parseField #ref raw_))
instance (C.Marshal DiskManager'get'params (C.Parsed DiskManager'get'params)) where
    marshalInto raw_ DiskManager'get'params{..} = (do
        (GH.encodeField #ref ref raw_)
        (Std_.pure ())
        )
instance (GH.HasField "ref" GH.Slot DiskManager'get'params Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'get'results 
type instance (R.ReprFor DiskManager'get'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'get'results) where
    typeId  = 17227882542503472307
instance (C.TypedStruct DiskManager'get'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'get'results) where
    type AllocHint DiskManager'get'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'get'results (C.Parsed DiskManager'get'results))
instance (C.AllocateList DiskManager'get'results) where
    type ListAllocHint DiskManager'get'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'get'results (C.Parsed DiskManager'get'results))
data instance C.Parsed DiskManager'get'results
    = DiskManager'get'results 
        {disk :: (RP.Parsed Disk)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'get'results))
deriving instance (Std_.Eq (C.Parsed DiskManager'get'results))
instance (C.Parse DiskManager'get'results (C.Parsed DiskManager'get'results)) where
    parse raw_ = (DiskManager'get'results <$> (GH.parseField #disk raw_))
instance (C.Marshal DiskManager'get'results (C.Parsed DiskManager'get'results)) where
    marshalInto raw_ DiskManager'get'results{..} = (do
        (GH.encodeField #disk disk raw_)
        (Std_.pure ())
        )
instance (GH.HasField "disk" GH.Slot DiskManager'get'results Disk) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'create'params 
type instance (R.ReprFor DiskManager'create'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'create'params) where
    typeId  = 9243994220839124909
instance (C.TypedStruct DiskManager'create'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'create'params) where
    type AllocHint DiskManager'create'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'create'params (C.Parsed DiskManager'create'params))
instance (C.AllocateList DiskManager'create'params) where
    type ListAllocHint DiskManager'create'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'create'params (C.Parsed DiskManager'create'params))
data instance C.Parsed DiskManager'create'params
    = DiskManager'create'params 
        {params :: (RP.Parsed DiskCreateParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'create'params))
deriving instance (Std_.Eq (C.Parsed DiskManager'create'params))
instance (C.Parse DiskManager'create'params (C.Parsed DiskManager'create'params)) where
    parse raw_ = (DiskManager'create'params <$> (GH.parseField #params raw_))
instance (C.Marshal DiskManager'create'params (C.Parsed DiskManager'create'params)) where
    marshalInto raw_ DiskManager'create'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot DiskManager'create'params DiskCreateParams) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'create'results 
type instance (R.ReprFor DiskManager'create'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'create'results) where
    typeId  = 17034464103355732000
instance (C.TypedStruct DiskManager'create'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'create'results) where
    type AllocHint DiskManager'create'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'create'results (C.Parsed DiskManager'create'results))
instance (C.AllocateList DiskManager'create'results) where
    type ListAllocHint DiskManager'create'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'create'results (C.Parsed DiskManager'create'results))
data instance C.Parsed DiskManager'create'results
    = DiskManager'create'results 
        {disk :: (RP.Parsed Disk)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'create'results))
deriving instance (Std_.Eq (C.Parsed DiskManager'create'results))
instance (C.Parse DiskManager'create'results (C.Parsed DiskManager'create'results)) where
    parse raw_ = (DiskManager'create'results <$> (GH.parseField #disk raw_))
instance (C.Marshal DiskManager'create'results (C.Parsed DiskManager'create'results)) where
    marshalInto raw_ DiskManager'create'results{..} = (do
        (GH.encodeField #disk disk raw_)
        (Std_.pure ())
        )
instance (GH.HasField "disk" GH.Slot DiskManager'create'results Disk) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'register'params 
type instance (R.ReprFor DiskManager'register'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'register'params) where
    typeId  = 13322999700191829903
instance (C.TypedStruct DiskManager'register'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'register'params) where
    type AllocHint DiskManager'register'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'register'params (C.Parsed DiskManager'register'params))
instance (C.AllocateList DiskManager'register'params) where
    type ListAllocHint DiskManager'register'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'register'params (C.Parsed DiskManager'register'params))
data instance C.Parsed DiskManager'register'params
    = DiskManager'register'params 
        {params :: (RP.Parsed DiskRegisterParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'register'params))
deriving instance (Std_.Eq (C.Parsed DiskManager'register'params))
instance (C.Parse DiskManager'register'params (C.Parsed DiskManager'register'params)) where
    parse raw_ = (DiskManager'register'params <$> (GH.parseField #params raw_))
instance (C.Marshal DiskManager'register'params (C.Parsed DiskManager'register'params)) where
    marshalInto raw_ DiskManager'register'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot DiskManager'register'params DiskRegisterParams) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'register'results 
type instance (R.ReprFor DiskManager'register'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'register'results) where
    typeId  = 13518716184100279746
instance (C.TypedStruct DiskManager'register'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'register'results) where
    type AllocHint DiskManager'register'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'register'results (C.Parsed DiskManager'register'results))
instance (C.AllocateList DiskManager'register'results) where
    type ListAllocHint DiskManager'register'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'register'results (C.Parsed DiskManager'register'results))
data instance C.Parsed DiskManager'register'results
    = DiskManager'register'results 
        {disk :: (RP.Parsed Disk)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'register'results))
deriving instance (Std_.Eq (C.Parsed DiskManager'register'results))
instance (C.Parse DiskManager'register'results (C.Parsed DiskManager'register'results)) where
    parse raw_ = (DiskManager'register'results <$> (GH.parseField #disk raw_))
instance (C.Marshal DiskManager'register'results (C.Parsed DiskManager'register'results)) where
    marshalInto raw_ DiskManager'register'results{..} = (do
        (GH.encodeField #disk disk raw_)
        (Std_.pure ())
        )
instance (GH.HasField "disk" GH.Slot DiskManager'register'results Disk) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'createOverlay'params 
type instance (R.ReprFor DiskManager'createOverlay'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'createOverlay'params) where
    typeId  = 15323789314642229717
instance (C.TypedStruct DiskManager'createOverlay'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'createOverlay'params) where
    type AllocHint DiskManager'createOverlay'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'createOverlay'params (C.Parsed DiskManager'createOverlay'params))
instance (C.AllocateList DiskManager'createOverlay'params) where
    type ListAllocHint DiskManager'createOverlay'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'createOverlay'params (C.Parsed DiskManager'createOverlay'params))
data instance C.Parsed DiskManager'createOverlay'params
    = DiskManager'createOverlay'params 
        {params :: (RP.Parsed DiskCreateOverlayParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'createOverlay'params))
deriving instance (Std_.Eq (C.Parsed DiskManager'createOverlay'params))
instance (C.Parse DiskManager'createOverlay'params (C.Parsed DiskManager'createOverlay'params)) where
    parse raw_ = (DiskManager'createOverlay'params <$> (GH.parseField #params raw_))
instance (C.Marshal DiskManager'createOverlay'params (C.Parsed DiskManager'createOverlay'params)) where
    marshalInto raw_ DiskManager'createOverlay'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot DiskManager'createOverlay'params DiskCreateOverlayParams) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'createOverlay'results 
type instance (R.ReprFor DiskManager'createOverlay'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'createOverlay'results) where
    typeId  = 17957279606171688436
instance (C.TypedStruct DiskManager'createOverlay'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'createOverlay'results) where
    type AllocHint DiskManager'createOverlay'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'createOverlay'results (C.Parsed DiskManager'createOverlay'results))
instance (C.AllocateList DiskManager'createOverlay'results) where
    type ListAllocHint DiskManager'createOverlay'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'createOverlay'results (C.Parsed DiskManager'createOverlay'results))
data instance C.Parsed DiskManager'createOverlay'results
    = DiskManager'createOverlay'results 
        {disk :: (RP.Parsed Disk)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'createOverlay'results))
deriving instance (Std_.Eq (C.Parsed DiskManager'createOverlay'results))
instance (C.Parse DiskManager'createOverlay'results (C.Parsed DiskManager'createOverlay'results)) where
    parse raw_ = (DiskManager'createOverlay'results <$> (GH.parseField #disk raw_))
instance (C.Marshal DiskManager'createOverlay'results (C.Parsed DiskManager'createOverlay'results)) where
    marshalInto raw_ DiskManager'createOverlay'results{..} = (do
        (GH.encodeField #disk disk raw_)
        (Std_.pure ())
        )
instance (GH.HasField "disk" GH.Slot DiskManager'createOverlay'results Disk) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'clone'params 
type instance (R.ReprFor DiskManager'clone'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'clone'params) where
    typeId  = 15780787854605125006
instance (C.TypedStruct DiskManager'clone'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'clone'params) where
    type AllocHint DiskManager'clone'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'clone'params (C.Parsed DiskManager'clone'params))
instance (C.AllocateList DiskManager'clone'params) where
    type ListAllocHint DiskManager'clone'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'clone'params (C.Parsed DiskManager'clone'params))
data instance C.Parsed DiskManager'clone'params
    = DiskManager'clone'params 
        {params :: (RP.Parsed DiskCloneParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'clone'params))
deriving instance (Std_.Eq (C.Parsed DiskManager'clone'params))
instance (C.Parse DiskManager'clone'params (C.Parsed DiskManager'clone'params)) where
    parse raw_ = (DiskManager'clone'params <$> (GH.parseField #params raw_))
instance (C.Marshal DiskManager'clone'params (C.Parsed DiskManager'clone'params)) where
    marshalInto raw_ DiskManager'clone'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot DiskManager'clone'params DiskCloneParams) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'clone'results 
type instance (R.ReprFor DiskManager'clone'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'clone'results) where
    typeId  = 9360768069093452595
instance (C.TypedStruct DiskManager'clone'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'clone'results) where
    type AllocHint DiskManager'clone'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'clone'results (C.Parsed DiskManager'clone'results))
instance (C.AllocateList DiskManager'clone'results) where
    type ListAllocHint DiskManager'clone'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'clone'results (C.Parsed DiskManager'clone'results))
data instance C.Parsed DiskManager'clone'results
    = DiskManager'clone'results 
        {disk :: (RP.Parsed Disk)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'clone'results))
deriving instance (Std_.Eq (C.Parsed DiskManager'clone'results))
instance (C.Parse DiskManager'clone'results (C.Parsed DiskManager'clone'results)) where
    parse raw_ = (DiskManager'clone'results <$> (GH.parseField #disk raw_))
instance (C.Marshal DiskManager'clone'results (C.Parsed DiskManager'clone'results)) where
    marshalInto raw_ DiskManager'clone'results{..} = (do
        (GH.encodeField #disk disk raw_)
        (Std_.pure ())
        )
instance (GH.HasField "disk" GH.Slot DiskManager'clone'results Disk) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'rebase'params 
type instance (R.ReprFor DiskManager'rebase'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'rebase'params) where
    typeId  = 12341671180160284263
instance (C.TypedStruct DiskManager'rebase'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'rebase'params) where
    type AllocHint DiskManager'rebase'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'rebase'params (C.Parsed DiskManager'rebase'params))
instance (C.AllocateList DiskManager'rebase'params) where
    type ListAllocHint DiskManager'rebase'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'rebase'params (C.Parsed DiskManager'rebase'params))
data instance C.Parsed DiskManager'rebase'params
    = DiskManager'rebase'params 
        {params :: (RP.Parsed DiskRebaseParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'rebase'params))
deriving instance (Std_.Eq (C.Parsed DiskManager'rebase'params))
instance (C.Parse DiskManager'rebase'params (C.Parsed DiskManager'rebase'params)) where
    parse raw_ = (DiskManager'rebase'params <$> (GH.parseField #params raw_))
instance (C.Marshal DiskManager'rebase'params (C.Parsed DiskManager'rebase'params)) where
    marshalInto raw_ DiskManager'rebase'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot DiskManager'rebase'params DiskRebaseParams) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'rebase'results 
type instance (R.ReprFor DiskManager'rebase'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'rebase'results) where
    typeId  = 17133670312857880257
instance (C.TypedStruct DiskManager'rebase'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate DiskManager'rebase'results) where
    type AllocHint DiskManager'rebase'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'rebase'results (C.Parsed DiskManager'rebase'results))
instance (C.AllocateList DiskManager'rebase'results) where
    type ListAllocHint DiskManager'rebase'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'rebase'results (C.Parsed DiskManager'rebase'results))
data instance C.Parsed DiskManager'rebase'results
    = DiskManager'rebase'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'rebase'results))
deriving instance (Std_.Eq (C.Parsed DiskManager'rebase'results))
instance (C.Parse DiskManager'rebase'results (C.Parsed DiskManager'rebase'results)) where
    parse raw_ = (Std_.pure DiskManager'rebase'results)
instance (C.Marshal DiskManager'rebase'results (C.Parsed DiskManager'rebase'results)) where
    marshalInto _raw (DiskManager'rebase'results) = (Std_.pure ())
data DiskManager'importUrl'params 
type instance (R.ReprFor DiskManager'importUrl'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'importUrl'params) where
    typeId  = 16883505006148718011
instance (C.TypedStruct DiskManager'importUrl'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'importUrl'params) where
    type AllocHint DiskManager'importUrl'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'importUrl'params (C.Parsed DiskManager'importUrl'params))
instance (C.AllocateList DiskManager'importUrl'params) where
    type ListAllocHint DiskManager'importUrl'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'importUrl'params (C.Parsed DiskManager'importUrl'params))
data instance C.Parsed DiskManager'importUrl'params
    = DiskManager'importUrl'params 
        {params :: (RP.Parsed DiskImportUrlParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'importUrl'params))
deriving instance (Std_.Eq (C.Parsed DiskManager'importUrl'params))
instance (C.Parse DiskManager'importUrl'params (C.Parsed DiskManager'importUrl'params)) where
    parse raw_ = (DiskManager'importUrl'params <$> (GH.parseField #params raw_))
instance (C.Marshal DiskManager'importUrl'params (C.Parsed DiskManager'importUrl'params)) where
    marshalInto raw_ DiskManager'importUrl'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot DiskManager'importUrl'params DiskImportUrlParams) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'importUrl'results 
type instance (R.ReprFor DiskManager'importUrl'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'importUrl'results) where
    typeId  = 16277257290701250735
instance (C.TypedStruct DiskManager'importUrl'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate DiskManager'importUrl'results) where
    type AllocHint DiskManager'importUrl'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'importUrl'results (C.Parsed DiskManager'importUrl'results))
instance (C.AllocateList DiskManager'importUrl'results) where
    type ListAllocHint DiskManager'importUrl'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'importUrl'results (C.Parsed DiskManager'importUrl'results))
data instance C.Parsed DiskManager'importUrl'results
    = DiskManager'importUrl'results 
        {taskId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'importUrl'results))
deriving instance (Std_.Eq (C.Parsed DiskManager'importUrl'results))
instance (C.Parse DiskManager'importUrl'results (C.Parsed DiskManager'importUrl'results)) where
    parse raw_ = (DiskManager'importUrl'results <$> (GH.parseField #taskId raw_))
instance (C.Marshal DiskManager'importUrl'results (C.Parsed DiskManager'importUrl'results)) where
    marshalInto raw_ DiskManager'importUrl'results{..} = (do
        (GH.encodeField #taskId taskId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "taskId" GH.Slot DiskManager'importUrl'results Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data DiskManager'import'params 
type instance (R.ReprFor DiskManager'import'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'import'params) where
    typeId  = 9928129343169362524
instance (C.TypedStruct DiskManager'import'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'import'params) where
    type AllocHint DiskManager'import'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'import'params (C.Parsed DiskManager'import'params))
instance (C.AllocateList DiskManager'import'params) where
    type ListAllocHint DiskManager'import'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'import'params (C.Parsed DiskManager'import'params))
data instance C.Parsed DiskManager'import'params
    = DiskManager'import'params 
        {params :: (RP.Parsed DiskImportParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'import'params))
deriving instance (Std_.Eq (C.Parsed DiskManager'import'params))
instance (C.Parse DiskManager'import'params (C.Parsed DiskManager'import'params)) where
    parse raw_ = (DiskManager'import'params <$> (GH.parseField #params raw_))
instance (C.Marshal DiskManager'import'params (C.Parsed DiskManager'import'params)) where
    marshalInto raw_ DiskManager'import'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot DiskManager'import'params DiskImportParams) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'import'results 
type instance (R.ReprFor DiskManager'import'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'import'results) where
    typeId  = 10183704444560911261
instance (C.TypedStruct DiskManager'import'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'import'results) where
    type AllocHint DiskManager'import'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'import'results (C.Parsed DiskManager'import'results))
instance (C.AllocateList DiskManager'import'results) where
    type ListAllocHint DiskManager'import'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'import'results (C.Parsed DiskManager'import'results))
data instance C.Parsed DiskManager'import'results
    = DiskManager'import'results 
        {disk :: (RP.Parsed Disk)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'import'results))
deriving instance (Std_.Eq (C.Parsed DiskManager'import'results))
instance (C.Parse DiskManager'import'results (C.Parsed DiskManager'import'results)) where
    parse raw_ = (DiskManager'import'results <$> (GH.parseField #disk raw_))
instance (C.Marshal DiskManager'import'results (C.Parsed DiskManager'import'results)) where
    marshalInto raw_ DiskManager'import'results{..} = (do
        (GH.encodeField #disk disk raw_)
        (Std_.pure ())
        )
instance (GH.HasField "disk" GH.Slot DiskManager'import'results Disk) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'flatten'params 
type instance (R.ReprFor DiskManager'flatten'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'flatten'params) where
    typeId  = 14309189818799315536
instance (C.TypedStruct DiskManager'flatten'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'flatten'params) where
    type AllocHint DiskManager'flatten'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'flatten'params (C.Parsed DiskManager'flatten'params))
instance (C.AllocateList DiskManager'flatten'params) where
    type ListAllocHint DiskManager'flatten'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'flatten'params (C.Parsed DiskManager'flatten'params))
data instance C.Parsed DiskManager'flatten'params
    = DiskManager'flatten'params 
        {diskRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'flatten'params))
deriving instance (Std_.Eq (C.Parsed DiskManager'flatten'params))
instance (C.Parse DiskManager'flatten'params (C.Parsed DiskManager'flatten'params)) where
    parse raw_ = (DiskManager'flatten'params <$> (GH.parseField #diskRef raw_))
instance (C.Marshal DiskManager'flatten'params (C.Parsed DiskManager'flatten'params)) where
    marshalInto raw_ DiskManager'flatten'params{..} = (do
        (GH.encodeField #diskRef diskRef raw_)
        (Std_.pure ())
        )
instance (GH.HasField "diskRef" GH.Slot DiskManager'flatten'params Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'flatten'results 
type instance (R.ReprFor DiskManager'flatten'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'flatten'results) where
    typeId  = 18122896146734311769
instance (C.TypedStruct DiskManager'flatten'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate DiskManager'flatten'results) where
    type AllocHint DiskManager'flatten'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'flatten'results (C.Parsed DiskManager'flatten'results))
instance (C.AllocateList DiskManager'flatten'results) where
    type ListAllocHint DiskManager'flatten'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'flatten'results (C.Parsed DiskManager'flatten'results))
data instance C.Parsed DiskManager'flatten'results
    = DiskManager'flatten'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'flatten'results))
deriving instance (Std_.Eq (C.Parsed DiskManager'flatten'results))
instance (C.Parse DiskManager'flatten'results (C.Parsed DiskManager'flatten'results)) where
    parse raw_ = (Std_.pure DiskManager'flatten'results)
instance (C.Marshal DiskManager'flatten'results (C.Parsed DiskManager'flatten'results)) where
    marshalInto _raw (DiskManager'flatten'results) = (Std_.pure ())
data DiskManager'copy'params 
type instance (R.ReprFor DiskManager'copy'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'copy'params) where
    typeId  = 9847204764816947922
instance (C.TypedStruct DiskManager'copy'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'copy'params) where
    type AllocHint DiskManager'copy'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'copy'params (C.Parsed DiskManager'copy'params))
instance (C.AllocateList DiskManager'copy'params) where
    type ListAllocHint DiskManager'copy'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'copy'params (C.Parsed DiskManager'copy'params))
data instance C.Parsed DiskManager'copy'params
    = DiskManager'copy'params 
        {params :: (RP.Parsed DiskCopyParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'copy'params))
deriving instance (Std_.Eq (C.Parsed DiskManager'copy'params))
instance (C.Parse DiskManager'copy'params (C.Parsed DiskManager'copy'params)) where
    parse raw_ = (DiskManager'copy'params <$> (GH.parseField #params raw_))
instance (C.Marshal DiskManager'copy'params (C.Parsed DiskManager'copy'params)) where
    marshalInto raw_ DiskManager'copy'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot DiskManager'copy'params DiskCopyParams) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'copy'results 
type instance (R.ReprFor DiskManager'copy'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'copy'results) where
    typeId  = 12556882617831631231
instance (C.TypedStruct DiskManager'copy'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate DiskManager'copy'results) where
    type AllocHint DiskManager'copy'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'copy'results (C.Parsed DiskManager'copy'results))
instance (C.AllocateList DiskManager'copy'results) where
    type ListAllocHint DiskManager'copy'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'copy'results (C.Parsed DiskManager'copy'results))
data instance C.Parsed DiskManager'copy'results
    = DiskManager'copy'results 
        {taskId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'copy'results))
deriving instance (Std_.Eq (C.Parsed DiskManager'copy'results))
instance (C.Parse DiskManager'copy'results (C.Parsed DiskManager'copy'results)) where
    parse raw_ = (DiskManager'copy'results <$> (GH.parseField #taskId raw_))
instance (C.Marshal DiskManager'copy'results (C.Parsed DiskManager'copy'results)) where
    marshalInto raw_ DiskManager'copy'results{..} = (do
        (GH.encodeField #taskId taskId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "taskId" GH.Slot DiskManager'copy'results Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data DiskManager'move'params 
type instance (R.ReprFor DiskManager'move'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'move'params) where
    typeId  = 16774640712740262938
instance (C.TypedStruct DiskManager'move'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate DiskManager'move'params) where
    type AllocHint DiskManager'move'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'move'params (C.Parsed DiskManager'move'params))
instance (C.AllocateList DiskManager'move'params) where
    type ListAllocHint DiskManager'move'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'move'params (C.Parsed DiskManager'move'params))
data instance C.Parsed DiskManager'move'params
    = DiskManager'move'params 
        {params :: (RP.Parsed DiskMoveParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'move'params))
deriving instance (Std_.Eq (C.Parsed DiskManager'move'params))
instance (C.Parse DiskManager'move'params (C.Parsed DiskManager'move'params)) where
    parse raw_ = (DiskManager'move'params <$> (GH.parseField #params raw_))
instance (C.Marshal DiskManager'move'params (C.Parsed DiskManager'move'params)) where
    marshalInto raw_ DiskManager'move'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot DiskManager'move'params DiskMoveParams) where
    fieldByLabel  = (GH.ptrField 0)
data DiskManager'move'results 
type instance (R.ReprFor DiskManager'move'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DiskManager'move'results) where
    typeId  = 10845935175882287879
instance (C.TypedStruct DiskManager'move'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate DiskManager'move'results) where
    type AllocHint DiskManager'move'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DiskManager'move'results (C.Parsed DiskManager'move'results))
instance (C.AllocateList DiskManager'move'results) where
    type ListAllocHint DiskManager'move'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DiskManager'move'results (C.Parsed DiskManager'move'results))
data instance C.Parsed DiskManager'move'results
    = DiskManager'move'results 
        {taskId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DiskManager'move'results))
deriving instance (Std_.Eq (C.Parsed DiskManager'move'results))
instance (C.Parse DiskManager'move'results (C.Parsed DiskManager'move'results)) where
    parse raw_ = (DiskManager'move'results <$> (GH.parseField #taskId raw_))
instance (C.Marshal DiskManager'move'results (C.Parsed DiskManager'move'results)) where
    marshalInto raw_ DiskManager'move'results{..} = (do
        (GH.encodeField #taskId taskId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "taskId" GH.Slot DiskManager'move'results Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Disk 
type instance (R.ReprFor Disk) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Disk) where
    typeId  = 16575700408835115110
instance (C.Parse Disk (GH.Client Disk)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Disk) where
    type Server Disk = Disk'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Disk)) [(GH.toUntypedMethodHandler ((disk'show) s_))
                                                                      ,(GH.toUntypedMethodHandler ((disk'delete) s_))
                                                                      ,(GH.toUntypedMethodHandler ((disk'refresh) s_))
                                                                      ,(GH.toUntypedMethodHandler ((disk'resize) s_))
                                                                      ,(GH.toUntypedMethodHandler ((disk'snapshotCreate) s_))
                                                                      ,(GH.toUntypedMethodHandler ((disk'snapshotList) s_))
                                                                      ,(GH.toUntypedMethodHandler ((disk'snapshotGet) s_))] [])
class (Disk'server_ s_) where
    {-# MINIMAL disk'show,disk'delete,disk'refresh,disk'resize,disk'snapshotCreate,disk'snapshotList,disk'snapshotGet #-}
    disk'show :: s_ -> (GH.MethodHandler Disk'show'params Disk'show'results)
    disk'show _ = GH.methodUnimplemented
    disk'delete :: s_ -> (GH.MethodHandler Disk'delete'params Disk'delete'results)
    disk'delete _ = GH.methodUnimplemented
    disk'refresh :: s_ -> (GH.MethodHandler Disk'refresh'params Disk'refresh'results)
    disk'refresh _ = GH.methodUnimplemented
    disk'resize :: s_ -> (GH.MethodHandler Disk'resize'params Disk'resize'results)
    disk'resize _ = GH.methodUnimplemented
    disk'snapshotCreate :: s_ -> (GH.MethodHandler Disk'snapshotCreate'params Disk'snapshotCreate'results)
    disk'snapshotCreate _ = GH.methodUnimplemented
    disk'snapshotList :: s_ -> (GH.MethodHandler Disk'snapshotList'params Disk'snapshotList'results)
    disk'snapshotList _ = GH.methodUnimplemented
    disk'snapshotGet :: s_ -> (GH.MethodHandler Disk'snapshotGet'params Disk'snapshotGet'results)
    disk'snapshotGet _ = GH.methodUnimplemented
instance (GH.HasMethod "show" Disk Disk'show'params Disk'show'results) where
    methodByLabel  = (GH.Method 16575700408835115110 0)
instance (GH.HasMethod "delete" Disk Disk'delete'params Disk'delete'results) where
    methodByLabel  = (GH.Method 16575700408835115110 1)
instance (GH.HasMethod "refresh" Disk Disk'refresh'params Disk'refresh'results) where
    methodByLabel  = (GH.Method 16575700408835115110 2)
instance (GH.HasMethod "resize" Disk Disk'resize'params Disk'resize'results) where
    methodByLabel  = (GH.Method 16575700408835115110 3)
instance (GH.HasMethod "snapshotCreate" Disk Disk'snapshotCreate'params Disk'snapshotCreate'results) where
    methodByLabel  = (GH.Method 16575700408835115110 4)
instance (GH.HasMethod "snapshotList" Disk Disk'snapshotList'params Disk'snapshotList'results) where
    methodByLabel  = (GH.Method 16575700408835115110 5)
instance (GH.HasMethod "snapshotGet" Disk Disk'snapshotGet'params Disk'snapshotGet'results) where
    methodByLabel  = (GH.Method 16575700408835115110 6)
data Disk'show'params 
type instance (R.ReprFor Disk'show'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Disk'show'params) where
    typeId  = 12767879452773709895
instance (C.TypedStruct Disk'show'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Disk'show'params) where
    type AllocHint Disk'show'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Disk'show'params (C.Parsed Disk'show'params))
instance (C.AllocateList Disk'show'params) where
    type ListAllocHint Disk'show'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Disk'show'params (C.Parsed Disk'show'params))
data instance C.Parsed Disk'show'params
    = Disk'show'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Disk'show'params))
deriving instance (Std_.Eq (C.Parsed Disk'show'params))
instance (C.Parse Disk'show'params (C.Parsed Disk'show'params)) where
    parse raw_ = (Std_.pure Disk'show'params)
instance (C.Marshal Disk'show'params (C.Parsed Disk'show'params)) where
    marshalInto _raw (Disk'show'params) = (Std_.pure ())
data Disk'show'results 
type instance (R.ReprFor Disk'show'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Disk'show'results) where
    typeId  = 14730870486321688063
instance (C.TypedStruct Disk'show'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Disk'show'results) where
    type AllocHint Disk'show'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Disk'show'results (C.Parsed Disk'show'results))
instance (C.AllocateList Disk'show'results) where
    type ListAllocHint Disk'show'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Disk'show'results (C.Parsed Disk'show'results))
data instance C.Parsed Disk'show'results
    = Disk'show'results 
        {info :: (RP.Parsed DiskImageInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Disk'show'results))
deriving instance (Std_.Eq (C.Parsed Disk'show'results))
instance (C.Parse Disk'show'results (C.Parsed Disk'show'results)) where
    parse raw_ = (Disk'show'results <$> (GH.parseField #info raw_))
instance (C.Marshal Disk'show'results (C.Parsed Disk'show'results)) where
    marshalInto raw_ Disk'show'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot Disk'show'results DiskImageInfo) where
    fieldByLabel  = (GH.ptrField 0)
data Disk'delete'params 
type instance (R.ReprFor Disk'delete'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Disk'delete'params) where
    typeId  = 14199659191158583955
instance (C.TypedStruct Disk'delete'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Disk'delete'params) where
    type AllocHint Disk'delete'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Disk'delete'params (C.Parsed Disk'delete'params))
instance (C.AllocateList Disk'delete'params) where
    type ListAllocHint Disk'delete'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Disk'delete'params (C.Parsed Disk'delete'params))
data instance C.Parsed Disk'delete'params
    = Disk'delete'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Disk'delete'params))
deriving instance (Std_.Eq (C.Parsed Disk'delete'params))
instance (C.Parse Disk'delete'params (C.Parsed Disk'delete'params)) where
    parse raw_ = (Std_.pure Disk'delete'params)
instance (C.Marshal Disk'delete'params (C.Parsed Disk'delete'params)) where
    marshalInto _raw (Disk'delete'params) = (Std_.pure ())
data Disk'delete'results 
type instance (R.ReprFor Disk'delete'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Disk'delete'results) where
    typeId  = 17409737581797726729
instance (C.TypedStruct Disk'delete'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Disk'delete'results) where
    type AllocHint Disk'delete'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Disk'delete'results (C.Parsed Disk'delete'results))
instance (C.AllocateList Disk'delete'results) where
    type ListAllocHint Disk'delete'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Disk'delete'results (C.Parsed Disk'delete'results))
data instance C.Parsed Disk'delete'results
    = Disk'delete'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Disk'delete'results))
deriving instance (Std_.Eq (C.Parsed Disk'delete'results))
instance (C.Parse Disk'delete'results (C.Parsed Disk'delete'results)) where
    parse raw_ = (Std_.pure Disk'delete'results)
instance (C.Marshal Disk'delete'results (C.Parsed Disk'delete'results)) where
    marshalInto _raw (Disk'delete'results) = (Std_.pure ())
data Disk'refresh'params 
type instance (R.ReprFor Disk'refresh'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Disk'refresh'params) where
    typeId  = 17481404712813945568
instance (C.TypedStruct Disk'refresh'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Disk'refresh'params) where
    type AllocHint Disk'refresh'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Disk'refresh'params (C.Parsed Disk'refresh'params))
instance (C.AllocateList Disk'refresh'params) where
    type ListAllocHint Disk'refresh'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Disk'refresh'params (C.Parsed Disk'refresh'params))
data instance C.Parsed Disk'refresh'params
    = Disk'refresh'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Disk'refresh'params))
deriving instance (Std_.Eq (C.Parsed Disk'refresh'params))
instance (C.Parse Disk'refresh'params (C.Parsed Disk'refresh'params)) where
    parse raw_ = (Std_.pure Disk'refresh'params)
instance (C.Marshal Disk'refresh'params (C.Parsed Disk'refresh'params)) where
    marshalInto _raw (Disk'refresh'params) = (Std_.pure ())
data Disk'refresh'results 
type instance (R.ReprFor Disk'refresh'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Disk'refresh'results) where
    typeId  = 15134081414815803375
instance (C.TypedStruct Disk'refresh'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Disk'refresh'results) where
    type AllocHint Disk'refresh'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Disk'refresh'results (C.Parsed Disk'refresh'results))
instance (C.AllocateList Disk'refresh'results) where
    type ListAllocHint Disk'refresh'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Disk'refresh'results (C.Parsed Disk'refresh'results))
data instance C.Parsed Disk'refresh'results
    = Disk'refresh'results 
        {info :: (RP.Parsed DiskImageInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Disk'refresh'results))
deriving instance (Std_.Eq (C.Parsed Disk'refresh'results))
instance (C.Parse Disk'refresh'results (C.Parsed Disk'refresh'results)) where
    parse raw_ = (Disk'refresh'results <$> (GH.parseField #info raw_))
instance (C.Marshal Disk'refresh'results (C.Parsed Disk'refresh'results)) where
    marshalInto raw_ Disk'refresh'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot Disk'refresh'results DiskImageInfo) where
    fieldByLabel  = (GH.ptrField 0)
data Disk'resize'params 
type instance (R.ReprFor Disk'resize'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Disk'resize'params) where
    typeId  = 9251522327640169316
instance (C.TypedStruct Disk'resize'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Disk'resize'params) where
    type AllocHint Disk'resize'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Disk'resize'params (C.Parsed Disk'resize'params))
instance (C.AllocateList Disk'resize'params) where
    type ListAllocHint Disk'resize'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Disk'resize'params (C.Parsed Disk'resize'params))
data instance C.Parsed Disk'resize'params
    = Disk'resize'params 
        {newSizeMb :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Disk'resize'params))
deriving instance (Std_.Eq (C.Parsed Disk'resize'params))
instance (C.Parse Disk'resize'params (C.Parsed Disk'resize'params)) where
    parse raw_ = (Disk'resize'params <$> (GH.parseField #newSizeMb raw_))
instance (C.Marshal Disk'resize'params (C.Parsed Disk'resize'params)) where
    marshalInto raw_ Disk'resize'params{..} = (do
        (GH.encodeField #newSizeMb newSizeMb raw_)
        (Std_.pure ())
        )
instance (GH.HasField "newSizeMb" GH.Slot Disk'resize'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Disk'resize'results 
type instance (R.ReprFor Disk'resize'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Disk'resize'results) where
    typeId  = 14736582858844548057
instance (C.TypedStruct Disk'resize'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Disk'resize'results) where
    type AllocHint Disk'resize'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Disk'resize'results (C.Parsed Disk'resize'results))
instance (C.AllocateList Disk'resize'results) where
    type ListAllocHint Disk'resize'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Disk'resize'results (C.Parsed Disk'resize'results))
data instance C.Parsed Disk'resize'results
    = Disk'resize'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Disk'resize'results))
deriving instance (Std_.Eq (C.Parsed Disk'resize'results))
instance (C.Parse Disk'resize'results (C.Parsed Disk'resize'results)) where
    parse raw_ = (Std_.pure Disk'resize'results)
instance (C.Marshal Disk'resize'results (C.Parsed Disk'resize'results)) where
    marshalInto _raw (Disk'resize'results) = (Std_.pure ())
data Disk'snapshotCreate'params 
type instance (R.ReprFor Disk'snapshotCreate'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Disk'snapshotCreate'params) where
    typeId  = 11260087998731707172
instance (C.TypedStruct Disk'snapshotCreate'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Disk'snapshotCreate'params) where
    type AllocHint Disk'snapshotCreate'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Disk'snapshotCreate'params (C.Parsed Disk'snapshotCreate'params))
instance (C.AllocateList Disk'snapshotCreate'params) where
    type ListAllocHint Disk'snapshotCreate'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Disk'snapshotCreate'params (C.Parsed Disk'snapshotCreate'params))
data instance C.Parsed Disk'snapshotCreate'params
    = Disk'snapshotCreate'params 
        {name :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Disk'snapshotCreate'params))
deriving instance (Std_.Eq (C.Parsed Disk'snapshotCreate'params))
instance (C.Parse Disk'snapshotCreate'params (C.Parsed Disk'snapshotCreate'params)) where
    parse raw_ = (Disk'snapshotCreate'params <$> (GH.parseField #name raw_))
instance (C.Marshal Disk'snapshotCreate'params (C.Parsed Disk'snapshotCreate'params)) where
    marshalInto raw_ Disk'snapshotCreate'params{..} = (do
        (GH.encodeField #name name raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot Disk'snapshotCreate'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Disk'snapshotCreate'results 
type instance (R.ReprFor Disk'snapshotCreate'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Disk'snapshotCreate'results) where
    typeId  = 10348819924698284856
instance (C.TypedStruct Disk'snapshotCreate'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Disk'snapshotCreate'results) where
    type AllocHint Disk'snapshotCreate'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Disk'snapshotCreate'results (C.Parsed Disk'snapshotCreate'results))
instance (C.AllocateList Disk'snapshotCreate'results) where
    type ListAllocHint Disk'snapshotCreate'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Disk'snapshotCreate'results (C.Parsed Disk'snapshotCreate'results))
data instance C.Parsed Disk'snapshotCreate'results
    = Disk'snapshotCreate'results 
        {snapshot :: (RP.Parsed Snapshot)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Disk'snapshotCreate'results))
deriving instance (Std_.Eq (C.Parsed Disk'snapshotCreate'results))
instance (C.Parse Disk'snapshotCreate'results (C.Parsed Disk'snapshotCreate'results)) where
    parse raw_ = (Disk'snapshotCreate'results <$> (GH.parseField #snapshot raw_))
instance (C.Marshal Disk'snapshotCreate'results (C.Parsed Disk'snapshotCreate'results)) where
    marshalInto raw_ Disk'snapshotCreate'results{..} = (do
        (GH.encodeField #snapshot snapshot raw_)
        (Std_.pure ())
        )
instance (GH.HasField "snapshot" GH.Slot Disk'snapshotCreate'results Snapshot) where
    fieldByLabel  = (GH.ptrField 0)
data Disk'snapshotList'params 
type instance (R.ReprFor Disk'snapshotList'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Disk'snapshotList'params) where
    typeId  = 17023208089407895105
instance (C.TypedStruct Disk'snapshotList'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Disk'snapshotList'params) where
    type AllocHint Disk'snapshotList'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Disk'snapshotList'params (C.Parsed Disk'snapshotList'params))
instance (C.AllocateList Disk'snapshotList'params) where
    type ListAllocHint Disk'snapshotList'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Disk'snapshotList'params (C.Parsed Disk'snapshotList'params))
data instance C.Parsed Disk'snapshotList'params
    = Disk'snapshotList'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Disk'snapshotList'params))
deriving instance (Std_.Eq (C.Parsed Disk'snapshotList'params))
instance (C.Parse Disk'snapshotList'params (C.Parsed Disk'snapshotList'params)) where
    parse raw_ = (Std_.pure Disk'snapshotList'params)
instance (C.Marshal Disk'snapshotList'params (C.Parsed Disk'snapshotList'params)) where
    marshalInto _raw (Disk'snapshotList'params) = (Std_.pure ())
data Disk'snapshotList'results 
type instance (R.ReprFor Disk'snapshotList'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Disk'snapshotList'results) where
    typeId  = 12335316405713278864
instance (C.TypedStruct Disk'snapshotList'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Disk'snapshotList'results) where
    type AllocHint Disk'snapshotList'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Disk'snapshotList'results (C.Parsed Disk'snapshotList'results))
instance (C.AllocateList Disk'snapshotList'results) where
    type ListAllocHint Disk'snapshotList'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Disk'snapshotList'results (C.Parsed Disk'snapshotList'results))
data instance C.Parsed Disk'snapshotList'results
    = Disk'snapshotList'results 
        {snapshots :: (RP.Parsed (R.List SnapshotInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Disk'snapshotList'results))
deriving instance (Std_.Eq (C.Parsed Disk'snapshotList'results))
instance (C.Parse Disk'snapshotList'results (C.Parsed Disk'snapshotList'results)) where
    parse raw_ = (Disk'snapshotList'results <$> (GH.parseField #snapshots raw_))
instance (C.Marshal Disk'snapshotList'results (C.Parsed Disk'snapshotList'results)) where
    marshalInto raw_ Disk'snapshotList'results{..} = (do
        (GH.encodeField #snapshots snapshots raw_)
        (Std_.pure ())
        )
instance (GH.HasField "snapshots" GH.Slot Disk'snapshotList'results (R.List SnapshotInfo)) where
    fieldByLabel  = (GH.ptrField 0)
data Disk'snapshotGet'params 
type instance (R.ReprFor Disk'snapshotGet'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Disk'snapshotGet'params) where
    typeId  = 14992770626676760813
instance (C.TypedStruct Disk'snapshotGet'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Disk'snapshotGet'params) where
    type AllocHint Disk'snapshotGet'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Disk'snapshotGet'params (C.Parsed Disk'snapshotGet'params))
instance (C.AllocateList Disk'snapshotGet'params) where
    type ListAllocHint Disk'snapshotGet'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Disk'snapshotGet'params (C.Parsed Disk'snapshotGet'params))
data instance C.Parsed Disk'snapshotGet'params
    = Disk'snapshotGet'params 
        {ref :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Disk'snapshotGet'params))
deriving instance (Std_.Eq (C.Parsed Disk'snapshotGet'params))
instance (C.Parse Disk'snapshotGet'params (C.Parsed Disk'snapshotGet'params)) where
    parse raw_ = (Disk'snapshotGet'params <$> (GH.parseField #ref raw_))
instance (C.Marshal Disk'snapshotGet'params (C.Parsed Disk'snapshotGet'params)) where
    marshalInto raw_ Disk'snapshotGet'params{..} = (do
        (GH.encodeField #ref ref raw_)
        (Std_.pure ())
        )
instance (GH.HasField "ref" GH.Slot Disk'snapshotGet'params Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
data Disk'snapshotGet'results 
type instance (R.ReprFor Disk'snapshotGet'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Disk'snapshotGet'results) where
    typeId  = 16326137085177563544
instance (C.TypedStruct Disk'snapshotGet'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Disk'snapshotGet'results) where
    type AllocHint Disk'snapshotGet'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Disk'snapshotGet'results (C.Parsed Disk'snapshotGet'results))
instance (C.AllocateList Disk'snapshotGet'results) where
    type ListAllocHint Disk'snapshotGet'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Disk'snapshotGet'results (C.Parsed Disk'snapshotGet'results))
data instance C.Parsed Disk'snapshotGet'results
    = Disk'snapshotGet'results 
        {snapshot :: (RP.Parsed Snapshot)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Disk'snapshotGet'results))
deriving instance (Std_.Eq (C.Parsed Disk'snapshotGet'results))
instance (C.Parse Disk'snapshotGet'results (C.Parsed Disk'snapshotGet'results)) where
    parse raw_ = (Disk'snapshotGet'results <$> (GH.parseField #snapshot raw_))
instance (C.Marshal Disk'snapshotGet'results (C.Parsed Disk'snapshotGet'results)) where
    marshalInto raw_ Disk'snapshotGet'results{..} = (do
        (GH.encodeField #snapshot snapshot raw_)
        (Std_.pure ())
        )
instance (GH.HasField "snapshot" GH.Slot Disk'snapshotGet'results Snapshot) where
    fieldByLabel  = (GH.ptrField 0)
data Snapshot 
type instance (R.ReprFor Snapshot) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Snapshot) where
    typeId  = 17881196093664876188
instance (C.Parse Snapshot (GH.Client Snapshot)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Snapshot) where
    type Server Snapshot = Snapshot'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Snapshot)) [(GH.toUntypedMethodHandler ((snapshot'show) s_))
                                                                          ,(GH.toUntypedMethodHandler ((snapshot'delete) s_))
                                                                          ,(GH.toUntypedMethodHandler ((snapshot'rollback) s_))
                                                                          ,(GH.toUntypedMethodHandler ((snapshot'merge) s_))] [])
class (Snapshot'server_ s_) where
    {-# MINIMAL snapshot'show,snapshot'delete,snapshot'rollback,snapshot'merge #-}
    snapshot'show :: s_ -> (GH.MethodHandler Snapshot'show'params Snapshot'show'results)
    snapshot'show _ = GH.methodUnimplemented
    snapshot'delete :: s_ -> (GH.MethodHandler Snapshot'delete'params Snapshot'delete'results)
    snapshot'delete _ = GH.methodUnimplemented
    snapshot'rollback :: s_ -> (GH.MethodHandler Snapshot'rollback'params Snapshot'rollback'results)
    snapshot'rollback _ = GH.methodUnimplemented
    snapshot'merge :: s_ -> (GH.MethodHandler Snapshot'merge'params Snapshot'merge'results)
    snapshot'merge _ = GH.methodUnimplemented
instance (GH.HasMethod "show" Snapshot Snapshot'show'params Snapshot'show'results) where
    methodByLabel  = (GH.Method 17881196093664876188 0)
instance (GH.HasMethod "delete" Snapshot Snapshot'delete'params Snapshot'delete'results) where
    methodByLabel  = (GH.Method 17881196093664876188 1)
instance (GH.HasMethod "rollback" Snapshot Snapshot'rollback'params Snapshot'rollback'results) where
    methodByLabel  = (GH.Method 17881196093664876188 2)
instance (GH.HasMethod "merge" Snapshot Snapshot'merge'params Snapshot'merge'results) where
    methodByLabel  = (GH.Method 17881196093664876188 3)
data Snapshot'show'params 
type instance (R.ReprFor Snapshot'show'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Snapshot'show'params) where
    typeId  = 14815731956967339827
instance (C.TypedStruct Snapshot'show'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Snapshot'show'params) where
    type AllocHint Snapshot'show'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Snapshot'show'params (C.Parsed Snapshot'show'params))
instance (C.AllocateList Snapshot'show'params) where
    type ListAllocHint Snapshot'show'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Snapshot'show'params (C.Parsed Snapshot'show'params))
data instance C.Parsed Snapshot'show'params
    = Snapshot'show'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Snapshot'show'params))
deriving instance (Std_.Eq (C.Parsed Snapshot'show'params))
instance (C.Parse Snapshot'show'params (C.Parsed Snapshot'show'params)) where
    parse raw_ = (Std_.pure Snapshot'show'params)
instance (C.Marshal Snapshot'show'params (C.Parsed Snapshot'show'params)) where
    marshalInto _raw (Snapshot'show'params) = (Std_.pure ())
data Snapshot'show'results 
type instance (R.ReprFor Snapshot'show'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Snapshot'show'results) where
    typeId  = 16388732829522865360
instance (C.TypedStruct Snapshot'show'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Snapshot'show'results) where
    type AllocHint Snapshot'show'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Snapshot'show'results (C.Parsed Snapshot'show'results))
instance (C.AllocateList Snapshot'show'results) where
    type ListAllocHint Snapshot'show'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Snapshot'show'results (C.Parsed Snapshot'show'results))
data instance C.Parsed Snapshot'show'results
    = Snapshot'show'results 
        {info :: (RP.Parsed SnapshotInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Snapshot'show'results))
deriving instance (Std_.Eq (C.Parsed Snapshot'show'results))
instance (C.Parse Snapshot'show'results (C.Parsed Snapshot'show'results)) where
    parse raw_ = (Snapshot'show'results <$> (GH.parseField #info raw_))
instance (C.Marshal Snapshot'show'results (C.Parsed Snapshot'show'results)) where
    marshalInto raw_ Snapshot'show'results{..} = (do
        (GH.encodeField #info info raw_)
        (Std_.pure ())
        )
instance (GH.HasField "info" GH.Slot Snapshot'show'results SnapshotInfo) where
    fieldByLabel  = (GH.ptrField 0)
data Snapshot'delete'params 
type instance (R.ReprFor Snapshot'delete'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Snapshot'delete'params) where
    typeId  = 18325171911679548609
instance (C.TypedStruct Snapshot'delete'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Snapshot'delete'params) where
    type AllocHint Snapshot'delete'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Snapshot'delete'params (C.Parsed Snapshot'delete'params))
instance (C.AllocateList Snapshot'delete'params) where
    type ListAllocHint Snapshot'delete'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Snapshot'delete'params (C.Parsed Snapshot'delete'params))
data instance C.Parsed Snapshot'delete'params
    = Snapshot'delete'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Snapshot'delete'params))
deriving instance (Std_.Eq (C.Parsed Snapshot'delete'params))
instance (C.Parse Snapshot'delete'params (C.Parsed Snapshot'delete'params)) where
    parse raw_ = (Std_.pure Snapshot'delete'params)
instance (C.Marshal Snapshot'delete'params (C.Parsed Snapshot'delete'params)) where
    marshalInto _raw (Snapshot'delete'params) = (Std_.pure ())
data Snapshot'delete'results 
type instance (R.ReprFor Snapshot'delete'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Snapshot'delete'results) where
    typeId  = 10159087474064915190
instance (C.TypedStruct Snapshot'delete'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Snapshot'delete'results) where
    type AllocHint Snapshot'delete'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Snapshot'delete'results (C.Parsed Snapshot'delete'results))
instance (C.AllocateList Snapshot'delete'results) where
    type ListAllocHint Snapshot'delete'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Snapshot'delete'results (C.Parsed Snapshot'delete'results))
data instance C.Parsed Snapshot'delete'results
    = Snapshot'delete'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Snapshot'delete'results))
deriving instance (Std_.Eq (C.Parsed Snapshot'delete'results))
instance (C.Parse Snapshot'delete'results (C.Parsed Snapshot'delete'results)) where
    parse raw_ = (Std_.pure Snapshot'delete'results)
instance (C.Marshal Snapshot'delete'results (C.Parsed Snapshot'delete'results)) where
    marshalInto _raw (Snapshot'delete'results) = (Std_.pure ())
data Snapshot'rollback'params 
type instance (R.ReprFor Snapshot'rollback'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Snapshot'rollback'params) where
    typeId  = 10827958444244213531
instance (C.TypedStruct Snapshot'rollback'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Snapshot'rollback'params) where
    type AllocHint Snapshot'rollback'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Snapshot'rollback'params (C.Parsed Snapshot'rollback'params))
instance (C.AllocateList Snapshot'rollback'params) where
    type ListAllocHint Snapshot'rollback'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Snapshot'rollback'params (C.Parsed Snapshot'rollback'params))
data instance C.Parsed Snapshot'rollback'params
    = Snapshot'rollback'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Snapshot'rollback'params))
deriving instance (Std_.Eq (C.Parsed Snapshot'rollback'params))
instance (C.Parse Snapshot'rollback'params (C.Parsed Snapshot'rollback'params)) where
    parse raw_ = (Std_.pure Snapshot'rollback'params)
instance (C.Marshal Snapshot'rollback'params (C.Parsed Snapshot'rollback'params)) where
    marshalInto _raw (Snapshot'rollback'params) = (Std_.pure ())
data Snapshot'rollback'results 
type instance (R.ReprFor Snapshot'rollback'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Snapshot'rollback'results) where
    typeId  = 13449225324947028639
instance (C.TypedStruct Snapshot'rollback'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Snapshot'rollback'results) where
    type AllocHint Snapshot'rollback'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Snapshot'rollback'results (C.Parsed Snapshot'rollback'results))
instance (C.AllocateList Snapshot'rollback'results) where
    type ListAllocHint Snapshot'rollback'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Snapshot'rollback'results (C.Parsed Snapshot'rollback'results))
data instance C.Parsed Snapshot'rollback'results
    = Snapshot'rollback'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Snapshot'rollback'results))
deriving instance (Std_.Eq (C.Parsed Snapshot'rollback'results))
instance (C.Parse Snapshot'rollback'results (C.Parsed Snapshot'rollback'results)) where
    parse raw_ = (Std_.pure Snapshot'rollback'results)
instance (C.Marshal Snapshot'rollback'results (C.Parsed Snapshot'rollback'results)) where
    marshalInto _raw (Snapshot'rollback'results) = (Std_.pure ())
data Snapshot'merge'params 
type instance (R.ReprFor Snapshot'merge'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Snapshot'merge'params) where
    typeId  = 13974556262621503247
instance (C.TypedStruct Snapshot'merge'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Snapshot'merge'params) where
    type AllocHint Snapshot'merge'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Snapshot'merge'params (C.Parsed Snapshot'merge'params))
instance (C.AllocateList Snapshot'merge'params) where
    type ListAllocHint Snapshot'merge'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Snapshot'merge'params (C.Parsed Snapshot'merge'params))
data instance C.Parsed Snapshot'merge'params
    = Snapshot'merge'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Snapshot'merge'params))
deriving instance (Std_.Eq (C.Parsed Snapshot'merge'params))
instance (C.Parse Snapshot'merge'params (C.Parsed Snapshot'merge'params)) where
    parse raw_ = (Std_.pure Snapshot'merge'params)
instance (C.Marshal Snapshot'merge'params (C.Parsed Snapshot'merge'params)) where
    marshalInto _raw (Snapshot'merge'params) = (Std_.pure ())
data Snapshot'merge'results 
type instance (R.ReprFor Snapshot'merge'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Snapshot'merge'results) where
    typeId  = 14629902582713103224
instance (C.TypedStruct Snapshot'merge'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Snapshot'merge'results) where
    type AllocHint Snapshot'merge'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Snapshot'merge'results (C.Parsed Snapshot'merge'results))
instance (C.AllocateList Snapshot'merge'results) where
    type ListAllocHint Snapshot'merge'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Snapshot'merge'results (C.Parsed Snapshot'merge'results))
data instance C.Parsed Snapshot'merge'results
    = Snapshot'merge'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Snapshot'merge'results))
deriving instance (Std_.Eq (C.Parsed Snapshot'merge'results))
instance (C.Parse Snapshot'merge'results (C.Parsed Snapshot'merge'results)) where
    parse raw_ = (Std_.pure Snapshot'merge'results)
instance (C.Marshal Snapshot'merge'results (C.Parsed Snapshot'merge'results)) where
    marshalInto _raw (Snapshot'merge'results) = (Std_.pure ())