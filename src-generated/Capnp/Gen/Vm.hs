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
module Capnp.Gen.Vm where
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP
import qualified Capnp.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers as GH
import qualified Capnp.Classes as C
import qualified GHC.Generics as Generics
import qualified Capnp.GenHelpers.Rpc as GH
import qualified Capnp.Gen.ById.X9b1373e2334a09e9
import qualified Capnp.Gen.ById.X9bd452a518ed3917
import qualified Capnp.Gen.ById.Xa6341bd086aa89f6
import qualified Capnp.Gen.ById.Xbf9b09f64c0dd40d
import qualified Capnp.Gen.ById.Xeb6a435f11477f84
import qualified Capnp.Gen.ById.Xec449e11027b2949
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data VmInfo 
type instance (R.ReprFor VmInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmInfo) where
    typeId  = 14337324896033347465
instance (C.TypedStruct VmInfo) where
    numStructWords  = 4
    numStructPtrs  = 1
instance (C.Allocate VmInfo) where
    type AllocHint VmInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmInfo (C.Parsed VmInfo))
instance (C.AllocateList VmInfo) where
    type ListAllocHint VmInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmInfo (C.Parsed VmInfo))
data instance C.Parsed VmInfo
    = VmInfo 
        {id :: (RP.Parsed Std_.Int64)
        ,name :: (RP.Parsed Basics.Text)
        ,status :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.VmStatus)
        ,cpuCount :: (RP.Parsed Std_.Int32)
        ,ramMb :: (RP.Parsed Std_.Int32)
        ,headless :: (RP.Parsed Std_.Bool)
        ,guestAgent :: (RP.Parsed Std_.Bool)
        ,cloudInit :: (RP.Parsed Std_.Bool)
        ,lastHealthcheck :: (RP.Parsed Std_.Int64)
        ,autostart :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmInfo))
deriving instance (Std_.Eq (C.Parsed VmInfo))
instance (C.Parse VmInfo (C.Parsed VmInfo)) where
    parse raw_ = (VmInfo <$> (GH.parseField #id raw_)
                         <*> (GH.parseField #name raw_)
                         <*> (GH.parseField #status raw_)
                         <*> (GH.parseField #cpuCount raw_)
                         <*> (GH.parseField #ramMb raw_)
                         <*> (GH.parseField #headless raw_)
                         <*> (GH.parseField #guestAgent raw_)
                         <*> (GH.parseField #cloudInit raw_)
                         <*> (GH.parseField #lastHealthcheck raw_)
                         <*> (GH.parseField #autostart raw_))
instance (C.Marshal VmInfo (C.Parsed VmInfo)) where
    marshalInto raw_ VmInfo{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #status status raw_)
        (GH.encodeField #cpuCount cpuCount raw_)
        (GH.encodeField #ramMb ramMb raw_)
        (GH.encodeField #headless headless raw_)
        (GH.encodeField #guestAgent guestAgent raw_)
        (GH.encodeField #cloudInit cloudInit raw_)
        (GH.encodeField #lastHealthcheck lastHealthcheck raw_)
        (GH.encodeField #autostart autostart raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot VmInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "name" GH.Slot VmInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "status" GH.Slot VmInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.VmStatus) where
    fieldByLabel  = (GH.dataField 0 1 16 0)
instance (GH.HasField "cpuCount" GH.Slot VmInfo Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 1 32 0)
instance (GH.HasField "ramMb" GH.Slot VmInfo Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 2 32 0)
instance (GH.HasField "headless" GH.Slot VmInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 16 1 1 0)
instance (GH.HasField "guestAgent" GH.Slot VmInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 17 1 1 0)
instance (GH.HasField "cloudInit" GH.Slot VmInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 18 1 1 0)
instance (GH.HasField "lastHealthcheck" GH.Slot VmInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 3 64 0)
instance (GH.HasField "autostart" GH.Slot VmInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 19 1 1 0)
data VmDetails 
type instance (R.ReprFor VmDetails) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmDetails) where
    typeId  = 12709035652582668216
instance (C.TypedStruct VmDetails) where
    numStructWords  = 7
    numStructPtrs  = 10
instance (C.Allocate VmDetails) where
    type AllocHint VmDetails = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmDetails (C.Parsed VmDetails))
instance (C.AllocateList VmDetails) where
    type ListAllocHint VmDetails = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmDetails (C.Parsed VmDetails))
data instance C.Parsed VmDetails
    = VmDetails 
        {id :: (RP.Parsed Std_.Int64)
        ,name :: (RP.Parsed Basics.Text)
        ,createdAt :: (RP.Parsed Std_.Int64)
        ,status :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.VmStatus)
        ,cpuCount :: (RP.Parsed Std_.Int32)
        ,ramMb :: (RP.Parsed Std_.Int32)
        ,description :: (RP.Parsed Basics.Text)
        ,drives :: (RP.Parsed (R.List DriveInfo))
        ,netIfs :: (RP.Parsed (R.List NetIfInfo))
        ,sharedDirs :: (RP.Parsed (R.List SharedDirInfo))
        ,headless :: (RP.Parsed Std_.Bool)
        ,monitorSocket :: (RP.Parsed Basics.Text)
        ,spicePort :: (RP.Parsed Std_.Int32)
        ,vsockCid :: (RP.Parsed Std_.Int32)
        ,serialSocket :: (RP.Parsed Basics.Text)
        ,guestAgentSocket :: (RP.Parsed Basics.Text)
        ,guestAgent :: (RP.Parsed Std_.Bool)
        ,cloudInit :: (RP.Parsed Std_.Bool)
        ,cloudInitConfig :: (RP.Parsed Capnp.Gen.ById.Xeb6a435f11477f84.CloudInitInfo)
        ,lastHealthcheck :: (RP.Parsed Std_.Int64)
        ,autostart :: (RP.Parsed Std_.Bool)
        ,errorMessage :: (RP.Parsed Basics.Text)
        ,lastErrorAt :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmDetails))
deriving instance (Std_.Eq (C.Parsed VmDetails))
instance (C.Parse VmDetails (C.Parsed VmDetails)) where
    parse raw_ = (VmDetails <$> (GH.parseField #id raw_)
                            <*> (GH.parseField #name raw_)
                            <*> (GH.parseField #createdAt raw_)
                            <*> (GH.parseField #status raw_)
                            <*> (GH.parseField #cpuCount raw_)
                            <*> (GH.parseField #ramMb raw_)
                            <*> (GH.parseField #description raw_)
                            <*> (GH.parseField #drives raw_)
                            <*> (GH.parseField #netIfs raw_)
                            <*> (GH.parseField #sharedDirs raw_)
                            <*> (GH.parseField #headless raw_)
                            <*> (GH.parseField #monitorSocket raw_)
                            <*> (GH.parseField #spicePort raw_)
                            <*> (GH.parseField #vsockCid raw_)
                            <*> (GH.parseField #serialSocket raw_)
                            <*> (GH.parseField #guestAgentSocket raw_)
                            <*> (GH.parseField #guestAgent raw_)
                            <*> (GH.parseField #cloudInit raw_)
                            <*> (GH.parseField #cloudInitConfig raw_)
                            <*> (GH.parseField #lastHealthcheck raw_)
                            <*> (GH.parseField #autostart raw_)
                            <*> (GH.parseField #errorMessage raw_)
                            <*> (GH.parseField #lastErrorAt raw_))
instance (C.Marshal VmDetails (C.Parsed VmDetails)) where
    marshalInto raw_ VmDetails{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #createdAt createdAt raw_)
        (GH.encodeField #status status raw_)
        (GH.encodeField #cpuCount cpuCount raw_)
        (GH.encodeField #ramMb ramMb raw_)
        (GH.encodeField #description description raw_)
        (GH.encodeField #drives drives raw_)
        (GH.encodeField #netIfs netIfs raw_)
        (GH.encodeField #sharedDirs sharedDirs raw_)
        (GH.encodeField #headless headless raw_)
        (GH.encodeField #monitorSocket monitorSocket raw_)
        (GH.encodeField #spicePort spicePort raw_)
        (GH.encodeField #vsockCid vsockCid raw_)
        (GH.encodeField #serialSocket serialSocket raw_)
        (GH.encodeField #guestAgentSocket guestAgentSocket raw_)
        (GH.encodeField #guestAgent guestAgent raw_)
        (GH.encodeField #cloudInit cloudInit raw_)
        (GH.encodeField #cloudInitConfig cloudInitConfig raw_)
        (GH.encodeField #lastHealthcheck lastHealthcheck raw_)
        (GH.encodeField #autostart autostart raw_)
        (GH.encodeField #errorMessage errorMessage raw_)
        (GH.encodeField #lastErrorAt lastErrorAt raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot VmDetails Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "name" GH.Slot VmDetails Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "createdAt" GH.Slot VmDetails Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "status" GH.Slot VmDetails Capnp.Gen.ById.Xbf9b09f64c0dd40d.VmStatus) where
    fieldByLabel  = (GH.dataField 0 2 16 0)
instance (GH.HasField "cpuCount" GH.Slot VmDetails Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 2 32 0)
instance (GH.HasField "ramMb" GH.Slot VmDetails Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 3 32 0)
instance (GH.HasField "description" GH.Slot VmDetails Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "drives" GH.Slot VmDetails (R.List DriveInfo)) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "netIfs" GH.Slot VmDetails (R.List NetIfInfo)) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "sharedDirs" GH.Slot VmDetails (R.List SharedDirInfo)) where
    fieldByLabel  = (GH.ptrField 4)
instance (GH.HasField "headless" GH.Slot VmDetails Std_.Bool) where
    fieldByLabel  = (GH.dataField 16 2 1 0)
instance (GH.HasField "monitorSocket" GH.Slot VmDetails Basics.Text) where
    fieldByLabel  = (GH.ptrField 5)
instance (GH.HasField "spicePort" GH.Slot VmDetails Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 3 32 0)
instance (GH.HasField "vsockCid" GH.Slot VmDetails Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 4 32 0)
instance (GH.HasField "serialSocket" GH.Slot VmDetails Basics.Text) where
    fieldByLabel  = (GH.ptrField 6)
instance (GH.HasField "guestAgentSocket" GH.Slot VmDetails Basics.Text) where
    fieldByLabel  = (GH.ptrField 7)
instance (GH.HasField "guestAgent" GH.Slot VmDetails Std_.Bool) where
    fieldByLabel  = (GH.dataField 17 2 1 0)
instance (GH.HasField "cloudInit" GH.Slot VmDetails Std_.Bool) where
    fieldByLabel  = (GH.dataField 18 2 1 0)
instance (GH.HasField "cloudInitConfig" GH.Slot VmDetails Capnp.Gen.ById.Xeb6a435f11477f84.CloudInitInfo) where
    fieldByLabel  = (GH.ptrField 8)
instance (GH.HasField "lastHealthcheck" GH.Slot VmDetails Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 5 64 0)
instance (GH.HasField "autostart" GH.Slot VmDetails Std_.Bool) where
    fieldByLabel  = (GH.dataField 19 2 1 0)
instance (GH.HasField "errorMessage" GH.Slot VmDetails Basics.Text) where
    fieldByLabel  = (GH.ptrField 9)
instance (GH.HasField "lastErrorAt" GH.Slot VmDetails Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 6 64 0)
data DriveInfo 
type instance (R.ReprFor DriveInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DriveInfo) where
    typeId  = 13983985780014021557
instance (C.TypedStruct DriveInfo) where
    numStructWords  = 4
    numStructPtrs  = 2
instance (C.Allocate DriveInfo) where
    type AllocHint DriveInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DriveInfo (C.Parsed DriveInfo))
instance (C.AllocateList DriveInfo) where
    type ListAllocHint DriveInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DriveInfo (C.Parsed DriveInfo))
data instance C.Parsed DriveInfo
    = DriveInfo 
        {id :: (RP.Parsed Std_.Int64)
        ,diskImageId :: (RP.Parsed Std_.Int64)
        ,diskImageName :: (RP.Parsed Basics.Text)
        ,interface :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveInterface)
        ,filePath :: (RP.Parsed Basics.Text)
        ,format :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveFormat)
        ,media :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveMedia)
        ,readOnly :: (RP.Parsed Std_.Bool)
        ,cacheType :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.CacheType)
        ,discard :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DriveInfo))
deriving instance (Std_.Eq (C.Parsed DriveInfo))
instance (C.Parse DriveInfo (C.Parsed DriveInfo)) where
    parse raw_ = (DriveInfo <$> (GH.parseField #id raw_)
                            <*> (GH.parseField #diskImageId raw_)
                            <*> (GH.parseField #diskImageName raw_)
                            <*> (GH.parseField #interface raw_)
                            <*> (GH.parseField #filePath raw_)
                            <*> (GH.parseField #format raw_)
                            <*> (GH.parseField #media raw_)
                            <*> (GH.parseField #readOnly raw_)
                            <*> (GH.parseField #cacheType raw_)
                            <*> (GH.parseField #discard raw_))
instance (C.Marshal DriveInfo (C.Parsed DriveInfo)) where
    marshalInto raw_ DriveInfo{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #diskImageId diskImageId raw_)
        (GH.encodeField #diskImageName diskImageName raw_)
        (GH.encodeField #interface interface raw_)
        (GH.encodeField #filePath filePath raw_)
        (GH.encodeField #format format raw_)
        (GH.encodeField #media media raw_)
        (GH.encodeField #readOnly readOnly raw_)
        (GH.encodeField #cacheType cacheType raw_)
        (GH.encodeField #discard discard raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot DriveInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "diskImageId" GH.Slot DriveInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 1 64 0)
instance (GH.HasField "diskImageName" GH.Slot DriveInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "interface" GH.Slot DriveInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveInterface) where
    fieldByLabel  = (GH.dataField 0 2 16 0)
instance (GH.HasField "filePath" GH.Slot DriveInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "format" GH.Slot DriveInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveFormat) where
    fieldByLabel  = (GH.dataField 16 2 16 0)
instance (GH.HasField "media" GH.Slot DriveInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveMedia) where
    fieldByLabel  = (GH.dataField 32 2 16 0)
instance (GH.HasField "readOnly" GH.Slot DriveInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 48 2 1 0)
instance (GH.HasField "cacheType" GH.Slot DriveInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.CacheType) where
    fieldByLabel  = (GH.dataField 0 3 16 0)
instance (GH.HasField "discard" GH.Slot DriveInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 49 2 1 0)
data NetIfInfo 
type instance (R.ReprFor NetIfInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetIfInfo) where
    typeId  = 12350622186082163555
instance (C.TypedStruct NetIfInfo) where
    numStructWords  = 3
    numStructPtrs  = 4
instance (C.Allocate NetIfInfo) where
    type AllocHint NetIfInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetIfInfo (C.Parsed NetIfInfo))
instance (C.AllocateList NetIfInfo) where
    type ListAllocHint NetIfInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetIfInfo (C.Parsed NetIfInfo))
data instance C.Parsed NetIfInfo
    = NetIfInfo 
        {id :: (RP.Parsed Std_.Int64)
        ,type_ :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.NetInterfaceType)
        ,hostDevice :: (RP.Parsed Basics.Text)
        ,macAddress :: (RP.Parsed Basics.Text)
        ,networkId :: (RP.Parsed Std_.Int64)
        ,networkName :: (RP.Parsed Basics.Text)
        ,guestIpAddresses :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetIfInfo))
deriving instance (Std_.Eq (C.Parsed NetIfInfo))
instance (C.Parse NetIfInfo (C.Parsed NetIfInfo)) where
    parse raw_ = (NetIfInfo <$> (GH.parseField #id raw_)
                            <*> (GH.parseField #type_ raw_)
                            <*> (GH.parseField #hostDevice raw_)
                            <*> (GH.parseField #macAddress raw_)
                            <*> (GH.parseField #networkId raw_)
                            <*> (GH.parseField #networkName raw_)
                            <*> (GH.parseField #guestIpAddresses raw_))
instance (C.Marshal NetIfInfo (C.Parsed NetIfInfo)) where
    marshalInto raw_ NetIfInfo{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #type_ type_ raw_)
        (GH.encodeField #hostDevice hostDevice raw_)
        (GH.encodeField #macAddress macAddress raw_)
        (GH.encodeField #networkId networkId raw_)
        (GH.encodeField #networkName networkName raw_)
        (GH.encodeField #guestIpAddresses guestIpAddresses raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot NetIfInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "type_" GH.Slot NetIfInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.NetInterfaceType) where
    fieldByLabel  = (GH.dataField 0 1 16 0)
instance (GH.HasField "hostDevice" GH.Slot NetIfInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "macAddress" GH.Slot NetIfInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "networkId" GH.Slot NetIfInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 2 64 0)
instance (GH.HasField "networkName" GH.Slot NetIfInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "guestIpAddresses" GH.Slot NetIfInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 3)
data SharedDirInfo 
type instance (R.ReprFor SharedDirInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId SharedDirInfo) where
    typeId  = 11676741422263871025
instance (C.TypedStruct SharedDirInfo) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate SharedDirInfo) where
    type AllocHint SharedDirInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc SharedDirInfo (C.Parsed SharedDirInfo))
instance (C.AllocateList SharedDirInfo) where
    type ListAllocHint SharedDirInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc SharedDirInfo (C.Parsed SharedDirInfo))
data instance C.Parsed SharedDirInfo
    = SharedDirInfo 
        {id :: (RP.Parsed Std_.Int64)
        ,path :: (RP.Parsed Basics.Text)
        ,tag :: (RP.Parsed Basics.Text)
        ,cache :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.SharedDirCache)
        ,readOnly :: (RP.Parsed Std_.Bool)
        ,pid :: (RP.Parsed Std_.Int32)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed SharedDirInfo))
deriving instance (Std_.Eq (C.Parsed SharedDirInfo))
instance (C.Parse SharedDirInfo (C.Parsed SharedDirInfo)) where
    parse raw_ = (SharedDirInfo <$> (GH.parseField #id raw_)
                                <*> (GH.parseField #path raw_)
                                <*> (GH.parseField #tag raw_)
                                <*> (GH.parseField #cache raw_)
                                <*> (GH.parseField #readOnly raw_)
                                <*> (GH.parseField #pid raw_))
instance (C.Marshal SharedDirInfo (C.Parsed SharedDirInfo)) where
    marshalInto raw_ SharedDirInfo{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #path path raw_)
        (GH.encodeField #tag tag raw_)
        (GH.encodeField #cache cache raw_)
        (GH.encodeField #readOnly readOnly raw_)
        (GH.encodeField #pid pid raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot SharedDirInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "path" GH.Slot SharedDirInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "tag" GH.Slot SharedDirInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "cache" GH.Slot SharedDirInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.SharedDirCache) where
    fieldByLabel  = (GH.dataField 0 1 16 0)
instance (GH.HasField "readOnly" GH.Slot SharedDirInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 16 1 1 0)
instance (GH.HasField "pid" GH.Slot SharedDirInfo Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 1 32 0)
data VmCreateParams 
type instance (R.ReprFor VmCreateParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmCreateParams) where
    typeId  = 12441806447859270163
instance (C.TypedStruct VmCreateParams) where
    numStructWords  = 2
    numStructPtrs  = 3
instance (C.Allocate VmCreateParams) where
    type AllocHint VmCreateParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmCreateParams (C.Parsed VmCreateParams))
instance (C.AllocateList VmCreateParams) where
    type ListAllocHint VmCreateParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmCreateParams (C.Parsed VmCreateParams))
data instance C.Parsed VmCreateParams
    = VmCreateParams 
        {name :: (RP.Parsed Basics.Text)
        ,cpuCount :: (RP.Parsed Std_.Int32)
        ,ramMb :: (RP.Parsed Std_.Int32)
        ,description :: (RP.Parsed Basics.Text)
        ,headless :: (RP.Parsed Std_.Bool)
        ,guestAgent :: (RP.Parsed Std_.Bool)
        ,cloudInit :: (RP.Parsed Std_.Bool)
        ,autostart :: (RP.Parsed Std_.Bool)
        ,node :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmCreateParams))
deriving instance (Std_.Eq (C.Parsed VmCreateParams))
instance (C.Parse VmCreateParams (C.Parsed VmCreateParams)) where
    parse raw_ = (VmCreateParams <$> (GH.parseField #name raw_)
                                 <*> (GH.parseField #cpuCount raw_)
                                 <*> (GH.parseField #ramMb raw_)
                                 <*> (GH.parseField #description raw_)
                                 <*> (GH.parseField #headless raw_)
                                 <*> (GH.parseField #guestAgent raw_)
                                 <*> (GH.parseField #cloudInit raw_)
                                 <*> (GH.parseField #autostart raw_)
                                 <*> (GH.parseField #node raw_))
instance (C.Marshal VmCreateParams (C.Parsed VmCreateParams)) where
    marshalInto raw_ VmCreateParams{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #cpuCount cpuCount raw_)
        (GH.encodeField #ramMb ramMb raw_)
        (GH.encodeField #description description raw_)
        (GH.encodeField #headless headless raw_)
        (GH.encodeField #guestAgent guestAgent raw_)
        (GH.encodeField #cloudInit cloudInit raw_)
        (GH.encodeField #autostart autostart raw_)
        (GH.encodeField #node node raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot VmCreateParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "cpuCount" GH.Slot VmCreateParams Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 1)
instance (GH.HasField "ramMb" GH.Slot VmCreateParams Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 0 32 1024)
instance (GH.HasField "description" GH.Slot VmCreateParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "headless" GH.Slot VmCreateParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 1 1 0)
instance (GH.HasField "guestAgent" GH.Slot VmCreateParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 1 1 0)
instance (GH.HasField "cloudInit" GH.Slot VmCreateParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 2 1 1 0)
instance (GH.HasField "autostart" GH.Slot VmCreateParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 3 1 1 0)
instance (GH.HasField "node" GH.Slot VmCreateParams Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 2)
data VmEditParams 
type instance (R.ReprFor VmEditParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmEditParams) where
    typeId  = 10951018206413285449
instance (C.TypedStruct VmEditParams) where
    numStructWords  = 2
    numStructPtrs  = 2
instance (C.Allocate VmEditParams) where
    type AllocHint VmEditParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmEditParams (C.Parsed VmEditParams))
instance (C.AllocateList VmEditParams) where
    type ListAllocHint VmEditParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmEditParams (C.Parsed VmEditParams))
data instance C.Parsed VmEditParams
    = VmEditParams 
        {hasName :: (RP.Parsed Std_.Bool)
        ,name :: (RP.Parsed Basics.Text)
        ,hasCpuCount :: (RP.Parsed Std_.Bool)
        ,cpuCount :: (RP.Parsed Std_.Int32)
        ,hasRamMb :: (RP.Parsed Std_.Bool)
        ,ramMb :: (RP.Parsed Std_.Int32)
        ,hasDescription :: (RP.Parsed Std_.Bool)
        ,description :: (RP.Parsed Basics.Text)
        ,hasHeadless :: (RP.Parsed Std_.Bool)
        ,headless :: (RP.Parsed Std_.Bool)
        ,hasGuestAgent :: (RP.Parsed Std_.Bool)
        ,guestAgent :: (RP.Parsed Std_.Bool)
        ,hasCloudInit :: (RP.Parsed Std_.Bool)
        ,cloudInit :: (RP.Parsed Std_.Bool)
        ,hasAutostart :: (RP.Parsed Std_.Bool)
        ,autostart :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmEditParams))
deriving instance (Std_.Eq (C.Parsed VmEditParams))
instance (C.Parse VmEditParams (C.Parsed VmEditParams)) where
    parse raw_ = (VmEditParams <$> (GH.parseField #hasName raw_)
                               <*> (GH.parseField #name raw_)
                               <*> (GH.parseField #hasCpuCount raw_)
                               <*> (GH.parseField #cpuCount raw_)
                               <*> (GH.parseField #hasRamMb raw_)
                               <*> (GH.parseField #ramMb raw_)
                               <*> (GH.parseField #hasDescription raw_)
                               <*> (GH.parseField #description raw_)
                               <*> (GH.parseField #hasHeadless raw_)
                               <*> (GH.parseField #headless raw_)
                               <*> (GH.parseField #hasGuestAgent raw_)
                               <*> (GH.parseField #guestAgent raw_)
                               <*> (GH.parseField #hasCloudInit raw_)
                               <*> (GH.parseField #cloudInit raw_)
                               <*> (GH.parseField #hasAutostart raw_)
                               <*> (GH.parseField #autostart raw_))
instance (C.Marshal VmEditParams (C.Parsed VmEditParams)) where
    marshalInto raw_ VmEditParams{..} = (do
        (GH.encodeField #hasName hasName raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #hasCpuCount hasCpuCount raw_)
        (GH.encodeField #cpuCount cpuCount raw_)
        (GH.encodeField #hasRamMb hasRamMb raw_)
        (GH.encodeField #ramMb ramMb raw_)
        (GH.encodeField #hasDescription hasDescription raw_)
        (GH.encodeField #description description raw_)
        (GH.encodeField #hasHeadless hasHeadless raw_)
        (GH.encodeField #headless headless raw_)
        (GH.encodeField #hasGuestAgent hasGuestAgent raw_)
        (GH.encodeField #guestAgent guestAgent raw_)
        (GH.encodeField #hasCloudInit hasCloudInit raw_)
        (GH.encodeField #cloudInit cloudInit raw_)
        (GH.encodeField #hasAutostart hasAutostart raw_)
        (GH.encodeField #autostart autostart raw_)
        (Std_.pure ())
        )
instance (GH.HasField "hasName" GH.Slot VmEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (GH.HasField "name" GH.Slot VmEditParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "hasCpuCount" GH.Slot VmEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 0 1 0)
instance (GH.HasField "cpuCount" GH.Slot VmEditParams Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
instance (GH.HasField "hasRamMb" GH.Slot VmEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 2 0 1 0)
instance (GH.HasField "ramMb" GH.Slot VmEditParams Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
instance (GH.HasField "hasDescription" GH.Slot VmEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 3 0 1 0)
instance (GH.HasField "description" GH.Slot VmEditParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "hasHeadless" GH.Slot VmEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 4 0 1 0)
instance (GH.HasField "headless" GH.Slot VmEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 5 0 1 0)
instance (GH.HasField "hasGuestAgent" GH.Slot VmEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 6 0 1 0)
instance (GH.HasField "guestAgent" GH.Slot VmEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 7 0 1 0)
instance (GH.HasField "hasCloudInit" GH.Slot VmEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 8 0 1 0)
instance (GH.HasField "cloudInit" GH.Slot VmEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 9 0 1 0)
instance (GH.HasField "hasAutostart" GH.Slot VmEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 10 0 1 0)
instance (GH.HasField "autostart" GH.Slot VmEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 11 0 1 0)
data DriveAttachParams 
type instance (R.ReprFor DriveAttachParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId DriveAttachParams) where
    typeId  = 12591019515819060321
instance (C.TypedStruct DriveAttachParams) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate DriveAttachParams) where
    type AllocHint DriveAttachParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc DriveAttachParams (C.Parsed DriveAttachParams))
instance (C.AllocateList DriveAttachParams) where
    type ListAllocHint DriveAttachParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc DriveAttachParams (C.Parsed DriveAttachParams))
data instance C.Parsed DriveAttachParams
    = DriveAttachParams 
        {diskRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)
        ,interface :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveInterface)
        ,media :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveMedia)
        ,readOnly :: (RP.Parsed Std_.Bool)
        ,cacheType :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.CacheType)
        ,discard :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed DriveAttachParams))
deriving instance (Std_.Eq (C.Parsed DriveAttachParams))
instance (C.Parse DriveAttachParams (C.Parsed DriveAttachParams)) where
    parse raw_ = (DriveAttachParams <$> (GH.parseField #diskRef raw_)
                                    <*> (GH.parseField #interface raw_)
                                    <*> (GH.parseField #media raw_)
                                    <*> (GH.parseField #readOnly raw_)
                                    <*> (GH.parseField #cacheType raw_)
                                    <*> (GH.parseField #discard raw_))
instance (C.Marshal DriveAttachParams (C.Parsed DriveAttachParams)) where
    marshalInto raw_ DriveAttachParams{..} = (do
        (GH.encodeField #diskRef diskRef raw_)
        (GH.encodeField #interface interface raw_)
        (GH.encodeField #media media raw_)
        (GH.encodeField #readOnly readOnly raw_)
        (GH.encodeField #cacheType cacheType raw_)
        (GH.encodeField #discard discard raw_)
        (Std_.pure ())
        )
instance (GH.HasField "diskRef" GH.Slot DriveAttachParams Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "interface" GH.Slot DriveAttachParams Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveInterface) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "media" GH.Slot DriveAttachParams Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveMedia) where
    fieldByLabel  = (GH.dataField 16 0 16 0)
instance (GH.HasField "readOnly" GH.Slot DriveAttachParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 32 0 1 0)
instance (GH.HasField "cacheType" GH.Slot DriveAttachParams Capnp.Gen.ById.Xbf9b09f64c0dd40d.CacheType) where
    fieldByLabel  = (GH.dataField 48 0 16 1)
instance (GH.HasField "discard" GH.Slot DriveAttachParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 33 0 1 0)
data NetIfAddParams 
type instance (R.ReprFor NetIfAddParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NetIfAddParams) where
    typeId  = 9545785621897940361
instance (C.TypedStruct NetIfAddParams) where
    numStructWords  = 1
    numStructPtrs  = 3
instance (C.Allocate NetIfAddParams) where
    type AllocHint NetIfAddParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NetIfAddParams (C.Parsed NetIfAddParams))
instance (C.AllocateList NetIfAddParams) where
    type ListAllocHint NetIfAddParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NetIfAddParams (C.Parsed NetIfAddParams))
data instance C.Parsed NetIfAddParams
    = NetIfAddParams 
        {type_ :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.NetInterfaceType)
        ,hostDevice :: (RP.Parsed Basics.Text)
        ,macAddress :: (RP.Parsed Basics.Text)
        ,networkRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NetIfAddParams))
deriving instance (Std_.Eq (C.Parsed NetIfAddParams))
instance (C.Parse NetIfAddParams (C.Parsed NetIfAddParams)) where
    parse raw_ = (NetIfAddParams <$> (GH.parseField #type_ raw_)
                                 <*> (GH.parseField #hostDevice raw_)
                                 <*> (GH.parseField #macAddress raw_)
                                 <*> (GH.parseField #networkRef raw_))
instance (C.Marshal NetIfAddParams (C.Parsed NetIfAddParams)) where
    marshalInto raw_ NetIfAddParams{..} = (do
        (GH.encodeField #type_ type_ raw_)
        (GH.encodeField #hostDevice hostDevice raw_)
        (GH.encodeField #macAddress macAddress raw_)
        (GH.encodeField #networkRef networkRef raw_)
        (Std_.pure ())
        )
instance (GH.HasField "type_" GH.Slot NetIfAddParams Capnp.Gen.ById.Xbf9b09f64c0dd40d.NetInterfaceType) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "hostDevice" GH.Slot NetIfAddParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "macAddress" GH.Slot NetIfAddParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "networkRef" GH.Slot NetIfAddParams Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 2)
data SharedDirAddParams 
type instance (R.ReprFor SharedDirAddParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId SharedDirAddParams) where
    typeId  = 16950180398000578498
instance (C.TypedStruct SharedDirAddParams) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate SharedDirAddParams) where
    type AllocHint SharedDirAddParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc SharedDirAddParams (C.Parsed SharedDirAddParams))
instance (C.AllocateList SharedDirAddParams) where
    type ListAllocHint SharedDirAddParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc SharedDirAddParams (C.Parsed SharedDirAddParams))
data instance C.Parsed SharedDirAddParams
    = SharedDirAddParams 
        {path :: (RP.Parsed Basics.Text)
        ,tag :: (RP.Parsed Basics.Text)
        ,cache :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.SharedDirCache)
        ,readOnly :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed SharedDirAddParams))
deriving instance (Std_.Eq (C.Parsed SharedDirAddParams))
instance (C.Parse SharedDirAddParams (C.Parsed SharedDirAddParams)) where
    parse raw_ = (SharedDirAddParams <$> (GH.parseField #path raw_)
                                     <*> (GH.parseField #tag raw_)
                                     <*> (GH.parseField #cache raw_)
                                     <*> (GH.parseField #readOnly raw_))
instance (C.Marshal SharedDirAddParams (C.Parsed SharedDirAddParams)) where
    marshalInto raw_ SharedDirAddParams{..} = (do
        (GH.encodeField #path path raw_)
        (GH.encodeField #tag tag raw_)
        (GH.encodeField #cache cache raw_)
        (GH.encodeField #readOnly readOnly raw_)
        (Std_.pure ())
        )
instance (GH.HasField "path" GH.Slot SharedDirAddParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "tag" GH.Slot SharedDirAddParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "cache" GH.Slot SharedDirAddParams Capnp.Gen.ById.Xbf9b09f64c0dd40d.SharedDirCache) where
    fieldByLabel  = (GH.dataField 0 0 16 1)
instance (GH.HasField "readOnly" GH.Slot SharedDirAddParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 16 0 1 0)
data GuestExecResult 
type instance (R.ReprFor GuestExecResult) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId GuestExecResult) where
    typeId  = 16964713191986031040
instance (C.TypedStruct GuestExecResult) where
    numStructWords  = 1
    numStructPtrs  = 2
instance (C.Allocate GuestExecResult) where
    type AllocHint GuestExecResult = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc GuestExecResult (C.Parsed GuestExecResult))
instance (C.AllocateList GuestExecResult) where
    type ListAllocHint GuestExecResult = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc GuestExecResult (C.Parsed GuestExecResult))
data instance C.Parsed GuestExecResult
    = GuestExecResult 
        {exitCode :: (RP.Parsed Std_.Int32)
        ,stdout :: (RP.Parsed Basics.Text)
        ,stderr :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed GuestExecResult))
deriving instance (Std_.Eq (C.Parsed GuestExecResult))
instance (C.Parse GuestExecResult (C.Parsed GuestExecResult)) where
    parse raw_ = (GuestExecResult <$> (GH.parseField #exitCode raw_)
                                  <*> (GH.parseField #stdout raw_)
                                  <*> (GH.parseField #stderr raw_))
instance (C.Marshal GuestExecResult (C.Parsed GuestExecResult)) where
    marshalInto raw_ GuestExecResult{..} = (do
        (GH.encodeField #exitCode exitCode raw_)
        (GH.encodeField #stdout stdout raw_)
        (GH.encodeField #stderr stderr raw_)
        (Std_.pure ())
        )
instance (GH.HasField "exitCode" GH.Slot GuestExecResult Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 0)
instance (GH.HasField "stdout" GH.Slot GuestExecResult Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "stderr" GH.Slot GuestExecResult Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
data VmManager 
type instance (R.ReprFor VmManager) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId VmManager) where
    typeId  = 10855964284799393326
instance (C.Parse VmManager (GH.Client VmManager)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export VmManager) where
    type Server VmManager = VmManager'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(VmManager)) [(GH.toUntypedMethodHandler ((vmManager'list) s_))
                                                                           ,(GH.toUntypedMethodHandler ((vmManager'get) s_))
                                                                           ,(GH.toUntypedMethodHandler ((vmManager'create) s_))] [])
class (VmManager'server_ s_) where
    {-# MINIMAL vmManager'list,vmManager'get,vmManager'create #-}
    vmManager'list :: s_ -> (GH.MethodHandler VmManager'list'params VmManager'list'results)
    vmManager'list _ = GH.methodUnimplemented
    vmManager'get :: s_ -> (GH.MethodHandler VmManager'get'params VmManager'get'results)
    vmManager'get _ = GH.methodUnimplemented
    vmManager'create :: s_ -> (GH.MethodHandler VmManager'create'params VmManager'create'results)
    vmManager'create _ = GH.methodUnimplemented
instance (GH.HasMethod "list" VmManager VmManager'list'params VmManager'list'results) where
    methodByLabel  = (GH.Method 10855964284799393326 0)
instance (GH.HasMethod "get" VmManager VmManager'get'params VmManager'get'results) where
    methodByLabel  = (GH.Method 10855964284799393326 1)
instance (GH.HasMethod "create" VmManager VmManager'create'params VmManager'create'results) where
    methodByLabel  = (GH.Method 10855964284799393326 2)
data VmManager'list'params 
type instance (R.ReprFor VmManager'list'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmManager'list'params) where
    typeId  = 9887958901601233732
instance (C.TypedStruct VmManager'list'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate VmManager'list'params) where
    type AllocHint VmManager'list'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmManager'list'params (C.Parsed VmManager'list'params))
instance (C.AllocateList VmManager'list'params) where
    type ListAllocHint VmManager'list'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmManager'list'params (C.Parsed VmManager'list'params))
data instance C.Parsed VmManager'list'params
    = VmManager'list'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmManager'list'params))
deriving instance (Std_.Eq (C.Parsed VmManager'list'params))
instance (C.Parse VmManager'list'params (C.Parsed VmManager'list'params)) where
    parse raw_ = (Std_.pure VmManager'list'params)
instance (C.Marshal VmManager'list'params (C.Parsed VmManager'list'params)) where
    marshalInto _raw (VmManager'list'params) = (Std_.pure ())
data VmManager'list'results 
type instance (R.ReprFor VmManager'list'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmManager'list'results) where
    typeId  = 15213160889700237074
instance (C.TypedStruct VmManager'list'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate VmManager'list'results) where
    type AllocHint VmManager'list'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmManager'list'results (C.Parsed VmManager'list'results))
instance (C.AllocateList VmManager'list'results) where
    type ListAllocHint VmManager'list'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmManager'list'results (C.Parsed VmManager'list'results))
data instance C.Parsed VmManager'list'results
    = VmManager'list'results 
        {vms :: (RP.Parsed (R.List VmInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmManager'list'results))
deriving instance (Std_.Eq (C.Parsed VmManager'list'results))
instance (C.Parse VmManager'list'results (C.Parsed VmManager'list'results)) where
    parse raw_ = (VmManager'list'results <$> (GH.parseField #vms raw_))
instance (C.Marshal VmManager'list'results (C.Parsed VmManager'list'results)) where
    marshalInto raw_ VmManager'list'results{..} = (do
        (GH.encodeField #vms vms raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vms" GH.Slot VmManager'list'results (R.List VmInfo)) where
    fieldByLabel  = (GH.ptrField 0)
data VmManager'get'params 
type instance (R.ReprFor VmManager'get'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmManager'get'params) where
    typeId  = 11327179599489891895
instance (C.TypedStruct VmManager'get'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate VmManager'get'params) where
    type AllocHint VmManager'get'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmManager'get'params (C.Parsed VmManager'get'params))
instance (C.AllocateList VmManager'get'params) where
    type ListAllocHint VmManager'get'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmManager'get'params (C.Parsed VmManager'get'params))
data instance C.Parsed VmManager'get'params
    = VmManager'get'params 
        {ref :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmManager'get'params))
deriving instance (Std_.Eq (C.Parsed VmManager'get'params))
instance (C.Parse VmManager'get'params (C.Parsed VmManager'get'params)) where
    parse raw_ = (VmManager'get'params <$> (GH.parseField #ref raw_))
instance (C.Marshal VmManager'get'params (C.Parsed VmManager'get'params)) where
    marshalInto raw_ VmManager'get'params{..} = (do
        (GH.encodeField #ref ref raw_)
        (Std_.pure ())
        )
instance (GH.HasField "ref" GH.Slot VmManager'get'params Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
data VmManager'get'results 
type instance (R.ReprFor VmManager'get'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmManager'get'results) where
    typeId  = 10878197849910707192
instance (C.TypedStruct VmManager'get'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate VmManager'get'results) where
    type AllocHint VmManager'get'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmManager'get'results (C.Parsed VmManager'get'results))
instance (C.AllocateList VmManager'get'results) where
    type ListAllocHint VmManager'get'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmManager'get'results (C.Parsed VmManager'get'results))
data instance C.Parsed VmManager'get'results
    = VmManager'get'results 
        {vm :: (RP.Parsed Vm)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmManager'get'results))
deriving instance (Std_.Eq (C.Parsed VmManager'get'results))
instance (C.Parse VmManager'get'results (C.Parsed VmManager'get'results)) where
    parse raw_ = (VmManager'get'results <$> (GH.parseField #vm raw_))
instance (C.Marshal VmManager'get'results (C.Parsed VmManager'get'results)) where
    marshalInto raw_ VmManager'get'results{..} = (do
        (GH.encodeField #vm vm raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vm" GH.Slot VmManager'get'results Vm) where
    fieldByLabel  = (GH.ptrField 0)
data VmManager'create'params 
type instance (R.ReprFor VmManager'create'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmManager'create'params) where
    typeId  = 11196318238028721239
instance (C.TypedStruct VmManager'create'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate VmManager'create'params) where
    type AllocHint VmManager'create'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmManager'create'params (C.Parsed VmManager'create'params))
instance (C.AllocateList VmManager'create'params) where
    type ListAllocHint VmManager'create'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmManager'create'params (C.Parsed VmManager'create'params))
data instance C.Parsed VmManager'create'params
    = VmManager'create'params 
        {params :: (RP.Parsed VmCreateParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmManager'create'params))
deriving instance (Std_.Eq (C.Parsed VmManager'create'params))
instance (C.Parse VmManager'create'params (C.Parsed VmManager'create'params)) where
    parse raw_ = (VmManager'create'params <$> (GH.parseField #params raw_))
instance (C.Marshal VmManager'create'params (C.Parsed VmManager'create'params)) where
    marshalInto raw_ VmManager'create'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot VmManager'create'params VmCreateParams) where
    fieldByLabel  = (GH.ptrField 0)
data VmManager'create'results 
type instance (R.ReprFor VmManager'create'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmManager'create'results) where
    typeId  = 18215856284280980290
instance (C.TypedStruct VmManager'create'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate VmManager'create'results) where
    type AllocHint VmManager'create'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmManager'create'results (C.Parsed VmManager'create'results))
instance (C.AllocateList VmManager'create'results) where
    type ListAllocHint VmManager'create'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmManager'create'results (C.Parsed VmManager'create'results))
data instance C.Parsed VmManager'create'results
    = VmManager'create'results 
        {vm :: (RP.Parsed Vm)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmManager'create'results))
deriving instance (Std_.Eq (C.Parsed VmManager'create'results))
instance (C.Parse VmManager'create'results (C.Parsed VmManager'create'results)) where
    parse raw_ = (VmManager'create'results <$> (GH.parseField #vm raw_))
instance (C.Marshal VmManager'create'results (C.Parsed VmManager'create'results)) where
    marshalInto raw_ VmManager'create'results{..} = (do
        (GH.encodeField #vm vm raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vm" GH.Slot VmManager'create'results Vm) where
    fieldByLabel  = (GH.ptrField 0)
data Vm 
type instance (R.ReprFor Vm) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Vm) where
    typeId  = 17269745093196220462
instance (C.Parse Vm (GH.Client Vm)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Vm) where
    type Server Vm = Vm'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Vm)) [(GH.toUntypedMethodHandler ((vm'show) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'start) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'stop) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'pause) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'reset) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'edit) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'delete) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'cloudInit) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'viewGrant) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'guestExec) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'sendCtrlAltDel) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'serialConsole) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'serialConsoleFlush) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'hmpMonitor) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'hmpMonitorFlush) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'subscribeGuestAgent) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'attachDisk) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'detachDisk) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'addNetIf) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'removeNetIf) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'listNetIfs) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'addSharedDir) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'removeSharedDir) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'listSharedDirs) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'snapshotCreate) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'snapshotList) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'snapshotGet) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'attachSshKey) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'detachSshKey) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'listSshKeys) s_))
                                                                    ,(GH.toUntypedMethodHandler ((vm'migrate) s_))] [])
class (Vm'server_ s_) where
    {-# MINIMAL vm'show,vm'start,vm'stop,vm'pause,vm'reset,vm'edit,vm'delete,vm'cloudInit,vm'viewGrant,vm'guestExec,vm'sendCtrlAltDel,vm'serialConsole,vm'serialConsoleFlush,vm'hmpMonitor,vm'hmpMonitorFlush,vm'subscribeGuestAgent,vm'attachDisk,vm'detachDisk,vm'addNetIf,vm'removeNetIf,vm'listNetIfs,vm'addSharedDir,vm'removeSharedDir,vm'listSharedDirs,vm'snapshotCreate,vm'snapshotList,vm'snapshotGet,vm'attachSshKey,vm'detachSshKey,vm'listSshKeys,vm'migrate #-}
    vm'show :: s_ -> (GH.MethodHandler Vm'show'params Vm'show'results)
    vm'show _ = GH.methodUnimplemented
    vm'start :: s_ -> (GH.MethodHandler Vm'start'params Vm'start'results)
    vm'start _ = GH.methodUnimplemented
    vm'stop :: s_ -> (GH.MethodHandler Vm'stop'params Vm'stop'results)
    vm'stop _ = GH.methodUnimplemented
    vm'pause :: s_ -> (GH.MethodHandler Vm'pause'params Vm'pause'results)
    vm'pause _ = GH.methodUnimplemented
    vm'reset :: s_ -> (GH.MethodHandler Vm'reset'params Vm'reset'results)
    vm'reset _ = GH.methodUnimplemented
    vm'edit :: s_ -> (GH.MethodHandler Vm'edit'params Vm'edit'results)
    vm'edit _ = GH.methodUnimplemented
    vm'delete :: s_ -> (GH.MethodHandler Vm'delete'params Vm'delete'results)
    vm'delete _ = GH.methodUnimplemented
    vm'cloudInit :: s_ -> (GH.MethodHandler Vm'cloudInit'params Vm'cloudInit'results)
    vm'cloudInit _ = GH.methodUnimplemented
    vm'viewGrant :: s_ -> (GH.MethodHandler Vm'viewGrant'params Vm'viewGrant'results)
    vm'viewGrant _ = GH.methodUnimplemented
    vm'guestExec :: s_ -> (GH.MethodHandler Vm'guestExec'params Vm'guestExec'results)
    vm'guestExec _ = GH.methodUnimplemented
    vm'sendCtrlAltDel :: s_ -> (GH.MethodHandler Vm'sendCtrlAltDel'params Vm'sendCtrlAltDel'results)
    vm'sendCtrlAltDel _ = GH.methodUnimplemented
    vm'serialConsole :: s_ -> (GH.MethodHandler Vm'serialConsole'params Vm'serialConsole'results)
    vm'serialConsole _ = GH.methodUnimplemented
    vm'serialConsoleFlush :: s_ -> (GH.MethodHandler Vm'serialConsoleFlush'params Vm'serialConsoleFlush'results)
    vm'serialConsoleFlush _ = GH.methodUnimplemented
    vm'hmpMonitor :: s_ -> (GH.MethodHandler Vm'hmpMonitor'params Vm'hmpMonitor'results)
    vm'hmpMonitor _ = GH.methodUnimplemented
    vm'hmpMonitorFlush :: s_ -> (GH.MethodHandler Vm'hmpMonitorFlush'params Vm'hmpMonitorFlush'results)
    vm'hmpMonitorFlush _ = GH.methodUnimplemented
    vm'subscribeGuestAgent :: s_ -> (GH.MethodHandler Vm'subscribeGuestAgent'params Vm'subscribeGuestAgent'results)
    vm'subscribeGuestAgent _ = GH.methodUnimplemented
    vm'attachDisk :: s_ -> (GH.MethodHandler Vm'attachDisk'params Vm'attachDisk'results)
    vm'attachDisk _ = GH.methodUnimplemented
    vm'detachDisk :: s_ -> (GH.MethodHandler Vm'detachDisk'params Vm'detachDisk'results)
    vm'detachDisk _ = GH.methodUnimplemented
    vm'addNetIf :: s_ -> (GH.MethodHandler Vm'addNetIf'params Vm'addNetIf'results)
    vm'addNetIf _ = GH.methodUnimplemented
    vm'removeNetIf :: s_ -> (GH.MethodHandler Vm'removeNetIf'params Vm'removeNetIf'results)
    vm'removeNetIf _ = GH.methodUnimplemented
    vm'listNetIfs :: s_ -> (GH.MethodHandler Vm'listNetIfs'params Vm'listNetIfs'results)
    vm'listNetIfs _ = GH.methodUnimplemented
    vm'addSharedDir :: s_ -> (GH.MethodHandler Vm'addSharedDir'params Vm'addSharedDir'results)
    vm'addSharedDir _ = GH.methodUnimplemented
    vm'removeSharedDir :: s_ -> (GH.MethodHandler Vm'removeSharedDir'params Vm'removeSharedDir'results)
    vm'removeSharedDir _ = GH.methodUnimplemented
    vm'listSharedDirs :: s_ -> (GH.MethodHandler Vm'listSharedDirs'params Vm'listSharedDirs'results)
    vm'listSharedDirs _ = GH.methodUnimplemented
    vm'snapshotCreate :: s_ -> (GH.MethodHandler Vm'snapshotCreate'params Vm'snapshotCreate'results)
    vm'snapshotCreate _ = GH.methodUnimplemented
    vm'snapshotList :: s_ -> (GH.MethodHandler Vm'snapshotList'params Vm'snapshotList'results)
    vm'snapshotList _ = GH.methodUnimplemented
    vm'snapshotGet :: s_ -> (GH.MethodHandler Vm'snapshotGet'params Vm'snapshotGet'results)
    vm'snapshotGet _ = GH.methodUnimplemented
    vm'attachSshKey :: s_ -> (GH.MethodHandler Vm'attachSshKey'params Vm'attachSshKey'results)
    vm'attachSshKey _ = GH.methodUnimplemented
    vm'detachSshKey :: s_ -> (GH.MethodHandler Vm'detachSshKey'params Vm'detachSshKey'results)
    vm'detachSshKey _ = GH.methodUnimplemented
    vm'listSshKeys :: s_ -> (GH.MethodHandler Vm'listSshKeys'params Vm'listSshKeys'results)
    vm'listSshKeys _ = GH.methodUnimplemented
    vm'migrate :: s_ -> (GH.MethodHandler Vm'migrate'params Vm'migrate'results)
    vm'migrate _ = GH.methodUnimplemented
instance (GH.HasMethod "show" Vm Vm'show'params Vm'show'results) where
    methodByLabel  = (GH.Method 17269745093196220462 0)
instance (GH.HasMethod "start" Vm Vm'start'params Vm'start'results) where
    methodByLabel  = (GH.Method 17269745093196220462 1)
instance (GH.HasMethod "stop" Vm Vm'stop'params Vm'stop'results) where
    methodByLabel  = (GH.Method 17269745093196220462 2)
instance (GH.HasMethod "pause" Vm Vm'pause'params Vm'pause'results) where
    methodByLabel  = (GH.Method 17269745093196220462 3)
instance (GH.HasMethod "reset" Vm Vm'reset'params Vm'reset'results) where
    methodByLabel  = (GH.Method 17269745093196220462 4)
instance (GH.HasMethod "edit" Vm Vm'edit'params Vm'edit'results) where
    methodByLabel  = (GH.Method 17269745093196220462 5)
instance (GH.HasMethod "delete" Vm Vm'delete'params Vm'delete'results) where
    methodByLabel  = (GH.Method 17269745093196220462 6)
instance (GH.HasMethod "cloudInit" Vm Vm'cloudInit'params Vm'cloudInit'results) where
    methodByLabel  = (GH.Method 17269745093196220462 7)
instance (GH.HasMethod "viewGrant" Vm Vm'viewGrant'params Vm'viewGrant'results) where
    methodByLabel  = (GH.Method 17269745093196220462 8)
instance (GH.HasMethod "guestExec" Vm Vm'guestExec'params Vm'guestExec'results) where
    methodByLabel  = (GH.Method 17269745093196220462 9)
instance (GH.HasMethod "sendCtrlAltDel" Vm Vm'sendCtrlAltDel'params Vm'sendCtrlAltDel'results) where
    methodByLabel  = (GH.Method 17269745093196220462 10)
instance (GH.HasMethod "serialConsole" Vm Vm'serialConsole'params Vm'serialConsole'results) where
    methodByLabel  = (GH.Method 17269745093196220462 11)
instance (GH.HasMethod "serialConsoleFlush" Vm Vm'serialConsoleFlush'params Vm'serialConsoleFlush'results) where
    methodByLabel  = (GH.Method 17269745093196220462 12)
instance (GH.HasMethod "hmpMonitor" Vm Vm'hmpMonitor'params Vm'hmpMonitor'results) where
    methodByLabel  = (GH.Method 17269745093196220462 13)
instance (GH.HasMethod "hmpMonitorFlush" Vm Vm'hmpMonitorFlush'params Vm'hmpMonitorFlush'results) where
    methodByLabel  = (GH.Method 17269745093196220462 14)
instance (GH.HasMethod "subscribeGuestAgent" Vm Vm'subscribeGuestAgent'params Vm'subscribeGuestAgent'results) where
    methodByLabel  = (GH.Method 17269745093196220462 15)
instance (GH.HasMethod "attachDisk" Vm Vm'attachDisk'params Vm'attachDisk'results) where
    methodByLabel  = (GH.Method 17269745093196220462 16)
instance (GH.HasMethod "detachDisk" Vm Vm'detachDisk'params Vm'detachDisk'results) where
    methodByLabel  = (GH.Method 17269745093196220462 17)
instance (GH.HasMethod "addNetIf" Vm Vm'addNetIf'params Vm'addNetIf'results) where
    methodByLabel  = (GH.Method 17269745093196220462 18)
instance (GH.HasMethod "removeNetIf" Vm Vm'removeNetIf'params Vm'removeNetIf'results) where
    methodByLabel  = (GH.Method 17269745093196220462 19)
instance (GH.HasMethod "listNetIfs" Vm Vm'listNetIfs'params Vm'listNetIfs'results) where
    methodByLabel  = (GH.Method 17269745093196220462 20)
instance (GH.HasMethod "addSharedDir" Vm Vm'addSharedDir'params Vm'addSharedDir'results) where
    methodByLabel  = (GH.Method 17269745093196220462 21)
instance (GH.HasMethod "removeSharedDir" Vm Vm'removeSharedDir'params Vm'removeSharedDir'results) where
    methodByLabel  = (GH.Method 17269745093196220462 22)
instance (GH.HasMethod "listSharedDirs" Vm Vm'listSharedDirs'params Vm'listSharedDirs'results) where
    methodByLabel  = (GH.Method 17269745093196220462 23)
instance (GH.HasMethod "snapshotCreate" Vm Vm'snapshotCreate'params Vm'snapshotCreate'results) where
    methodByLabel  = (GH.Method 17269745093196220462 24)
instance (GH.HasMethod "snapshotList" Vm Vm'snapshotList'params Vm'snapshotList'results) where
    methodByLabel  = (GH.Method 17269745093196220462 25)
instance (GH.HasMethod "snapshotGet" Vm Vm'snapshotGet'params Vm'snapshotGet'results) where
    methodByLabel  = (GH.Method 17269745093196220462 26)
instance (GH.HasMethod "attachSshKey" Vm Vm'attachSshKey'params Vm'attachSshKey'results) where
    methodByLabel  = (GH.Method 17269745093196220462 27)
instance (GH.HasMethod "detachSshKey" Vm Vm'detachSshKey'params Vm'detachSshKey'results) where
    methodByLabel  = (GH.Method 17269745093196220462 28)
instance (GH.HasMethod "listSshKeys" Vm Vm'listSshKeys'params Vm'listSshKeys'results) where
    methodByLabel  = (GH.Method 17269745093196220462 29)
instance (GH.HasMethod "migrate" Vm Vm'migrate'params Vm'migrate'results) where
    methodByLabel  = (GH.Method 17269745093196220462 30)
data Vm'show'params 
type instance (R.ReprFor Vm'show'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'show'params) where
    typeId  = 15634172725223088769
instance (C.TypedStruct Vm'show'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'show'params) where
    type AllocHint Vm'show'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'show'params (C.Parsed Vm'show'params))
instance (C.AllocateList Vm'show'params) where
    type ListAllocHint Vm'show'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'show'params (C.Parsed Vm'show'params))
data instance C.Parsed Vm'show'params
    = Vm'show'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'show'params))
deriving instance (Std_.Eq (C.Parsed Vm'show'params))
instance (C.Parse Vm'show'params (C.Parsed Vm'show'params)) where
    parse raw_ = (Std_.pure Vm'show'params)
instance (C.Marshal Vm'show'params (C.Parsed Vm'show'params)) where
    marshalInto _raw (Vm'show'params) = (Std_.pure ())
data Vm'show'results 
type instance (R.ReprFor Vm'show'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'show'results) where
    typeId  = 10034707256484370315
instance (C.TypedStruct Vm'show'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'show'results) where
    type AllocHint Vm'show'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'show'results (C.Parsed Vm'show'results))
instance (C.AllocateList Vm'show'results) where
    type ListAllocHint Vm'show'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'show'results (C.Parsed Vm'show'results))
data instance C.Parsed Vm'show'results
    = Vm'show'results 
        {details :: (RP.Parsed VmDetails)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'show'results))
deriving instance (Std_.Eq (C.Parsed Vm'show'results))
instance (C.Parse Vm'show'results (C.Parsed Vm'show'results)) where
    parse raw_ = (Vm'show'results <$> (GH.parseField #details raw_))
instance (C.Marshal Vm'show'results (C.Parsed Vm'show'results)) where
    marshalInto raw_ Vm'show'results{..} = (do
        (GH.encodeField #details details raw_)
        (Std_.pure ())
        )
instance (GH.HasField "details" GH.Slot Vm'show'results VmDetails) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'start'params 
type instance (R.ReprFor Vm'start'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'start'params) where
    typeId  = 10412327403539960810
instance (C.TypedStruct Vm'start'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Vm'start'params) where
    type AllocHint Vm'start'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'start'params (C.Parsed Vm'start'params))
instance (C.AllocateList Vm'start'params) where
    type ListAllocHint Vm'start'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'start'params (C.Parsed Vm'start'params))
data instance C.Parsed Vm'start'params
    = Vm'start'params 
        {wait :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'start'params))
deriving instance (Std_.Eq (C.Parsed Vm'start'params))
instance (C.Parse Vm'start'params (C.Parsed Vm'start'params)) where
    parse raw_ = (Vm'start'params <$> (GH.parseField #wait raw_))
instance (C.Marshal Vm'start'params (C.Parsed Vm'start'params)) where
    marshalInto raw_ Vm'start'params{..} = (do
        (GH.encodeField #wait wait raw_)
        (Std_.pure ())
        )
instance (GH.HasField "wait" GH.Slot Vm'start'params Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
data Vm'start'results 
type instance (R.ReprFor Vm'start'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'start'results) where
    typeId  = 14844884516321016313
instance (C.TypedStruct Vm'start'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Vm'start'results) where
    type AllocHint Vm'start'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'start'results (C.Parsed Vm'start'results))
instance (C.AllocateList Vm'start'results) where
    type ListAllocHint Vm'start'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'start'results (C.Parsed Vm'start'results))
data instance C.Parsed Vm'start'results
    = Vm'start'results 
        {status :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.VmStatus)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'start'results))
deriving instance (Std_.Eq (C.Parsed Vm'start'results))
instance (C.Parse Vm'start'results (C.Parsed Vm'start'results)) where
    parse raw_ = (Vm'start'results <$> (GH.parseField #status raw_))
instance (C.Marshal Vm'start'results (C.Parsed Vm'start'results)) where
    marshalInto raw_ Vm'start'results{..} = (do
        (GH.encodeField #status status raw_)
        (Std_.pure ())
        )
instance (GH.HasField "status" GH.Slot Vm'start'results Capnp.Gen.ById.Xbf9b09f64c0dd40d.VmStatus) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
data Vm'stop'params 
type instance (R.ReprFor Vm'stop'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'stop'params) where
    typeId  = 9429036431754149178
instance (C.TypedStruct Vm'stop'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Vm'stop'params) where
    type AllocHint Vm'stop'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'stop'params (C.Parsed Vm'stop'params))
instance (C.AllocateList Vm'stop'params) where
    type ListAllocHint Vm'stop'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'stop'params (C.Parsed Vm'stop'params))
data instance C.Parsed Vm'stop'params
    = Vm'stop'params 
        {wait :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'stop'params))
deriving instance (Std_.Eq (C.Parsed Vm'stop'params))
instance (C.Parse Vm'stop'params (C.Parsed Vm'stop'params)) where
    parse raw_ = (Vm'stop'params <$> (GH.parseField #wait raw_))
instance (C.Marshal Vm'stop'params (C.Parsed Vm'stop'params)) where
    marshalInto raw_ Vm'stop'params{..} = (do
        (GH.encodeField #wait wait raw_)
        (Std_.pure ())
        )
instance (GH.HasField "wait" GH.Slot Vm'stop'params Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
data Vm'stop'results 
type instance (R.ReprFor Vm'stop'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'stop'results) where
    typeId  = 16764298016335805328
instance (C.TypedStruct Vm'stop'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Vm'stop'results) where
    type AllocHint Vm'stop'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'stop'results (C.Parsed Vm'stop'results))
instance (C.AllocateList Vm'stop'results) where
    type ListAllocHint Vm'stop'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'stop'results (C.Parsed Vm'stop'results))
data instance C.Parsed Vm'stop'results
    = Vm'stop'results 
        {status :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.VmStatus)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'stop'results))
deriving instance (Std_.Eq (C.Parsed Vm'stop'results))
instance (C.Parse Vm'stop'results (C.Parsed Vm'stop'results)) where
    parse raw_ = (Vm'stop'results <$> (GH.parseField #status raw_))
instance (C.Marshal Vm'stop'results (C.Parsed Vm'stop'results)) where
    marshalInto raw_ Vm'stop'results{..} = (do
        (GH.encodeField #status status raw_)
        (Std_.pure ())
        )
instance (GH.HasField "status" GH.Slot Vm'stop'results Capnp.Gen.ById.Xbf9b09f64c0dd40d.VmStatus) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
data Vm'pause'params 
type instance (R.ReprFor Vm'pause'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'pause'params) where
    typeId  = 11658676208766599179
instance (C.TypedStruct Vm'pause'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'pause'params) where
    type AllocHint Vm'pause'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'pause'params (C.Parsed Vm'pause'params))
instance (C.AllocateList Vm'pause'params) where
    type ListAllocHint Vm'pause'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'pause'params (C.Parsed Vm'pause'params))
data instance C.Parsed Vm'pause'params
    = Vm'pause'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'pause'params))
deriving instance (Std_.Eq (C.Parsed Vm'pause'params))
instance (C.Parse Vm'pause'params (C.Parsed Vm'pause'params)) where
    parse raw_ = (Std_.pure Vm'pause'params)
instance (C.Marshal Vm'pause'params (C.Parsed Vm'pause'params)) where
    marshalInto _raw (Vm'pause'params) = (Std_.pure ())
data Vm'pause'results 
type instance (R.ReprFor Vm'pause'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'pause'results) where
    typeId  = 11058700690921072347
instance (C.TypedStruct Vm'pause'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Vm'pause'results) where
    type AllocHint Vm'pause'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'pause'results (C.Parsed Vm'pause'results))
instance (C.AllocateList Vm'pause'results) where
    type ListAllocHint Vm'pause'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'pause'results (C.Parsed Vm'pause'results))
data instance C.Parsed Vm'pause'results
    = Vm'pause'results 
        {status :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.VmStatus)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'pause'results))
deriving instance (Std_.Eq (C.Parsed Vm'pause'results))
instance (C.Parse Vm'pause'results (C.Parsed Vm'pause'results)) where
    parse raw_ = (Vm'pause'results <$> (GH.parseField #status raw_))
instance (C.Marshal Vm'pause'results (C.Parsed Vm'pause'results)) where
    marshalInto raw_ Vm'pause'results{..} = (do
        (GH.encodeField #status status raw_)
        (Std_.pure ())
        )
instance (GH.HasField "status" GH.Slot Vm'pause'results Capnp.Gen.ById.Xbf9b09f64c0dd40d.VmStatus) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
data Vm'reset'params 
type instance (R.ReprFor Vm'reset'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'reset'params) where
    typeId  = 10930599273570311181
instance (C.TypedStruct Vm'reset'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'reset'params) where
    type AllocHint Vm'reset'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'reset'params (C.Parsed Vm'reset'params))
instance (C.AllocateList Vm'reset'params) where
    type ListAllocHint Vm'reset'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'reset'params (C.Parsed Vm'reset'params))
data instance C.Parsed Vm'reset'params
    = Vm'reset'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'reset'params))
deriving instance (Std_.Eq (C.Parsed Vm'reset'params))
instance (C.Parse Vm'reset'params (C.Parsed Vm'reset'params)) where
    parse raw_ = (Std_.pure Vm'reset'params)
instance (C.Marshal Vm'reset'params (C.Parsed Vm'reset'params)) where
    marshalInto _raw (Vm'reset'params) = (Std_.pure ())
data Vm'reset'results 
type instance (R.ReprFor Vm'reset'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'reset'results) where
    typeId  = 16256723828784782012
instance (C.TypedStruct Vm'reset'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Vm'reset'results) where
    type AllocHint Vm'reset'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'reset'results (C.Parsed Vm'reset'results))
instance (C.AllocateList Vm'reset'results) where
    type ListAllocHint Vm'reset'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'reset'results (C.Parsed Vm'reset'results))
data instance C.Parsed Vm'reset'results
    = Vm'reset'results 
        {status :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.VmStatus)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'reset'results))
deriving instance (Std_.Eq (C.Parsed Vm'reset'results))
instance (C.Parse Vm'reset'results (C.Parsed Vm'reset'results)) where
    parse raw_ = (Vm'reset'results <$> (GH.parseField #status raw_))
instance (C.Marshal Vm'reset'results (C.Parsed Vm'reset'results)) where
    marshalInto raw_ Vm'reset'results{..} = (do
        (GH.encodeField #status status raw_)
        (Std_.pure ())
        )
instance (GH.HasField "status" GH.Slot Vm'reset'results Capnp.Gen.ById.Xbf9b09f64c0dd40d.VmStatus) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
data Vm'edit'params 
type instance (R.ReprFor Vm'edit'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'edit'params) where
    typeId  = 17235200047524777255
instance (C.TypedStruct Vm'edit'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'edit'params) where
    type AllocHint Vm'edit'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'edit'params (C.Parsed Vm'edit'params))
instance (C.AllocateList Vm'edit'params) where
    type ListAllocHint Vm'edit'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'edit'params (C.Parsed Vm'edit'params))
data instance C.Parsed Vm'edit'params
    = Vm'edit'params 
        {params :: (RP.Parsed VmEditParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'edit'params))
deriving instance (Std_.Eq (C.Parsed Vm'edit'params))
instance (C.Parse Vm'edit'params (C.Parsed Vm'edit'params)) where
    parse raw_ = (Vm'edit'params <$> (GH.parseField #params raw_))
instance (C.Marshal Vm'edit'params (C.Parsed Vm'edit'params)) where
    marshalInto raw_ Vm'edit'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot Vm'edit'params VmEditParams) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'edit'results 
type instance (R.ReprFor Vm'edit'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'edit'results) where
    typeId  = 10843191053054078203
instance (C.TypedStruct Vm'edit'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'edit'results) where
    type AllocHint Vm'edit'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'edit'results (C.Parsed Vm'edit'results))
instance (C.AllocateList Vm'edit'results) where
    type ListAllocHint Vm'edit'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'edit'results (C.Parsed Vm'edit'results))
data instance C.Parsed Vm'edit'results
    = Vm'edit'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'edit'results))
deriving instance (Std_.Eq (C.Parsed Vm'edit'results))
instance (C.Parse Vm'edit'results (C.Parsed Vm'edit'results)) where
    parse raw_ = (Std_.pure Vm'edit'results)
instance (C.Marshal Vm'edit'results (C.Parsed Vm'edit'results)) where
    marshalInto _raw (Vm'edit'results) = (Std_.pure ())
data Vm'delete'params 
type instance (R.ReprFor Vm'delete'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'delete'params) where
    typeId  = 14182547241940191921
instance (C.TypedStruct Vm'delete'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Vm'delete'params) where
    type AllocHint Vm'delete'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'delete'params (C.Parsed Vm'delete'params))
instance (C.AllocateList Vm'delete'params) where
    type ListAllocHint Vm'delete'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'delete'params (C.Parsed Vm'delete'params))
data instance C.Parsed Vm'delete'params
    = Vm'delete'params 
        {deleteDisks :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'delete'params))
deriving instance (Std_.Eq (C.Parsed Vm'delete'params))
instance (C.Parse Vm'delete'params (C.Parsed Vm'delete'params)) where
    parse raw_ = (Vm'delete'params <$> (GH.parseField #deleteDisks raw_))
instance (C.Marshal Vm'delete'params (C.Parsed Vm'delete'params)) where
    marshalInto raw_ Vm'delete'params{..} = (do
        (GH.encodeField #deleteDisks deleteDisks raw_)
        (Std_.pure ())
        )
instance (GH.HasField "deleteDisks" GH.Slot Vm'delete'params Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
data Vm'delete'results 
type instance (R.ReprFor Vm'delete'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'delete'results) where
    typeId  = 15698598960571366080
instance (C.TypedStruct Vm'delete'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'delete'results) where
    type AllocHint Vm'delete'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'delete'results (C.Parsed Vm'delete'results))
instance (C.AllocateList Vm'delete'results) where
    type ListAllocHint Vm'delete'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'delete'results (C.Parsed Vm'delete'results))
data instance C.Parsed Vm'delete'results
    = Vm'delete'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'delete'results))
deriving instance (Std_.Eq (C.Parsed Vm'delete'results))
instance (C.Parse Vm'delete'results (C.Parsed Vm'delete'results)) where
    parse raw_ = (Std_.pure Vm'delete'results)
instance (C.Marshal Vm'delete'results (C.Parsed Vm'delete'results)) where
    marshalInto _raw (Vm'delete'results) = (Std_.pure ())
data Vm'cloudInit'params 
type instance (R.ReprFor Vm'cloudInit'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'cloudInit'params) where
    typeId  = 10064979255211626575
instance (C.TypedStruct Vm'cloudInit'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'cloudInit'params) where
    type AllocHint Vm'cloudInit'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'cloudInit'params (C.Parsed Vm'cloudInit'params))
instance (C.AllocateList Vm'cloudInit'params) where
    type ListAllocHint Vm'cloudInit'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'cloudInit'params (C.Parsed Vm'cloudInit'params))
data instance C.Parsed Vm'cloudInit'params
    = Vm'cloudInit'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'cloudInit'params))
deriving instance (Std_.Eq (C.Parsed Vm'cloudInit'params))
instance (C.Parse Vm'cloudInit'params (C.Parsed Vm'cloudInit'params)) where
    parse raw_ = (Std_.pure Vm'cloudInit'params)
instance (C.Marshal Vm'cloudInit'params (C.Parsed Vm'cloudInit'params)) where
    marshalInto _raw (Vm'cloudInit'params) = (Std_.pure ())
data Vm'cloudInit'results 
type instance (R.ReprFor Vm'cloudInit'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'cloudInit'results) where
    typeId  = 16221492179408132154
instance (C.TypedStruct Vm'cloudInit'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'cloudInit'results) where
    type AllocHint Vm'cloudInit'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'cloudInit'results (C.Parsed Vm'cloudInit'results))
instance (C.AllocateList Vm'cloudInit'results) where
    type ListAllocHint Vm'cloudInit'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'cloudInit'results (C.Parsed Vm'cloudInit'results))
data instance C.Parsed Vm'cloudInit'results
    = Vm'cloudInit'results 
        {config :: (RP.Parsed Capnp.Gen.ById.Xeb6a435f11477f84.CloudInitInfo)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'cloudInit'results))
deriving instance (Std_.Eq (C.Parsed Vm'cloudInit'results))
instance (C.Parse Vm'cloudInit'results (C.Parsed Vm'cloudInit'results)) where
    parse raw_ = (Vm'cloudInit'results <$> (GH.parseField #config raw_))
instance (C.Marshal Vm'cloudInit'results (C.Parsed Vm'cloudInit'results)) where
    marshalInto raw_ Vm'cloudInit'results{..} = (do
        (GH.encodeField #config config raw_)
        (Std_.pure ())
        )
instance (GH.HasField "config" GH.Slot Vm'cloudInit'results Capnp.Gen.ById.Xeb6a435f11477f84.CloudInitInfo) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'viewGrant'params 
type instance (R.ReprFor Vm'viewGrant'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'viewGrant'params) where
    typeId  = 13472902694230052314
instance (C.TypedStruct Vm'viewGrant'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'viewGrant'params) where
    type AllocHint Vm'viewGrant'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'viewGrant'params (C.Parsed Vm'viewGrant'params))
instance (C.AllocateList Vm'viewGrant'params) where
    type ListAllocHint Vm'viewGrant'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'viewGrant'params (C.Parsed Vm'viewGrant'params))
data instance C.Parsed Vm'viewGrant'params
    = Vm'viewGrant'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'viewGrant'params))
deriving instance (Std_.Eq (C.Parsed Vm'viewGrant'params))
instance (C.Parse Vm'viewGrant'params (C.Parsed Vm'viewGrant'params)) where
    parse raw_ = (Std_.pure Vm'viewGrant'params)
instance (C.Marshal Vm'viewGrant'params (C.Parsed Vm'viewGrant'params)) where
    marshalInto _raw (Vm'viewGrant'params) = (Std_.pure ())
data Vm'viewGrant'results 
type instance (R.ReprFor Vm'viewGrant'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'viewGrant'results) where
    typeId  = 15177559789690744078
instance (C.TypedStruct Vm'viewGrant'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'viewGrant'results) where
    type AllocHint Vm'viewGrant'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'viewGrant'results (C.Parsed Vm'viewGrant'results))
instance (C.AllocateList Vm'viewGrant'results) where
    type ListAllocHint Vm'viewGrant'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'viewGrant'results (C.Parsed Vm'viewGrant'results))
data instance C.Parsed Vm'viewGrant'results
    = Vm'viewGrant'results 
        {grant :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.ViewGrant)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'viewGrant'results))
deriving instance (Std_.Eq (C.Parsed Vm'viewGrant'results))
instance (C.Parse Vm'viewGrant'results (C.Parsed Vm'viewGrant'results)) where
    parse raw_ = (Vm'viewGrant'results <$> (GH.parseField #grant raw_))
instance (C.Marshal Vm'viewGrant'results (C.Parsed Vm'viewGrant'results)) where
    marshalInto raw_ Vm'viewGrant'results{..} = (do
        (GH.encodeField #grant grant raw_)
        (Std_.pure ())
        )
instance (GH.HasField "grant" GH.Slot Vm'viewGrant'results Capnp.Gen.ById.X9b1373e2334a09e9.ViewGrant) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'guestExec'params 
type instance (R.ReprFor Vm'guestExec'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'guestExec'params) where
    typeId  = 12698017320510543686
instance (C.TypedStruct Vm'guestExec'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'guestExec'params) where
    type AllocHint Vm'guestExec'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'guestExec'params (C.Parsed Vm'guestExec'params))
instance (C.AllocateList Vm'guestExec'params) where
    type ListAllocHint Vm'guestExec'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'guestExec'params (C.Parsed Vm'guestExec'params))
data instance C.Parsed Vm'guestExec'params
    = Vm'guestExec'params 
        {command :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'guestExec'params))
deriving instance (Std_.Eq (C.Parsed Vm'guestExec'params))
instance (C.Parse Vm'guestExec'params (C.Parsed Vm'guestExec'params)) where
    parse raw_ = (Vm'guestExec'params <$> (GH.parseField #command raw_))
instance (C.Marshal Vm'guestExec'params (C.Parsed Vm'guestExec'params)) where
    marshalInto raw_ Vm'guestExec'params{..} = (do
        (GH.encodeField #command command raw_)
        (Std_.pure ())
        )
instance (GH.HasField "command" GH.Slot Vm'guestExec'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'guestExec'results 
type instance (R.ReprFor Vm'guestExec'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'guestExec'results) where
    typeId  = 10356857213081534997
instance (C.TypedStruct Vm'guestExec'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'guestExec'results) where
    type AllocHint Vm'guestExec'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'guestExec'results (C.Parsed Vm'guestExec'results))
instance (C.AllocateList Vm'guestExec'results) where
    type ListAllocHint Vm'guestExec'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'guestExec'results (C.Parsed Vm'guestExec'results))
data instance C.Parsed Vm'guestExec'results
    = Vm'guestExec'results 
        {result :: (RP.Parsed GuestExecResult)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'guestExec'results))
deriving instance (Std_.Eq (C.Parsed Vm'guestExec'results))
instance (C.Parse Vm'guestExec'results (C.Parsed Vm'guestExec'results)) where
    parse raw_ = (Vm'guestExec'results <$> (GH.parseField #result raw_))
instance (C.Marshal Vm'guestExec'results (C.Parsed Vm'guestExec'results)) where
    marshalInto raw_ Vm'guestExec'results{..} = (do
        (GH.encodeField #result result raw_)
        (Std_.pure ())
        )
instance (GH.HasField "result" GH.Slot Vm'guestExec'results GuestExecResult) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'sendCtrlAltDel'params 
type instance (R.ReprFor Vm'sendCtrlAltDel'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'sendCtrlAltDel'params) where
    typeId  = 10014589446619720706
instance (C.TypedStruct Vm'sendCtrlAltDel'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'sendCtrlAltDel'params) where
    type AllocHint Vm'sendCtrlAltDel'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'sendCtrlAltDel'params (C.Parsed Vm'sendCtrlAltDel'params))
instance (C.AllocateList Vm'sendCtrlAltDel'params) where
    type ListAllocHint Vm'sendCtrlAltDel'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'sendCtrlAltDel'params (C.Parsed Vm'sendCtrlAltDel'params))
data instance C.Parsed Vm'sendCtrlAltDel'params
    = Vm'sendCtrlAltDel'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'sendCtrlAltDel'params))
deriving instance (Std_.Eq (C.Parsed Vm'sendCtrlAltDel'params))
instance (C.Parse Vm'sendCtrlAltDel'params (C.Parsed Vm'sendCtrlAltDel'params)) where
    parse raw_ = (Std_.pure Vm'sendCtrlAltDel'params)
instance (C.Marshal Vm'sendCtrlAltDel'params (C.Parsed Vm'sendCtrlAltDel'params)) where
    marshalInto _raw (Vm'sendCtrlAltDel'params) = (Std_.pure ())
data Vm'sendCtrlAltDel'results 
type instance (R.ReprFor Vm'sendCtrlAltDel'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'sendCtrlAltDel'results) where
    typeId  = 12177529456933387869
instance (C.TypedStruct Vm'sendCtrlAltDel'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'sendCtrlAltDel'results) where
    type AllocHint Vm'sendCtrlAltDel'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'sendCtrlAltDel'results (C.Parsed Vm'sendCtrlAltDel'results))
instance (C.AllocateList Vm'sendCtrlAltDel'results) where
    type ListAllocHint Vm'sendCtrlAltDel'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'sendCtrlAltDel'results (C.Parsed Vm'sendCtrlAltDel'results))
data instance C.Parsed Vm'sendCtrlAltDel'results
    = Vm'sendCtrlAltDel'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'sendCtrlAltDel'results))
deriving instance (Std_.Eq (C.Parsed Vm'sendCtrlAltDel'results))
instance (C.Parse Vm'sendCtrlAltDel'results (C.Parsed Vm'sendCtrlAltDel'results)) where
    parse raw_ = (Std_.pure Vm'sendCtrlAltDel'results)
instance (C.Marshal Vm'sendCtrlAltDel'results (C.Parsed Vm'sendCtrlAltDel'results)) where
    marshalInto _raw (Vm'sendCtrlAltDel'results) = (Std_.pure ())
data Vm'serialConsole'params 
type instance (R.ReprFor Vm'serialConsole'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'serialConsole'params) where
    typeId  = 14320079604290457465
instance (C.TypedStruct Vm'serialConsole'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'serialConsole'params) where
    type AllocHint Vm'serialConsole'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'serialConsole'params (C.Parsed Vm'serialConsole'params))
instance (C.AllocateList Vm'serialConsole'params) where
    type ListAllocHint Vm'serialConsole'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'serialConsole'params (C.Parsed Vm'serialConsole'params))
data instance C.Parsed Vm'serialConsole'params
    = Vm'serialConsole'params 
        {sink :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.ByteSink)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'serialConsole'params))
deriving instance (Std_.Eq (C.Parsed Vm'serialConsole'params))
instance (C.Parse Vm'serialConsole'params (C.Parsed Vm'serialConsole'params)) where
    parse raw_ = (Vm'serialConsole'params <$> (GH.parseField #sink raw_))
instance (C.Marshal Vm'serialConsole'params (C.Parsed Vm'serialConsole'params)) where
    marshalInto raw_ Vm'serialConsole'params{..} = (do
        (GH.encodeField #sink sink raw_)
        (Std_.pure ())
        )
instance (GH.HasField "sink" GH.Slot Vm'serialConsole'params Capnp.Gen.ById.X9bd452a518ed3917.ByteSink) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'serialConsole'results 
type instance (R.ReprFor Vm'serialConsole'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'serialConsole'results) where
    typeId  = 10824920989577743193
instance (C.TypedStruct Vm'serialConsole'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'serialConsole'results) where
    type AllocHint Vm'serialConsole'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'serialConsole'results (C.Parsed Vm'serialConsole'results))
instance (C.AllocateList Vm'serialConsole'results) where
    type ListAllocHint Vm'serialConsole'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'serialConsole'results (C.Parsed Vm'serialConsole'results))
data instance C.Parsed Vm'serialConsole'results
    = Vm'serialConsole'results 
        {input :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.ByteSink)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'serialConsole'results))
deriving instance (Std_.Eq (C.Parsed Vm'serialConsole'results))
instance (C.Parse Vm'serialConsole'results (C.Parsed Vm'serialConsole'results)) where
    parse raw_ = (Vm'serialConsole'results <$> (GH.parseField #input raw_))
instance (C.Marshal Vm'serialConsole'results (C.Parsed Vm'serialConsole'results)) where
    marshalInto raw_ Vm'serialConsole'results{..} = (do
        (GH.encodeField #input input raw_)
        (Std_.pure ())
        )
instance (GH.HasField "input" GH.Slot Vm'serialConsole'results Capnp.Gen.ById.X9bd452a518ed3917.ByteSink) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'serialConsoleFlush'params 
type instance (R.ReprFor Vm'serialConsoleFlush'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'serialConsoleFlush'params) where
    typeId  = 14110991215072704421
instance (C.TypedStruct Vm'serialConsoleFlush'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'serialConsoleFlush'params) where
    type AllocHint Vm'serialConsoleFlush'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'serialConsoleFlush'params (C.Parsed Vm'serialConsoleFlush'params))
instance (C.AllocateList Vm'serialConsoleFlush'params) where
    type ListAllocHint Vm'serialConsoleFlush'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'serialConsoleFlush'params (C.Parsed Vm'serialConsoleFlush'params))
data instance C.Parsed Vm'serialConsoleFlush'params
    = Vm'serialConsoleFlush'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'serialConsoleFlush'params))
deriving instance (Std_.Eq (C.Parsed Vm'serialConsoleFlush'params))
instance (C.Parse Vm'serialConsoleFlush'params (C.Parsed Vm'serialConsoleFlush'params)) where
    parse raw_ = (Std_.pure Vm'serialConsoleFlush'params)
instance (C.Marshal Vm'serialConsoleFlush'params (C.Parsed Vm'serialConsoleFlush'params)) where
    marshalInto _raw (Vm'serialConsoleFlush'params) = (Std_.pure ())
data Vm'serialConsoleFlush'results 
type instance (R.ReprFor Vm'serialConsoleFlush'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'serialConsoleFlush'results) where
    typeId  = 18257568621799469190
instance (C.TypedStruct Vm'serialConsoleFlush'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'serialConsoleFlush'results) where
    type AllocHint Vm'serialConsoleFlush'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'serialConsoleFlush'results (C.Parsed Vm'serialConsoleFlush'results))
instance (C.AllocateList Vm'serialConsoleFlush'results) where
    type ListAllocHint Vm'serialConsoleFlush'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'serialConsoleFlush'results (C.Parsed Vm'serialConsoleFlush'results))
data instance C.Parsed Vm'serialConsoleFlush'results
    = Vm'serialConsoleFlush'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'serialConsoleFlush'results))
deriving instance (Std_.Eq (C.Parsed Vm'serialConsoleFlush'results))
instance (C.Parse Vm'serialConsoleFlush'results (C.Parsed Vm'serialConsoleFlush'results)) where
    parse raw_ = (Std_.pure Vm'serialConsoleFlush'results)
instance (C.Marshal Vm'serialConsoleFlush'results (C.Parsed Vm'serialConsoleFlush'results)) where
    marshalInto _raw (Vm'serialConsoleFlush'results) = (Std_.pure ())
data Vm'hmpMonitor'params 
type instance (R.ReprFor Vm'hmpMonitor'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'hmpMonitor'params) where
    typeId  = 9629329085716753466
instance (C.TypedStruct Vm'hmpMonitor'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'hmpMonitor'params) where
    type AllocHint Vm'hmpMonitor'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'hmpMonitor'params (C.Parsed Vm'hmpMonitor'params))
instance (C.AllocateList Vm'hmpMonitor'params) where
    type ListAllocHint Vm'hmpMonitor'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'hmpMonitor'params (C.Parsed Vm'hmpMonitor'params))
data instance C.Parsed Vm'hmpMonitor'params
    = Vm'hmpMonitor'params 
        {sink :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.ByteSink)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'hmpMonitor'params))
deriving instance (Std_.Eq (C.Parsed Vm'hmpMonitor'params))
instance (C.Parse Vm'hmpMonitor'params (C.Parsed Vm'hmpMonitor'params)) where
    parse raw_ = (Vm'hmpMonitor'params <$> (GH.parseField #sink raw_))
instance (C.Marshal Vm'hmpMonitor'params (C.Parsed Vm'hmpMonitor'params)) where
    marshalInto raw_ Vm'hmpMonitor'params{..} = (do
        (GH.encodeField #sink sink raw_)
        (Std_.pure ())
        )
instance (GH.HasField "sink" GH.Slot Vm'hmpMonitor'params Capnp.Gen.ById.X9bd452a518ed3917.ByteSink) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'hmpMonitor'results 
type instance (R.ReprFor Vm'hmpMonitor'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'hmpMonitor'results) where
    typeId  = 13641418416655942490
instance (C.TypedStruct Vm'hmpMonitor'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'hmpMonitor'results) where
    type AllocHint Vm'hmpMonitor'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'hmpMonitor'results (C.Parsed Vm'hmpMonitor'results))
instance (C.AllocateList Vm'hmpMonitor'results) where
    type ListAllocHint Vm'hmpMonitor'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'hmpMonitor'results (C.Parsed Vm'hmpMonitor'results))
data instance C.Parsed Vm'hmpMonitor'results
    = Vm'hmpMonitor'results 
        {input :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.ByteSink)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'hmpMonitor'results))
deriving instance (Std_.Eq (C.Parsed Vm'hmpMonitor'results))
instance (C.Parse Vm'hmpMonitor'results (C.Parsed Vm'hmpMonitor'results)) where
    parse raw_ = (Vm'hmpMonitor'results <$> (GH.parseField #input raw_))
instance (C.Marshal Vm'hmpMonitor'results (C.Parsed Vm'hmpMonitor'results)) where
    marshalInto raw_ Vm'hmpMonitor'results{..} = (do
        (GH.encodeField #input input raw_)
        (Std_.pure ())
        )
instance (GH.HasField "input" GH.Slot Vm'hmpMonitor'results Capnp.Gen.ById.X9bd452a518ed3917.ByteSink) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'hmpMonitorFlush'params 
type instance (R.ReprFor Vm'hmpMonitorFlush'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'hmpMonitorFlush'params) where
    typeId  = 16500943151352811874
instance (C.TypedStruct Vm'hmpMonitorFlush'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'hmpMonitorFlush'params) where
    type AllocHint Vm'hmpMonitorFlush'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'hmpMonitorFlush'params (C.Parsed Vm'hmpMonitorFlush'params))
instance (C.AllocateList Vm'hmpMonitorFlush'params) where
    type ListAllocHint Vm'hmpMonitorFlush'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'hmpMonitorFlush'params (C.Parsed Vm'hmpMonitorFlush'params))
data instance C.Parsed Vm'hmpMonitorFlush'params
    = Vm'hmpMonitorFlush'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'hmpMonitorFlush'params))
deriving instance (Std_.Eq (C.Parsed Vm'hmpMonitorFlush'params))
instance (C.Parse Vm'hmpMonitorFlush'params (C.Parsed Vm'hmpMonitorFlush'params)) where
    parse raw_ = (Std_.pure Vm'hmpMonitorFlush'params)
instance (C.Marshal Vm'hmpMonitorFlush'params (C.Parsed Vm'hmpMonitorFlush'params)) where
    marshalInto _raw (Vm'hmpMonitorFlush'params) = (Std_.pure ())
data Vm'hmpMonitorFlush'results 
type instance (R.ReprFor Vm'hmpMonitorFlush'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'hmpMonitorFlush'results) where
    typeId  = 12976430671951141164
instance (C.TypedStruct Vm'hmpMonitorFlush'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'hmpMonitorFlush'results) where
    type AllocHint Vm'hmpMonitorFlush'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'hmpMonitorFlush'results (C.Parsed Vm'hmpMonitorFlush'results))
instance (C.AllocateList Vm'hmpMonitorFlush'results) where
    type ListAllocHint Vm'hmpMonitorFlush'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'hmpMonitorFlush'results (C.Parsed Vm'hmpMonitorFlush'results))
data instance C.Parsed Vm'hmpMonitorFlush'results
    = Vm'hmpMonitorFlush'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'hmpMonitorFlush'results))
deriving instance (Std_.Eq (C.Parsed Vm'hmpMonitorFlush'results))
instance (C.Parse Vm'hmpMonitorFlush'results (C.Parsed Vm'hmpMonitorFlush'results)) where
    parse raw_ = (Std_.pure Vm'hmpMonitorFlush'results)
instance (C.Marshal Vm'hmpMonitorFlush'results (C.Parsed Vm'hmpMonitorFlush'results)) where
    marshalInto _raw (Vm'hmpMonitorFlush'results) = (Std_.pure ())
data Vm'subscribeGuestAgent'params 
type instance (R.ReprFor Vm'subscribeGuestAgent'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'subscribeGuestAgent'params) where
    typeId  = 17767865192542557777
instance (C.TypedStruct Vm'subscribeGuestAgent'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'subscribeGuestAgent'params) where
    type AllocHint Vm'subscribeGuestAgent'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'subscribeGuestAgent'params (C.Parsed Vm'subscribeGuestAgent'params))
instance (C.AllocateList Vm'subscribeGuestAgent'params) where
    type ListAllocHint Vm'subscribeGuestAgent'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'subscribeGuestAgent'params (C.Parsed Vm'subscribeGuestAgent'params))
data instance C.Parsed Vm'subscribeGuestAgent'params
    = Vm'subscribeGuestAgent'params 
        {sink :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.GuestAgentStatusSink)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'subscribeGuestAgent'params))
deriving instance (Std_.Eq (C.Parsed Vm'subscribeGuestAgent'params))
instance (C.Parse Vm'subscribeGuestAgent'params (C.Parsed Vm'subscribeGuestAgent'params)) where
    parse raw_ = (Vm'subscribeGuestAgent'params <$> (GH.parseField #sink raw_))
instance (C.Marshal Vm'subscribeGuestAgent'params (C.Parsed Vm'subscribeGuestAgent'params)) where
    marshalInto raw_ Vm'subscribeGuestAgent'params{..} = (do
        (GH.encodeField #sink sink raw_)
        (Std_.pure ())
        )
instance (GH.HasField "sink" GH.Slot Vm'subscribeGuestAgent'params Capnp.Gen.ById.X9bd452a518ed3917.GuestAgentStatusSink) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'subscribeGuestAgent'results 
type instance (R.ReprFor Vm'subscribeGuestAgent'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'subscribeGuestAgent'results) where
    typeId  = 15247930981063921507
instance (C.TypedStruct Vm'subscribeGuestAgent'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'subscribeGuestAgent'results) where
    type AllocHint Vm'subscribeGuestAgent'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'subscribeGuestAgent'results (C.Parsed Vm'subscribeGuestAgent'results))
instance (C.AllocateList Vm'subscribeGuestAgent'results) where
    type ListAllocHint Vm'subscribeGuestAgent'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'subscribeGuestAgent'results (C.Parsed Vm'subscribeGuestAgent'results))
data instance C.Parsed Vm'subscribeGuestAgent'results
    = Vm'subscribeGuestAgent'results 
        {handle :: (RP.Parsed Capnp.Gen.ById.X9bd452a518ed3917.Handle)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'subscribeGuestAgent'results))
deriving instance (Std_.Eq (C.Parsed Vm'subscribeGuestAgent'results))
instance (C.Parse Vm'subscribeGuestAgent'results (C.Parsed Vm'subscribeGuestAgent'results)) where
    parse raw_ = (Vm'subscribeGuestAgent'results <$> (GH.parseField #handle raw_))
instance (C.Marshal Vm'subscribeGuestAgent'results (C.Parsed Vm'subscribeGuestAgent'results)) where
    marshalInto raw_ Vm'subscribeGuestAgent'results{..} = (do
        (GH.encodeField #handle handle raw_)
        (Std_.pure ())
        )
instance (GH.HasField "handle" GH.Slot Vm'subscribeGuestAgent'results Capnp.Gen.ById.X9bd452a518ed3917.Handle) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'attachDisk'params 
type instance (R.ReprFor Vm'attachDisk'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'attachDisk'params) where
    typeId  = 10279976995614901397
instance (C.TypedStruct Vm'attachDisk'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'attachDisk'params) where
    type AllocHint Vm'attachDisk'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'attachDisk'params (C.Parsed Vm'attachDisk'params))
instance (C.AllocateList Vm'attachDisk'params) where
    type ListAllocHint Vm'attachDisk'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'attachDisk'params (C.Parsed Vm'attachDisk'params))
data instance C.Parsed Vm'attachDisk'params
    = Vm'attachDisk'params 
        {params :: (RP.Parsed DriveAttachParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'attachDisk'params))
deriving instance (Std_.Eq (C.Parsed Vm'attachDisk'params))
instance (C.Parse Vm'attachDisk'params (C.Parsed Vm'attachDisk'params)) where
    parse raw_ = (Vm'attachDisk'params <$> (GH.parseField #params raw_))
instance (C.Marshal Vm'attachDisk'params (C.Parsed Vm'attachDisk'params)) where
    marshalInto raw_ Vm'attachDisk'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot Vm'attachDisk'params DriveAttachParams) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'attachDisk'results 
type instance (R.ReprFor Vm'attachDisk'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'attachDisk'results) where
    typeId  = 14361868519980949415
instance (C.TypedStruct Vm'attachDisk'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Vm'attachDisk'results) where
    type AllocHint Vm'attachDisk'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'attachDisk'results (C.Parsed Vm'attachDisk'results))
instance (C.AllocateList Vm'attachDisk'results) where
    type ListAllocHint Vm'attachDisk'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'attachDisk'results (C.Parsed Vm'attachDisk'results))
data instance C.Parsed Vm'attachDisk'results
    = Vm'attachDisk'results 
        {driveId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'attachDisk'results))
deriving instance (Std_.Eq (C.Parsed Vm'attachDisk'results))
instance (C.Parse Vm'attachDisk'results (C.Parsed Vm'attachDisk'results)) where
    parse raw_ = (Vm'attachDisk'results <$> (GH.parseField #driveId raw_))
instance (C.Marshal Vm'attachDisk'results (C.Parsed Vm'attachDisk'results)) where
    marshalInto raw_ Vm'attachDisk'results{..} = (do
        (GH.encodeField #driveId driveId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "driveId" GH.Slot Vm'attachDisk'results Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Vm'detachDisk'params 
type instance (R.ReprFor Vm'detachDisk'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'detachDisk'params) where
    typeId  = 11340778709287363016
instance (C.TypedStruct Vm'detachDisk'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Vm'detachDisk'params) where
    type AllocHint Vm'detachDisk'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'detachDisk'params (C.Parsed Vm'detachDisk'params))
instance (C.AllocateList Vm'detachDisk'params) where
    type ListAllocHint Vm'detachDisk'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'detachDisk'params (C.Parsed Vm'detachDisk'params))
data instance C.Parsed Vm'detachDisk'params
    = Vm'detachDisk'params 
        {driveId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'detachDisk'params))
deriving instance (Std_.Eq (C.Parsed Vm'detachDisk'params))
instance (C.Parse Vm'detachDisk'params (C.Parsed Vm'detachDisk'params)) where
    parse raw_ = (Vm'detachDisk'params <$> (GH.parseField #driveId raw_))
instance (C.Marshal Vm'detachDisk'params (C.Parsed Vm'detachDisk'params)) where
    marshalInto raw_ Vm'detachDisk'params{..} = (do
        (GH.encodeField #driveId driveId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "driveId" GH.Slot Vm'detachDisk'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Vm'detachDisk'results 
type instance (R.ReprFor Vm'detachDisk'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'detachDisk'results) where
    typeId  = 18213893569326757689
instance (C.TypedStruct Vm'detachDisk'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'detachDisk'results) where
    type AllocHint Vm'detachDisk'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'detachDisk'results (C.Parsed Vm'detachDisk'results))
instance (C.AllocateList Vm'detachDisk'results) where
    type ListAllocHint Vm'detachDisk'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'detachDisk'results (C.Parsed Vm'detachDisk'results))
data instance C.Parsed Vm'detachDisk'results
    = Vm'detachDisk'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'detachDisk'results))
deriving instance (Std_.Eq (C.Parsed Vm'detachDisk'results))
instance (C.Parse Vm'detachDisk'results (C.Parsed Vm'detachDisk'results)) where
    parse raw_ = (Std_.pure Vm'detachDisk'results)
instance (C.Marshal Vm'detachDisk'results (C.Parsed Vm'detachDisk'results)) where
    marshalInto _raw (Vm'detachDisk'results) = (Std_.pure ())
data Vm'addNetIf'params 
type instance (R.ReprFor Vm'addNetIf'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'addNetIf'params) where
    typeId  = 16560034665893044458
instance (C.TypedStruct Vm'addNetIf'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'addNetIf'params) where
    type AllocHint Vm'addNetIf'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'addNetIf'params (C.Parsed Vm'addNetIf'params))
instance (C.AllocateList Vm'addNetIf'params) where
    type ListAllocHint Vm'addNetIf'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'addNetIf'params (C.Parsed Vm'addNetIf'params))
data instance C.Parsed Vm'addNetIf'params
    = Vm'addNetIf'params 
        {params :: (RP.Parsed NetIfAddParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'addNetIf'params))
deriving instance (Std_.Eq (C.Parsed Vm'addNetIf'params))
instance (C.Parse Vm'addNetIf'params (C.Parsed Vm'addNetIf'params)) where
    parse raw_ = (Vm'addNetIf'params <$> (GH.parseField #params raw_))
instance (C.Marshal Vm'addNetIf'params (C.Parsed Vm'addNetIf'params)) where
    marshalInto raw_ Vm'addNetIf'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot Vm'addNetIf'params NetIfAddParams) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'addNetIf'results 
type instance (R.ReprFor Vm'addNetIf'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'addNetIf'results) where
    typeId  = 17930849424740651173
instance (C.TypedStruct Vm'addNetIf'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Vm'addNetIf'results) where
    type AllocHint Vm'addNetIf'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'addNetIf'results (C.Parsed Vm'addNetIf'results))
instance (C.AllocateList Vm'addNetIf'results) where
    type ListAllocHint Vm'addNetIf'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'addNetIf'results (C.Parsed Vm'addNetIf'results))
data instance C.Parsed Vm'addNetIf'results
    = Vm'addNetIf'results 
        {netIfId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'addNetIf'results))
deriving instance (Std_.Eq (C.Parsed Vm'addNetIf'results))
instance (C.Parse Vm'addNetIf'results (C.Parsed Vm'addNetIf'results)) where
    parse raw_ = (Vm'addNetIf'results <$> (GH.parseField #netIfId raw_))
instance (C.Marshal Vm'addNetIf'results (C.Parsed Vm'addNetIf'results)) where
    marshalInto raw_ Vm'addNetIf'results{..} = (do
        (GH.encodeField #netIfId netIfId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "netIfId" GH.Slot Vm'addNetIf'results Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Vm'removeNetIf'params 
type instance (R.ReprFor Vm'removeNetIf'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'removeNetIf'params) where
    typeId  = 16725472100594501779
instance (C.TypedStruct Vm'removeNetIf'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Vm'removeNetIf'params) where
    type AllocHint Vm'removeNetIf'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'removeNetIf'params (C.Parsed Vm'removeNetIf'params))
instance (C.AllocateList Vm'removeNetIf'params) where
    type ListAllocHint Vm'removeNetIf'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'removeNetIf'params (C.Parsed Vm'removeNetIf'params))
data instance C.Parsed Vm'removeNetIf'params
    = Vm'removeNetIf'params 
        {netIfId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'removeNetIf'params))
deriving instance (Std_.Eq (C.Parsed Vm'removeNetIf'params))
instance (C.Parse Vm'removeNetIf'params (C.Parsed Vm'removeNetIf'params)) where
    parse raw_ = (Vm'removeNetIf'params <$> (GH.parseField #netIfId raw_))
instance (C.Marshal Vm'removeNetIf'params (C.Parsed Vm'removeNetIf'params)) where
    marshalInto raw_ Vm'removeNetIf'params{..} = (do
        (GH.encodeField #netIfId netIfId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "netIfId" GH.Slot Vm'removeNetIf'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Vm'removeNetIf'results 
type instance (R.ReprFor Vm'removeNetIf'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'removeNetIf'results) where
    typeId  = 13591952673003147545
instance (C.TypedStruct Vm'removeNetIf'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'removeNetIf'results) where
    type AllocHint Vm'removeNetIf'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'removeNetIf'results (C.Parsed Vm'removeNetIf'results))
instance (C.AllocateList Vm'removeNetIf'results) where
    type ListAllocHint Vm'removeNetIf'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'removeNetIf'results (C.Parsed Vm'removeNetIf'results))
data instance C.Parsed Vm'removeNetIf'results
    = Vm'removeNetIf'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'removeNetIf'results))
deriving instance (Std_.Eq (C.Parsed Vm'removeNetIf'results))
instance (C.Parse Vm'removeNetIf'results (C.Parsed Vm'removeNetIf'results)) where
    parse raw_ = (Std_.pure Vm'removeNetIf'results)
instance (C.Marshal Vm'removeNetIf'results (C.Parsed Vm'removeNetIf'results)) where
    marshalInto _raw (Vm'removeNetIf'results) = (Std_.pure ())
data Vm'listNetIfs'params 
type instance (R.ReprFor Vm'listNetIfs'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'listNetIfs'params) where
    typeId  = 11618702900790820676
instance (C.TypedStruct Vm'listNetIfs'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'listNetIfs'params) where
    type AllocHint Vm'listNetIfs'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'listNetIfs'params (C.Parsed Vm'listNetIfs'params))
instance (C.AllocateList Vm'listNetIfs'params) where
    type ListAllocHint Vm'listNetIfs'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'listNetIfs'params (C.Parsed Vm'listNetIfs'params))
data instance C.Parsed Vm'listNetIfs'params
    = Vm'listNetIfs'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'listNetIfs'params))
deriving instance (Std_.Eq (C.Parsed Vm'listNetIfs'params))
instance (C.Parse Vm'listNetIfs'params (C.Parsed Vm'listNetIfs'params)) where
    parse raw_ = (Std_.pure Vm'listNetIfs'params)
instance (C.Marshal Vm'listNetIfs'params (C.Parsed Vm'listNetIfs'params)) where
    marshalInto _raw (Vm'listNetIfs'params) = (Std_.pure ())
data Vm'listNetIfs'results 
type instance (R.ReprFor Vm'listNetIfs'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'listNetIfs'results) where
    typeId  = 14460865647486701223
instance (C.TypedStruct Vm'listNetIfs'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'listNetIfs'results) where
    type AllocHint Vm'listNetIfs'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'listNetIfs'results (C.Parsed Vm'listNetIfs'results))
instance (C.AllocateList Vm'listNetIfs'results) where
    type ListAllocHint Vm'listNetIfs'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'listNetIfs'results (C.Parsed Vm'listNetIfs'results))
data instance C.Parsed Vm'listNetIfs'results
    = Vm'listNetIfs'results 
        {netIfs :: (RP.Parsed (R.List NetIfInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'listNetIfs'results))
deriving instance (Std_.Eq (C.Parsed Vm'listNetIfs'results))
instance (C.Parse Vm'listNetIfs'results (C.Parsed Vm'listNetIfs'results)) where
    parse raw_ = (Vm'listNetIfs'results <$> (GH.parseField #netIfs raw_))
instance (C.Marshal Vm'listNetIfs'results (C.Parsed Vm'listNetIfs'results)) where
    marshalInto raw_ Vm'listNetIfs'results{..} = (do
        (GH.encodeField #netIfs netIfs raw_)
        (Std_.pure ())
        )
instance (GH.HasField "netIfs" GH.Slot Vm'listNetIfs'results (R.List NetIfInfo)) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'addSharedDir'params 
type instance (R.ReprFor Vm'addSharedDir'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'addSharedDir'params) where
    typeId  = 15075941145620139145
instance (C.TypedStruct Vm'addSharedDir'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'addSharedDir'params) where
    type AllocHint Vm'addSharedDir'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'addSharedDir'params (C.Parsed Vm'addSharedDir'params))
instance (C.AllocateList Vm'addSharedDir'params) where
    type ListAllocHint Vm'addSharedDir'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'addSharedDir'params (C.Parsed Vm'addSharedDir'params))
data instance C.Parsed Vm'addSharedDir'params
    = Vm'addSharedDir'params 
        {params :: (RP.Parsed SharedDirAddParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'addSharedDir'params))
deriving instance (Std_.Eq (C.Parsed Vm'addSharedDir'params))
instance (C.Parse Vm'addSharedDir'params (C.Parsed Vm'addSharedDir'params)) where
    parse raw_ = (Vm'addSharedDir'params <$> (GH.parseField #params raw_))
instance (C.Marshal Vm'addSharedDir'params (C.Parsed Vm'addSharedDir'params)) where
    marshalInto raw_ Vm'addSharedDir'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot Vm'addSharedDir'params SharedDirAddParams) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'addSharedDir'results 
type instance (R.ReprFor Vm'addSharedDir'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'addSharedDir'results) where
    typeId  = 16369636480366727316
instance (C.TypedStruct Vm'addSharedDir'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Vm'addSharedDir'results) where
    type AllocHint Vm'addSharedDir'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'addSharedDir'results (C.Parsed Vm'addSharedDir'results))
instance (C.AllocateList Vm'addSharedDir'results) where
    type ListAllocHint Vm'addSharedDir'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'addSharedDir'results (C.Parsed Vm'addSharedDir'results))
data instance C.Parsed Vm'addSharedDir'results
    = Vm'addSharedDir'results 
        {sharedDirId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'addSharedDir'results))
deriving instance (Std_.Eq (C.Parsed Vm'addSharedDir'results))
instance (C.Parse Vm'addSharedDir'results (C.Parsed Vm'addSharedDir'results)) where
    parse raw_ = (Vm'addSharedDir'results <$> (GH.parseField #sharedDirId raw_))
instance (C.Marshal Vm'addSharedDir'results (C.Parsed Vm'addSharedDir'results)) where
    marshalInto raw_ Vm'addSharedDir'results{..} = (do
        (GH.encodeField #sharedDirId sharedDirId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "sharedDirId" GH.Slot Vm'addSharedDir'results Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Vm'removeSharedDir'params 
type instance (R.ReprFor Vm'removeSharedDir'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'removeSharedDir'params) where
    typeId  = 14022662961514373534
instance (C.TypedStruct Vm'removeSharedDir'params) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Vm'removeSharedDir'params) where
    type AllocHint Vm'removeSharedDir'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'removeSharedDir'params (C.Parsed Vm'removeSharedDir'params))
instance (C.AllocateList Vm'removeSharedDir'params) where
    type ListAllocHint Vm'removeSharedDir'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'removeSharedDir'params (C.Parsed Vm'removeSharedDir'params))
data instance C.Parsed Vm'removeSharedDir'params
    = Vm'removeSharedDir'params 
        {sharedDirId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'removeSharedDir'params))
deriving instance (Std_.Eq (C.Parsed Vm'removeSharedDir'params))
instance (C.Parse Vm'removeSharedDir'params (C.Parsed Vm'removeSharedDir'params)) where
    parse raw_ = (Vm'removeSharedDir'params <$> (GH.parseField #sharedDirId raw_))
instance (C.Marshal Vm'removeSharedDir'params (C.Parsed Vm'removeSharedDir'params)) where
    marshalInto raw_ Vm'removeSharedDir'params{..} = (do
        (GH.encodeField #sharedDirId sharedDirId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "sharedDirId" GH.Slot Vm'removeSharedDir'params Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data Vm'removeSharedDir'results 
type instance (R.ReprFor Vm'removeSharedDir'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'removeSharedDir'results) where
    typeId  = 12862338058562088595
instance (C.TypedStruct Vm'removeSharedDir'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'removeSharedDir'results) where
    type AllocHint Vm'removeSharedDir'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'removeSharedDir'results (C.Parsed Vm'removeSharedDir'results))
instance (C.AllocateList Vm'removeSharedDir'results) where
    type ListAllocHint Vm'removeSharedDir'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'removeSharedDir'results (C.Parsed Vm'removeSharedDir'results))
data instance C.Parsed Vm'removeSharedDir'results
    = Vm'removeSharedDir'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'removeSharedDir'results))
deriving instance (Std_.Eq (C.Parsed Vm'removeSharedDir'results))
instance (C.Parse Vm'removeSharedDir'results (C.Parsed Vm'removeSharedDir'results)) where
    parse raw_ = (Std_.pure Vm'removeSharedDir'results)
instance (C.Marshal Vm'removeSharedDir'results (C.Parsed Vm'removeSharedDir'results)) where
    marshalInto _raw (Vm'removeSharedDir'results) = (Std_.pure ())
data Vm'listSharedDirs'params 
type instance (R.ReprFor Vm'listSharedDirs'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'listSharedDirs'params) where
    typeId  = 14920266415700813331
instance (C.TypedStruct Vm'listSharedDirs'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'listSharedDirs'params) where
    type AllocHint Vm'listSharedDirs'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'listSharedDirs'params (C.Parsed Vm'listSharedDirs'params))
instance (C.AllocateList Vm'listSharedDirs'params) where
    type ListAllocHint Vm'listSharedDirs'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'listSharedDirs'params (C.Parsed Vm'listSharedDirs'params))
data instance C.Parsed Vm'listSharedDirs'params
    = Vm'listSharedDirs'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'listSharedDirs'params))
deriving instance (Std_.Eq (C.Parsed Vm'listSharedDirs'params))
instance (C.Parse Vm'listSharedDirs'params (C.Parsed Vm'listSharedDirs'params)) where
    parse raw_ = (Std_.pure Vm'listSharedDirs'params)
instance (C.Marshal Vm'listSharedDirs'params (C.Parsed Vm'listSharedDirs'params)) where
    marshalInto _raw (Vm'listSharedDirs'params) = (Std_.pure ())
data Vm'listSharedDirs'results 
type instance (R.ReprFor Vm'listSharedDirs'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'listSharedDirs'results) where
    typeId  = 15378312462461005792
instance (C.TypedStruct Vm'listSharedDirs'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'listSharedDirs'results) where
    type AllocHint Vm'listSharedDirs'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'listSharedDirs'results (C.Parsed Vm'listSharedDirs'results))
instance (C.AllocateList Vm'listSharedDirs'results) where
    type ListAllocHint Vm'listSharedDirs'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'listSharedDirs'results (C.Parsed Vm'listSharedDirs'results))
data instance C.Parsed Vm'listSharedDirs'results
    = Vm'listSharedDirs'results 
        {sharedDirs :: (RP.Parsed (R.List SharedDirInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'listSharedDirs'results))
deriving instance (Std_.Eq (C.Parsed Vm'listSharedDirs'results))
instance (C.Parse Vm'listSharedDirs'results (C.Parsed Vm'listSharedDirs'results)) where
    parse raw_ = (Vm'listSharedDirs'results <$> (GH.parseField #sharedDirs raw_))
instance (C.Marshal Vm'listSharedDirs'results (C.Parsed Vm'listSharedDirs'results)) where
    marshalInto raw_ Vm'listSharedDirs'results{..} = (do
        (GH.encodeField #sharedDirs sharedDirs raw_)
        (Std_.pure ())
        )
instance (GH.HasField "sharedDirs" GH.Slot Vm'listSharedDirs'results (R.List SharedDirInfo)) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'snapshotCreate'params 
type instance (R.ReprFor Vm'snapshotCreate'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'snapshotCreate'params) where
    typeId  = 14376437543014869723
instance (C.TypedStruct Vm'snapshotCreate'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'snapshotCreate'params) where
    type AllocHint Vm'snapshotCreate'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'snapshotCreate'params (C.Parsed Vm'snapshotCreate'params))
instance (C.AllocateList Vm'snapshotCreate'params) where
    type ListAllocHint Vm'snapshotCreate'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'snapshotCreate'params (C.Parsed Vm'snapshotCreate'params))
data instance C.Parsed Vm'snapshotCreate'params
    = Vm'snapshotCreate'params 
        {name :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'snapshotCreate'params))
deriving instance (Std_.Eq (C.Parsed Vm'snapshotCreate'params))
instance (C.Parse Vm'snapshotCreate'params (C.Parsed Vm'snapshotCreate'params)) where
    parse raw_ = (Vm'snapshotCreate'params <$> (GH.parseField #name raw_))
instance (C.Marshal Vm'snapshotCreate'params (C.Parsed Vm'snapshotCreate'params)) where
    marshalInto raw_ Vm'snapshotCreate'params{..} = (do
        (GH.encodeField #name name raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot Vm'snapshotCreate'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'snapshotCreate'results 
type instance (R.ReprFor Vm'snapshotCreate'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'snapshotCreate'results) where
    typeId  = 17394418354235175569
instance (C.TypedStruct Vm'snapshotCreate'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'snapshotCreate'results) where
    type AllocHint Vm'snapshotCreate'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'snapshotCreate'results (C.Parsed Vm'snapshotCreate'results))
instance (C.AllocateList Vm'snapshotCreate'results) where
    type ListAllocHint Vm'snapshotCreate'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'snapshotCreate'results (C.Parsed Vm'snapshotCreate'results))
data instance C.Parsed Vm'snapshotCreate'results
    = Vm'snapshotCreate'results 
        {snapshot :: (RP.Parsed Capnp.Gen.ById.Xec449e11027b2949.Snapshot)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'snapshotCreate'results))
deriving instance (Std_.Eq (C.Parsed Vm'snapshotCreate'results))
instance (C.Parse Vm'snapshotCreate'results (C.Parsed Vm'snapshotCreate'results)) where
    parse raw_ = (Vm'snapshotCreate'results <$> (GH.parseField #snapshot raw_))
instance (C.Marshal Vm'snapshotCreate'results (C.Parsed Vm'snapshotCreate'results)) where
    marshalInto raw_ Vm'snapshotCreate'results{..} = (do
        (GH.encodeField #snapshot snapshot raw_)
        (Std_.pure ())
        )
instance (GH.HasField "snapshot" GH.Slot Vm'snapshotCreate'results Capnp.Gen.ById.Xec449e11027b2949.Snapshot) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'snapshotList'params 
type instance (R.ReprFor Vm'snapshotList'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'snapshotList'params) where
    typeId  = 15069852262617715782
instance (C.TypedStruct Vm'snapshotList'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'snapshotList'params) where
    type AllocHint Vm'snapshotList'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'snapshotList'params (C.Parsed Vm'snapshotList'params))
instance (C.AllocateList Vm'snapshotList'params) where
    type ListAllocHint Vm'snapshotList'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'snapshotList'params (C.Parsed Vm'snapshotList'params))
data instance C.Parsed Vm'snapshotList'params
    = Vm'snapshotList'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'snapshotList'params))
deriving instance (Std_.Eq (C.Parsed Vm'snapshotList'params))
instance (C.Parse Vm'snapshotList'params (C.Parsed Vm'snapshotList'params)) where
    parse raw_ = (Std_.pure Vm'snapshotList'params)
instance (C.Marshal Vm'snapshotList'params (C.Parsed Vm'snapshotList'params)) where
    marshalInto _raw (Vm'snapshotList'params) = (Std_.pure ())
data Vm'snapshotList'results 
type instance (R.ReprFor Vm'snapshotList'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'snapshotList'results) where
    typeId  = 10341561697833673243
instance (C.TypedStruct Vm'snapshotList'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'snapshotList'results) where
    type AllocHint Vm'snapshotList'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'snapshotList'results (C.Parsed Vm'snapshotList'results))
instance (C.AllocateList Vm'snapshotList'results) where
    type ListAllocHint Vm'snapshotList'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'snapshotList'results (C.Parsed Vm'snapshotList'results))
data instance C.Parsed Vm'snapshotList'results
    = Vm'snapshotList'results 
        {snapshots :: (RP.Parsed (R.List Capnp.Gen.ById.Xec449e11027b2949.SnapshotInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'snapshotList'results))
deriving instance (Std_.Eq (C.Parsed Vm'snapshotList'results))
instance (C.Parse Vm'snapshotList'results (C.Parsed Vm'snapshotList'results)) where
    parse raw_ = (Vm'snapshotList'results <$> (GH.parseField #snapshots raw_))
instance (C.Marshal Vm'snapshotList'results (C.Parsed Vm'snapshotList'results)) where
    marshalInto raw_ Vm'snapshotList'results{..} = (do
        (GH.encodeField #snapshots snapshots raw_)
        (Std_.pure ())
        )
instance (GH.HasField "snapshots" GH.Slot Vm'snapshotList'results (R.List Capnp.Gen.ById.Xec449e11027b2949.SnapshotInfo)) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'snapshotGet'params 
type instance (R.ReprFor Vm'snapshotGet'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'snapshotGet'params) where
    typeId  = 11663265021132023161
instance (C.TypedStruct Vm'snapshotGet'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'snapshotGet'params) where
    type AllocHint Vm'snapshotGet'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'snapshotGet'params (C.Parsed Vm'snapshotGet'params))
instance (C.AllocateList Vm'snapshotGet'params) where
    type ListAllocHint Vm'snapshotGet'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'snapshotGet'params (C.Parsed Vm'snapshotGet'params))
data instance C.Parsed Vm'snapshotGet'params
    = Vm'snapshotGet'params 
        {ref :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'snapshotGet'params))
deriving instance (Std_.Eq (C.Parsed Vm'snapshotGet'params))
instance (C.Parse Vm'snapshotGet'params (C.Parsed Vm'snapshotGet'params)) where
    parse raw_ = (Vm'snapshotGet'params <$> (GH.parseField #ref raw_))
instance (C.Marshal Vm'snapshotGet'params (C.Parsed Vm'snapshotGet'params)) where
    marshalInto raw_ Vm'snapshotGet'params{..} = (do
        (GH.encodeField #ref ref raw_)
        (Std_.pure ())
        )
instance (GH.HasField "ref" GH.Slot Vm'snapshotGet'params Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'snapshotGet'results 
type instance (R.ReprFor Vm'snapshotGet'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'snapshotGet'results) where
    typeId  = 14052326479082387967
instance (C.TypedStruct Vm'snapshotGet'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'snapshotGet'results) where
    type AllocHint Vm'snapshotGet'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'snapshotGet'results (C.Parsed Vm'snapshotGet'results))
instance (C.AllocateList Vm'snapshotGet'results) where
    type ListAllocHint Vm'snapshotGet'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'snapshotGet'results (C.Parsed Vm'snapshotGet'results))
data instance C.Parsed Vm'snapshotGet'results
    = Vm'snapshotGet'results 
        {snapshot :: (RP.Parsed Capnp.Gen.ById.Xec449e11027b2949.Snapshot)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'snapshotGet'results))
deriving instance (Std_.Eq (C.Parsed Vm'snapshotGet'results))
instance (C.Parse Vm'snapshotGet'results (C.Parsed Vm'snapshotGet'results)) where
    parse raw_ = (Vm'snapshotGet'results <$> (GH.parseField #snapshot raw_))
instance (C.Marshal Vm'snapshotGet'results (C.Parsed Vm'snapshotGet'results)) where
    marshalInto raw_ Vm'snapshotGet'results{..} = (do
        (GH.encodeField #snapshot snapshot raw_)
        (Std_.pure ())
        )
instance (GH.HasField "snapshot" GH.Slot Vm'snapshotGet'results Capnp.Gen.ById.Xec449e11027b2949.Snapshot) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'attachSshKey'params 
type instance (R.ReprFor Vm'attachSshKey'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'attachSshKey'params) where
    typeId  = 15334283501653717773
instance (C.TypedStruct Vm'attachSshKey'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'attachSshKey'params) where
    type AllocHint Vm'attachSshKey'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'attachSshKey'params (C.Parsed Vm'attachSshKey'params))
instance (C.AllocateList Vm'attachSshKey'params) where
    type ListAllocHint Vm'attachSshKey'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'attachSshKey'params (C.Parsed Vm'attachSshKey'params))
data instance C.Parsed Vm'attachSshKey'params
    = Vm'attachSshKey'params 
        {keyRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'attachSshKey'params))
deriving instance (Std_.Eq (C.Parsed Vm'attachSshKey'params))
instance (C.Parse Vm'attachSshKey'params (C.Parsed Vm'attachSshKey'params)) where
    parse raw_ = (Vm'attachSshKey'params <$> (GH.parseField #keyRef raw_))
instance (C.Marshal Vm'attachSshKey'params (C.Parsed Vm'attachSshKey'params)) where
    marshalInto raw_ Vm'attachSshKey'params{..} = (do
        (GH.encodeField #keyRef keyRef raw_)
        (Std_.pure ())
        )
instance (GH.HasField "keyRef" GH.Slot Vm'attachSshKey'params Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'attachSshKey'results 
type instance (R.ReprFor Vm'attachSshKey'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'attachSshKey'results) where
    typeId  = 17807124570583137541
instance (C.TypedStruct Vm'attachSshKey'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'attachSshKey'results) where
    type AllocHint Vm'attachSshKey'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'attachSshKey'results (C.Parsed Vm'attachSshKey'results))
instance (C.AllocateList Vm'attachSshKey'results) where
    type ListAllocHint Vm'attachSshKey'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'attachSshKey'results (C.Parsed Vm'attachSshKey'results))
data instance C.Parsed Vm'attachSshKey'results
    = Vm'attachSshKey'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'attachSshKey'results))
deriving instance (Std_.Eq (C.Parsed Vm'attachSshKey'results))
instance (C.Parse Vm'attachSshKey'results (C.Parsed Vm'attachSshKey'results)) where
    parse raw_ = (Std_.pure Vm'attachSshKey'results)
instance (C.Marshal Vm'attachSshKey'results (C.Parsed Vm'attachSshKey'results)) where
    marshalInto _raw (Vm'attachSshKey'results) = (Std_.pure ())
data Vm'detachSshKey'params 
type instance (R.ReprFor Vm'detachSshKey'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'detachSshKey'params) where
    typeId  = 9645341381705313040
instance (C.TypedStruct Vm'detachSshKey'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'detachSshKey'params) where
    type AllocHint Vm'detachSshKey'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'detachSshKey'params (C.Parsed Vm'detachSshKey'params))
instance (C.AllocateList Vm'detachSshKey'params) where
    type ListAllocHint Vm'detachSshKey'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'detachSshKey'params (C.Parsed Vm'detachSshKey'params))
data instance C.Parsed Vm'detachSshKey'params
    = Vm'detachSshKey'params 
        {keyRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'detachSshKey'params))
deriving instance (Std_.Eq (C.Parsed Vm'detachSshKey'params))
instance (C.Parse Vm'detachSshKey'params (C.Parsed Vm'detachSshKey'params)) where
    parse raw_ = (Vm'detachSshKey'params <$> (GH.parseField #keyRef raw_))
instance (C.Marshal Vm'detachSshKey'params (C.Parsed Vm'detachSshKey'params)) where
    marshalInto raw_ Vm'detachSshKey'params{..} = (do
        (GH.encodeField #keyRef keyRef raw_)
        (Std_.pure ())
        )
instance (GH.HasField "keyRef" GH.Slot Vm'detachSshKey'params Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'detachSshKey'results 
type instance (R.ReprFor Vm'detachSshKey'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'detachSshKey'results) where
    typeId  = 9917745047128506196
instance (C.TypedStruct Vm'detachSshKey'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'detachSshKey'results) where
    type AllocHint Vm'detachSshKey'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'detachSshKey'results (C.Parsed Vm'detachSshKey'results))
instance (C.AllocateList Vm'detachSshKey'results) where
    type ListAllocHint Vm'detachSshKey'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'detachSshKey'results (C.Parsed Vm'detachSshKey'results))
data instance C.Parsed Vm'detachSshKey'results
    = Vm'detachSshKey'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'detachSshKey'results))
deriving instance (Std_.Eq (C.Parsed Vm'detachSshKey'results))
instance (C.Parse Vm'detachSshKey'results (C.Parsed Vm'detachSshKey'results)) where
    parse raw_ = (Std_.pure Vm'detachSshKey'results)
instance (C.Marshal Vm'detachSshKey'results (C.Parsed Vm'detachSshKey'results)) where
    marshalInto _raw (Vm'detachSshKey'results) = (Std_.pure ())
data Vm'listSshKeys'params 
type instance (R.ReprFor Vm'listSshKeys'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'listSshKeys'params) where
    typeId  = 13281161093432106558
instance (C.TypedStruct Vm'listSshKeys'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Vm'listSshKeys'params) where
    type AllocHint Vm'listSshKeys'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'listSshKeys'params (C.Parsed Vm'listSshKeys'params))
instance (C.AllocateList Vm'listSshKeys'params) where
    type ListAllocHint Vm'listSshKeys'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'listSshKeys'params (C.Parsed Vm'listSshKeys'params))
data instance C.Parsed Vm'listSshKeys'params
    = Vm'listSshKeys'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'listSshKeys'params))
deriving instance (Std_.Eq (C.Parsed Vm'listSshKeys'params))
instance (C.Parse Vm'listSshKeys'params (C.Parsed Vm'listSshKeys'params)) where
    parse raw_ = (Std_.pure Vm'listSshKeys'params)
instance (C.Marshal Vm'listSshKeys'params (C.Parsed Vm'listSshKeys'params)) where
    marshalInto _raw (Vm'listSshKeys'params) = (Std_.pure ())
data Vm'listSshKeys'results 
type instance (R.ReprFor Vm'listSshKeys'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'listSshKeys'results) where
    typeId  = 10810412818880359450
instance (C.TypedStruct Vm'listSshKeys'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'listSshKeys'results) where
    type AllocHint Vm'listSshKeys'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'listSshKeys'results (C.Parsed Vm'listSshKeys'results))
instance (C.AllocateList Vm'listSshKeys'results) where
    type ListAllocHint Vm'listSshKeys'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'listSshKeys'results (C.Parsed Vm'listSshKeys'results))
data instance C.Parsed Vm'listSshKeys'results
    = Vm'listSshKeys'results 
        {keys :: (RP.Parsed (R.List Capnp.Gen.ById.Xa6341bd086aa89f6.SshKeyInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'listSshKeys'results))
deriving instance (Std_.Eq (C.Parsed Vm'listSshKeys'results))
instance (C.Parse Vm'listSshKeys'results (C.Parsed Vm'listSshKeys'results)) where
    parse raw_ = (Vm'listSshKeys'results <$> (GH.parseField #keys raw_))
instance (C.Marshal Vm'listSshKeys'results (C.Parsed Vm'listSshKeys'results)) where
    marshalInto raw_ Vm'listSshKeys'results{..} = (do
        (GH.encodeField #keys keys raw_)
        (Std_.pure ())
        )
instance (GH.HasField "keys" GH.Slot Vm'listSshKeys'results (R.List Capnp.Gen.ById.Xa6341bd086aa89f6.SshKeyInfo)) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'migrate'params 
type instance (R.ReprFor Vm'migrate'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'migrate'params) where
    typeId  = 9453909330779409115
instance (C.TypedStruct Vm'migrate'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Vm'migrate'params) where
    type AllocHint Vm'migrate'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'migrate'params (C.Parsed Vm'migrate'params))
instance (C.AllocateList Vm'migrate'params) where
    type ListAllocHint Vm'migrate'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'migrate'params (C.Parsed Vm'migrate'params))
data instance C.Parsed Vm'migrate'params
    = Vm'migrate'params 
        {params :: (RP.Parsed VmMigrateParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'migrate'params))
deriving instance (Std_.Eq (C.Parsed Vm'migrate'params))
instance (C.Parse Vm'migrate'params (C.Parsed Vm'migrate'params)) where
    parse raw_ = (Vm'migrate'params <$> (GH.parseField #params raw_))
instance (C.Marshal Vm'migrate'params (C.Parsed Vm'migrate'params)) where
    marshalInto raw_ Vm'migrate'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot Vm'migrate'params VmMigrateParams) where
    fieldByLabel  = (GH.ptrField 0)
data Vm'migrate'results 
type instance (R.ReprFor Vm'migrate'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Vm'migrate'results) where
    typeId  = 9935088300684081619
instance (C.TypedStruct Vm'migrate'results) where
    numStructWords  = 1
    numStructPtrs  = 0
instance (C.Allocate Vm'migrate'results) where
    type AllocHint Vm'migrate'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Vm'migrate'results (C.Parsed Vm'migrate'results))
instance (C.AllocateList Vm'migrate'results) where
    type ListAllocHint Vm'migrate'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Vm'migrate'results (C.Parsed Vm'migrate'results))
data instance C.Parsed Vm'migrate'results
    = Vm'migrate'results 
        {taskId :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Vm'migrate'results))
deriving instance (Std_.Eq (C.Parsed Vm'migrate'results))
instance (C.Parse Vm'migrate'results (C.Parsed Vm'migrate'results)) where
    parse raw_ = (Vm'migrate'results <$> (GH.parseField #taskId raw_))
instance (C.Marshal Vm'migrate'results (C.Parsed Vm'migrate'results)) where
    marshalInto raw_ Vm'migrate'results{..} = (do
        (GH.encodeField #taskId taskId raw_)
        (Std_.pure ())
        )
instance (GH.HasField "taskId" GH.Slot Vm'migrate'results Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
data VmMigrateParams 
type instance (R.ReprFor VmMigrateParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId VmMigrateParams) where
    typeId  = 17275536786290198101
instance (C.TypedStruct VmMigrateParams) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate VmMigrateParams) where
    type AllocHint VmMigrateParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc VmMigrateParams (C.Parsed VmMigrateParams))
instance (C.AllocateList VmMigrateParams) where
    type ListAllocHint VmMigrateParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc VmMigrateParams (C.Parsed VmMigrateParams))
data instance C.Parsed VmMigrateParams
    = VmMigrateParams 
        {toNodeRef :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed VmMigrateParams))
deriving instance (Std_.Eq (C.Parsed VmMigrateParams))
instance (C.Parse VmMigrateParams (C.Parsed VmMigrateParams)) where
    parse raw_ = (VmMigrateParams <$> (GH.parseField #toNodeRef raw_))
instance (C.Marshal VmMigrateParams (C.Parsed VmMigrateParams)) where
    marshalInto raw_ VmMigrateParams{..} = (do
        (GH.encodeField #toNodeRef toNodeRef raw_)
        (Std_.pure ())
        )
instance (GH.HasField "toNodeRef" GH.Slot VmMigrateParams Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)