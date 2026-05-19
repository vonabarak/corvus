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
module Capnp.Gen.Template where
import qualified Capnp.Repr as R
import qualified Capnp.Repr.Parsed as RP
import qualified Capnp.Basics as Basics
import qualified GHC.OverloadedLabels as OL
import qualified Capnp.GenHelpers as GH
import qualified Capnp.Classes as C
import qualified GHC.Generics as Generics
import qualified Capnp.GenHelpers.Rpc as GH
import qualified Capnp.Gen.ById.X9b1373e2334a09e9
import qualified Capnp.Gen.ById.Xa7366eabdb0b1db4
import qualified Capnp.Gen.ById.Xbf9b09f64c0dd40d
import qualified Capnp.Gen.ById.Xeb6a435f11477f84
import qualified Prelude as Std_
import qualified Data.Word as Std_
import qualified Data.Int as Std_
import Prelude ((<$>), (<*>), (>>=))
data TemplateVmInfo 
type instance (R.ReprFor TemplateVmInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TemplateVmInfo) where
    typeId  = 13502303933436327559
instance (C.TypedStruct TemplateVmInfo) where
    numStructWords  = 3
    numStructPtrs  = 2
instance (C.Allocate TemplateVmInfo) where
    type AllocHint TemplateVmInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TemplateVmInfo (C.Parsed TemplateVmInfo))
instance (C.AllocateList TemplateVmInfo) where
    type ListAllocHint TemplateVmInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TemplateVmInfo (C.Parsed TemplateVmInfo))
data instance C.Parsed TemplateVmInfo
    = TemplateVmInfo 
        {id :: (RP.Parsed Std_.Int64)
        ,name :: (RP.Parsed Basics.Text)
        ,cpuCount :: (RP.Parsed Std_.Int32)
        ,ramMb :: (RP.Parsed Std_.Int32)
        ,description :: (RP.Parsed Basics.Text)
        ,headless :: (RP.Parsed Std_.Bool)
        ,guestAgent :: (RP.Parsed Std_.Bool)
        ,autostart :: (RP.Parsed Std_.Bool)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TemplateVmInfo))
deriving instance (Std_.Eq (C.Parsed TemplateVmInfo))
instance (C.Parse TemplateVmInfo (C.Parsed TemplateVmInfo)) where
    parse raw_ = (TemplateVmInfo <$> (GH.parseField #id raw_)
                                 <*> (GH.parseField #name raw_)
                                 <*> (GH.parseField #cpuCount raw_)
                                 <*> (GH.parseField #ramMb raw_)
                                 <*> (GH.parseField #description raw_)
                                 <*> (GH.parseField #headless raw_)
                                 <*> (GH.parseField #guestAgent raw_)
                                 <*> (GH.parseField #autostart raw_))
instance (C.Marshal TemplateVmInfo (C.Parsed TemplateVmInfo)) where
    marshalInto raw_ TemplateVmInfo{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #cpuCount cpuCount raw_)
        (GH.encodeField #ramMb ramMb raw_)
        (GH.encodeField #description description raw_)
        (GH.encodeField #headless headless raw_)
        (GH.encodeField #guestAgent guestAgent raw_)
        (GH.encodeField #autostart autostart raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot TemplateVmInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "name" GH.Slot TemplateVmInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "cpuCount" GH.Slot TemplateVmInfo Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
instance (GH.HasField "ramMb" GH.Slot TemplateVmInfo Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 1 32 0)
instance (GH.HasField "description" GH.Slot TemplateVmInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "headless" GH.Slot TemplateVmInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 2 1 0)
instance (GH.HasField "guestAgent" GH.Slot TemplateVmInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 2 1 0)
instance (GH.HasField "autostart" GH.Slot TemplateVmInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 2 2 1 0)
data TemplateDriveInfo 
type instance (R.ReprFor TemplateDriveInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TemplateDriveInfo) where
    typeId  = 9792678170896308609
instance (C.TypedStruct TemplateDriveInfo) where
    numStructWords  = 4
    numStructPtrs  = 1
instance (C.Allocate TemplateDriveInfo) where
    type AllocHint TemplateDriveInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TemplateDriveInfo (C.Parsed TemplateDriveInfo))
instance (C.AllocateList TemplateDriveInfo) where
    type ListAllocHint TemplateDriveInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TemplateDriveInfo (C.Parsed TemplateDriveInfo))
data instance C.Parsed TemplateDriveInfo
    = TemplateDriveInfo 
        {diskImageId :: (RP.Parsed Std_.Int64)
        ,diskImageName :: (RP.Parsed Basics.Text)
        ,interface :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveInterface)
        ,hasMedia :: (RP.Parsed Std_.Bool)
        ,media :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveMedia)
        ,readOnly :: (RP.Parsed Std_.Bool)
        ,cacheType :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.CacheType)
        ,discard :: (RP.Parsed Std_.Bool)
        ,cloneStrategy :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.TemplateCloneStrategy)
        ,sizeMb :: (RP.Parsed Std_.Int64)
        ,hasFormat :: (RP.Parsed Std_.Bool)
        ,format :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveFormat)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TemplateDriveInfo))
deriving instance (Std_.Eq (C.Parsed TemplateDriveInfo))
instance (C.Parse TemplateDriveInfo (C.Parsed TemplateDriveInfo)) where
    parse raw_ = (TemplateDriveInfo <$> (GH.parseField #diskImageId raw_)
                                    <*> (GH.parseField #diskImageName raw_)
                                    <*> (GH.parseField #interface raw_)
                                    <*> (GH.parseField #hasMedia raw_)
                                    <*> (GH.parseField #media raw_)
                                    <*> (GH.parseField #readOnly raw_)
                                    <*> (GH.parseField #cacheType raw_)
                                    <*> (GH.parseField #discard raw_)
                                    <*> (GH.parseField #cloneStrategy raw_)
                                    <*> (GH.parseField #sizeMb raw_)
                                    <*> (GH.parseField #hasFormat raw_)
                                    <*> (GH.parseField #format raw_))
instance (C.Marshal TemplateDriveInfo (C.Parsed TemplateDriveInfo)) where
    marshalInto raw_ TemplateDriveInfo{..} = (do
        (GH.encodeField #diskImageId diskImageId raw_)
        (GH.encodeField #diskImageName diskImageName raw_)
        (GH.encodeField #interface interface raw_)
        (GH.encodeField #hasMedia hasMedia raw_)
        (GH.encodeField #media media raw_)
        (GH.encodeField #readOnly readOnly raw_)
        (GH.encodeField #cacheType cacheType raw_)
        (GH.encodeField #discard discard raw_)
        (GH.encodeField #cloneStrategy cloneStrategy raw_)
        (GH.encodeField #sizeMb sizeMb raw_)
        (GH.encodeField #hasFormat hasFormat raw_)
        (GH.encodeField #format format raw_)
        (Std_.pure ())
        )
instance (GH.HasField "diskImageId" GH.Slot TemplateDriveInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "diskImageName" GH.Slot TemplateDriveInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "interface" GH.Slot TemplateDriveInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveInterface) where
    fieldByLabel  = (GH.dataField 0 1 16 0)
instance (GH.HasField "hasMedia" GH.Slot TemplateDriveInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 16 1 1 0)
instance (GH.HasField "media" GH.Slot TemplateDriveInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveMedia) where
    fieldByLabel  = (GH.dataField 32 1 16 0)
instance (GH.HasField "readOnly" GH.Slot TemplateDriveInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 17 1 1 0)
instance (GH.HasField "cacheType" GH.Slot TemplateDriveInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.CacheType) where
    fieldByLabel  = (GH.dataField 48 1 16 0)
instance (GH.HasField "discard" GH.Slot TemplateDriveInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 18 1 1 0)
instance (GH.HasField "cloneStrategy" GH.Slot TemplateDriveInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.TemplateCloneStrategy) where
    fieldByLabel  = (GH.dataField 0 2 16 0)
instance (GH.HasField "sizeMb" GH.Slot TemplateDriveInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 3 64 0)
instance (GH.HasField "hasFormat" GH.Slot TemplateDriveInfo Std_.Bool) where
    fieldByLabel  = (GH.dataField 19 1 1 0)
instance (GH.HasField "format" GH.Slot TemplateDriveInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.DriveFormat) where
    fieldByLabel  = (GH.dataField 16 2 16 0)
data TemplateNetIfInfo 
type instance (R.ReprFor TemplateNetIfInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TemplateNetIfInfo) where
    typeId  = 15385606902103944014
instance (C.TypedStruct TemplateNetIfInfo) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate TemplateNetIfInfo) where
    type AllocHint TemplateNetIfInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TemplateNetIfInfo (C.Parsed TemplateNetIfInfo))
instance (C.AllocateList TemplateNetIfInfo) where
    type ListAllocHint TemplateNetIfInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TemplateNetIfInfo (C.Parsed TemplateNetIfInfo))
data instance C.Parsed TemplateNetIfInfo
    = TemplateNetIfInfo 
        {type_ :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.NetInterfaceType)
        ,hostDevice :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TemplateNetIfInfo))
deriving instance (Std_.Eq (C.Parsed TemplateNetIfInfo))
instance (C.Parse TemplateNetIfInfo (C.Parsed TemplateNetIfInfo)) where
    parse raw_ = (TemplateNetIfInfo <$> (GH.parseField #type_ raw_)
                                    <*> (GH.parseField #hostDevice raw_))
instance (C.Marshal TemplateNetIfInfo (C.Parsed TemplateNetIfInfo)) where
    marshalInto raw_ TemplateNetIfInfo{..} = (do
        (GH.encodeField #type_ type_ raw_)
        (GH.encodeField #hostDevice hostDevice raw_)
        (Std_.pure ())
        )
instance (GH.HasField "type_" GH.Slot TemplateNetIfInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.NetInterfaceType) where
    fieldByLabel  = (GH.dataField 0 0 16 0)
instance (GH.HasField "hostDevice" GH.Slot TemplateNetIfInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data TemplateSshKeyInfo 
type instance (R.ReprFor TemplateSshKeyInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TemplateSshKeyInfo) where
    typeId  = 12438895007690612930
instance (C.TypedStruct TemplateSshKeyInfo) where
    numStructWords  = 1
    numStructPtrs  = 1
instance (C.Allocate TemplateSshKeyInfo) where
    type AllocHint TemplateSshKeyInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TemplateSshKeyInfo (C.Parsed TemplateSshKeyInfo))
instance (C.AllocateList TemplateSshKeyInfo) where
    type ListAllocHint TemplateSshKeyInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TemplateSshKeyInfo (C.Parsed TemplateSshKeyInfo))
data instance C.Parsed TemplateSshKeyInfo
    = TemplateSshKeyInfo 
        {id :: (RP.Parsed Std_.Int64)
        ,name :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TemplateSshKeyInfo))
deriving instance (Std_.Eq (C.Parsed TemplateSshKeyInfo))
instance (C.Parse TemplateSshKeyInfo (C.Parsed TemplateSshKeyInfo)) where
    parse raw_ = (TemplateSshKeyInfo <$> (GH.parseField #id raw_)
                                     <*> (GH.parseField #name raw_))
instance (C.Marshal TemplateSshKeyInfo (C.Parsed TemplateSshKeyInfo)) where
    marshalInto raw_ TemplateSshKeyInfo{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #name name raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot TemplateSshKeyInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "name" GH.Slot TemplateSshKeyInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data TemplateDetails 
type instance (R.ReprFor TemplateDetails) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TemplateDetails) where
    typeId  = 15861980235634274923
instance (C.TypedStruct TemplateDetails) where
    numStructWords  = 4
    numStructPtrs  = 6
instance (C.Allocate TemplateDetails) where
    type AllocHint TemplateDetails = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TemplateDetails (C.Parsed TemplateDetails))
instance (C.AllocateList TemplateDetails) where
    type ListAllocHint TemplateDetails = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TemplateDetails (C.Parsed TemplateDetails))
data instance C.Parsed TemplateDetails
    = TemplateDetails 
        {id :: (RP.Parsed Std_.Int64)
        ,name :: (RP.Parsed Basics.Text)
        ,cpuCount :: (RP.Parsed Std_.Int32)
        ,ramMb :: (RP.Parsed Std_.Int32)
        ,description :: (RP.Parsed Basics.Text)
        ,headless :: (RP.Parsed Std_.Bool)
        ,cloudInit :: (RP.Parsed Std_.Bool)
        ,guestAgent :: (RP.Parsed Std_.Bool)
        ,autostart :: (RP.Parsed Std_.Bool)
        ,cloudInitConfig :: (RP.Parsed Capnp.Gen.ById.Xeb6a435f11477f84.CloudInitInfo)
        ,createdAt :: (RP.Parsed Std_.Int64)
        ,drives :: (RP.Parsed (R.List TemplateDriveInfo))
        ,netIfs :: (RP.Parsed (R.List TemplateNetIfInfo))
        ,sshKeys :: (RP.Parsed (R.List TemplateSshKeyInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TemplateDetails))
deriving instance (Std_.Eq (C.Parsed TemplateDetails))
instance (C.Parse TemplateDetails (C.Parsed TemplateDetails)) where
    parse raw_ = (TemplateDetails <$> (GH.parseField #id raw_)
                                  <*> (GH.parseField #name raw_)
                                  <*> (GH.parseField #cpuCount raw_)
                                  <*> (GH.parseField #ramMb raw_)
                                  <*> (GH.parseField #description raw_)
                                  <*> (GH.parseField #headless raw_)
                                  <*> (GH.parseField #cloudInit raw_)
                                  <*> (GH.parseField #guestAgent raw_)
                                  <*> (GH.parseField #autostart raw_)
                                  <*> (GH.parseField #cloudInitConfig raw_)
                                  <*> (GH.parseField #createdAt raw_)
                                  <*> (GH.parseField #drives raw_)
                                  <*> (GH.parseField #netIfs raw_)
                                  <*> (GH.parseField #sshKeys raw_))
instance (C.Marshal TemplateDetails (C.Parsed TemplateDetails)) where
    marshalInto raw_ TemplateDetails{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #cpuCount cpuCount raw_)
        (GH.encodeField #ramMb ramMb raw_)
        (GH.encodeField #description description raw_)
        (GH.encodeField #headless headless raw_)
        (GH.encodeField #cloudInit cloudInit raw_)
        (GH.encodeField #guestAgent guestAgent raw_)
        (GH.encodeField #autostart autostart raw_)
        (GH.encodeField #cloudInitConfig cloudInitConfig raw_)
        (GH.encodeField #createdAt createdAt raw_)
        (GH.encodeField #drives drives raw_)
        (GH.encodeField #netIfs netIfs raw_)
        (GH.encodeField #sshKeys sshKeys raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot TemplateDetails Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "name" GH.Slot TemplateDetails Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "cpuCount" GH.Slot TemplateDetails Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
instance (GH.HasField "ramMb" GH.Slot TemplateDetails Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 1 32 0)
instance (GH.HasField "description" GH.Slot TemplateDetails Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "headless" GH.Slot TemplateDetails Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 2 1 0)
instance (GH.HasField "cloudInit" GH.Slot TemplateDetails Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 2 1 0)
instance (GH.HasField "guestAgent" GH.Slot TemplateDetails Std_.Bool) where
    fieldByLabel  = (GH.dataField 2 2 1 0)
instance (GH.HasField "autostart" GH.Slot TemplateDetails Std_.Bool) where
    fieldByLabel  = (GH.dataField 3 2 1 0)
instance (GH.HasField "cloudInitConfig" GH.Slot TemplateDetails Capnp.Gen.ById.Xeb6a435f11477f84.CloudInitInfo) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "createdAt" GH.Slot TemplateDetails Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 3 64 0)
instance (GH.HasField "drives" GH.Slot TemplateDetails (R.List TemplateDriveInfo)) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "netIfs" GH.Slot TemplateDetails (R.List TemplateNetIfInfo)) where
    fieldByLabel  = (GH.ptrField 4)
instance (GH.HasField "sshKeys" GH.Slot TemplateDetails (R.List TemplateSshKeyInfo)) where
    fieldByLabel  = (GH.ptrField 5)
data TemplateManager 
type instance (R.ReprFor TemplateManager) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId TemplateManager) where
    typeId  = 11470161416002571742
instance (C.Parse TemplateManager (GH.Client TemplateManager)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export TemplateManager) where
    type Server TemplateManager = TemplateManager'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(TemplateManager)) [(GH.toUntypedMethodHandler ((templateManager'list) s_))
                                                                                 ,(GH.toUntypedMethodHandler ((templateManager'get) s_))
                                                                                 ,(GH.toUntypedMethodHandler ((templateManager'create) s_))] [])
class (TemplateManager'server_ s_) where
    {-# MINIMAL templateManager'list,templateManager'get,templateManager'create #-}
    templateManager'list :: s_ -> (GH.MethodHandler TemplateManager'list'params TemplateManager'list'results)
    templateManager'list _ = GH.methodUnimplemented
    templateManager'get :: s_ -> (GH.MethodHandler TemplateManager'get'params TemplateManager'get'results)
    templateManager'get _ = GH.methodUnimplemented
    templateManager'create :: s_ -> (GH.MethodHandler TemplateManager'create'params TemplateManager'create'results)
    templateManager'create _ = GH.methodUnimplemented
instance (GH.HasMethod "list" TemplateManager TemplateManager'list'params TemplateManager'list'results) where
    methodByLabel  = (GH.Method 11470161416002571742 0)
instance (GH.HasMethod "get" TemplateManager TemplateManager'get'params TemplateManager'get'results) where
    methodByLabel  = (GH.Method 11470161416002571742 1)
instance (GH.HasMethod "create" TemplateManager TemplateManager'create'params TemplateManager'create'results) where
    methodByLabel  = (GH.Method 11470161416002571742 2)
data TemplateManager'list'params 
type instance (R.ReprFor TemplateManager'list'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TemplateManager'list'params) where
    typeId  = 14053678505770670219
instance (C.TypedStruct TemplateManager'list'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate TemplateManager'list'params) where
    type AllocHint TemplateManager'list'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TemplateManager'list'params (C.Parsed TemplateManager'list'params))
instance (C.AllocateList TemplateManager'list'params) where
    type ListAllocHint TemplateManager'list'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TemplateManager'list'params (C.Parsed TemplateManager'list'params))
data instance C.Parsed TemplateManager'list'params
    = TemplateManager'list'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TemplateManager'list'params))
deriving instance (Std_.Eq (C.Parsed TemplateManager'list'params))
instance (C.Parse TemplateManager'list'params (C.Parsed TemplateManager'list'params)) where
    parse raw_ = (Std_.pure TemplateManager'list'params)
instance (C.Marshal TemplateManager'list'params (C.Parsed TemplateManager'list'params)) where
    marshalInto _raw (TemplateManager'list'params) = (Std_.pure ())
data TemplateManager'list'results 
type instance (R.ReprFor TemplateManager'list'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TemplateManager'list'results) where
    typeId  = 10374716171346326632
instance (C.TypedStruct TemplateManager'list'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate TemplateManager'list'results) where
    type AllocHint TemplateManager'list'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TemplateManager'list'results (C.Parsed TemplateManager'list'results))
instance (C.AllocateList TemplateManager'list'results) where
    type ListAllocHint TemplateManager'list'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TemplateManager'list'results (C.Parsed TemplateManager'list'results))
data instance C.Parsed TemplateManager'list'results
    = TemplateManager'list'results 
        {templates :: (RP.Parsed (R.List TemplateVmInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TemplateManager'list'results))
deriving instance (Std_.Eq (C.Parsed TemplateManager'list'results))
instance (C.Parse TemplateManager'list'results (C.Parsed TemplateManager'list'results)) where
    parse raw_ = (TemplateManager'list'results <$> (GH.parseField #templates raw_))
instance (C.Marshal TemplateManager'list'results (C.Parsed TemplateManager'list'results)) where
    marshalInto raw_ TemplateManager'list'results{..} = (do
        (GH.encodeField #templates templates raw_)
        (Std_.pure ())
        )
instance (GH.HasField "templates" GH.Slot TemplateManager'list'results (R.List TemplateVmInfo)) where
    fieldByLabel  = (GH.ptrField 0)
data TemplateManager'get'params 
type instance (R.ReprFor TemplateManager'get'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TemplateManager'get'params) where
    typeId  = 14921537289311125397
instance (C.TypedStruct TemplateManager'get'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate TemplateManager'get'params) where
    type AllocHint TemplateManager'get'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TemplateManager'get'params (C.Parsed TemplateManager'get'params))
instance (C.AllocateList TemplateManager'get'params) where
    type ListAllocHint TemplateManager'get'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TemplateManager'get'params (C.Parsed TemplateManager'get'params))
data instance C.Parsed TemplateManager'get'params
    = TemplateManager'get'params 
        {ref :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TemplateManager'get'params))
deriving instance (Std_.Eq (C.Parsed TemplateManager'get'params))
instance (C.Parse TemplateManager'get'params (C.Parsed TemplateManager'get'params)) where
    parse raw_ = (TemplateManager'get'params <$> (GH.parseField #ref raw_))
instance (C.Marshal TemplateManager'get'params (C.Parsed TemplateManager'get'params)) where
    marshalInto raw_ TemplateManager'get'params{..} = (do
        (GH.encodeField #ref ref raw_)
        (Std_.pure ())
        )
instance (GH.HasField "ref" GH.Slot TemplateManager'get'params Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
data TemplateManager'get'results 
type instance (R.ReprFor TemplateManager'get'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TemplateManager'get'results) where
    typeId  = 15990190738256219539
instance (C.TypedStruct TemplateManager'get'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate TemplateManager'get'results) where
    type AllocHint TemplateManager'get'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TemplateManager'get'results (C.Parsed TemplateManager'get'results))
instance (C.AllocateList TemplateManager'get'results) where
    type ListAllocHint TemplateManager'get'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TemplateManager'get'results (C.Parsed TemplateManager'get'results))
data instance C.Parsed TemplateManager'get'results
    = TemplateManager'get'results 
        {template :: (RP.Parsed Template)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TemplateManager'get'results))
deriving instance (Std_.Eq (C.Parsed TemplateManager'get'results))
instance (C.Parse TemplateManager'get'results (C.Parsed TemplateManager'get'results)) where
    parse raw_ = (TemplateManager'get'results <$> (GH.parseField #template raw_))
instance (C.Marshal TemplateManager'get'results (C.Parsed TemplateManager'get'results)) where
    marshalInto raw_ TemplateManager'get'results{..} = (do
        (GH.encodeField #template template raw_)
        (Std_.pure ())
        )
instance (GH.HasField "template" GH.Slot TemplateManager'get'results Template) where
    fieldByLabel  = (GH.ptrField 0)
data TemplateManager'create'params 
type instance (R.ReprFor TemplateManager'create'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TemplateManager'create'params) where
    typeId  = 10206283132667501545
instance (C.TypedStruct TemplateManager'create'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate TemplateManager'create'params) where
    type AllocHint TemplateManager'create'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TemplateManager'create'params (C.Parsed TemplateManager'create'params))
instance (C.AllocateList TemplateManager'create'params) where
    type ListAllocHint TemplateManager'create'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TemplateManager'create'params (C.Parsed TemplateManager'create'params))
data instance C.Parsed TemplateManager'create'params
    = TemplateManager'create'params 
        {yaml :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TemplateManager'create'params))
deriving instance (Std_.Eq (C.Parsed TemplateManager'create'params))
instance (C.Parse TemplateManager'create'params (C.Parsed TemplateManager'create'params)) where
    parse raw_ = (TemplateManager'create'params <$> (GH.parseField #yaml raw_))
instance (C.Marshal TemplateManager'create'params (C.Parsed TemplateManager'create'params)) where
    marshalInto raw_ TemplateManager'create'params{..} = (do
        (GH.encodeField #yaml yaml raw_)
        (Std_.pure ())
        )
instance (GH.HasField "yaml" GH.Slot TemplateManager'create'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data TemplateManager'create'results 
type instance (R.ReprFor TemplateManager'create'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId TemplateManager'create'results) where
    typeId  = 9224409178913014260
instance (C.TypedStruct TemplateManager'create'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate TemplateManager'create'results) where
    type AllocHint TemplateManager'create'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc TemplateManager'create'results (C.Parsed TemplateManager'create'results))
instance (C.AllocateList TemplateManager'create'results) where
    type ListAllocHint TemplateManager'create'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc TemplateManager'create'results (C.Parsed TemplateManager'create'results))
data instance C.Parsed TemplateManager'create'results
    = TemplateManager'create'results 
        {template :: (RP.Parsed Template)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed TemplateManager'create'results))
deriving instance (Std_.Eq (C.Parsed TemplateManager'create'results))
instance (C.Parse TemplateManager'create'results (C.Parsed TemplateManager'create'results)) where
    parse raw_ = (TemplateManager'create'results <$> (GH.parseField #template raw_))
instance (C.Marshal TemplateManager'create'results (C.Parsed TemplateManager'create'results)) where
    marshalInto raw_ TemplateManager'create'results{..} = (do
        (GH.encodeField #template template raw_)
        (Std_.pure ())
        )
instance (GH.HasField "template" GH.Slot TemplateManager'create'results Template) where
    fieldByLabel  = (GH.ptrField 0)
data Template 
type instance (R.ReprFor Template) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Template) where
    typeId  = 15546638347777094346
instance (C.Parse Template (GH.Client Template)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Template) where
    type Server Template = Template'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Template)) [(GH.toUntypedMethodHandler ((template'show) s_))
                                                                          ,(GH.toUntypedMethodHandler ((template'delete) s_))
                                                                          ,(GH.toUntypedMethodHandler ((template'instantiate) s_))
                                                                          ,(GH.toUntypedMethodHandler ((template'update) s_))] [])
class (Template'server_ s_) where
    {-# MINIMAL template'show,template'delete,template'instantiate,template'update #-}
    template'show :: s_ -> (GH.MethodHandler Template'show'params Template'show'results)
    template'show _ = GH.methodUnimplemented
    template'delete :: s_ -> (GH.MethodHandler Template'delete'params Template'delete'results)
    template'delete _ = GH.methodUnimplemented
    template'instantiate :: s_ -> (GH.MethodHandler Template'instantiate'params Template'instantiate'results)
    template'instantiate _ = GH.methodUnimplemented
    template'update :: s_ -> (GH.MethodHandler Template'update'params Template'update'results)
    template'update _ = GH.methodUnimplemented
instance (GH.HasMethod "show" Template Template'show'params Template'show'results) where
    methodByLabel  = (GH.Method 15546638347777094346 0)
instance (GH.HasMethod "delete" Template Template'delete'params Template'delete'results) where
    methodByLabel  = (GH.Method 15546638347777094346 1)
instance (GH.HasMethod "instantiate" Template Template'instantiate'params Template'instantiate'results) where
    methodByLabel  = (GH.Method 15546638347777094346 2)
instance (GH.HasMethod "update" Template Template'update'params Template'update'results) where
    methodByLabel  = (GH.Method 15546638347777094346 3)
data Template'show'params 
type instance (R.ReprFor Template'show'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Template'show'params) where
    typeId  = 15712543663182714018
instance (C.TypedStruct Template'show'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Template'show'params) where
    type AllocHint Template'show'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Template'show'params (C.Parsed Template'show'params))
instance (C.AllocateList Template'show'params) where
    type ListAllocHint Template'show'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Template'show'params (C.Parsed Template'show'params))
data instance C.Parsed Template'show'params
    = Template'show'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Template'show'params))
deriving instance (Std_.Eq (C.Parsed Template'show'params))
instance (C.Parse Template'show'params (C.Parsed Template'show'params)) where
    parse raw_ = (Std_.pure Template'show'params)
instance (C.Marshal Template'show'params (C.Parsed Template'show'params)) where
    marshalInto _raw (Template'show'params) = (Std_.pure ())
data Template'show'results 
type instance (R.ReprFor Template'show'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Template'show'results) where
    typeId  = 18060360011986660734
instance (C.TypedStruct Template'show'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Template'show'results) where
    type AllocHint Template'show'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Template'show'results (C.Parsed Template'show'results))
instance (C.AllocateList Template'show'results) where
    type ListAllocHint Template'show'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Template'show'results (C.Parsed Template'show'results))
data instance C.Parsed Template'show'results
    = Template'show'results 
        {details :: (RP.Parsed TemplateDetails)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Template'show'results))
deriving instance (Std_.Eq (C.Parsed Template'show'results))
instance (C.Parse Template'show'results (C.Parsed Template'show'results)) where
    parse raw_ = (Template'show'results <$> (GH.parseField #details raw_))
instance (C.Marshal Template'show'results (C.Parsed Template'show'results)) where
    marshalInto raw_ Template'show'results{..} = (do
        (GH.encodeField #details details raw_)
        (Std_.pure ())
        )
instance (GH.HasField "details" GH.Slot Template'show'results TemplateDetails) where
    fieldByLabel  = (GH.ptrField 0)
data Template'delete'params 
type instance (R.ReprFor Template'delete'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Template'delete'params) where
    typeId  = 15059821146489763716
instance (C.TypedStruct Template'delete'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Template'delete'params) where
    type AllocHint Template'delete'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Template'delete'params (C.Parsed Template'delete'params))
instance (C.AllocateList Template'delete'params) where
    type ListAllocHint Template'delete'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Template'delete'params (C.Parsed Template'delete'params))
data instance C.Parsed Template'delete'params
    = Template'delete'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Template'delete'params))
deriving instance (Std_.Eq (C.Parsed Template'delete'params))
instance (C.Parse Template'delete'params (C.Parsed Template'delete'params)) where
    parse raw_ = (Std_.pure Template'delete'params)
instance (C.Marshal Template'delete'params (C.Parsed Template'delete'params)) where
    marshalInto _raw (Template'delete'params) = (Std_.pure ())
data Template'delete'results 
type instance (R.ReprFor Template'delete'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Template'delete'results) where
    typeId  = 13065661108707472177
instance (C.TypedStruct Template'delete'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Template'delete'results) where
    type AllocHint Template'delete'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Template'delete'results (C.Parsed Template'delete'results))
instance (C.AllocateList Template'delete'results) where
    type ListAllocHint Template'delete'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Template'delete'results (C.Parsed Template'delete'results))
data instance C.Parsed Template'delete'results
    = Template'delete'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Template'delete'results))
deriving instance (Std_.Eq (C.Parsed Template'delete'results))
instance (C.Parse Template'delete'results (C.Parsed Template'delete'results)) where
    parse raw_ = (Std_.pure Template'delete'results)
instance (C.Marshal Template'delete'results (C.Parsed Template'delete'results)) where
    marshalInto _raw (Template'delete'results) = (Std_.pure ())
data Template'instantiate'params 
type instance (R.ReprFor Template'instantiate'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Template'instantiate'params) where
    typeId  = 12030698055293344497
instance (C.TypedStruct Template'instantiate'params) where
    numStructWords  = 0
    numStructPtrs  = 2
instance (C.Allocate Template'instantiate'params) where
    type AllocHint Template'instantiate'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Template'instantiate'params (C.Parsed Template'instantiate'params))
instance (C.AllocateList Template'instantiate'params) where
    type ListAllocHint Template'instantiate'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Template'instantiate'params (C.Parsed Template'instantiate'params))
data instance C.Parsed Template'instantiate'params
    = Template'instantiate'params 
        {name :: (RP.Parsed Basics.Text)
        ,node :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Template'instantiate'params))
deriving instance (Std_.Eq (C.Parsed Template'instantiate'params))
instance (C.Parse Template'instantiate'params (C.Parsed Template'instantiate'params)) where
    parse raw_ = (Template'instantiate'params <$> (GH.parseField #name raw_)
                                              <*> (GH.parseField #node raw_))
instance (C.Marshal Template'instantiate'params (C.Parsed Template'instantiate'params)) where
    marshalInto raw_ Template'instantiate'params{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #node node raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot Template'instantiate'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "node" GH.Slot Template'instantiate'params Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 1)
data Template'instantiate'results 
type instance (R.ReprFor Template'instantiate'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Template'instantiate'results) where
    typeId  = 14143080381239508402
instance (C.TypedStruct Template'instantiate'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Template'instantiate'results) where
    type AllocHint Template'instantiate'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Template'instantiate'results (C.Parsed Template'instantiate'results))
instance (C.AllocateList Template'instantiate'results) where
    type ListAllocHint Template'instantiate'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Template'instantiate'results (C.Parsed Template'instantiate'results))
data instance C.Parsed Template'instantiate'results
    = Template'instantiate'results 
        {vm :: (RP.Parsed Capnp.Gen.ById.Xa7366eabdb0b1db4.Vm)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Template'instantiate'results))
deriving instance (Std_.Eq (C.Parsed Template'instantiate'results))
instance (C.Parse Template'instantiate'results (C.Parsed Template'instantiate'results)) where
    parse raw_ = (Template'instantiate'results <$> (GH.parseField #vm raw_))
instance (C.Marshal Template'instantiate'results (C.Parsed Template'instantiate'results)) where
    marshalInto raw_ Template'instantiate'results{..} = (do
        (GH.encodeField #vm vm raw_)
        (Std_.pure ())
        )
instance (GH.HasField "vm" GH.Slot Template'instantiate'results Capnp.Gen.ById.Xa7366eabdb0b1db4.Vm) where
    fieldByLabel  = (GH.ptrField 0)
data Template'update'params 
type instance (R.ReprFor Template'update'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Template'update'params) where
    typeId  = 13702316202849971487
instance (C.TypedStruct Template'update'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Template'update'params) where
    type AllocHint Template'update'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Template'update'params (C.Parsed Template'update'params))
instance (C.AllocateList Template'update'params) where
    type ListAllocHint Template'update'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Template'update'params (C.Parsed Template'update'params))
data instance C.Parsed Template'update'params
    = Template'update'params 
        {yaml :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Template'update'params))
deriving instance (Std_.Eq (C.Parsed Template'update'params))
instance (C.Parse Template'update'params (C.Parsed Template'update'params)) where
    parse raw_ = (Template'update'params <$> (GH.parseField #yaml raw_))
instance (C.Marshal Template'update'params (C.Parsed Template'update'params)) where
    marshalInto raw_ Template'update'params{..} = (do
        (GH.encodeField #yaml yaml raw_)
        (Std_.pure ())
        )
instance (GH.HasField "yaml" GH.Slot Template'update'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Template'update'results 
type instance (R.ReprFor Template'update'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Template'update'results) where
    typeId  = 9584083578250822571
instance (C.TypedStruct Template'update'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Template'update'results) where
    type AllocHint Template'update'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Template'update'results (C.Parsed Template'update'results))
instance (C.AllocateList Template'update'results) where
    type ListAllocHint Template'update'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Template'update'results (C.Parsed Template'update'results))
data instance C.Parsed Template'update'results
    = Template'update'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Template'update'results))
deriving instance (Std_.Eq (C.Parsed Template'update'results))
instance (C.Parse Template'update'results (C.Parsed Template'update'results)) where
    parse raw_ = (Std_.pure Template'update'results)
instance (C.Marshal Template'update'results (C.Parsed Template'update'results)) where
    marshalInto _raw (Template'update'results) = (Std_.pure ())