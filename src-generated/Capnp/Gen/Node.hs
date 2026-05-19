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
module Capnp.Gen.Node where
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
data NodeInfo 
type instance (R.ReprFor NodeInfo) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeInfo) where
    typeId  = 14898094878476756240
instance (C.TypedStruct NodeInfo) where
    numStructWords  = 10
    numStructPtrs  = 2
instance (C.Allocate NodeInfo) where
    type AllocHint NodeInfo = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeInfo (C.Parsed NodeInfo))
instance (C.AllocateList NodeInfo) where
    type ListAllocHint NodeInfo = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeInfo (C.Parsed NodeInfo))
data instance C.Parsed NodeInfo
    = NodeInfo 
        {id :: (RP.Parsed Std_.Int64)
        ,name :: (RP.Parsed Basics.Text)
        ,host :: (RP.Parsed Basics.Text)
        ,nodeAgentPort :: (RP.Parsed Std_.Int32)
        ,netAgentPort :: (RP.Parsed Std_.Int32)
        ,adminState :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.NodeAdminState)
        ,createdAt :: (RP.Parsed Std_.Int64)
        ,cpuCount :: (RP.Parsed Std_.Int32)
        ,ramMbTotal :: (RP.Parsed Std_.Int32)
        ,ramMbFree :: (RP.Parsed Std_.Int32)
        ,storageBytesTotal :: (RP.Parsed Std_.Int64)
        ,storageBytesFree :: (RP.Parsed Std_.Int64)
        ,loadAvg1 :: (RP.Parsed Std_.Double)
        ,lastNodeAgentPushAt :: (RP.Parsed Std_.Int64)
        ,lastNetAgentPushAt :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeInfo))
deriving instance (Std_.Eq (C.Parsed NodeInfo))
instance (C.Parse NodeInfo (C.Parsed NodeInfo)) where
    parse raw_ = (NodeInfo <$> (GH.parseField #id raw_)
                           <*> (GH.parseField #name raw_)
                           <*> (GH.parseField #host raw_)
                           <*> (GH.parseField #nodeAgentPort raw_)
                           <*> (GH.parseField #netAgentPort raw_)
                           <*> (GH.parseField #adminState raw_)
                           <*> (GH.parseField #createdAt raw_)
                           <*> (GH.parseField #cpuCount raw_)
                           <*> (GH.parseField #ramMbTotal raw_)
                           <*> (GH.parseField #ramMbFree raw_)
                           <*> (GH.parseField #storageBytesTotal raw_)
                           <*> (GH.parseField #storageBytesFree raw_)
                           <*> (GH.parseField #loadAvg1 raw_)
                           <*> (GH.parseField #lastNodeAgentPushAt raw_)
                           <*> (GH.parseField #lastNetAgentPushAt raw_))
instance (C.Marshal NodeInfo (C.Parsed NodeInfo)) where
    marshalInto raw_ NodeInfo{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #host host raw_)
        (GH.encodeField #nodeAgentPort nodeAgentPort raw_)
        (GH.encodeField #netAgentPort netAgentPort raw_)
        (GH.encodeField #adminState adminState raw_)
        (GH.encodeField #createdAt createdAt raw_)
        (GH.encodeField #cpuCount cpuCount raw_)
        (GH.encodeField #ramMbTotal ramMbTotal raw_)
        (GH.encodeField #ramMbFree ramMbFree raw_)
        (GH.encodeField #storageBytesTotal storageBytesTotal raw_)
        (GH.encodeField #storageBytesFree storageBytesFree raw_)
        (GH.encodeField #loadAvg1 loadAvg1 raw_)
        (GH.encodeField #lastNodeAgentPushAt lastNodeAgentPushAt raw_)
        (GH.encodeField #lastNetAgentPushAt lastNetAgentPushAt raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot NodeInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "name" GH.Slot NodeInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "host" GH.Slot NodeInfo Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "nodeAgentPort" GH.Slot NodeInfo Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
instance (GH.HasField "netAgentPort" GH.Slot NodeInfo Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 1 32 0)
instance (GH.HasField "adminState" GH.Slot NodeInfo Capnp.Gen.ById.Xbf9b09f64c0dd40d.NodeAdminState) where
    fieldByLabel  = (GH.dataField 0 2 16 0)
instance (GH.HasField "createdAt" GH.Slot NodeInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 3 64 0)
instance (GH.HasField "cpuCount" GH.Slot NodeInfo Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 2 32 0)
instance (GH.HasField "ramMbTotal" GH.Slot NodeInfo Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 4 32 0)
instance (GH.HasField "ramMbFree" GH.Slot NodeInfo Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 4 32 0)
instance (GH.HasField "storageBytesTotal" GH.Slot NodeInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 5 64 0)
instance (GH.HasField "storageBytesFree" GH.Slot NodeInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 6 64 0)
instance (GH.HasField "loadAvg1" GH.Slot NodeInfo Std_.Double) where
    fieldByLabel  = (GH.dataField 0 7 64 0)
instance (GH.HasField "lastNodeAgentPushAt" GH.Slot NodeInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 8 64 0)
instance (GH.HasField "lastNetAgentPushAt" GH.Slot NodeInfo Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 9 64 0)
data NodeDetails 
type instance (R.ReprFor NodeDetails) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeDetails) where
    typeId  = 16495589323068328717
instance (C.TypedStruct NodeDetails) where
    numStructWords  = 12
    numStructPtrs  = 6
instance (C.Allocate NodeDetails) where
    type AllocHint NodeDetails = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeDetails (C.Parsed NodeDetails))
instance (C.AllocateList NodeDetails) where
    type ListAllocHint NodeDetails = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeDetails (C.Parsed NodeDetails))
data instance C.Parsed NodeDetails
    = NodeDetails 
        {id :: (RP.Parsed Std_.Int64)
        ,name :: (RP.Parsed Basics.Text)
        ,host :: (RP.Parsed Basics.Text)
        ,nodeAgentPort :: (RP.Parsed Std_.Int32)
        ,netAgentPort :: (RP.Parsed Std_.Int32)
        ,basePath :: (RP.Parsed Basics.Text)
        ,description :: (RP.Parsed Basics.Text)
        ,adminState :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.NodeAdminState)
        ,createdAt :: (RP.Parsed Std_.Int64)
        ,cpuCount :: (RP.Parsed Std_.Int32)
        ,ramMbTotal :: (RP.Parsed Std_.Int32)
        ,ramMbFree :: (RP.Parsed Std_.Int32)
        ,storageBytesTotal :: (RP.Parsed Std_.Int64)
        ,storageBytesFree :: (RP.Parsed Std_.Int64)
        ,loadAvg1 :: (RP.Parsed Std_.Double)
        ,loadAvg5 :: (RP.Parsed Std_.Double)
        ,loadAvg15 :: (RP.Parsed Std_.Double)
        ,kernelRelease :: (RP.Parsed Basics.Text)
        ,agentVersion :: (RP.Parsed Basics.Text)
        ,lastNodeAgentPushAt :: (RP.Parsed Std_.Int64)
        ,lastNetAgentPushAt :: (RP.Parsed Std_.Int64)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeDetails))
deriving instance (Std_.Eq (C.Parsed NodeDetails))
instance (C.Parse NodeDetails (C.Parsed NodeDetails)) where
    parse raw_ = (NodeDetails <$> (GH.parseField #id raw_)
                              <*> (GH.parseField #name raw_)
                              <*> (GH.parseField #host raw_)
                              <*> (GH.parseField #nodeAgentPort raw_)
                              <*> (GH.parseField #netAgentPort raw_)
                              <*> (GH.parseField #basePath raw_)
                              <*> (GH.parseField #description raw_)
                              <*> (GH.parseField #adminState raw_)
                              <*> (GH.parseField #createdAt raw_)
                              <*> (GH.parseField #cpuCount raw_)
                              <*> (GH.parseField #ramMbTotal raw_)
                              <*> (GH.parseField #ramMbFree raw_)
                              <*> (GH.parseField #storageBytesTotal raw_)
                              <*> (GH.parseField #storageBytesFree raw_)
                              <*> (GH.parseField #loadAvg1 raw_)
                              <*> (GH.parseField #loadAvg5 raw_)
                              <*> (GH.parseField #loadAvg15 raw_)
                              <*> (GH.parseField #kernelRelease raw_)
                              <*> (GH.parseField #agentVersion raw_)
                              <*> (GH.parseField #lastNodeAgentPushAt raw_)
                              <*> (GH.parseField #lastNetAgentPushAt raw_))
instance (C.Marshal NodeDetails (C.Parsed NodeDetails)) where
    marshalInto raw_ NodeDetails{..} = (do
        (GH.encodeField #id id raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #host host raw_)
        (GH.encodeField #nodeAgentPort nodeAgentPort raw_)
        (GH.encodeField #netAgentPort netAgentPort raw_)
        (GH.encodeField #basePath basePath raw_)
        (GH.encodeField #description description raw_)
        (GH.encodeField #adminState adminState raw_)
        (GH.encodeField #createdAt createdAt raw_)
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
        (GH.encodeField #lastNodeAgentPushAt lastNodeAgentPushAt raw_)
        (GH.encodeField #lastNetAgentPushAt lastNetAgentPushAt raw_)
        (Std_.pure ())
        )
instance (GH.HasField "id" GH.Slot NodeDetails Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 0 64 0)
instance (GH.HasField "name" GH.Slot NodeDetails Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "host" GH.Slot NodeDetails Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "nodeAgentPort" GH.Slot NodeDetails Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
instance (GH.HasField "netAgentPort" GH.Slot NodeDetails Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 1 32 0)
instance (GH.HasField "basePath" GH.Slot NodeDetails Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "description" GH.Slot NodeDetails Basics.Text) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "adminState" GH.Slot NodeDetails Capnp.Gen.ById.Xbf9b09f64c0dd40d.NodeAdminState) where
    fieldByLabel  = (GH.dataField 0 2 16 0)
instance (GH.HasField "createdAt" GH.Slot NodeDetails Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 3 64 0)
instance (GH.HasField "cpuCount" GH.Slot NodeDetails Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 2 32 0)
instance (GH.HasField "ramMbTotal" GH.Slot NodeDetails Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 4 32 0)
instance (GH.HasField "ramMbFree" GH.Slot NodeDetails Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 4 32 0)
instance (GH.HasField "storageBytesTotal" GH.Slot NodeDetails Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 5 64 0)
instance (GH.HasField "storageBytesFree" GH.Slot NodeDetails Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 6 64 0)
instance (GH.HasField "loadAvg1" GH.Slot NodeDetails Std_.Double) where
    fieldByLabel  = (GH.dataField 0 7 64 0)
instance (GH.HasField "loadAvg5" GH.Slot NodeDetails Std_.Double) where
    fieldByLabel  = (GH.dataField 0 8 64 0)
instance (GH.HasField "loadAvg15" GH.Slot NodeDetails Std_.Double) where
    fieldByLabel  = (GH.dataField 0 9 64 0)
instance (GH.HasField "kernelRelease" GH.Slot NodeDetails Basics.Text) where
    fieldByLabel  = (GH.ptrField 4)
instance (GH.HasField "agentVersion" GH.Slot NodeDetails Basics.Text) where
    fieldByLabel  = (GH.ptrField 5)
instance (GH.HasField "lastNodeAgentPushAt" GH.Slot NodeDetails Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 10 64 0)
instance (GH.HasField "lastNetAgentPushAt" GH.Slot NodeDetails Std_.Int64) where
    fieldByLabel  = (GH.dataField 0 11 64 0)
data NodeAddParams 
type instance (R.ReprFor NodeAddParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeAddParams) where
    typeId  = 12932035708207979705
instance (C.TypedStruct NodeAddParams) where
    numStructWords  = 2
    numStructPtrs  = 4
instance (C.Allocate NodeAddParams) where
    type AllocHint NodeAddParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeAddParams (C.Parsed NodeAddParams))
instance (C.AllocateList NodeAddParams) where
    type ListAllocHint NodeAddParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeAddParams (C.Parsed NodeAddParams))
data instance C.Parsed NodeAddParams
    = NodeAddParams 
        {name :: (RP.Parsed Basics.Text)
        ,host :: (RP.Parsed Basics.Text)
        ,nodeAgentPort :: (RP.Parsed Std_.Int32)
        ,netAgentPort :: (RP.Parsed Std_.Int32)
        ,basePath :: (RP.Parsed Basics.Text)
        ,description :: (RP.Parsed Basics.Text)
        ,adminState :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.NodeAdminState)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeAddParams))
deriving instance (Std_.Eq (C.Parsed NodeAddParams))
instance (C.Parse NodeAddParams (C.Parsed NodeAddParams)) where
    parse raw_ = (NodeAddParams <$> (GH.parseField #name raw_)
                                <*> (GH.parseField #host raw_)
                                <*> (GH.parseField #nodeAgentPort raw_)
                                <*> (GH.parseField #netAgentPort raw_)
                                <*> (GH.parseField #basePath raw_)
                                <*> (GH.parseField #description raw_)
                                <*> (GH.parseField #adminState raw_))
instance (C.Marshal NodeAddParams (C.Parsed NodeAddParams)) where
    marshalInto raw_ NodeAddParams{..} = (do
        (GH.encodeField #name name raw_)
        (GH.encodeField #host host raw_)
        (GH.encodeField #nodeAgentPort nodeAgentPort raw_)
        (GH.encodeField #netAgentPort netAgentPort raw_)
        (GH.encodeField #basePath basePath raw_)
        (GH.encodeField #description description raw_)
        (GH.encodeField #adminState adminState raw_)
        (Std_.pure ())
        )
instance (GH.HasField "name" GH.Slot NodeAddParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "host" GH.Slot NodeAddParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "nodeAgentPort" GH.Slot NodeAddParams Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 0 32 9878)
instance (GH.HasField "netAgentPort" GH.Slot NodeAddParams Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 0 32 9877)
instance (GH.HasField "basePath" GH.Slot NodeAddParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "description" GH.Slot NodeAddParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "adminState" GH.Slot NodeAddParams Capnp.Gen.ById.Xbf9b09f64c0dd40d.NodeAdminState) where
    fieldByLabel  = (GH.dataField 0 1 16 0)
data NodeEditParams 
type instance (R.ReprFor NodeEditParams) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeEditParams) where
    typeId  = 17834580106764019165
instance (C.TypedStruct NodeEditParams) where
    numStructWords  = 2
    numStructPtrs  = 4
instance (C.Allocate NodeEditParams) where
    type AllocHint NodeEditParams = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeEditParams (C.Parsed NodeEditParams))
instance (C.AllocateList NodeEditParams) where
    type ListAllocHint NodeEditParams = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeEditParams (C.Parsed NodeEditParams))
data instance C.Parsed NodeEditParams
    = NodeEditParams 
        {hasName :: (RP.Parsed Std_.Bool)
        ,name :: (RP.Parsed Basics.Text)
        ,hasHost :: (RP.Parsed Std_.Bool)
        ,host :: (RP.Parsed Basics.Text)
        ,hasNodeAgentPort :: (RP.Parsed Std_.Bool)
        ,nodeAgentPort :: (RP.Parsed Std_.Int32)
        ,hasNetAgentPort :: (RP.Parsed Std_.Bool)
        ,netAgentPort :: (RP.Parsed Std_.Int32)
        ,hasBasePath :: (RP.Parsed Std_.Bool)
        ,basePath :: (RP.Parsed Basics.Text)
        ,hasDescription :: (RP.Parsed Std_.Bool)
        ,description :: (RP.Parsed Basics.Text)
        ,hasAdminState :: (RP.Parsed Std_.Bool)
        ,adminState :: (RP.Parsed Capnp.Gen.ById.Xbf9b09f64c0dd40d.NodeAdminState)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeEditParams))
deriving instance (Std_.Eq (C.Parsed NodeEditParams))
instance (C.Parse NodeEditParams (C.Parsed NodeEditParams)) where
    parse raw_ = (NodeEditParams <$> (GH.parseField #hasName raw_)
                                 <*> (GH.parseField #name raw_)
                                 <*> (GH.parseField #hasHost raw_)
                                 <*> (GH.parseField #host raw_)
                                 <*> (GH.parseField #hasNodeAgentPort raw_)
                                 <*> (GH.parseField #nodeAgentPort raw_)
                                 <*> (GH.parseField #hasNetAgentPort raw_)
                                 <*> (GH.parseField #netAgentPort raw_)
                                 <*> (GH.parseField #hasBasePath raw_)
                                 <*> (GH.parseField #basePath raw_)
                                 <*> (GH.parseField #hasDescription raw_)
                                 <*> (GH.parseField #description raw_)
                                 <*> (GH.parseField #hasAdminState raw_)
                                 <*> (GH.parseField #adminState raw_))
instance (C.Marshal NodeEditParams (C.Parsed NodeEditParams)) where
    marshalInto raw_ NodeEditParams{..} = (do
        (GH.encodeField #hasName hasName raw_)
        (GH.encodeField #name name raw_)
        (GH.encodeField #hasHost hasHost raw_)
        (GH.encodeField #host host raw_)
        (GH.encodeField #hasNodeAgentPort hasNodeAgentPort raw_)
        (GH.encodeField #nodeAgentPort nodeAgentPort raw_)
        (GH.encodeField #hasNetAgentPort hasNetAgentPort raw_)
        (GH.encodeField #netAgentPort netAgentPort raw_)
        (GH.encodeField #hasBasePath hasBasePath raw_)
        (GH.encodeField #basePath basePath raw_)
        (GH.encodeField #hasDescription hasDescription raw_)
        (GH.encodeField #description description raw_)
        (GH.encodeField #hasAdminState hasAdminState raw_)
        (GH.encodeField #adminState adminState raw_)
        (Std_.pure ())
        )
instance (GH.HasField "hasName" GH.Slot NodeEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 0 0 1 0)
instance (GH.HasField "name" GH.Slot NodeEditParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
instance (GH.HasField "hasHost" GH.Slot NodeEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 1 0 1 0)
instance (GH.HasField "host" GH.Slot NodeEditParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 1)
instance (GH.HasField "hasNodeAgentPort" GH.Slot NodeEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 2 0 1 0)
instance (GH.HasField "nodeAgentPort" GH.Slot NodeEditParams Std_.Int32) where
    fieldByLabel  = (GH.dataField 32 0 32 0)
instance (GH.HasField "hasNetAgentPort" GH.Slot NodeEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 3 0 1 0)
instance (GH.HasField "netAgentPort" GH.Slot NodeEditParams Std_.Int32) where
    fieldByLabel  = (GH.dataField 0 1 32 0)
instance (GH.HasField "hasBasePath" GH.Slot NodeEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 4 0 1 0)
instance (GH.HasField "basePath" GH.Slot NodeEditParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 2)
instance (GH.HasField "hasDescription" GH.Slot NodeEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 5 0 1 0)
instance (GH.HasField "description" GH.Slot NodeEditParams Basics.Text) where
    fieldByLabel  = (GH.ptrField 3)
instance (GH.HasField "hasAdminState" GH.Slot NodeEditParams Std_.Bool) where
    fieldByLabel  = (GH.dataField 6 0 1 0)
instance (GH.HasField "adminState" GH.Slot NodeEditParams Capnp.Gen.ById.Xbf9b09f64c0dd40d.NodeAdminState) where
    fieldByLabel  = (GH.dataField 16 0 16 0)
data NodeManager 
type instance (R.ReprFor NodeManager) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId NodeManager) where
    typeId  = 12475330351773991822
instance (C.Parse NodeManager (GH.Client NodeManager)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export NodeManager) where
    type Server NodeManager = NodeManager'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(NodeManager)) [(GH.toUntypedMethodHandler ((nodeManager'list) s_))
                                                                             ,(GH.toUntypedMethodHandler ((nodeManager'get) s_))
                                                                             ,(GH.toUntypedMethodHandler ((nodeManager'create) s_))] [])
class (NodeManager'server_ s_) where
    {-# MINIMAL nodeManager'list,nodeManager'get,nodeManager'create #-}
    nodeManager'list :: s_ -> (GH.MethodHandler NodeManager'list'params NodeManager'list'results)
    nodeManager'list _ = GH.methodUnimplemented
    nodeManager'get :: s_ -> (GH.MethodHandler NodeManager'get'params NodeManager'get'results)
    nodeManager'get _ = GH.methodUnimplemented
    nodeManager'create :: s_ -> (GH.MethodHandler NodeManager'create'params NodeManager'create'results)
    nodeManager'create _ = GH.methodUnimplemented
instance (GH.HasMethod "list" NodeManager NodeManager'list'params NodeManager'list'results) where
    methodByLabel  = (GH.Method 12475330351773991822 0)
instance (GH.HasMethod "get" NodeManager NodeManager'get'params NodeManager'get'results) where
    methodByLabel  = (GH.Method 12475330351773991822 1)
instance (GH.HasMethod "create" NodeManager NodeManager'create'params NodeManager'create'results) where
    methodByLabel  = (GH.Method 12475330351773991822 2)
data NodeManager'list'params 
type instance (R.ReprFor NodeManager'list'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeManager'list'params) where
    typeId  = 16215172202660944918
instance (C.TypedStruct NodeManager'list'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate NodeManager'list'params) where
    type AllocHint NodeManager'list'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeManager'list'params (C.Parsed NodeManager'list'params))
instance (C.AllocateList NodeManager'list'params) where
    type ListAllocHint NodeManager'list'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeManager'list'params (C.Parsed NodeManager'list'params))
data instance C.Parsed NodeManager'list'params
    = NodeManager'list'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeManager'list'params))
deriving instance (Std_.Eq (C.Parsed NodeManager'list'params))
instance (C.Parse NodeManager'list'params (C.Parsed NodeManager'list'params)) where
    parse raw_ = (Std_.pure NodeManager'list'params)
instance (C.Marshal NodeManager'list'params (C.Parsed NodeManager'list'params)) where
    marshalInto _raw (NodeManager'list'params) = (Std_.pure ())
data NodeManager'list'results 
type instance (R.ReprFor NodeManager'list'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeManager'list'results) where
    typeId  = 17640186820935550825
instance (C.TypedStruct NodeManager'list'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NodeManager'list'results) where
    type AllocHint NodeManager'list'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeManager'list'results (C.Parsed NodeManager'list'results))
instance (C.AllocateList NodeManager'list'results) where
    type ListAllocHint NodeManager'list'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeManager'list'results (C.Parsed NodeManager'list'results))
data instance C.Parsed NodeManager'list'results
    = NodeManager'list'results 
        {nodes :: (RP.Parsed (R.List NodeInfo))}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeManager'list'results))
deriving instance (Std_.Eq (C.Parsed NodeManager'list'results))
instance (C.Parse NodeManager'list'results (C.Parsed NodeManager'list'results)) where
    parse raw_ = (NodeManager'list'results <$> (GH.parseField #nodes raw_))
instance (C.Marshal NodeManager'list'results (C.Parsed NodeManager'list'results)) where
    marshalInto raw_ NodeManager'list'results{..} = (do
        (GH.encodeField #nodes nodes raw_)
        (Std_.pure ())
        )
instance (GH.HasField "nodes" GH.Slot NodeManager'list'results (R.List NodeInfo)) where
    fieldByLabel  = (GH.ptrField 0)
data NodeManager'get'params 
type instance (R.ReprFor NodeManager'get'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeManager'get'params) where
    typeId  = 15146525871799667536
instance (C.TypedStruct NodeManager'get'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NodeManager'get'params) where
    type AllocHint NodeManager'get'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeManager'get'params (C.Parsed NodeManager'get'params))
instance (C.AllocateList NodeManager'get'params) where
    type ListAllocHint NodeManager'get'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeManager'get'params (C.Parsed NodeManager'get'params))
data instance C.Parsed NodeManager'get'params
    = NodeManager'get'params 
        {ref :: (RP.Parsed Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeManager'get'params))
deriving instance (Std_.Eq (C.Parsed NodeManager'get'params))
instance (C.Parse NodeManager'get'params (C.Parsed NodeManager'get'params)) where
    parse raw_ = (NodeManager'get'params <$> (GH.parseField #ref raw_))
instance (C.Marshal NodeManager'get'params (C.Parsed NodeManager'get'params)) where
    marshalInto raw_ NodeManager'get'params{..} = (do
        (GH.encodeField #ref ref raw_)
        (Std_.pure ())
        )
instance (GH.HasField "ref" GH.Slot NodeManager'get'params Capnp.Gen.ById.X9b1373e2334a09e9.EntityRef) where
    fieldByLabel  = (GH.ptrField 0)
data NodeManager'get'results 
type instance (R.ReprFor NodeManager'get'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeManager'get'results) where
    typeId  = 14877101391832682342
instance (C.TypedStruct NodeManager'get'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NodeManager'get'results) where
    type AllocHint NodeManager'get'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeManager'get'results (C.Parsed NodeManager'get'results))
instance (C.AllocateList NodeManager'get'results) where
    type ListAllocHint NodeManager'get'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeManager'get'results (C.Parsed NodeManager'get'results))
data instance C.Parsed NodeManager'get'results
    = NodeManager'get'results 
        {node :: (RP.Parsed Node)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeManager'get'results))
deriving instance (Std_.Eq (C.Parsed NodeManager'get'results))
instance (C.Parse NodeManager'get'results (C.Parsed NodeManager'get'results)) where
    parse raw_ = (NodeManager'get'results <$> (GH.parseField #node raw_))
instance (C.Marshal NodeManager'get'results (C.Parsed NodeManager'get'results)) where
    marshalInto raw_ NodeManager'get'results{..} = (do
        (GH.encodeField #node node raw_)
        (Std_.pure ())
        )
instance (GH.HasField "node" GH.Slot NodeManager'get'results Node) where
    fieldByLabel  = (GH.ptrField 0)
data NodeManager'create'params 
type instance (R.ReprFor NodeManager'create'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeManager'create'params) where
    typeId  = 10454743362237887224
instance (C.TypedStruct NodeManager'create'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NodeManager'create'params) where
    type AllocHint NodeManager'create'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeManager'create'params (C.Parsed NodeManager'create'params))
instance (C.AllocateList NodeManager'create'params) where
    type ListAllocHint NodeManager'create'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeManager'create'params (C.Parsed NodeManager'create'params))
data instance C.Parsed NodeManager'create'params
    = NodeManager'create'params 
        {params :: (RP.Parsed NodeAddParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeManager'create'params))
deriving instance (Std_.Eq (C.Parsed NodeManager'create'params))
instance (C.Parse NodeManager'create'params (C.Parsed NodeManager'create'params)) where
    parse raw_ = (NodeManager'create'params <$> (GH.parseField #params raw_))
instance (C.Marshal NodeManager'create'params (C.Parsed NodeManager'create'params)) where
    marshalInto raw_ NodeManager'create'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot NodeManager'create'params NodeAddParams) where
    fieldByLabel  = (GH.ptrField 0)
data NodeManager'create'results 
type instance (R.ReprFor NodeManager'create'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId NodeManager'create'results) where
    typeId  = 15229036818325531431
instance (C.TypedStruct NodeManager'create'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate NodeManager'create'results) where
    type AllocHint NodeManager'create'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc NodeManager'create'results (C.Parsed NodeManager'create'results))
instance (C.AllocateList NodeManager'create'results) where
    type ListAllocHint NodeManager'create'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc NodeManager'create'results (C.Parsed NodeManager'create'results))
data instance C.Parsed NodeManager'create'results
    = NodeManager'create'results 
        {node :: (RP.Parsed Node)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed NodeManager'create'results))
deriving instance (Std_.Eq (C.Parsed NodeManager'create'results))
instance (C.Parse NodeManager'create'results (C.Parsed NodeManager'create'results)) where
    parse raw_ = (NodeManager'create'results <$> (GH.parseField #node raw_))
instance (C.Marshal NodeManager'create'results (C.Parsed NodeManager'create'results)) where
    marshalInto raw_ NodeManager'create'results{..} = (do
        (GH.encodeField #node node raw_)
        (Std_.pure ())
        )
instance (GH.HasField "node" GH.Slot NodeManager'create'results Node) where
    fieldByLabel  = (GH.ptrField 0)
data Node 
type instance (R.ReprFor Node) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Node) where
    typeId  = 9857192839459206243
instance (C.Parse Node (GH.Client Node)) where
    parse  = GH.parseCap
    encode  = GH.encodeCap
instance (GH.Export Node) where
    type Server Node = Node'server_
    methodHandlerTree _ s_ = (GH.MethodHandlerTree (C.typeId @(Node)) [(GH.toUntypedMethodHandler ((node'show) s_))
                                                                      ,(GH.toUntypedMethodHandler ((node'edit) s_))
                                                                      ,(GH.toUntypedMethodHandler ((node'drain) s_))
                                                                      ,(GH.toUntypedMethodHandler ((node'delete) s_))] [])
class (Node'server_ s_) where
    {-# MINIMAL node'show,node'edit,node'drain,node'delete #-}
    node'show :: s_ -> (GH.MethodHandler Node'show'params Node'show'results)
    node'show _ = GH.methodUnimplemented
    node'edit :: s_ -> (GH.MethodHandler Node'edit'params Node'edit'results)
    node'edit _ = GH.methodUnimplemented
    node'drain :: s_ -> (GH.MethodHandler Node'drain'params Node'drain'results)
    node'drain _ = GH.methodUnimplemented
    node'delete :: s_ -> (GH.MethodHandler Node'delete'params Node'delete'results)
    node'delete _ = GH.methodUnimplemented
instance (GH.HasMethod "show" Node Node'show'params Node'show'results) where
    methodByLabel  = (GH.Method 9857192839459206243 0)
instance (GH.HasMethod "edit" Node Node'edit'params Node'edit'results) where
    methodByLabel  = (GH.Method 9857192839459206243 1)
instance (GH.HasMethod "drain" Node Node'drain'params Node'drain'results) where
    methodByLabel  = (GH.Method 9857192839459206243 2)
instance (GH.HasMethod "delete" Node Node'delete'params Node'delete'results) where
    methodByLabel  = (GH.Method 9857192839459206243 3)
data Node'show'params 
type instance (R.ReprFor Node'show'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'show'params) where
    typeId  = 11634045753821670599
instance (C.TypedStruct Node'show'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Node'show'params) where
    type AllocHint Node'show'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'show'params (C.Parsed Node'show'params))
instance (C.AllocateList Node'show'params) where
    type ListAllocHint Node'show'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'show'params (C.Parsed Node'show'params))
data instance C.Parsed Node'show'params
    = Node'show'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'show'params))
deriving instance (Std_.Eq (C.Parsed Node'show'params))
instance (C.Parse Node'show'params (C.Parsed Node'show'params)) where
    parse raw_ = (Std_.pure Node'show'params)
instance (C.Marshal Node'show'params (C.Parsed Node'show'params)) where
    marshalInto _raw (Node'show'params) = (Std_.pure ())
data Node'show'results 
type instance (R.ReprFor Node'show'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'show'results) where
    typeId  = 17486611529788549725
instance (C.TypedStruct Node'show'results) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Node'show'results) where
    type AllocHint Node'show'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'show'results (C.Parsed Node'show'results))
instance (C.AllocateList Node'show'results) where
    type ListAllocHint Node'show'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'show'results (C.Parsed Node'show'results))
data instance C.Parsed Node'show'results
    = Node'show'results 
        {details :: (RP.Parsed NodeDetails)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'show'results))
deriving instance (Std_.Eq (C.Parsed Node'show'results))
instance (C.Parse Node'show'results (C.Parsed Node'show'results)) where
    parse raw_ = (Node'show'results <$> (GH.parseField #details raw_))
instance (C.Marshal Node'show'results (C.Parsed Node'show'results)) where
    marshalInto raw_ Node'show'results{..} = (do
        (GH.encodeField #details details raw_)
        (Std_.pure ())
        )
instance (GH.HasField "details" GH.Slot Node'show'results NodeDetails) where
    fieldByLabel  = (GH.ptrField 0)
data Node'edit'params 
type instance (R.ReprFor Node'edit'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'edit'params) where
    typeId  = 9621874160681602820
instance (C.TypedStruct Node'edit'params) where
    numStructWords  = 0
    numStructPtrs  = 1
instance (C.Allocate Node'edit'params) where
    type AllocHint Node'edit'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'edit'params (C.Parsed Node'edit'params))
instance (C.AllocateList Node'edit'params) where
    type ListAllocHint Node'edit'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'edit'params (C.Parsed Node'edit'params))
data instance C.Parsed Node'edit'params
    = Node'edit'params 
        {params :: (RP.Parsed NodeEditParams)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'edit'params))
deriving instance (Std_.Eq (C.Parsed Node'edit'params))
instance (C.Parse Node'edit'params (C.Parsed Node'edit'params)) where
    parse raw_ = (Node'edit'params <$> (GH.parseField #params raw_))
instance (C.Marshal Node'edit'params (C.Parsed Node'edit'params)) where
    marshalInto raw_ Node'edit'params{..} = (do
        (GH.encodeField #params params raw_)
        (Std_.pure ())
        )
instance (GH.HasField "params" GH.Slot Node'edit'params NodeEditParams) where
    fieldByLabel  = (GH.ptrField 0)
data Node'edit'results 
type instance (R.ReprFor Node'edit'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'edit'results) where
    typeId  = 10614928140000642263
instance (C.TypedStruct Node'edit'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Node'edit'results) where
    type AllocHint Node'edit'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'edit'results (C.Parsed Node'edit'results))
instance (C.AllocateList Node'edit'results) where
    type ListAllocHint Node'edit'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'edit'results (C.Parsed Node'edit'results))
data instance C.Parsed Node'edit'results
    = Node'edit'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'edit'results))
deriving instance (Std_.Eq (C.Parsed Node'edit'results))
instance (C.Parse Node'edit'results (C.Parsed Node'edit'results)) where
    parse raw_ = (Std_.pure Node'edit'results)
instance (C.Marshal Node'edit'results (C.Parsed Node'edit'results)) where
    marshalInto _raw (Node'edit'results) = (Std_.pure ())
data Node'drain'params 
type instance (R.ReprFor Node'drain'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'drain'params) where
    typeId  = 13753470049033962609
instance (C.TypedStruct Node'drain'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Node'drain'params) where
    type AllocHint Node'drain'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'drain'params (C.Parsed Node'drain'params))
instance (C.AllocateList Node'drain'params) where
    type ListAllocHint Node'drain'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'drain'params (C.Parsed Node'drain'params))
data instance C.Parsed Node'drain'params
    = Node'drain'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'drain'params))
deriving instance (Std_.Eq (C.Parsed Node'drain'params))
instance (C.Parse Node'drain'params (C.Parsed Node'drain'params)) where
    parse raw_ = (Std_.pure Node'drain'params)
instance (C.Marshal Node'drain'params (C.Parsed Node'drain'params)) where
    marshalInto _raw (Node'drain'params) = (Std_.pure ())
data Node'drain'results 
type instance (R.ReprFor Node'drain'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'drain'results) where
    typeId  = 9681010458543830079
instance (C.TypedStruct Node'drain'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Node'drain'results) where
    type AllocHint Node'drain'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'drain'results (C.Parsed Node'drain'results))
instance (C.AllocateList Node'drain'results) where
    type ListAllocHint Node'drain'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'drain'results (C.Parsed Node'drain'results))
data instance C.Parsed Node'drain'results
    = Node'drain'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'drain'results))
deriving instance (Std_.Eq (C.Parsed Node'drain'results))
instance (C.Parse Node'drain'results (C.Parsed Node'drain'results)) where
    parse raw_ = (Std_.pure Node'drain'results)
instance (C.Marshal Node'drain'results (C.Parsed Node'drain'results)) where
    marshalInto _raw (Node'drain'results) = (Std_.pure ())
data Node'delete'params 
type instance (R.ReprFor Node'delete'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'delete'params) where
    typeId  = 11348307210076776614
instance (C.TypedStruct Node'delete'params) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Node'delete'params) where
    type AllocHint Node'delete'params = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'delete'params (C.Parsed Node'delete'params))
instance (C.AllocateList Node'delete'params) where
    type ListAllocHint Node'delete'params = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'delete'params (C.Parsed Node'delete'params))
data instance C.Parsed Node'delete'params
    = Node'delete'params 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'delete'params))
deriving instance (Std_.Eq (C.Parsed Node'delete'params))
instance (C.Parse Node'delete'params (C.Parsed Node'delete'params)) where
    parse raw_ = (Std_.pure Node'delete'params)
instance (C.Marshal Node'delete'params (C.Parsed Node'delete'params)) where
    marshalInto _raw (Node'delete'params) = (Std_.pure ())
data Node'delete'results 
type instance (R.ReprFor Node'delete'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Node'delete'results) where
    typeId  = 13098022870608149411
instance (C.TypedStruct Node'delete'results) where
    numStructWords  = 0
    numStructPtrs  = 0
instance (C.Allocate Node'delete'results) where
    type AllocHint Node'delete'results = ()
    new _ = C.newTypedStruct
instance (C.EstimateAlloc Node'delete'results (C.Parsed Node'delete'results))
instance (C.AllocateList Node'delete'results) where
    type ListAllocHint Node'delete'results = Std_.Int
    newList  = C.newTypedStructList
instance (C.EstimateListAlloc Node'delete'results (C.Parsed Node'delete'results))
data instance C.Parsed Node'delete'results
    = Node'delete'results 
        {}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Node'delete'results))
deriving instance (Std_.Eq (C.Parsed Node'delete'results))
instance (C.Parse Node'delete'results (C.Parsed Node'delete'results)) where
    parse raw_ = (Std_.pure Node'delete'results)
instance (C.Marshal Node'delete'results (C.Parsed Node'delete'results)) where
    marshalInto _raw (Node'delete'results) = (Std_.pure ())