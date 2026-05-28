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
module Capnp.Gen.Enums where
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
data VmStatus 
    = VmStatus'stopped 
    | VmStatus'starting 
    | VmStatus'running 
    | VmStatus'stopping 
    | VmStatus'paused 
    | VmStatus'error 
    | VmStatus'saved 
    | VmStatus'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor VmStatus) = (R.Data R.Sz16)
instance (C.HasTypeId VmStatus) where
    typeId  = 16042181794392547501
instance (Std_.Enum VmStatus) where
    toEnum n_ = case n_ of
        0 ->
            VmStatus'stopped
        1 ->
            VmStatus'starting
        2 ->
            VmStatus'running
        3 ->
            VmStatus'stopping
        4 ->
            VmStatus'paused
        5 ->
            VmStatus'error
        6 ->
            VmStatus'saved
        tag_ ->
            (VmStatus'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (VmStatus'stopped) ->
            0
        (VmStatus'starting) ->
            1
        (VmStatus'running) ->
            2
        (VmStatus'stopping) ->
            3
        (VmStatus'paused) ->
            4
        (VmStatus'error) ->
            5
        (VmStatus'saved) ->
            6
        (VmStatus'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord VmStatus) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse VmStatus VmStatus) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList VmStatus) where
    type ListAllocHint VmStatus = Std_.Int
instance (C.EstimateListAlloc VmStatus VmStatus)
data DriveInterface 
    = DriveInterface'virtio 
    | DriveInterface'ide 
    | DriveInterface'scsi 
    | DriveInterface'sata 
    | DriveInterface'nvme 
    | DriveInterface'pflash 
    | DriveInterface'floppy 
    | DriveInterface'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor DriveInterface) = (R.Data R.Sz16)
instance (C.HasTypeId DriveInterface) where
    typeId  = 12975139502622129028
instance (Std_.Enum DriveInterface) where
    toEnum n_ = case n_ of
        0 ->
            DriveInterface'virtio
        1 ->
            DriveInterface'ide
        2 ->
            DriveInterface'scsi
        3 ->
            DriveInterface'sata
        4 ->
            DriveInterface'nvme
        5 ->
            DriveInterface'pflash
        6 ->
            DriveInterface'floppy
        tag_ ->
            (DriveInterface'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (DriveInterface'virtio) ->
            0
        (DriveInterface'ide) ->
            1
        (DriveInterface'scsi) ->
            2
        (DriveInterface'sata) ->
            3
        (DriveInterface'nvme) ->
            4
        (DriveInterface'pflash) ->
            5
        (DriveInterface'floppy) ->
            6
        (DriveInterface'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord DriveInterface) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse DriveInterface DriveInterface) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList DriveInterface) where
    type ListAllocHint DriveInterface = Std_.Int
instance (C.EstimateListAlloc DriveInterface DriveInterface)
data DriveFormat 
    = DriveFormat'qcow2 
    | DriveFormat'raw 
    | DriveFormat'vmdk 
    | DriveFormat'vdi 
    | DriveFormat'vpc 
    | DriveFormat'vhdx 
    | DriveFormat'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor DriveFormat) = (R.Data R.Sz16)
instance (C.HasTypeId DriveFormat) where
    typeId  = 18085981199538007448
instance (Std_.Enum DriveFormat) where
    toEnum n_ = case n_ of
        0 ->
            DriveFormat'qcow2
        1 ->
            DriveFormat'raw
        2 ->
            DriveFormat'vmdk
        3 ->
            DriveFormat'vdi
        4 ->
            DriveFormat'vpc
        5 ->
            DriveFormat'vhdx
        tag_ ->
            (DriveFormat'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (DriveFormat'qcow2) ->
            0
        (DriveFormat'raw) ->
            1
        (DriveFormat'vmdk) ->
            2
        (DriveFormat'vdi) ->
            3
        (DriveFormat'vpc) ->
            4
        (DriveFormat'vhdx) ->
            5
        (DriveFormat'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord DriveFormat) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse DriveFormat DriveFormat) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList DriveFormat) where
    type ListAllocHint DriveFormat = Std_.Int
instance (C.EstimateListAlloc DriveFormat DriveFormat)
data DriveMedia 
    = DriveMedia'disk 
    | DriveMedia'cdrom 
    | DriveMedia'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor DriveMedia) = (R.Data R.Sz16)
instance (C.HasTypeId DriveMedia) where
    typeId  = 9874747946360888652
instance (Std_.Enum DriveMedia) where
    toEnum n_ = case n_ of
        0 ->
            DriveMedia'disk
        1 ->
            DriveMedia'cdrom
        tag_ ->
            (DriveMedia'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (DriveMedia'disk) ->
            0
        (DriveMedia'cdrom) ->
            1
        (DriveMedia'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord DriveMedia) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse DriveMedia DriveMedia) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList DriveMedia) where
    type ListAllocHint DriveMedia = Std_.Int
instance (C.EstimateListAlloc DriveMedia DriveMedia)
data CacheType 
    = CacheType'none 
    | CacheType'writeback 
    | CacheType'writethrough 
    | CacheType'directsync 
    | CacheType'unsafe 
    | CacheType'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor CacheType) = (R.Data R.Sz16)
instance (C.HasTypeId CacheType) where
    typeId  = 16966865199020515970
instance (Std_.Enum CacheType) where
    toEnum n_ = case n_ of
        0 ->
            CacheType'none
        1 ->
            CacheType'writeback
        2 ->
            CacheType'writethrough
        3 ->
            CacheType'directsync
        4 ->
            CacheType'unsafe
        tag_ ->
            (CacheType'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (CacheType'none) ->
            0
        (CacheType'writeback) ->
            1
        (CacheType'writethrough) ->
            2
        (CacheType'directsync) ->
            3
        (CacheType'unsafe) ->
            4
        (CacheType'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord CacheType) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse CacheType CacheType) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList CacheType) where
    type ListAllocHint CacheType = Std_.Int
instance (C.EstimateListAlloc CacheType CacheType)
data NetInterfaceType 
    = NetInterfaceType'user 
    | NetInterfaceType'tap 
    | NetInterfaceType'bridge 
    | NetInterfaceType'macvtap 
    | NetInterfaceType'vde 
    | NetInterfaceType'managed 
    | NetInterfaceType'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor NetInterfaceType) = (R.Data R.Sz16)
instance (C.HasTypeId NetInterfaceType) where
    typeId  = 12438602364159069701
instance (Std_.Enum NetInterfaceType) where
    toEnum n_ = case n_ of
        0 ->
            NetInterfaceType'user
        1 ->
            NetInterfaceType'tap
        2 ->
            NetInterfaceType'bridge
        3 ->
            NetInterfaceType'macvtap
        4 ->
            NetInterfaceType'vde
        5 ->
            NetInterfaceType'managed
        tag_ ->
            (NetInterfaceType'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (NetInterfaceType'user) ->
            0
        (NetInterfaceType'tap) ->
            1
        (NetInterfaceType'bridge) ->
            2
        (NetInterfaceType'macvtap) ->
            3
        (NetInterfaceType'vde) ->
            4
        (NetInterfaceType'managed) ->
            5
        (NetInterfaceType'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord NetInterfaceType) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse NetInterfaceType NetInterfaceType) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList NetInterfaceType) where
    type ListAllocHint NetInterfaceType = Std_.Int
instance (C.EstimateListAlloc NetInterfaceType NetInterfaceType)
data SharedDirCache 
    = SharedDirCache'always 
    | SharedDirCache'auto 
    | SharedDirCache'never 
    | SharedDirCache'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor SharedDirCache) = (R.Data R.Sz16)
instance (C.HasTypeId SharedDirCache) where
    typeId  = 11716759257366294022
instance (Std_.Enum SharedDirCache) where
    toEnum n_ = case n_ of
        0 ->
            SharedDirCache'always
        1 ->
            SharedDirCache'auto
        2 ->
            SharedDirCache'never
        tag_ ->
            (SharedDirCache'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (SharedDirCache'always) ->
            0
        (SharedDirCache'auto) ->
            1
        (SharedDirCache'never) ->
            2
        (SharedDirCache'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord SharedDirCache) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse SharedDirCache SharedDirCache) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList SharedDirCache) where
    type ListAllocHint SharedDirCache = Std_.Int
instance (C.EstimateListAlloc SharedDirCache SharedDirCache)
data TemplateCloneStrategy 
    = TemplateCloneStrategy'clone 
    | TemplateCloneStrategy'overlay 
    | TemplateCloneStrategy'direct 
    | TemplateCloneStrategy'create 
    | TemplateCloneStrategy'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor TemplateCloneStrategy) = (R.Data R.Sz16)
instance (C.HasTypeId TemplateCloneStrategy) where
    typeId  = 12806442252199992262
instance (Std_.Enum TemplateCloneStrategy) where
    toEnum n_ = case n_ of
        0 ->
            TemplateCloneStrategy'clone
        1 ->
            TemplateCloneStrategy'overlay
        2 ->
            TemplateCloneStrategy'direct
        3 ->
            TemplateCloneStrategy'create
        tag_ ->
            (TemplateCloneStrategy'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (TemplateCloneStrategy'clone) ->
            0
        (TemplateCloneStrategy'overlay) ->
            1
        (TemplateCloneStrategy'direct) ->
            2
        (TemplateCloneStrategy'create) ->
            3
        (TemplateCloneStrategy'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord TemplateCloneStrategy) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse TemplateCloneStrategy TemplateCloneStrategy) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList TemplateCloneStrategy) where
    type ListAllocHint TemplateCloneStrategy = Std_.Int
instance (C.EstimateListAlloc TemplateCloneStrategy TemplateCloneStrategy)
data TaskSubsystem 
    = TaskSubsystem'vm 
    | TaskSubsystem'disk 
    | TaskSubsystem'network 
    | TaskSubsystem'sshKey 
    | TaskSubsystem'template 
    | TaskSubsystem'sharedDir 
    | TaskSubsystem'snapshot 
    | TaskSubsystem'system 
    | TaskSubsystem'apply 
    | TaskSubsystem'build 
    | TaskSubsystem'node 
    | TaskSubsystem'migration 
    | TaskSubsystem'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor TaskSubsystem) = (R.Data R.Sz16)
instance (C.HasTypeId TaskSubsystem) where
    typeId  = 11109553244217234025
instance (Std_.Enum TaskSubsystem) where
    toEnum n_ = case n_ of
        0 ->
            TaskSubsystem'vm
        1 ->
            TaskSubsystem'disk
        2 ->
            TaskSubsystem'network
        3 ->
            TaskSubsystem'sshKey
        4 ->
            TaskSubsystem'template
        5 ->
            TaskSubsystem'sharedDir
        6 ->
            TaskSubsystem'snapshot
        7 ->
            TaskSubsystem'system
        8 ->
            TaskSubsystem'apply
        9 ->
            TaskSubsystem'build
        10 ->
            TaskSubsystem'node
        11 ->
            TaskSubsystem'migration
        tag_ ->
            (TaskSubsystem'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (TaskSubsystem'vm) ->
            0
        (TaskSubsystem'disk) ->
            1
        (TaskSubsystem'network) ->
            2
        (TaskSubsystem'sshKey) ->
            3
        (TaskSubsystem'template) ->
            4
        (TaskSubsystem'sharedDir) ->
            5
        (TaskSubsystem'snapshot) ->
            6
        (TaskSubsystem'system) ->
            7
        (TaskSubsystem'apply) ->
            8
        (TaskSubsystem'build) ->
            9
        (TaskSubsystem'node) ->
            10
        (TaskSubsystem'migration) ->
            11
        (TaskSubsystem'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord TaskSubsystem) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse TaskSubsystem TaskSubsystem) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList TaskSubsystem) where
    type ListAllocHint TaskSubsystem = Std_.Int
instance (C.EstimateListAlloc TaskSubsystem TaskSubsystem)
data TaskResult 
    = TaskResult'running 
    | TaskResult'success 
    | TaskResult'error 
    | TaskResult'notStarted 
    | TaskResult'cancelled 
    | TaskResult'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor TaskResult) = (R.Data R.Sz16)
instance (C.HasTypeId TaskResult) where
    typeId  = 15131102630855907230
instance (Std_.Enum TaskResult) where
    toEnum n_ = case n_ of
        0 ->
            TaskResult'running
        1 ->
            TaskResult'success
        2 ->
            TaskResult'error
        3 ->
            TaskResult'notStarted
        4 ->
            TaskResult'cancelled
        tag_ ->
            (TaskResult'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (TaskResult'running) ->
            0
        (TaskResult'success) ->
            1
        (TaskResult'error) ->
            2
        (TaskResult'notStarted) ->
            3
        (TaskResult'cancelled) ->
            4
        (TaskResult'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord TaskResult) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse TaskResult TaskResult) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList TaskResult) where
    type ListAllocHint TaskResult = Std_.Int
instance (C.EstimateListAlloc TaskResult TaskResult)
data NodeAdminState 
    = NodeAdminState'online 
    | NodeAdminState'draining 
    | NodeAdminState'maintenance 
    | NodeAdminState'unknown' Std_.Word16
    deriving(Std_.Eq
            ,Std_.Show
            ,Generics.Generic)
type instance (R.ReprFor NodeAdminState) = (R.Data R.Sz16)
instance (C.HasTypeId NodeAdminState) where
    typeId  = 10729504723721137657
instance (Std_.Enum NodeAdminState) where
    toEnum n_ = case n_ of
        0 ->
            NodeAdminState'online
        1 ->
            NodeAdminState'draining
        2 ->
            NodeAdminState'maintenance
        tag_ ->
            (NodeAdminState'unknown' (Std_.fromIntegral tag_))
    fromEnum value_ = case value_ of
        (NodeAdminState'online) ->
            0
        (NodeAdminState'draining) ->
            1
        (NodeAdminState'maintenance) ->
            2
        (NodeAdminState'unknown' tag_) ->
            (Std_.fromIntegral tag_)
instance (C.IsWord NodeAdminState) where
    fromWord w_ = (Std_.toEnum (Std_.fromIntegral w_))
    toWord v_ = (Std_.fromIntegral (Std_.fromEnum v_))
instance (C.Parse NodeAdminState NodeAdminState) where
    parse  = GH.parseEnum
    encode  = GH.encodeEnum
instance (C.AllocateList NodeAdminState) where
    type ListAllocHint NodeAdminState = Std_.Int
instance (C.EstimateListAlloc NodeAdminState NodeAdminState)