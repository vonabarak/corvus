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
module Capnp.Gen.Echo where
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
data Echo 
type instance (R.ReprFor Echo) = (R.Ptr (Std_.Just R.Cap))
instance (C.HasTypeId Echo) where
    typeId  = 16940812395455687611
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
    methodByLabel  = (GH.Method 16940812395455687611 0)
data Echo'echo'params 
type instance (R.ReprFor Echo'echo'params) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Echo'echo'params) where
    typeId  = 11805419372138457970
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
        {query :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Echo'echo'params))
deriving instance (Std_.Eq (C.Parsed Echo'echo'params))
instance (C.Parse Echo'echo'params (C.Parsed Echo'echo'params)) where
    parse raw_ = (Echo'echo'params <$> (GH.parseField #query raw_))
instance (C.Marshal Echo'echo'params (C.Parsed Echo'echo'params)) where
    marshalInto raw_ Echo'echo'params{..} = (do
        (GH.encodeField #query query raw_)
        (Std_.pure ())
        )
instance (GH.HasField "query" GH.Slot Echo'echo'params Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)
data Echo'echo'results 
type instance (R.ReprFor Echo'echo'results) = (R.Ptr (Std_.Just R.Struct))
instance (C.HasTypeId Echo'echo'results) where
    typeId  = 9634882973594762090
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
        {reply :: (RP.Parsed Basics.Text)}
    deriving(Generics.Generic)
deriving instance (Std_.Show (C.Parsed Echo'echo'results))
deriving instance (Std_.Eq (C.Parsed Echo'echo'results))
instance (C.Parse Echo'echo'results (C.Parsed Echo'echo'results)) where
    parse raw_ = (Echo'echo'results <$> (GH.parseField #reply raw_))
instance (C.Marshal Echo'echo'results (C.Parsed Echo'echo'results)) where
    marshalInto raw_ Echo'echo'results{..} = (do
        (GH.encodeField #reply reply raw_)
        (Std_.pure ())
        )
instance (GH.HasField "reply" GH.Slot Echo'echo'results Basics.Text) where
    fieldByLabel  = (GH.ptrField 0)