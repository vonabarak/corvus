{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Compile-time schema extraction for 'Request' (and any other sum-of-
-- records type) via 'GHC.Generics' + 'Data.Typeable'. Used by the
-- @gen-python-client@ executable to auto-generate the Python 'Client'.
--
-- The approach: walk 'Rep' constructors; for each record field, capture
-- the selector name and a 'TypeRep' of the field's type. The generator
-- then maps @TypeRep@s through a small table to Python annotations.
module Corvus.Protocol.Schema
  ( ConstructorInfo (..)
  , FieldInfo (..)
  , datatypeConstructors
  , GConstructors
  , GFields
  )
where

import Data.Proxy (Proxy (..))
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.Generics

-- | Constructor name + ordered list of record fields.
data ConstructorInfo = ConstructorInfo
  { ciName :: !String
  , ciFields :: ![FieldInfo]
  }
  deriving (Show)

-- | A single record field: selector name + runtime type representation.
data FieldInfo = FieldInfo
  { fiName :: !String
  , fiTypeRep :: !TypeRep
  }
  deriving (Show)

-- | Enumerate all constructors of a 'Generic' type.
datatypeConstructors
  :: forall a. (Generic a, GConstructors (Rep a)) => Proxy a -> [ConstructorInfo]
datatypeConstructors _ = gconstructors (Proxy @(Rep a))

-- | Walks a sum representation, producing a 'ConstructorInfo' per branch.
class GConstructors (f :: * -> *) where
  gconstructors :: Proxy f -> [ConstructorInfo]

instance GConstructors V1 where
  gconstructors _ = []

instance (GConstructors f) => GConstructors (M1 D d f) where
  gconstructors _ = gconstructors (Proxy @f)

instance (GConstructors f, GConstructors g) => GConstructors (f :+: g) where
  gconstructors _ = gconstructors (Proxy @f) ++ gconstructors (Proxy @g)

instance (Constructor c, GFields f) => GConstructors (M1 C c f) where
  gconstructors _ =
    [ ConstructorInfo
        { ciName = conName (undefined :: M1 C c f p)
        , ciFields = gfields (Proxy @f)
        }
    ]

-- | Walks a product representation, producing the list of field infos.
class GFields (f :: * -> *) where
  gfields :: Proxy f -> [FieldInfo]

instance GFields U1 where
  gfields _ = []

instance (GFields f, GFields g) => GFields (f :*: g) where
  gfields _ = gfields (Proxy @f) ++ gfields (Proxy @g)

-- | A single selector. 'K1 R t' (alias 'Rec0 t') is the wrapper around
-- the actual field type @t@; we pin @t@ with 'Typeable' so the generator
-- can recover it at runtime via 'typeRep'.
instance (Selector s, Typeable t) => GFields (M1 S s (K1 R t)) where
  gfields _ =
    [ FieldInfo
        { fiName = selName (undefined :: M1 S s (K1 R t) p)
        , fiTypeRep = typeRep (Proxy @t)
        }
    ]
