{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Trans.TypeToHaskell (paramsContext, tCapnp, fieldLocTypeToType, fieldLocTypeToFieldKind, typeToType, compositeTypeToType, interfaceTypeToType, fieldLocTypeToField, tReprName, ToType (..)) where

import qualified Capnp.Repr as R
import Data.String (IsString (fromString))
import Data.Word (Word64)
import qualified IR.AbstractOp as AO
import qualified IR.Common as C
import qualified IR.Haskell as Hs
import qualified IR.Name as Name
import Trans.ToHaskellCommon

paramsContext :: [Hs.Type] -> [Hs.Type]
paramsContext = map paramConstraints

-- | Constraints required for a capnproto type parameter. The returned
-- expression has kind 'Constraint'.
paramConstraints :: Hs.Type -> Hs.Type
paramConstraints t =
  Hs.TApp (tgName ["GH"] "TypeParam") [t]

tCapnp :: Word64 -> Name.CapnpQ -> Hs.Type
tCapnp thisMod Name.CapnpQ {local, fileId}
  | thisMod == fileId = Hs.TLName local
  | otherwise = tgName (map Name.renderUnQ $ idToModule fileId) local

fieldLocTypeToType :: Word64 -> C.FieldLocType AO.Brand Name.CapnpQ -> Hs.Type
fieldLocTypeToType thisMod = \case
  C.VoidField -> Hs.TUnit
  C.DataField _ t -> wordTypeToType thisMod t
  C.PtrField _ t -> ptrTypeToType thisMod t
  C.HereField t -> compositeTypeToType thisMod t

fieldLocTypeToFieldKind :: C.FieldLocType b n -> Name.GlobalQ
fieldLocTypeToFieldKind = \case
  C.HereField _ -> gName ["GH"] "Group"
  _ -> gName ["GH"] "Slot"

wordTypeToType :: Word64 -> C.WordType Name.CapnpQ -> Hs.Type
wordTypeToType thisMod = \case
  C.EnumType t -> tCapnp thisMod t
  C.PrimWord t -> primWordToType t

primWordToType :: C.PrimWord -> Hs.Type
primWordToType = \case
  C.PrimInt t -> intTypeToType t
  C.PrimFloat32 -> tStd_ "Float"
  C.PrimFloat64 -> tStd_ "Double"
  C.PrimBool -> tStd_ "Bool"

intTypeToType :: C.IntType -> Hs.Type
intTypeToType (C.IntType sign size) =
  let prefix = case sign of
        C.Signed -> "Int"
        C.Unsigned -> "Word"
   in tStd_ $ fromString $ prefix ++ show (C.sizeBits size)

wordTypeBits :: C.WordType r -> Int
wordTypeBits = \case
  C.EnumType _ -> 16
  C.PrimWord (C.PrimInt (C.IntType _ size)) -> C.sizeBits size
  C.PrimWord C.PrimFloat32 -> 32
  C.PrimWord C.PrimFloat64 -> 64
  C.PrimWord C.PrimBool -> 1

ptrTypeToType :: Word64 -> C.PtrType (C.ListBrand Name.CapnpQ) Name.CapnpQ -> Hs.Type
ptrTypeToType thisMod = \case
  C.ListOf t -> Hs.TApp (tgName ["R"] "List") [typeToType thisMod t]
  C.PrimPtr t -> primPtrToType t
  C.PtrComposite t -> compositeTypeToType thisMod t
  C.PtrInterface t -> interfaceTypeToType thisMod t
  C.PtrParam t -> typeParamToType t

typeToType :: Word64 -> C.Type (C.ListBrand Name.CapnpQ) Name.CapnpQ -> Hs.Type
typeToType thisMod = \case
  C.CompositeType t -> compositeTypeToType thisMod t
  C.VoidType -> Hs.TUnit
  C.WordType t -> wordTypeToType thisMod t
  C.PtrType t -> ptrTypeToType thisMod t

primPtrToType :: C.PrimPtr -> Hs.Type
primPtrToType = \case
  C.PrimText -> tgName ["Basics"] "Text"
  C.PrimData -> tgName ["Basics"] "Data"
  C.PrimAnyPtr t -> anyPtrToType t

anyPtrToType :: C.AnyPtr -> Hs.Type
anyPtrToType t = case t of
  C.Struct -> basics "AnyStruct"
  C.List -> basics "AnyList"
  C.Cap -> basics "Capability"
  C.Ptr -> Hs.TApp (tStd_ "Maybe") [basics "AnyPointer"]
  where
    basics = tgName ["Basics"]

compositeTypeToType :: Word64 -> C.CompositeType (C.ListBrand Name.CapnpQ) Name.CapnpQ -> Hs.Type
compositeTypeToType thisMod (C.StructType name brand) = namedType thisMod name brand

interfaceTypeToType :: Word64 -> C.InterfaceType (C.ListBrand Name.CapnpQ) Name.CapnpQ -> Hs.Type
interfaceTypeToType thisMod (C.InterfaceType name brand) = namedType thisMod name brand

typeParamToType :: C.TypeParamRef r -> Hs.Type
typeParamToType = Hs.TVar . Name.typeVarName . C.paramName

namedType :: Word64 -> Name.CapnpQ -> C.ListBrand Name.CapnpQ -> Hs.Type
namedType thisMod name (C.ListBrand []) = tCapnp thisMod name
namedType thisMod name (C.ListBrand args) =
  Hs.TApp
    (tCapnp thisMod name)
    [typeToType thisMod (C.PtrType t) | t <- args]

fieldLocTypeToField :: C.FieldLocType b r -> Hs.Exp
fieldLocTypeToField = \case
  C.DataField loc wt ->
    let shift = C.dataOff loc
        index = C.dataIdx loc
        nbits = wordTypeBits wt
        defaultValue = C.dataDef loc
     in Hs.EApp
          (egName ["GH"] "dataField")
          [ Hs.EInt $ fromIntegral shift
          , Hs.EInt $ fromIntegral index
          , Hs.EInt $ fromIntegral nbits
          , Hs.EInt $ fromIntegral defaultValue
          ]
  C.PtrField idx _ ->
    Hs.EApp (egName ["GH"] "ptrField") [Hs.EInt $ fromIntegral idx]
  C.VoidField ->
    egName ["GH"] "voidField"
  C.HereField _ ->
    egName ["GH"] "groupField"

class ToType a where
  toType :: a -> Hs.Type

instance ToType R.Repr where
  toType (R.Ptr p) = rApp "Ptr" [toType p]
  toType (R.Data d) = rApp "Data" [toType d]

instance (ToType a) => ToType (Maybe a) where
  toType Nothing = tStd_ "Nothing"
  toType (Just a) = Hs.TApp (tStd_ "Just") [toType a]

instance ToType R.PtrRepr where
  toType R.Cap = tReprName "Cap"
  toType (R.List r) = rApp "List" [toType r]
  toType R.Struct = tReprName "Struct"

instance ToType R.ListRepr where
  toType (R.ListNormal nl) = rApp "ListNormal" [toType nl]
  toType R.ListComposite = tReprName "ListComposite"

instance ToType R.NormalListRepr where
  toType (R.NormalListData r) = rApp "ListData" [toType r]
  toType R.NormalListPtr = tReprName "ListPtr"

instance ToType R.DataSz where
  toType = tReprName . fromString . show

rApp :: Name.LocalQ -> [Hs.Type] -> Hs.Type
rApp n = Hs.TApp (tReprName n)

tReprName :: Name.LocalQ -> Hs.Type
tReprName = tgName ["R"]
