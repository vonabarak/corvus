{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Shared text, JSON and database conversion for model enums.
module Corvus.Model.EnumText
  ( EnumText (..)
  , parseEnumJSON
  , toEnumJSON
  , enumToPersistValue
  , enumFromPersistValue
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Types as AT
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (PersistValue (..))

-- | Type class for enums that serialize to/from Text
class (Eq a) => EnumText a where
  -- | The mapping between enum values and their text representations
  enumMapping :: [(a, Text)]

  -- | Type name for error messages
  enumTypeName :: Text

  -- | Convert enum to text (derived from mapping)
  enumToText :: a -> Text
  enumToText val =
    fromMaybe ("<unknown " <> enumTypeName @a <> ">") (lookup val enumMapping)

  -- | Convert text to enum (derived from mapping)
  enumFromText :: Text -> Either Text a
  enumFromText t =
    case find (\(_, txt) -> T.toLower txt == T.toLower t) enumMapping of
      Just (val, _) -> Right val
      Nothing -> Left $ "Invalid " <> enumTypeName @a <> ": " <> t

-- | Standard parser for enums using EnumText
parseEnumJSON :: (EnumText a) => Value -> AT.Parser a
parseEnumJSON (String t) =
  case enumFromText t of
    Right val -> pure val
    Left err -> fail (T.unpack err)
parseEnumJSON _ = fail "Expected String"

-- | Standard serializer for enums using EnumText
toEnumJSON :: (EnumText a) => a -> Value
toEnumJSON = String . enumToText

-- | Helper to create PersistField instance
enumToPersistValue :: (EnumText a) => a -> PersistValue
enumToPersistValue = PersistText . enumToText

-- | Helper to create PersistField instance
enumFromPersistValue :: forall a. (EnumText a) => PersistValue -> Either Text a
enumFromPersistValue (PersistText t) = enumFromText t
enumFromPersistValue x = Left $ "Expected Text for " <> enumTypeName @a <> ", got: " <> T.pack (show x)
