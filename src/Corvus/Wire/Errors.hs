{-# LANGUAGE LambdaCase #-}

-- | Typed errors for the Cap'n Proto wire layer.
--
-- These are the failure cases that 'Corvus.Wire' can produce while
-- decoding values from a Cap'n Proto message. Successful encodes are
-- always total (Haskell → Cap'n Proto cannot fail at this layer).
module Corvus.Wire.Errors
  ( WireError (..)
  , showWireError
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16)

-- | Errors raised when converting a Cap'n Proto parsed value into a
-- Haskell domain type. A successful encode (Haskell → Cap'n Proto) is
-- always total, so this type covers only the decode direction.
data WireError
  = -- | Cap'n Proto enum carried a tag value the schema does not know
    -- about (i.e. the schema was extended by a newer peer). Carries
    -- the enum type name and the raw tag.
    WireUnknownEnum !Text !Word16
  | -- | A union-shaped value (e.g. 'EntityRef') was received with no
    -- variant set. Carries the type name.
    WireMissingUnionVariant !Text
  | -- | Generic protocol violation. Carries a free-form message used
    -- when the structured cases above don't fit.
    WireMalformed !Text
  deriving (Eq, Show)

showWireError :: WireError -> Text
showWireError = \case
  WireUnknownEnum ty tag ->
    "unknown enum tag " <> T.pack (show tag) <> " for type " <> ty
  WireMissingUnionVariant ty ->
    "no variant set on union " <> ty
  WireMalformed msg ->
    "malformed value: " <> msg
