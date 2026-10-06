{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The single disk selector grammar used by every public interface.
module Corvus.DiskSelector
  ( DiskSelector (..)
  , parseDiskSelector
  , renderDiskSelector
  , validateImageName
  , validateImageTag
  , publicationSelector
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Int (Int64)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Read (readMaybe)

data DiskSelector = ImageId Int64 | ImageTag Text Text
  deriving (Eq, Ord, Show, Generic)

asciiDigit :: Char -> Bool
asciiDigit = isDigit

validateImageName :: Text -> Either Text ()
validateImageName name
  | T.null name = Left "Image name cannot be empty"
  | T.any (== ':') name = Left "Image name cannot contain a colon"
  | asciiDigit (T.head name) = Left "Image name cannot start with a digit"
  | T.any (`elem` ['/', '\\', '\0']) name || ".." `T.isInfixOf` name = Left "Image name contains unsafe path characters"
  | otherwise = Right ()

validateImageTag :: Text -> Either Text ()
validateImageTag tag
  | T.null tag || T.length tag > 128 = Left "Image tag must contain 1 to 128 characters"
  | not (first (T.head tag)) || not (T.all rest tag) = Left "Invalid image tag"
  | otherwise = Right ()
  where
    first c = isAsciiLower c || isAsciiUpper c || asciiDigit c || c == '_'
    rest c = first c || c == '.' || c == '-'

parseDiskSelector :: Text -> Either Text DiskSelector
parseDiskSelector value
  | T.null value = Left "Disk selector cannot be empty"
  | asciiDigit (T.head value) =
      case readMaybe (T.unpack value) :: Maybe Integer of
        Just n | T.all asciiDigit value && n > 0 && n <= toInteger (maxBound :: Int64) -> Right (ImageId (fromInteger n))
        _ -> Left "Invalid image ID: expected a positive decimal Int64"
  | otherwise = case T.splitOn ":" value of
      [name] -> named name "latest"
      [name, tag] -> named name tag
      _ -> Left "Disk selector must be a name, name:tag, or image ID"
  where
    named name tag = validateImageName name >> validateImageTag tag >> Right (ImageTag name tag)

publicationSelector :: Text -> Either Text (Text, Text)
publicationSelector value = do
  selector <- parseDiskSelector value
  case selector of
    ImageTag name tag -> Right (name, tag)
    ImageId _ -> Left "A new image requires a name, not an image ID"

renderDiskSelector :: DiskSelector -> Text
renderDiskSelector (ImageId imageId) = T.pack (show imageId)
renderDiskSelector (ImageTag name tag) = name <> ":" <> tag

instance FromJSON DiskSelector where
  parseJSON (String value) = either (fail . T.unpack) pure (parseDiskSelector value)
  parseJSON (Number value) = case toBoundedInteger value :: Maybe Int64 of
    Just n | n > 0 -> pure (ImageId n)
    _ -> fail "Image ID must be a positive integer"
  parseJSON _ = fail "diskImage must be a string selector or integer ID"

instance ToJSON DiskSelector where
  toJSON (ImageId imageId) = toJSON imageId
  toJSON selector = String (renderDiskSelector selector)
