{-# LANGUAGE OverloadedStrings #-}

-- | Exact binary sizes. Stored and machine-readable values are always bytes.
module Corvus.Size (parseBytes, parseRam, formatSize, sizeField, optionalSizeField, defaultSizeField, rejectLegacySizes, validateRam) where

import Control.Monad (when)
import Data.Aeson (Object, Value, withText, (.:), (.:?))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.Char (isDigit, toUpper)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

parseBytes :: (Integral a, Bounded a) => String -> Either String a
parseBytes raw =
  let (digits, suffix) = span isDigit raw
   in case (reads digits :: [(Integer, String)], map toUpper suffix) of
        ([(n, "")], [unit])
          | Just power <- lookup unit (zip "BKMGT" [0 :: Int ..]) ->
              let bytes = n * 1024 ^ power
                  result = fromInteger bytes
               in if bytes > 0 && bytes <= toInteger (maxBound `asTypeOf` result)
                    then Right result
                    else Left "Size must be positive and fit in the supported integer byte range"
        _ -> Left "Expected a positive integer with a B/K/M/G/T suffix (e.g. 768M)"

validateRam :: Int64 -> Either String Int64
validateRam bytes
  | bytes > 0 && bytes `mod` 1048576 == 0 = Right bytes
  | otherwise = Left "RAM must be positive and a whole multiple of 1M (1048576 bytes)"

parseRam :: String -> Either String Int64
parseRam raw = parseBytes raw >>= validateRam

formatSize :: (Integral a) => a -> String
formatSize input
  | bytes == 0 = "0B"
  | otherwise = go (zip "TGMK" [4, 3 .. 1 :: Int])
  where
    bytes = toInteger input
    go [] = show bytes ++ "B"
    go ((unit, power) : rest) =
      let factor = 1024 ^ power
       in if bytes `mod` factor == 0 then show (bytes `div` factor) ++ [unit] else go rest

rejectLegacySizes :: Object -> Parser ()
rejectLegacySizes o = mapM_ reject ["ramMb", "sizeMb", "sizeGb"]
  where
    reject key = when (KM.member key o) (fail (T.unpack (K.toText key) ++ " was removed; use ram/size with a B/K/M/G/T suffix"))

parseSizeValue :: Text -> Value -> Parser Int64
parseSizeValue key = withText (T.unpack key ++ " size") $ \t -> either fail pure (if key == "ram" then parseRam (T.unpack t) else parseBytes (T.unpack t))

sizeField :: Object -> Text -> Parser Int64
sizeField o key = rejectLegacySizes o >> (o .: K.fromText key >>= parseSizeValue key)

optionalSizeField :: Object -> Text -> Parser (Maybe Int64)
optionalSizeField o key = do
  rejectLegacySizes o
  value <- o .:? K.fromText key
  traverse (parseSizeValue key) value

defaultSizeField :: Object -> Text -> Int64 -> Parser Int64
defaultSizeField o key fallback = fromMaybe fallback <$> optionalSizeField o key
