{-# LANGUAGE OverloadedStrings #-}

-- | Reusable parser combinators shared across the per-subsystem parsers
-- in "Corvus.Client.Parser.*".
module Corvus.Client.Parser.Utility
  ( -- * Option readers
    readBool
  , parseSizeWithUnit
  , parseSizeBytes

    -- * Shared option parsers
  , waitOptionsParser
  )
where

import Corvus.Client.Types
import Data.Char (isDigit, toLower, toUpper)
import Data.Int (Int64)
import Data.Word (Word64)
import Options.Applicative

-- | Reader for boolean values (true/false/yes/no/1/0, case-insensitive).
readBool :: ReadM Bool
readBool = eitherReader $ \s -> case map toLower s of
  "true" -> Right True
  "false" -> Right False
  "yes" -> Right True
  "no" -> Right False
  "1" -> Right True
  "0" -> Right False
  _ -> Left $ "Invalid boolean: " ++ s ++ " (use true/false)"

-- | Parse a size with an optional unit suffix (M/G/T). Returns megabytes.
parseSizeWithUnit :: ReadM Int64
parseSizeWithUnit = eitherReader $ \s ->
  case reads s of
    [(n, "")] -> Right n
    [(n, "M")] -> Right n
    [(n, "G")] -> Right (n * 1024)
    [(n, "T")] -> Right (n * 1024 * 1024)
    _ -> Left $ "Invalid size format: " ++ s ++ " (use number with optional M/G/T suffix)"

-- | Parse a positive integer with a binary B/K/M/G/T suffix into bytes.
-- Compute in Integer before narrowing so multiplication cannot overflow.
parseSizeBytes :: ReadM Word64
parseSizeBytes = eitherReader $ \raw ->
  let (digits, suffix) = span isDigit raw
      units = zip "BKMGT" [0 :: Int ..]
   in case (reads digits :: [(Integer, String)], map toUpper suffix) of
        ([(n, "")], [unit])
          | Just power <- lookup unit units ->
              let bytes = n * 1024 ^ power
               in if bytes > 0 && bytes <= toInteger (maxBound :: Word64)
                    then Right (fromInteger bytes)
                    else Left "Size must be positive and fit in UInt64 bytes"
        _ -> Left "Expected a positive integer with a B/K/M/G/T suffix (e.g. 768M)"

-- | Parser for the shared @--wait@ option.
waitOptionsParser :: Parser WaitOptions
waitOptionsParser =
  WaitOptions
    <$> switch
      ( long "wait"
          <> short 'w'
          <> help "Block until the operation completes"
      )
    <*> pure Nothing
