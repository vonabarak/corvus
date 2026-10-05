{-# LANGUAGE OverloadedStrings #-}

-- | Reusable parser combinators shared across the per-subsystem parsers
-- in "Corvus.Client.Parser.*".
module Corvus.Client.Parser.Utility
  ( -- * Option readers
    readBool
  , parseSize
  , parseSizeBytes

    -- * Shared option parsers
  , waitOptionsParser
  )
where

import Corvus.Client.Types
import Corvus.Size (parseBytes)
import Data.Char (toLower)
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

-- | Parse an exact byte size with a required binary suffix.
parseSize :: ReadM Int64
parseSize = eitherReader parseBytes

-- | Balloon byte quantities retain their UInt64 range.
parseSizeBytes :: ReadM Word64
parseSizeBytes = eitherReader parseBytes

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
