{-# LANGUAGE OverloadedStrings #-}

-- | Reusable parser combinators shared across the per-subsystem parsers
-- in "Corvus.Client.Parser.*".
module Corvus.Client.Parser.Utility
  ( -- * Option readers
    readBool
  , parseSizeWithUnit

    -- * Shared option parsers
  , waitOptionsParser
  )
where

import Corvus.Client.Types
import Data.Char (toLower)
import Data.Int (Int64)
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

-- | Parser for the shared @--wait@ / @--timeout@ option pair.
waitOptionsParser :: Parser WaitOptions
waitOptionsParser =
  WaitOptions
    <$> switch
      ( long "wait"
          <> short 'w'
          <> help "Block until the operation completes"
      )
    <*> optional
      ( option
          auto
          ( long "timeout"
              <> short 't'
              <> metavar "SECONDS"
              <> help "Timeout in seconds when using --wait (default: 120)"
          )
      )
