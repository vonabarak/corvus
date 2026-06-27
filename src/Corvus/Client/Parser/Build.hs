{-# LANGUAGE OverloadedStrings #-}

-- | Command-line parser for @crv build@.
module Corvus.Client.Parser.Build
  ( buildCommand
  )
where

import Corvus.Client.Parser.Utility (waitOptionsParser)
import Corvus.Client.Types
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Text as T
import Options.Applicative

-- | Build OS images from a YAML pipeline file.
--
-- Async by default; the daemon returns a task id immediately and the
-- pipeline runs in the background. Pass @--wait@ to block until the
-- builds complete and surface the per-build artifact disk ids.
--
-- Cache flags (@--use-cache@, @--build-cache@, @--rebuild-from@) OR
-- with the YAML's own @useCache:@ / @buildCache:@ fields. The default
-- is no caching either way.
buildCommand :: Parser Command
buildCommand =
  Build
    <$> strArgument
      ( metavar "FILE"
          <> help "Path to YAML build configuration file"
          <> action "file"
      )
    <*> buildClientOptionsParser
    <*> waitOptionsParser

buildClientOptionsParser :: Parser BuildClientOptions
buildClientOptionsParser =
  BuildClientOptions
    <$> switch
      ( long "use-cache"
          <> help "Resume the build from any matching cached prefix"
      )
    <*> switch
      ( long "build-cache"
          <> help "Snapshot the bake VM after each successful step for future reuse"
      )
    <*> option
      auto
      ( long "rebuild-from"
          <> metavar "N"
          <> value 0
          <> showDefault
          <> help "1-based step index that caps the matched prefix (only meaningful with --use-cache)"
      )
    <*> many
      ( option
          (eitherReader parseVarFlag)
          ( long "var"
              <> metavar "KEY=VALUE"
              <> help "Set a build variable (repeatable). Expands {{ KEY }} in the YAML."
          )
      )
    <*> many
      ( strOption
          ( long "var-file"
              <> metavar "FILE"
              <> help "Load build variables from a YAML/JSON mapping (repeatable; later files override earlier ones, both lose to --var)"
              <> action "file"
          )
      )

-- | Parse a single @KEY=VALUE@ argument. @KEY@ must be a non-empty
-- identifier-shaped string; the value may contain @=@ characters
-- (the first @=@ separates key from value).
parseVarFlag :: String -> Either String (T.Text, T.Text)
parseVarFlag raw =
  let (k, eqv) = break (== '=') raw
   in case eqv of
        ('=' : v) | not (null k) && validIdent k -> Right (T.pack k, T.pack v)
        _ -> Left ("expected KEY=VALUE with an identifier KEY, got: " <> raw)

-- | @[A-Za-z_][A-Za-z0-9_]*@.
validIdent :: String -> Bool
validIdent (c : cs) =
  (isAsciiLetter c || c == '_')
    && all (\x -> isAsciiLetter x || isDigit x || x == '_') cs
validIdent [] = False

isAsciiLetter :: Char -> Bool
isAsciiLetter c = isAsciiUpper c || isAsciiLower c
