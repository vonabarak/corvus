{-# LANGUAGE OverloadedStrings #-}

-- | Command-line parser for @crv build@.
module Corvus.Client.Parser.Build
  ( buildCommand
  )
where

import Corvus.Client.Parser.Utility (waitOptionsParser)
import Corvus.Client.Types
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
