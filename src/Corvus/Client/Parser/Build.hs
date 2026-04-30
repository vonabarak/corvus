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
buildCommand :: Parser Command
buildCommand =
  Build
    <$> strArgument
      ( metavar "FILE"
          <> help "Path to YAML build configuration file"
          <> action "file"
      )
    <*> waitOptionsParser
