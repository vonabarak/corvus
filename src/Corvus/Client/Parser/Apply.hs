{-# LANGUAGE OverloadedStrings #-}

-- | Command-line parser for @crv apply@.
module Corvus.Client.Parser.Apply
  ( applyCommand
  )
where

import Corvus.Client.Parser.Utility
import Corvus.Client.Types
import Options.Applicative

-- | Apply environment from YAML config file
applyCommand :: Parser Command
applyCommand =
  Apply
    <$> strArgument
      ( metavar "FILE"
          <> help "Path to YAML configuration file"
          <> action "file"
      )
    <*> switch
      ( long "skip-existing"
          <> short 's'
          <> help "Skip resources that already exist instead of failing"
      )
    <*> waitOptionsParser
