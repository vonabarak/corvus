-- | Corvus client library.
--
-- Re-exports the user-facing surface of the client (config, CLI
-- types, parser, command dispatcher). The legacy 'Data.Binary'
-- RPC modules are gone — third-party code that needs to drive the
-- daemon should use "Corvus.Client.Capnp.Connection" and
-- "Corvus.Client.Capnp.Rpc" directly.
module Corvus.Client
  ( -- * Configuration
    ClientConfig (..)
  , defaultClientConfig

    -- * CLI types
  , Options (..)
  , Command (..)

    -- * Argument parsing
  , optionsParser
  , optsInfo
  , ClientDefaults (..)
  , readClientDefaults

    -- * Command execution
  , runCommand
  )
where

import Corvus.Client.Commands
import Corvus.Client.Config
import Corvus.Client.Parser
import Corvus.Client.Types
