-- | Corvus client library.
--
-- This module re-exports the main functionality from submodules:
--
-- * "Corvus.Client.Config" - Client configuration
-- * "Corvus.Client.Connection" - Connection handling
-- * "Corvus.Client.Rpc" - RPC call functions
-- * "Corvus.Client.Types" - CLI types
-- * "Corvus.Client.Parser" - Argument parsing
-- * "Corvus.Client.Commands" - Command execution
module Corvus.Client
  ( -- * Configuration
    ClientConfig (..),
    defaultClientConfig,

    -- * Connection
    Connection,
    withConnection,
    ConnectionError (..),

    -- * RPC Calls
    sendPing,
    getStatus,
    requestShutdown,
    listVms,
    showVm,

    -- * VM actions
    VmActionResult (..),
    vmStart,
    vmStop,
    vmPause,
    vmReset,

    -- * CLI types
    Options (..),
    Command (..),

    -- * Argument parsing
    optionsParser,
    optsInfo,

    -- * Command execution
    runCommand,
  )
where

import Corvus.Client.Commands
import Corvus.Client.Config
import Corvus.Client.Connection
import Corvus.Client.Parser
import Corvus.Client.Rpc
import Corvus.Client.Types
