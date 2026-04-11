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
    ClientConfig (..)
  , defaultClientConfig

    -- * Connection
  , Connection
  , withConnection
  , ConnectionError (..)

    -- * RPC Calls
  , sendPing
  , getStatus
  , requestShutdown
  , listVms
  , showVm

    -- * VM lifecycle
  , VmCreateResult (..)
  , VmDeleteResult (..)
  , vmCreate
  , vmDelete

    -- * VM actions
  , VmActionResult (..)
  , vmStart
  , vmStop
  , vmPause
  , vmReset

    -- * VM edit
  , VmEditResult (..)
  , vmEdit

    -- * Guest execution
  , GuestExecResult (..)
  , vmExec

    -- * Disk operations
  , DiskResult (..)
  , diskCreate
  , diskCreateOverlay
  , diskRegister
  , diskDelete
  , diskResize
  , diskClone
  , diskRebase
  , diskList
  , diskShow
  , diskAttach
  , diskDetach

    -- * Snapshot operations
  , SnapshotResult (..)
  , snapshotCreate
  , snapshotDelete
  , snapshotRollback
  , snapshotMerge
  , snapshotList

    -- * Shared directory operations
  , SharedDirResult (..)
  , sharedDirAdd
  , sharedDirRemove
  , sharedDirList

    -- * Network interface operations
  , NetIfResult (..)
  , netIfAdd
  , netIfRemove
  , netIfList

    -- * SSH key operations
  , SshKeyResult (..)
  , sshKeyCreate
  , sshKeyDelete
  , sshKeyList
  , sshKeyAttach
  , sshKeyDetach
  , sshKeyListForVm

    -- * Template operations
  , TemplateResult (..)
  , templateCreate
  , templateDelete
  , templateList
  , templateShow
  , templateInstantiate

    -- * CLI types
  , Options (..)
  , Command (..)

    -- * Argument parsing
  , optionsParser
  , optsInfo

    -- * Command execution
  , runCommand
  )
where

import Corvus.Client.Commands
import Corvus.Client.Config
import Corvus.Client.Connection
import Corvus.Client.Parser
import Corvus.Client.Rpc
import Corvus.Client.Types
