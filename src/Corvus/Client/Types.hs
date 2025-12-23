-- | Types for the Corvus client CLI.
module Corvus.Client.Types
  ( -- * Command line options
    Options (..),
    Command (..),
  )
where

import Data.Int (Int64)

-- | Command line options
data Options = Options
  { optSocket :: Maybe FilePath,
    optTcp :: Bool,
    optHost :: String,
    optPort :: Int,
    optCommand :: Command
  }
  deriving (Show)

-- | Available commands
data Command
  = Ping
  | Status
  | Shutdown
  | -- VM commands
    VmList
  | VmShow !Int64
  | VmStart !Int64
  | VmStop !Int64
  | VmPause !Int64
  | VmReset !Int64
  | -- | View VM via SPICE (runs remote-viewer)
    VmView !Int64
  | -- | Connect to VM's HMP monitor
    VmMonitor !Int64
  deriving (Show)
