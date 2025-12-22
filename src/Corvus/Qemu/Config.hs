{-# LANGUAGE OverloadedStrings #-}

-- | QEMU configuration options.
module Corvus.Qemu.Config
  ( QemuConfig (..),
    defaultQemuConfig,
    getEffectiveBasePath,
  )
where

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | QEMU configuration options
data QemuConfig = QemuConfig
  { -- | Path to qemu binary
    qcQemuBinary :: FilePath,
    -- | Base path for relative image paths (Nothing = $HOME/VMs)
    qcBasePath :: Maybe FilePath,
    -- | Path to virtiofsd binary
    qcVirtiofsdBinary :: FilePath,
    -- | Shared memory size for virtiofs (e.g. "4G"), Nothing uses VM RAM
    qcSharedMemSize :: Maybe String
  }
  deriving (Eq, Show)

-- | Default QEMU configuration
defaultQemuConfig :: QemuConfig
defaultQemuConfig =
  QemuConfig
    { qcQemuBinary = "qemu-system-x86_64",
      qcBasePath = Nothing, -- Will use $HOME/VMs at runtime
      qcVirtiofsdBinary = "/usr/libexec/virtiofsd",
      qcSharedMemSize = Nothing -- Will use VM RAM size
    }

-- | Get the effective base path for VM images
-- Uses qcBasePath if set, otherwise $HOME/VMs
getEffectiveBasePath :: QemuConfig -> IO FilePath
getEffectiveBasePath config = case qcBasePath config of
  Just path -> pure path
  Nothing -> do
    mHome <- lookupEnv "HOME"
    pure $ fromMaybe "/var/lib/qemu" mHome </> "VMs"
