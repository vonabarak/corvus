{-# LANGUAGE OverloadedStrings #-}

-- | QEMU configuration options.
module Corvus.Qemu.Config
  ( QemuConfig (..)
  , defaultQemuConfig
  , getEffectiveBasePath
  , getEffectiveRuntimeDir
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | QEMU configuration options
data QemuConfig = QemuConfig
  { qcQemuBinary :: FilePath
  -- ^ Path to qemu binary
  , qcBasePath :: Maybe FilePath
  -- ^ Base path for relative image paths (Nothing = $HOME/VMs)
  , qcRuntimeDir :: Maybe FilePath
  -- ^ Runtime directory for VM sockets (Nothing = $XDG_RUNTIME_DIR/corvus)
  , qcVirtiofsdBinary :: FilePath
  -- ^ Path to virtiofsd binary
  , qcSharedMemSize :: Maybe String
  -- ^ Shared memory size for virtiofs (e.g. "4G"), Nothing uses VM RAM
  , qcDnsmasqBinary :: FilePath
  -- ^ Path to dnsmasq binary
  , qcPastaBinary :: FilePath
  -- ^ Path to pasta binary (for NAT)
  , qcNftBinary :: FilePath
  -- ^ Path to nft binary (for NAT rules)
  , qcHealthcheckInterval :: Int
  -- ^ Healthcheck ping interval in seconds (default 10)
  , qcSpiceBindAddress :: !Text
  -- ^ Address QEMU binds the SPICE TCP listener to. Defaults to
  -- @127.0.0.1@ for Unix-socket daemons; for TCP daemons the CLI wires
  -- this to match the RPC listen host so operators exposing the daemon
  -- remotely automatically expose SPICE on the same interface.
  , qcSpicePortMin :: !Int
  -- ^ Low end (inclusive) of the TCP port range the SPICE allocator
  -- draws from (default: 5900).
  , qcSpicePortMax :: !Int
  -- ^ High end (inclusive) of the TCP port range the SPICE allocator
  -- draws from (default: 5999).
  , qcVsockCidMin :: !Int
  -- ^ Low end (inclusive) of the AF_VSOCK CID range allocated to VMs
  -- (default: 1000). CIDs 0/1/2 are reserved by the AF_VSOCK ABI.
  , qcVsockCidMax :: !Int
  -- ^ High end (inclusive) of the AF_VSOCK CID range allocated to VMs
  -- (default: 1_000_000).
  }
  deriving (Eq, Show)

-- | Default QEMU configuration
defaultQemuConfig :: QemuConfig
defaultQemuConfig =
  QemuConfig
    { qcQemuBinary = "qemu-system-x86_64"
    , qcBasePath = Nothing -- Will use $HOME/VMs at runtime
    , qcRuntimeDir = Nothing -- Will use $XDG_RUNTIME_DIR/corvus at runtime
    , qcVirtiofsdBinary = "/usr/libexec/virtiofsd"
    , qcSharedMemSize = Nothing -- Will use VM RAM size
    , qcDnsmasqBinary = "dnsmasq"
    , qcPastaBinary = "pasta"
    , qcNftBinary = "nft"
    , qcHealthcheckInterval = 10
    , qcSpiceBindAddress = "127.0.0.1"
    , qcSpicePortMin = 5900
    , qcSpicePortMax = 5999
    , qcVsockCidMin = 1000
    , qcVsockCidMax = 1000000
    }

-- | Get the effective base path for VM images
-- Uses qcBasePath if set, otherwise $HOME/VMs
getEffectiveBasePath :: QemuConfig -> IO FilePath
getEffectiveBasePath config = case qcBasePath config of
  Just path -> pure path
  Nothing -> do
    mHome <- lookupEnv "HOME"
    pure $ fromMaybe "/var/lib/qemu" mHome </> "VMs"

-- | Get the effective runtime directory for VM sockets
-- Uses qcRuntimeDir if set, otherwise $XDG_RUNTIME_DIR/corvus
getEffectiveRuntimeDir :: QemuConfig -> IO FilePath
getEffectiveRuntimeDir config = case qcRuntimeDir config of
  Just path -> pure path
  Nothing -> do
    mXdg <- lookupEnv "XDG_RUNTIME_DIR"
    case mXdg of
      Just xdg -> pure $ xdg </> "corvus"
      Nothing -> do
        mUid <- lookupEnv "UID"
        let uid = fromMaybe "1000" mUid
        pure $ "/tmp/corvus-" ++ uid
