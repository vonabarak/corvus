{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Network namespace management.
-- Creates and manages bridges, dnsmasq, and tap-up scripts inside
-- the daemon's shared network namespace using C FFI for namespace entry
-- (required for ambient capabilities after exec).
module Corvus.Qemu.Netns.Manager
  ( -- * Bridge lifecycle
    createBridge
  , destroyBridge

    -- * dnsmasq lifecycle
  , startDnsmasq
  , stopDnsmasq

    -- * Tap-up script
  , writeTapUpScript
  )
where

import Control.Exception (SomeException, try)
import Corvus.Qemu.Config (QemuConfig (..))
import Corvus.Qemu.Netns (nsExec, nsSpawn)
import Corvus.Qemu.Runtime (createNetworkRuntimeDir, getBridgeName, getDnsmasqLeaseFile, getTapUpScript)
import Corvus.Utils.Subnet (dhcpRangeEnd, dhcpRangeStart, gatewayAddress, prefixLength, subnetMask)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import System.Posix.Files (ownerExecuteMode, ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)
import System.Posix.Signals (sigTERM, signalProcess)

--------------------------------------------------------------------------------
-- Bridge lifecycle
--------------------------------------------------------------------------------

-- | Create a bridge inside the namespace for the given network.
-- If subnet is non-empty, assigns the gateway IP to the bridge.
createBridge :: Int -> QemuConfig -> Int64 -> Text -> IO (Either Text ())
createBridge nsPid config networkId subnet = do
  let bridge = getBridgeName networkId

  -- Create bridge
  result <- nsExec nsPid ["ip", "link", "add", bridge, "type", "bridge"]
  case result of
    Left err -> pure $ Left $ "Failed to create bridge: " <> err
    Right _ -> do
      -- Assign IP if subnet is configured
      if not (T.null subnet)
        then case (,) <$> gatewayAddress subnet <*> prefixLength subnet of
          Left err -> pure $ Left $ "Subnet error: " <> err
          Right (gw, prefix) -> do
            ipResult <- nsExec nsPid ["ip", "addr", "add", T.unpack gw <> "/" <> T.unpack prefix, "dev", bridge]
            case ipResult of
              Left err -> pure $ Left $ "Failed to assign IP: " <> err
              Right _ -> bringUpBridge nsPid bridge
        else bringUpBridge nsPid bridge
  where
    bringUpBridge :: Int -> String -> IO (Either Text ())
    bringUpBridge pid bridge = do
      upResult <- nsExec pid ["ip", "link", "set", bridge, "up"]
      case upResult of
        Left err -> pure $ Left $ "Failed to bring up bridge: " <> err
        Right _ -> do
          -- Write tap-up script for this bridge
          _ <- writeTapUpScript config networkId
          pure $ Right ()

-- | Destroy a bridge inside the namespace.
destroyBridge :: Int -> Int64 -> IO (Either Text ())
destroyBridge nsPid networkId = do
  let bridge = getBridgeName networkId
  result <- nsExec nsPid ["ip", "link", "del", bridge]
  case result of
    Left err -> pure $ Left $ "Failed to destroy bridge: " <> err
    Right _ -> pure $ Right ()

--------------------------------------------------------------------------------
-- dnsmasq lifecycle
--------------------------------------------------------------------------------

-- | Start dnsmasq on a bridge inside the namespace.
-- Returns the dnsmasq PID.
startDnsmasq :: Int -> QemuConfig -> Int64 -> Text -> IO (Either Text Int)
startDnsmasq nsPid config networkId subnet = do
  leaseFile <- getDnsmasqLeaseFile config networkId

  case (,,,,) <$> gatewayAddress subnet <*> prefixLength subnet <*> dhcpRangeStart subnet <*> dhcpRangeEnd subnet <*> subnetMask subnet of
    Left err -> pure $ Left $ "Subnet error: " <> err
    Right (gw, _prefix, rangeStart, rangeEnd, mask) -> do
      let dnsmasqBin = qcDnsmasqBinary config
          listenArg = "--listen-address=" <> T.unpack gw
          rangeArg = "--dhcp-range=" <> T.unpack rangeStart <> "," <> T.unpack rangeEnd <> "," <> T.unpack mask <> ",12h"
          leaseArg = "--dhcp-leasefile=" <> leaseFile
      nsSpawn
        nsPid
        [ dnsmasqBin
        , "--conf-file=/dev/null"
        , "--bind-interfaces"
        , listenArg
        , "--except-interface=lo"
        , rangeArg
        , "--keep-in-foreground"
        , "--no-hosts"
        , leaseArg
        ]

-- | Stop a dnsmasq process by PID.
stopDnsmasq :: Int -> IO ()
stopDnsmasq pid = do
  result <- try $ signalProcess sigTERM (fromIntegral pid)
  case result of
    Left (_ :: SomeException) -> pure ()
    Right () -> pure ()

--------------------------------------------------------------------------------
-- Tap-up script
--------------------------------------------------------------------------------

-- | Write a tap-up script for a network that adds a TAP device to the bridge.
-- Returns the path to the script.
writeTapUpScript :: QemuConfig -> Int64 -> IO FilePath
writeTapUpScript config networkId = do
  _ <- createNetworkRuntimeDir config networkId
  scriptPath <- getTapUpScript config networkId
  let bridge = getBridgeName networkId
      content =
        unlines
          [ "#!/bin/sh"
          , "ip link set \"$1\" master " <> bridge
          , "ip link set \"$1\" up"
          ]
  writeFile scriptPath content
  let execMode = ownerReadMode `unionFileModes` ownerWriteMode `unionFileModes` ownerExecuteMode
  setFileMode scriptPath execMode
  pure scriptPath
