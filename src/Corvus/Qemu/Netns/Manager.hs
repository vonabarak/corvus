{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Network namespace management.
-- Creates and manages bridges, dnsmasq, NAT (pasta + nftables), and
-- tap-up scripts inside the daemon's shared network namespace.
module Corvus.Qemu.Netns.Manager
  ( -- * Bridge lifecycle
    createBridge
  , destroyBridge

    -- * dnsmasq lifecycle
  , startDnsmasq
  , stopDnsmasq

    -- * Tap-up script
  , writeTapUpScript

    -- * pasta (NAT uplink)
  , startPasta
  , stopPasta

    -- * nftables NAT
  , enableIpForwarding
  , setupNatTable
  , teardownNatTable
  , addNatRule
  , removeNatRule
  )
where

import Control.Exception (SomeException, try)
import Corvus.Qemu.Config (QemuConfig (..))
import Corvus.Qemu.Netns (nsExec, nsSpawn)
import Corvus.Qemu.Runtime (createNetworkRuntimeDir, getBridgeName, getDnsmasqLeaseFile, getTapUpScript)
import Corvus.Utils.Subnet (dhcpRangeEnd, dhcpRangeStart, gatewayAddress, prefixLength, subnetMask)
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Posix.Files (ownerExecuteMode, ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)
import System.Posix.Signals (sigTERM, signalProcess)
import System.Process (CreateProcess (..), StdStream (..), createProcess, getPid, proc)

--------------------------------------------------------------------------------
-- Bridge lifecycle
--------------------------------------------------------------------------------

-- | Create a bridge inside the namespace for the given network.
-- If subnet is non-empty, assigns the gateway IP to the bridge.
createBridge :: Int -> QemuConfig -> Int64 -> Text -> IO (Either Text ())
createBridge nsPid _config networkId subnet = do
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
          _ <- writeTapUpScript _config networkId
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
-- When nat is True, adds upstream DNS server arguments so dnsmasq
-- can forward queries for internet domains.
-- Returns the dnsmasq PID.
startDnsmasq :: Int -> QemuConfig -> Int64 -> Text -> Bool -> IO (Either Text Int)
startDnsmasq nsPid config networkId subnet nat = do
  leaseFile <- getDnsmasqLeaseFile config networkId

  case (,,,,) <$> gatewayAddress subnet <*> prefixLength subnet <*> dhcpRangeStart subnet <*> dhcpRangeEnd subnet <*> subnetMask subnet of
    Left err -> pure $ Left $ "Subnet error: " <> err
    Right (gw, _prefix, rangeStart, rangeEnd, mask) -> do
      let dnsmasqBin = qcDnsmasqBinary config
          listenArg = "--listen-address=" <> T.unpack gw
          rangeArg = "--dhcp-range=" <> T.unpack rangeStart <> "," <> T.unpack rangeEnd <> "," <> T.unpack mask <> ",12h"
          leaseArg = "--dhcp-leasefile=" <> leaseFile
      -- When NAT is enabled, add upstream DNS servers so dnsmasq can resolve internet names
      serverArgs <-
        if nat
          then map ("--server=" <>) <$> readHostDnsServers
          else pure []
      nsSpawn
        nsPid
        ( [ dnsmasqBin
          , "--conf-file=/dev/null"
          , "--bind-interfaces"
          , listenArg
          , "--except-interface=lo"
          , rangeArg
          , "--keep-in-foreground"
          , "--no-hosts"
          , leaseArg
          ]
            ++ serverArgs
        )

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

--------------------------------------------------------------------------------
-- pasta (NAT uplink)
--------------------------------------------------------------------------------

-- | Start pasta on the HOST to create a TAP interface inside the namespace
-- connected to the host's network. Returns the pasta PID.
--
-- pasta runs on the host (not inside the namespace) because it needs
-- access to the host's network stack to bridge traffic.
startPasta :: Int -> QemuConfig -> IO (Either Text Int)
startPasta nsPid config = do
  let pastaBin = qcPastaBinary config
      pastaArgs =
        [ "--config-net"
        , "--ns-ifname"
        , "pasta0"
        , "-f"
        , show nsPid
        ]
      cp =
        (proc pastaBin pastaArgs)
          { std_out = CreatePipe
          , std_err = CreatePipe
          }
  result <- try $ createProcess cp
  case result of
    Left (err :: SomeException) ->
      pure $ Left $ "Failed to start pasta: " <> T.pack (show err)
    Right (_, _, _, ph) -> do
      mPid <- getPid ph
      case mPid of
        Just pid -> pure $ Right (fromIntegral pid)
        Nothing -> pure $ Left "Failed to get pasta PID"

-- | Stop pasta by PID.
stopPasta :: Int -> IO ()
stopPasta pid = do
  result <- try $ signalProcess sigTERM (fromIntegral pid)
  case result of
    Left (_ :: SomeException) -> pure ()
    Right () -> pure ()

--------------------------------------------------------------------------------
-- nftables NAT
--------------------------------------------------------------------------------

-- | Enable IP forwarding inside the namespace.
enableIpForwarding :: Int -> IO (Either Text ())
enableIpForwarding nsPid =
  nsExec nsPid ["sysctl", "-w", "net.ipv4.ip_forward=1"]

-- | Create the nftables NAT table and postrouting chain inside the namespace.
setupNatTable :: Int -> QemuConfig -> IO (Either Text ())
setupNatTable nsPid config = do
  let nft = qcNftBinary config
  r1 <- nsExec nsPid [nft, "add", "table", "ip", "corvus_nat"]
  case r1 of
    Left err -> pure $ Left $ "Failed to create NAT table: " <> err
    Right () ->
      nsExec nsPid [nft, "add", "chain", "ip", "corvus_nat", "postrouting", "{ type nat hook postrouting priority 100 ; }"]

-- | Remove the nftables NAT table (and all its rules) from the namespace.
teardownNatTable :: Int -> QemuConfig -> IO (Either Text ())
teardownNatTable nsPid config = do
  let nft = qcNftBinary config
  nsExec nsPid [nft, "delete", "table", "ip", "corvus_nat"]

-- | Add a MASQUERADE rule for a network's subnet going out via pasta0.
addNatRule :: Int -> QemuConfig -> Int64 -> Text -> IO (Either Text ())
addNatRule nsPid config networkId subnet = do
  let nft = qcNftBinary config
      bridge = getBridgeName networkId
  nsExec
    nsPid
    [ nft
    , "add"
    , "rule"
    , "ip"
    , "corvus_nat"
    , "postrouting"
    , "oifname"
    , "pasta0"
    , "ip"
    , "saddr"
    , T.unpack subnet
    , "masquerade"
    , "comment"
    , bridge
    ]

-- | Remove the MASQUERADE rule for a network by finding its handle via comment.
removeNatRule :: Int -> QemuConfig -> Int64 -> IO (Either Text ())
removeNatRule nsPid config networkId = do
  let nft = qcNftBinary config
      bridge = getBridgeName networkId
  nsExec
    nsPid
    [ "sh"
    , "-c"
    , nft
        <> " -a list chain ip corvus_nat postrouting 2>/dev/null | "
        <> "grep 'comment.*"
        <> bridge
        <> "' | "
        <> "grep -o 'handle [0-9]*' | "
        <> "awk '{print $2}' | "
        <> "while read h; do "
        <> nft
        <> " delete rule ip corvus_nat postrouting handle $h; done"
    ]

--------------------------------------------------------------------------------
-- DNS resolution
--------------------------------------------------------------------------------

-- | Read host DNS servers from /etc/resolv.conf.
-- Falls back to Google DNS if resolv.conf is unavailable or empty.
readHostDnsServers :: IO [String]
readHostDnsServers = do
  content <- try (readFile "/etc/resolv.conf")
  case content of
    Left (_ :: SomeException) -> pure fallback
    Right text -> do
      let servers = mapMaybe parseNameserver (lines text)
      pure $ if null servers then fallback else servers
  where
    fallback = ["8.8.8.8", "8.8.4.4"]
    parseNameserver line =
      case words line of
        ("nameserver" : ip : _) -> Just ip
        _ -> Nothing
