{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | QEMU command line generation.
-- Builds complete QEMU command lines from VM configuration.
module Corvus.Qemu.Command
  ( -- * Command generation
    generateQemuCommand
  , generateQemuCommandIO
  , generateQemuCommandWithSockets

    -- * Command building
  , buildCommandWithSockets
  , driveArgs
  , netArgs
  , sharedDirArgs
  )
where

import Control.Monad.IO.Class (liftIO)
import Corvus.Model
import Corvus.Qemu.Config
  ( QemuConfig (..)
  , getEffectiveBasePath
  )
import Corvus.Qemu.Runtime
  ( getMonitorSocket
  , getQmpSocket
  , getSerialSocket
  , getSpiceSocket
  , getVdeSwitchSocket
  , getVmRuntimeDir
  )
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend, SqlPersistT, toSqlKey)
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- Command Generation (Public API)
--------------------------------------------------------------------------------

-- | Generate QEMU command line from database (as single string)
generateQemuCommandIO :: Pool SqlBackend -> QemuConfig -> Int64 -> IO (Maybe String)
generateQemuCommandIO pool config vmId = do
  basePath <- getEffectiveBasePath config
  monitorSock <- getMonitorSocket vmId
  qmpSock <- getQmpSocket vmId
  spiceSock <- getSpiceSocket vmId
  serialSock <- getSerialSocket vmId
  vmRuntimeDir <- getVmRuntimeDir vmId
  result <- runSqlPool (generateQemuCommandWithSockets config vmId basePath monitorSock qmpSock spiceSock serialSock vmRuntimeDir) pool
  pure $ case result of
    Nothing -> Nothing
    Just (binary, args) -> Just $ unwords (binary : args)

-- | Generate QEMU command line (runs in SqlPersistT)
generateQemuCommand :: QemuConfig -> Int64 -> SqlPersistT IO (Maybe String)
generateQemuCommand config vmId = do
  let key = toSqlKey vmId :: VmId
  mVm <- get key
  case mVm of
    Nothing -> pure Nothing
    Just vm -> do
      drives <- selectList [DriveVmId ==. key] []
      netIfs <- selectList [NetworkInterfaceVmId ==. key] []
      sharedDirs <- selectList [SharedDirVmId ==. key] []
      -- Fetch disk images for each drive
      driveWithImages <- mapM fetchDriveWithImage drives
      -- Get actual paths from environment
      basePath <- liftIO $ getEffectiveBasePath config
      monitorSock <- liftIO $ getMonitorSocket vmId
      qmpSock <- liftIO $ getQmpSocket vmId
      spiceSock <- liftIO $ getSpiceSocket vmId
      serialSock <- liftIO $ getSerialSocket vmId
      vmRuntimeDir <- liftIO $ getVmRuntimeDir vmId
      -- Resolve network socket paths for VDE interfaces with networkId
      resolvedNetIfs <- liftIO $ mapM (resolveNetIfSocket . entityVal) netIfs
      let (binary, args) =
            buildCommandWithSockets
              config
              vmId
              vm
              basePath
              monitorSock
              qmpSock
              spiceSock
              serialSock
              vmRuntimeDir
              driveWithImages
              resolvedNetIfs
              (map entityVal sharedDirs)
      pure $ Just $ unwords (binary : args)

-- | Fetch a drive with its disk image
fetchDriveWithImage :: Entity Drive -> SqlPersistT IO (Drive, Maybe DiskImage)
fetchDriveWithImage (Entity _ drive) = do
  let diskImageKey = driveDiskImageId drive
  mDiskImage <- get diskImageKey
  pure (drive, mDiskImage)

-- | Generate QEMU command with socket paths (returns binary and args separately)
generateQemuCommandWithSockets
  :: QemuConfig
  -> Int64
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> SqlPersistT IO (Maybe (FilePath, [String]))
generateQemuCommandWithSockets config vmId basePath monitorSock qmpSock spiceSock serialSock vmRuntimeDir = do
  let key = toSqlKey vmId :: VmId
  mVm <- get key
  case mVm of
    Nothing -> pure Nothing
    Just vm -> do
      drives <- selectList [DriveVmId ==. key] []
      driveWithImages <- mapM fetchDriveWithImage drives
      netIfs <- selectList [NetworkInterfaceVmId ==. key] []
      sharedDirs <- selectList [SharedDirVmId ==. key] []
      -- Resolve network socket paths for VDE interfaces with networkId
      resolvedNetIfs <- liftIO $ mapM (resolveNetIfSocket . entityVal) netIfs
      pure $
        Just $
          buildCommandWithSockets
            config
            vmId
            vm
            basePath
            monitorSock
            qmpSock
            spiceSock
            serialSock
            vmRuntimeDir
            driveWithImages
            resolvedNetIfs
            (map entityVal sharedDirs)

--------------------------------------------------------------------------------
-- Command Building
--------------------------------------------------------------------------------

-- | Build the complete QEMU command with socket paths
buildCommandWithSockets
  :: QemuConfig
  -> Int64
  -> Vm
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> [(Drive, Maybe DiskImage)]
  -> [NetworkInterface]
  -> [SharedDir]
  -> (FilePath, [String])
buildCommandWithSockets QemuConfig {..} vmId vm basePath monitorSock qmpSock spiceSock serialSock vmRuntimeDir drives netIfs sharedDirs =
  ( qcQemuBinary
  , concatMap
      (filter (not . null))
      [ ["-name", T.unpack (vmName vm) ++ ",process=corvus-vm-" ++ show vmId]
      , ["-machine", "type=q35,accel=kvm"]
      , ["-cpu", "host"]
      , ["-enable-kvm"]
      , memoryArgs
      , ["-smp", show (vmCpuCount vm)]
      , displayArgs
      , monitorArgs
      , concatMap (driveArgs basePath) (zip [0 ..] drives)
      , concatMap netArgs (zip [0 ..] netIfs)
      , concatMap (sharedDirArgs vmRuntimeDir) (zip [0 ..] sharedDirs)
      ]
  )
  where
    -- Memory configuration: use shared memory if there are shared directories
    memSize = fromMaybe (show (vmRamMb vm) ++ "M") qcSharedMemSize
    memoryArgs
      | null sharedDirs = ["-m", show (vmRamMb vm)]
      | otherwise =
          [ "-m"
          , show (vmRamMb vm)
          , "-object"
          , "memory-backend-memfd,id=mem,size=" ++ memSize ++ ",share=on"
          , "-numa"
          , "node,memdev=mem"
          ]

    displayArgs
      | vmHeadless vm = serialConsoleArgs
      | otherwise = spiceArgs ++ usbRedirArgs

    serialConsoleArgs =
      [ "-chardev"
      , "socket,id=serial0,path=" ++ serialSock ++ ",server=on,wait=off"
      , "-serial"
      , "chardev:serial0"
      , "-display"
      , "none"
      , "-vga"
      , "none"
      ]

    spiceArgs =
      [ "-spice"
      , "unix=on,addr=" ++ spiceSock ++ ",disable-ticketing=on"
      , "-chardev"
      , "spicevmc,id=vdagent,name=vdagent"
      , "-device"
      , "virtio-vga"
      , "-device"
      , "virtio-serial"
      , "-device"
      , "virtserialport,chardev=vdagent,name=com.redhat.spice.0"
      ]

    -- USB redirection over SPICE (3 USB devices)
    usbRedirArgs =
      [ "-device"
      , "nec-usb-xhci,id=xhci"
      , "-chardev"
      , "spicevmc,id=usbredirchardev1,name=usbredir"
      , "-device"
      , "usb-redir,chardev=usbredirchardev1,id=usbredirdev1"
      , "-chardev"
      , "spicevmc,id=usbredirchardev2,name=usbredir"
      , "-device"
      , "usb-redir,chardev=usbredirchardev2,id=usbredirdev2"
      , "-chardev"
      , "spicevmc,id=usbredirchardev3,name=usbredir"
      , "-device"
      , "usb-redir,chardev=usbredirchardev3,id=usbredirdev3"
      ]

    -- monitor for human interaction
    monitorArgs =
      [ "-monitor"
      , "unix:" ++ monitorSock ++ ",server,nowait"
      , "-qmp"
      , "unix:" ++ qmpSock ++ ",server,nowait"
      ]

--------------------------------------------------------------------------------
-- Drive Arguments
--------------------------------------------------------------------------------

-- | Generate drive arguments
-- Resolves relative paths against the base path
driveArgs :: FilePath -> (Int, (Drive, Maybe DiskImage)) -> [String]
driveArgs basePath (idx, (drive, mDiskImage)) = case mDiskImage of
  Nothing -> [] -- Skip drives with missing disk images
  Just diskImage -> case driveInterface drive of
    -- Pflash uses simpler format (for UEFI firmware)
    InterfacePflash ->
      [ "-drive"
      , intercalate "," $
          catMaybes
            [ Just $ "file=" ++ filePath diskImage
            , Just $ "format=" ++ T.unpack (enumToText $ diskImageFormat diskImage)
            , Just "if=pflash"
            , if driveReadOnly drive then Just "readonly=on" else Nothing
            ]
      ]
    -- Regular drives
    _ ->
      [ "-drive"
      , intercalate "," $
          catMaybes
            [ Just $ "file=" ++ filePath diskImage
            , Just $ "format=" ++ T.unpack (enumToText $ diskImageFormat diskImage)
            , Just $ "if=" ++ interfaceForQemu (driveInterface drive)
            , fmap (\m -> "media=" ++ T.unpack (enumToText m)) (driveMedia drive)
            , Just $ "cache=" ++ T.unpack (enumToText $ driveCacheType drive)
            , if driveDiscard drive then Just "discard=on" else Just "discard=off"
            , if driveReadOnly drive then Just "readonly=on" else Nothing
            ]
      ]
  where
    -- Resolve relative paths against base path
    filePath diskImage =
      let rawPath = T.unpack (diskImageFilePath diskImage)
       in if "/" `isPrefixOf` rawPath
            then rawPath -- Absolute path
            else basePath </> rawPath -- Relative path

-- | Check if a string starts with a prefix
isPrefixOf :: String -> String -> Bool
isPrefixOf prefix str = take (length prefix) str == prefix

-- | QEMU interface string (special case: nvme uses "none")
interfaceForQemu :: DriveInterface -> String
interfaceForQemu InterfaceNvme = "none" -- NVMe uses -device instead
interfaceForQemu iface = T.unpack (enumToText iface)

--------------------------------------------------------------------------------
-- Network Arguments
--------------------------------------------------------------------------------

-- | Generate network arguments
netArgs :: (Int, NetworkInterface) -> [String]
netArgs (idx, netIf) =
  netdevArgs ++ deviceArgs
  where
    netId = "net" ++ show idx
    hostDev = T.unpack (networkInterfaceHostDevice netIf)

    netdevArgs = case networkInterfaceInterfaceType netIf of
      NetUser ->
        -- For user mode, hostDevice can contain options like "hostfwd=tcp::2222-:22"
        let userOpts = if null hostDev then "" else "," ++ hostDev
         in ["-netdev", "user,id=" ++ netId ++ userOpts]
      NetTap ->
        ["-netdev", "tap,id=" ++ netId ++ ",ifname=" ++ hostDev ++ ",script=no,downscript=no"]
      NetBridge ->
        ["-netdev", "bridge,id=" ++ netId ++ ",br=" ++ hostDev]
      NetMacvtap ->
        ["-netdev", "tap,id=" ++ netId ++ ",fd=3"] -- macvtap uses fd passing
      NetVde ->
        ["-netdev", "vde,id=" ++ netId ++ ",sock=" ++ hostDev]

    deviceArgs =
      ["-device", "virtio-net-pci,netdev=" ++ netId ++ ",mac=" ++ T.unpack (networkInterfaceMacAddress netIf)]

--------------------------------------------------------------------------------
-- Shared Directory Arguments (virtiofs)
--------------------------------------------------------------------------------

-- | Generate shared directory arguments for virtiofs
sharedDirArgs :: FilePath -> (Int, SharedDir) -> [String]
sharedDirArgs vmRuntimeDir (idx, dir) =
  chardevArgs ++ deviceArgs
  where
    charId = "virtiofs" ++ show idx
    tag = T.unpack (sharedDirTag dir)
    socketPath = vmRuntimeDir </> "virtiofsd-" ++ tag ++ ".sock"

    chardevArgs =
      ["-chardev", "socket,id=" ++ charId ++ ",path=" ++ socketPath]

    deviceArgs =
      ["-device", "vhost-user-fs-pci,chardev=" ++ charId ++ ",tag=" ++ tag]

--------------------------------------------------------------------------------
-- Network Socket Resolution
--------------------------------------------------------------------------------

-- | Resolve VDE socket path for network interfaces with a networkId.
-- Substitutes the socket path into hostDevice so that netArgs stays pure.
resolveNetIfSocket :: NetworkInterface -> IO NetworkInterface
resolveNetIfSocket netIf = case networkInterfaceNetworkId netIf of
  Just nwKey -> do
    socketPath <- getVdeSwitchSocket (fromSqlKey nwKey)
    pure $ netIf {networkInterfaceHostDevice = T.pack socketPath}
  Nothing -> pure netIf
