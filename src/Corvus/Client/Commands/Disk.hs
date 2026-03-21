{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Disk and snapshot command handlers for the Corvus client.
module Corvus.Client.Commands.Disk
  ( -- * Disk command handlers
    handleDiskCreate,
    handleDiskCreateOverlay,
    handleDiskImport,
    handleDiskDelete,
    handleDiskResize,
    handleDiskList,
    handleDiskShow,
    handleDiskClone,
    handleDiskAttach,
    handleDiskDetach,

    -- * Snapshot command handlers
    handleSnapshotCreate,
    handleSnapshotDelete,
    handleSnapshotRollback,
    handleSnapshotMerge,
    handleSnapshotList,

    -- * Parsers
    parseFormat,
    parseInterface,
    parseCacheType,
    parseMedia,

    -- * Formatters
    printDiskInfo,
    printSnapshotInfo,
  )
where

import Corvus.Client.Connection
import Corvus.Client.Rpc
import Corvus.Model (CacheType, DriveFormat (..), DriveInterface, DriveMedia, EnumText (..))
import Corvus.Protocol (DiskImageInfo (..), SnapshotInfo (..))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath (makeRelative, takeExtension, (</>))
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

-- | Parse format string to DriveFormat
parseFormat :: Text -> Either Text DriveFormat
parseFormat = enumFromText

-- | Parse interface string to DriveInterface
parseInterface :: Text -> Either Text DriveInterface
parseInterface = enumFromText

-- | Parse cache type string to CacheType
parseCacheType :: Text -> Either Text CacheType
parseCacheType = enumFromText

-- | Parse media string to DriveMedia
parseMedia :: Text -> Either Text DriveMedia
parseMedia = enumFromText

--------------------------------------------------------------------------------
-- Disk Command Handlers
--------------------------------------------------------------------------------

-- | Handle disk create command
handleDiskCreate :: Connection -> Text -> DriveFormat -> Int64 -> IO Bool
handleDiskCreate conn name format sizeMb = do
  resp <- diskCreate conn name format sizeMb
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (DiskCreated diskId) -> do
      putStrLn $ "Disk image created with ID: " ++ show diskId
      pure True
    Right (DiskError msg) -> do
      putStrLn $ "Error creating disk: " ++ T.unpack msg
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk overlay command
handleDiskCreateOverlay :: Connection -> Text -> Int64 -> IO Bool
handleDiskCreateOverlay conn name baseDiskId = do
  resp <- diskCreateOverlay conn name baseDiskId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (DiskCreated diskId) -> do
      putStrLn $ "Overlay created with ID: " ++ show diskId
      pure True
    Right DiskNotFound -> do
      putStrLn $ "Base disk with ID " ++ show baseDiskId ++ " not found."
      pure False
    Right (DiskError msg) -> do
      putStrLn $ "Error creating overlay: " ++ T.unpack msg
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk import command
handleDiskImport :: Connection -> Text -> FilePath -> Maybe Text -> IO Bool
handleDiskImport conn name path mFormatStr = do
  exists <- doesFileExist path
  if not exists
    then do
      putStrLn $ "Error: File not found: " ++ path
      pure False
    else do
      let ext = takeExtension path
          format = case mFormatStr of
            Just f -> parseFormat f
            Nothing -> detectFormat ext
      case format of
        Left err -> do
          putStrLn $ "Error: " ++ T.unpack err
          pure False
        Right fmt -> do
          absPath <- canonicalizePath path
          basePath <- getBaseImagesPath
          let storedPath =
                if basePath `isPrefixOfPath` absPath
                  then makeRelative basePath absPath
                  else absPath
          resp <- diskRegister conn name (T.pack storedPath) fmt Nothing
          case resp of
            Left err -> do
              putStrLn $ "Error: " ++ show err
              pure False
            Right (DiskCreated diskId) -> do
              putStrLn $ "Disk image imported with ID: " ++ show diskId
              if storedPath /= absPath
                then putStrLn $ "Stored as relative path: " ++ storedPath
                else putStrLn $ "Stored as absolute path: " ++ storedPath
              pure True
            Right (DiskError msg) -> do
              putStrLn $ "Error importing disk: " ++ T.unpack msg
              pure False
            Right other -> do
              putStrLn $ "Unexpected response: " ++ show other
              pure False
  where
    detectFormat :: String -> Either Text DriveFormat
    detectFormat ".qcow2" = Right FormatQcow2
    detectFormat ".raw" = Right FormatRaw
    detectFormat ".img" = Right FormatRaw
    detectFormat ".vmdk" = Right FormatVmdk
    detectFormat ".vdi" = Right FormatVdi
    detectFormat ext' = Left $ "Unknown format for extension: " <> T.pack ext' <> ". Use --format to specify."

    isPrefixOfPath :: FilePath -> FilePath -> Bool
    isPrefixOfPath prefix path' =
      let prefixWithSlash = if last prefix == '/' then prefix else prefix ++ "/"
       in prefixWithSlash == take (length prefixWithSlash) path' || prefix == path'

-- | Get base images path ($HOME/VMs by default)
getBaseImagesPath :: IO FilePath
getBaseImagesPath = do
  mHome <- lookupEnv "HOME"
  pure $ fromMaybe "/var/lib/qemu" mHome </> "VMs"

-- | Handle disk delete command
handleDiskDelete :: Connection -> Int64 -> IO Bool
handleDiskDelete conn diskId = do
  resp <- diskDelete conn diskId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right DiskOk -> do
      putStrLn "Disk image deleted."
      pure True
    Right DiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right (DiskInUse vmIds) -> do
      putStrLn $ "Disk is attached to VMs: " ++ show vmIds
      putStrLn "Detach the disk first before deleting."
      pure False
    Right (DiskHasOverlays overlayIds) -> do
      putStrLn $ "Disk is used as backing image for overlays: " ++ show overlayIds
      putStrLn "Delete the overlay disks first."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk resize command
handleDiskResize :: Connection -> Int64 -> Int64 -> IO Bool
handleDiskResize conn diskId newSizeMb = do
  resp <- diskResize conn diskId newSizeMb
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right DiskOk -> do
      putStrLn $ "Disk resized to " ++ show newSizeMb ++ " MB."
      pure True
    Right DiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right VmMustBeStopped -> do
      putStrLn "Cannot resize disk while VM is running. Stop the VM first."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk list command
handleDiskList :: Connection -> IO Bool
handleDiskList conn = do
  resp <- diskList conn
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (DiskListResult disks) -> do
      if null disks
        then putStrLn "No disk images found."
        else do
          putStrLn $
            printf
              "%-6s %-20s %-8s %10s %-20s"
              ("ID" :: String)
              ("NAME" :: String)
              ("FORMAT" :: String)
              ("SIZE_MB" :: String)
              ("ATTACHED_TO" :: String)
          putStrLn $ replicate 70 '-'
          mapM_ printDiskInfo disks
      pure True
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk show command
handleDiskShow :: Connection -> Int64 -> IO Bool
handleDiskShow conn diskId = do
  resp <- diskShow conn diskId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (DiskInfo info) -> do
      printDiskDetails info
      pure True
    Right DiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk clone command
handleDiskClone :: Connection -> Text -> Int64 -> Maybe Text -> IO Bool
handleDiskClone conn name baseDiskId optionalPath = do
  resp <- diskClone conn name baseDiskId optionalPath
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (DiskCreated diskId) -> do
      putStrLn $ "Disk cloned successfully. New disk ID: " ++ show diskId
      pure True
    Right DiskNotFound -> do
      putStrLn $ "Base disk with ID " ++ show baseDiskId ++ " not found."
      pure False
    Right VmMustBeStopped -> do
      putStrLn "Error: Disk is attached to a running VM. Please stop the VM before cloning."
      pure False
    Right (DiskError msg) -> do
      putStrLn $ "Error cloning disk: " ++ T.unpack msg
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk attach command
handleDiskAttach ::
  Connection ->
  Int64 ->
  Int64 ->
  DriveInterface ->
  Maybe DriveMedia ->
  Bool ->
  Bool ->
  CacheType ->
  IO Bool
handleDiskAttach conn vmId diskId iface media readOnly discard cache = do
  resp <- diskAttach conn vmId diskId iface media readOnly discard cache
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (DriveAttached driveId) -> do
      putStrLn $ "Disk attached. Drive ID: " ++ show driveId
      pure True
    Right DiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right DiskVmNotFound -> do
      putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right (DiskHasOverlays overlayIds) -> do
      putStrLn $ "Error: Disk is used as backing image for overlays: " ++ show overlayIds
      putStrLn "Base images must be attached in read-only mode using --read-only."
      pure False
    Right (DiskError msg) -> do
      putStrLn $ "Error attaching disk: " ++ T.unpack msg
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk detach command
handleDiskDetach :: Connection -> Int64 -> Int64 -> IO Bool
handleDiskDetach conn vmId driveId = do
  resp <- diskDetach conn vmId driveId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right DiskOk -> do
      putStrLn "Disk detached."
      pure True
    Right DriveNotFound -> do
      putStrLn $ "Drive with ID " ++ show driveId ++ " not found."
      pure False
    Right DiskVmNotFound -> do
      putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right (DiskError msg) -> do
      putStrLn $ "Error detaching disk: " ++ T.unpack msg
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

--------------------------------------------------------------------------------
-- Snapshot Command Handlers
--------------------------------------------------------------------------------

-- | Handle snapshot create command
handleSnapshotCreate :: Connection -> Int64 -> Text -> IO Bool
handleSnapshotCreate conn diskId name = do
  resp <- snapshotCreate conn diskId name
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (SnapshotCreated snapId) -> do
      putStrLn $ "Snapshot created with ID: " ++ show snapId
      pure True
    Right SnapshotDiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right (SnapshotFormatNotSupported msg) -> do
      putStrLn $ "Error: " ++ T.unpack msg
      pure False
    Right SnapshotVmMustBeStopped -> do
      putStrLn "Cannot create snapshot while VM is running. Stop the VM first."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle snapshot delete command
handleSnapshotDelete :: Connection -> Int64 -> Int64 -> IO Bool
handleSnapshotDelete conn diskId snapshotId = do
  resp <- snapshotDelete conn diskId snapshotId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right SnapshotOk -> do
      putStrLn "Snapshot deleted."
      pure True
    Right SnapshotNotFound -> do
      putStrLn $ "Snapshot with ID " ++ show snapshotId ++ " not found."
      pure False
    Right SnapshotDiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right SnapshotVmMustBeStopped -> do
      putStrLn "Cannot delete snapshot while VM is running. Stop the VM first."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle snapshot rollback command
handleSnapshotRollback :: Connection -> Int64 -> Int64 -> IO Bool
handleSnapshotRollback conn diskId snapshotId = do
  resp <- snapshotRollback conn diskId snapshotId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right SnapshotOk -> do
      putStrLn "Rollback complete."
      pure True
    Right SnapshotNotFound -> do
      putStrLn $ "Snapshot with ID " ++ show snapshotId ++ " not found."
      pure False
    Right SnapshotDiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right SnapshotVmMustBeStopped -> do
      putStrLn "Cannot rollback while VM is running. Stop the VM first."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle snapshot merge command
handleSnapshotMerge :: Connection -> Int64 -> Int64 -> IO Bool
handleSnapshotMerge conn diskId snapshotId = do
  resp <- snapshotMerge conn diskId snapshotId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right SnapshotOk -> do
      putStrLn "Snapshot merged."
      pure True
    Right SnapshotNotFound -> do
      putStrLn $ "Snapshot with ID " ++ show snapshotId ++ " not found."
      pure False
    Right SnapshotDiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right SnapshotVmMustBeStopped -> do
      putStrLn "Cannot merge while VM is running. Stop the VM first."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle snapshot list command
handleSnapshotList :: Connection -> Int64 -> IO Bool
handleSnapshotList conn diskId = do
  resp <- snapshotList conn diskId
  case resp of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      pure False
    Right (SnapshotListResult snaps) -> do
      if null snaps
        then putStrLn "No snapshots found."
        else do
          putStrLn $
            printf
              "%-6s %-30s %-20s %10s"
              ("ID" :: String)
              ("NAME" :: String)
              ("CREATED" :: String)
              ("SIZE_MB" :: String)
          putStrLn $ replicate 70 '-'
          mapM_ printSnapshotInfo snaps
      pure True
    Right SnapshotDiskNotFound -> do
      putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right other -> do
      putStrLn $ "Unexpected response: " ++ show other
      pure False

--------------------------------------------------------------------------------
-- Printers
--------------------------------------------------------------------------------

-- | Print disk image info in table format
printDiskInfo :: DiskImageInfo -> IO ()
printDiskInfo d =
  putStrLn $
    printf
      "%-6d %-20s %-8s %10s %-20s"
      (diiId d)
      (T.unpack $ diiName d)
      (T.unpack $ enumToText $ diiFormat d)
      (maybe "-" show $ diiSizeMb d)
      (if null (diiAttachedTo d) then "-" else show (diiAttachedTo d))

-- | Print disk image details
printDiskDetails :: DiskImageInfo -> IO ()
printDiskDetails d = do
  putStrLn $ "Disk ID:     " ++ show (diiId d)
  putStrLn $ "Name:        " ++ T.unpack (diiName d)
  putStrLn $ "File Path:   " ++ T.unpack (diiFilePath d)
  putStrLn $ "Format:      " ++ T.unpack (enumToText $ diiFormat d)
  putStrLn $ "Size (MB):   " ++ maybe "(unknown)" show (diiSizeMb d)
  putStrLn $ "Created:     " ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (diiCreatedAt d)
  putStrLn $ "Attached to: " ++ if null (diiAttachedTo d) then "(none)" else show (diiAttachedTo d)
  case diiBackingImageName d of
    Nothing -> pure ()
    Just backingName ->
      putStrLn $ "Backing:     " ++ T.unpack backingName ++ " (ID: " ++ maybe "?" show (diiBackingImageId d) ++ ")"

-- | Print snapshot info in table format
printSnapshotInfo :: SnapshotInfo -> IO ()
printSnapshotInfo s =
  putStrLn $
    printf
      "%-6d %-30s %-20s %10s"
      (sniId s)
      (T.unpack $ sniName s)
      (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (sniCreatedAt s))
      (maybe "-" show $ sniSizeMb s)
