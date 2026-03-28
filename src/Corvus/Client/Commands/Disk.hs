{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Disk and snapshot command handlers for the Corvus client.
module Corvus.Client.Commands.Disk
  ( -- * Disk command handlers
    handleDiskCreate
  , handleDiskCreateOverlay
  , handleDiskImport
  , handleDiskDelete
  , handleDiskResize
  , handleDiskList
  , handleDiskShow
  , handleDiskClone
  , handleDiskAttach
  , handleDiskDetach

    -- * Snapshot command handlers
  , handleSnapshotCreate
  , handleSnapshotDelete
  , handleSnapshotRollback
  , handleSnapshotMerge
  , handleSnapshotList

    -- * Parsers
  , parseFormat
  , parseInterface
  , parseCacheType
  , parseMedia

    -- * Formatters
  , printDiskInfo
  , printSnapshotInfo
  )
where

import Corvus.Client.Connection
import Corvus.Client.Output (isStructured, outputError, outputOk, outputOkWith, outputResult)
import Corvus.Client.Rpc
import Corvus.Client.Types (OutputFormat (..))
import Corvus.Model (CacheType, DriveFormat (..), DriveInterface, DriveMedia, EnumText (..))
import Corvus.Protocol (DiskImageInfo (..), SnapshotInfo (..))
import Data.Aeson (toJSON)
import Data.Int (Int64)
import Data.List (isPrefixOf)
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
handleDiskCreate :: OutputFormat -> Connection -> Text -> DriveFormat -> Int64 -> IO Bool
handleDiskCreate fmt conn name format sizeMb = do
  resp <- diskCreate conn name format sizeMb
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (DiskCreated diskId) -> do
      if isStructured fmt
        then outputOkWith fmt [("id", toJSON diskId)]
        else putStrLn $ "Disk image created with ID: " ++ show diskId
      pure True
    Right (DiskError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Error creating disk: " ++ T.unpack msg
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk overlay command
handleDiskCreateOverlay :: OutputFormat -> Connection -> Text -> Int64 -> Maybe Text -> IO Bool
handleDiskCreateOverlay fmt conn name baseDiskId optDirPath = do
  resp <- diskCreateOverlay conn name baseDiskId optDirPath
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (DiskCreated diskId) -> do
      if isStructured fmt
        then outputOkWith fmt [("id", toJSON diskId)]
        else putStrLn $ "Overlay created with ID: " ++ show diskId
      pure True
    Right DiskNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("Base disk with ID " <> T.pack (show baseDiskId) <> " not found")
        else putStrLn $ "Base disk with ID " ++ show baseDiskId ++ " not found."
      pure False
    Right (DiskError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Error creating overlay: " ++ T.unpack msg
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk import command (supports both local paths and HTTP/HTTPS URLs)
handleDiskImport :: OutputFormat -> Connection -> Text -> FilePath -> Maybe Text -> IO Bool
handleDiskImport fmt conn name path mFormatStr
  | isUrl = handleDiskImportFromUrl fmt conn name (T.pack path) mFormatStr
  | otherwise = handleDiskImportFromFile fmt conn name path mFormatStr
  where
    isUrl = "http://" `isPrefixOf` path || "https://" `isPrefixOf` path

-- | Import a disk from an HTTP/HTTPS URL (daemon downloads)
handleDiskImportFromUrl :: OutputFormat -> Connection -> Text -> Text -> Maybe Text -> IO Bool
handleDiskImportFromUrl fmt conn name url mFormatStr = do
  if isStructured fmt
    then pure ()
    else putStrLn $ "Downloading from URL: " ++ T.unpack url
  resp <- diskImportUrl conn name url mFormatStr
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (DiskCreated diskId) -> do
      if isStructured fmt
        then outputOkWith fmt [("id", toJSON diskId)]
        else putStrLn $ "Disk image imported with ID: " ++ show diskId
      pure True
    Right (DiskError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Error importing disk: " ++ T.unpack msg
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Import a disk from a local file path
handleDiskImportFromFile :: OutputFormat -> Connection -> Text -> FilePath -> Maybe Text -> IO Bool
handleDiskImportFromFile fmt conn name path mFormatStr = do
  exists <- doesFileExist path
  if not exists
    then do
      if isStructured fmt
        then outputError fmt "file_not_found" (T.pack $ "File not found: " ++ path)
        else putStrLn $ "Error: File not found: " ++ path
      pure False
    else do
      let ext = takeExtension path
          format = case mFormatStr of
            Just f -> parseFormat f
            Nothing -> detectFormat ext
      case format of
        Left err -> do
          if isStructured fmt
            then outputError fmt "invalid_format" err
            else putStrLn $ "Error: " ++ T.unpack err
          pure False
        Right fmt' -> do
          absPath <- canonicalizePath path
          basePath <- getBaseImagesPath
          let storedPath =
                if basePath `isPrefixOfPath` absPath
                  then makeRelative basePath absPath
                  else absPath
          resp <- diskRegister conn name (T.pack storedPath) fmt' Nothing
          case resp of
            Left err -> do
              if isStructured fmt
                then outputError fmt "rpc_error" (T.pack $ show err)
                else putStrLn $ "Error: " ++ show err
              pure False
            Right (DiskCreated diskId) -> do
              if isStructured fmt
                then outputOkWith fmt [("id", toJSON diskId)]
                else do
                  putStrLn $ "Disk image imported with ID: " ++ show diskId
                  if storedPath /= absPath
                    then putStrLn $ "Stored as relative path: " ++ storedPath
                    else putStrLn $ "Stored as absolute path: " ++ storedPath
              pure True
            Right (DiskError msg) -> do
              if isStructured fmt
                then outputError fmt "error" msg
                else putStrLn $ "Error importing disk: " ++ T.unpack msg
              pure False
            Right other -> do
              if isStructured fmt
                then outputError fmt "unexpected" (T.pack $ show other)
                else putStrLn $ "Unexpected response: " ++ show other
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
handleDiskDelete :: OutputFormat -> Connection -> Int64 -> IO Bool
handleDiskDelete fmt conn diskId = do
  resp <- diskDelete conn diskId
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right DiskOk -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn "Disk image deleted."
      pure True
    Right DiskNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("Disk with ID " <> T.pack (show diskId) <> " not found")
        else putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right (DiskInUse vmIds) -> do
      if isStructured fmt
        then outputError fmt "in_use" ("Disk is attached to VMs: " <> T.pack (show vmIds))
        else do
          putStrLn $ "Disk is attached to VMs: " ++ show vmIds
          putStrLn "Detach the disk first before deleting."
      pure False
    Right (DiskHasOverlays overlayIds) -> do
      if isStructured fmt
        then outputError fmt "has_overlays" ("Disk is used as backing image for overlays: " <> T.pack (show overlayIds))
        else do
          putStrLn $ "Disk is used as backing image for overlays: " ++ show overlayIds
          putStrLn "Delete the overlay disks first."
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk resize command
handleDiskResize :: OutputFormat -> Connection -> Int64 -> Int64 -> IO Bool
handleDiskResize fmt conn diskId newSizeMb = do
  resp <- diskResize conn diskId newSizeMb
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right DiskOk -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn $ "Disk resized to " ++ show newSizeMb ++ " MB."
      pure True
    Right DiskNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("Disk with ID " <> T.pack (show diskId) <> " not found")
        else putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right VmMustBeStopped -> do
      if isStructured fmt
        then outputError fmt "vm_must_be_stopped" "VM must be stopped for this operation"
        else putStrLn "Cannot resize disk while VM is running. Stop the VM first."
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk list command
handleDiskList :: OutputFormat -> Connection -> IO Bool
handleDiskList fmt conn = do
  resp <- diskList conn
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (DiskListResult disks) -> do
      if isStructured fmt
        then outputResult fmt disks
        else do
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
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk show command
handleDiskShow :: OutputFormat -> Connection -> Int64 -> IO Bool
handleDiskShow fmt conn diskId = do
  resp <- diskShow conn diskId
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (DiskInfo info) -> do
      if isStructured fmt
        then outputResult fmt info
        else printDiskDetails info
      pure True
    Right DiskNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("Disk with ID " <> T.pack (show diskId) <> " not found")
        else putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk clone command
handleDiskClone :: OutputFormat -> Connection -> Text -> Int64 -> Maybe Text -> IO Bool
handleDiskClone fmt conn name baseDiskId optionalPath = do
  resp <- diskClone conn name baseDiskId optionalPath
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (DiskCreated diskId) -> do
      if isStructured fmt
        then outputOkWith fmt [("id", toJSON diskId)]
        else putStrLn $ "Disk cloned successfully. New disk ID: " ++ show diskId
      pure True
    Right DiskNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("Base disk with ID " <> T.pack (show baseDiskId) <> " not found")
        else putStrLn $ "Base disk with ID " ++ show baseDiskId ++ " not found."
      pure False
    Right VmMustBeStopped -> do
      if isStructured fmt
        then outputError fmt "vm_must_be_stopped" "VM must be stopped for this operation"
        else putStrLn "Error: Disk is attached to a running VM. Please stop the VM before cloning."
      pure False
    Right (DiskError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Error cloning disk: " ++ T.unpack msg
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk attach command
handleDiskAttach
  :: OutputFormat
  -> Connection
  -> Int64
  -> Int64
  -> DriveInterface
  -> Maybe DriveMedia
  -> Bool
  -> Bool
  -> CacheType
  -> IO Bool
handleDiskAttach fmt conn vmId diskId iface media readOnly discard cache = do
  resp <- diskAttach conn vmId diskId iface media readOnly discard cache
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (DriveAttached driveId) -> do
      if isStructured fmt
        then outputOkWith fmt [("id", toJSON driveId)]
        else putStrLn $ "Disk attached. Drive ID: " ++ show driveId
      pure True
    Right DiskNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("Disk with ID " <> T.pack (show diskId) <> " not found")
        else putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right DiskVmNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("VM with ID " <> T.pack (show vmId) <> " not found")
        else putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right (DiskHasOverlays overlayIds) -> do
      if isStructured fmt
        then outputError fmt "has_overlays" ("Disk is used as backing image for overlays: " <> T.pack (show overlayIds))
        else do
          putStrLn $ "Error: Disk is used as backing image for overlays: " ++ show overlayIds
          putStrLn "Base images must be attached in read-only mode using --read-only."
      pure False
    Right (DiskError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Error attaching disk: " ++ T.unpack msg
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle disk detach command
handleDiskDetach :: OutputFormat -> Connection -> Int64 -> Int64 -> IO Bool
handleDiskDetach fmt conn vmId driveId = do
  resp <- diskDetach conn vmId driveId
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right DiskOk -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn "Disk detached."
      pure True
    Right DriveNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("Drive with ID " <> T.pack (show driveId) <> " not found")
        else putStrLn $ "Drive with ID " ++ show driveId ++ " not found."
      pure False
    Right DiskVmNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("VM with ID " <> T.pack (show vmId) <> " not found")
        else putStrLn $ "VM with ID " ++ show vmId ++ " not found."
      pure False
    Right (DiskError msg) -> do
      if isStructured fmt
        then outputError fmt "error" msg
        else putStrLn $ "Error detaching disk: " ++ T.unpack msg
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

--------------------------------------------------------------------------------
-- Snapshot Command Handlers
--------------------------------------------------------------------------------

-- | Handle snapshot create command
handleSnapshotCreate :: OutputFormat -> Connection -> Int64 -> Text -> IO Bool
handleSnapshotCreate fmt conn diskId name = do
  resp <- snapshotCreate conn diskId name
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (SnapshotCreated snapId) -> do
      if isStructured fmt
        then outputOkWith fmt [("id", toJSON snapId)]
        else putStrLn $ "Snapshot created with ID: " ++ show snapId
      pure True
    Right SnapshotDiskNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("Disk with ID " <> T.pack (show diskId) <> " not found")
        else putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right (SnapshotFormatNotSupported msg) -> do
      if isStructured fmt
        then outputError fmt "format_not_supported" msg
        else putStrLn $ "Error: " ++ T.unpack msg
      pure False
    Right SnapshotVmMustBeStopped -> do
      if isStructured fmt
        then outputError fmt "vm_must_be_stopped" "VM must be stopped for this operation"
        else putStrLn "Cannot create snapshot while VM is running. Stop the VM first."
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle snapshot delete command
handleSnapshotDelete :: OutputFormat -> Connection -> Int64 -> Int64 -> IO Bool
handleSnapshotDelete fmt conn diskId snapshotId = do
  resp <- snapshotDelete conn diskId snapshotId
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right SnapshotOk -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn "Snapshot deleted."
      pure True
    Right SnapshotNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("Snapshot with ID " <> T.pack (show snapshotId) <> " not found")
        else putStrLn $ "Snapshot with ID " ++ show snapshotId ++ " not found."
      pure False
    Right SnapshotDiskNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("Disk with ID " <> T.pack (show diskId) <> " not found")
        else putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right SnapshotVmMustBeStopped -> do
      if isStructured fmt
        then outputError fmt "vm_must_be_stopped" "VM must be stopped for this operation"
        else putStrLn "Cannot delete snapshot while VM is running. Stop the VM first."
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle snapshot rollback command
handleSnapshotRollback :: OutputFormat -> Connection -> Int64 -> Int64 -> IO Bool
handleSnapshotRollback fmt conn diskId snapshotId = do
  resp <- snapshotRollback conn diskId snapshotId
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right SnapshotOk -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn "Rollback complete."
      pure True
    Right SnapshotNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("Snapshot with ID " <> T.pack (show snapshotId) <> " not found")
        else putStrLn $ "Snapshot with ID " ++ show snapshotId ++ " not found."
      pure False
    Right SnapshotDiskNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("Disk with ID " <> T.pack (show diskId) <> " not found")
        else putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right SnapshotVmMustBeStopped -> do
      if isStructured fmt
        then outputError fmt "vm_must_be_stopped" "VM must be stopped for this operation"
        else putStrLn "Cannot rollback while VM is running. Stop the VM first."
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle snapshot merge command
handleSnapshotMerge :: OutputFormat -> Connection -> Int64 -> Int64 -> IO Bool
handleSnapshotMerge fmt conn diskId snapshotId = do
  resp <- snapshotMerge conn diskId snapshotId
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right SnapshotOk -> do
      if isStructured fmt
        then outputOk fmt
        else putStrLn "Snapshot merged."
      pure True
    Right SnapshotNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("Snapshot with ID " <> T.pack (show snapshotId) <> " not found")
        else putStrLn $ "Snapshot with ID " ++ show snapshotId ++ " not found."
      pure False
    Right SnapshotDiskNotFound -> do
      if isStructured fmt
        then outputError fmt "not_found" ("Disk with ID " <> T.pack (show diskId) <> " not found")
        else putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right SnapshotVmMustBeStopped -> do
      if isStructured fmt
        then outputError fmt "vm_must_be_stopped" "VM must be stopped for this operation"
        else putStrLn "Cannot merge while VM is running. Stop the VM first."
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
      pure False

-- | Handle snapshot list command
handleSnapshotList :: OutputFormat -> Connection -> Int64 -> IO Bool
handleSnapshotList fmt conn diskId = do
  resp <- snapshotList conn diskId
  case resp of
    Left err -> do
      if isStructured fmt
        then outputError fmt "rpc_error" (T.pack $ show err)
        else putStrLn $ "Error: " ++ show err
      pure False
    Right (SnapshotListResult snaps) -> do
      if isStructured fmt
        then outputResult fmt snaps
        else do
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
      if isStructured fmt
        then outputError fmt "not_found" ("Disk with ID " <> T.pack (show diskId) <> " not found")
        else putStrLn $ "Disk with ID " ++ show diskId ++ " not found."
      pure False
    Right other -> do
      if isStructured fmt
        then outputError fmt "unexpected" (T.pack $ show other)
        else putStrLn $ "Unexpected response: " ++ show other
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
