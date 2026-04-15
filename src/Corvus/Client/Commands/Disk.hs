{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Disk and snapshot command handlers for the Corvus client.
module Corvus.Client.Commands.Disk
  ( -- * Disk command handlers
    handleDiskCreate
  , handleDiskCreateOverlay
  , handleDiskRegister
  , handleDiskImport
  , handleDiskDelete
  , handleDiskResize
  , handleDiskList
  , handleDiskShow
  , handleDiskClone
  , handleDiskRebase
  , handleDiskRefresh
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

import Control.Monad (unless, when)
import Corvus.Client.Connection
import Corvus.Client.Output (emitError, emitOk, emitOkWith, emitResult, emitRpcError, isStructured, printField, printTableHeader)
import Corvus.Client.Rpc
import Corvus.Client.Types (OutputFormat (..), WaitOptions (..))
import Corvus.Model (CacheType, DriveFormat (..), DriveInterface, DriveMedia, EnumText (..))
import Corvus.Protocol (DiskImageInfo (..), SnapshotInfo (..))
import Data.Aeson (toJSON)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath (makeRelative, (</>))
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
handleDiskCreate :: OutputFormat -> Connection -> Text -> DriveFormat -> Int64 -> Maybe Text -> IO Bool
handleDiskCreate fmt conn name format sizeMb mPath = do
  resp <- diskCreate conn name format sizeMb mPath
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (DiskCreated diskId) -> do
      emitOkWith fmt [("id", toJSON diskId)] $
        putStrLn $
          "Disk image created with ID: " ++ show diskId
      pure True
    Right (DiskError msg) -> do
      emitError fmt "error" msg $ putStrLn $ "Error creating disk: " ++ T.unpack msg
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle disk overlay command
handleDiskCreateOverlay :: OutputFormat -> Connection -> Text -> Text -> Maybe Text -> IO Bool
handleDiskCreateOverlay fmt conn name baseDiskRef optDirPath = do
  resp <- diskCreateOverlay conn name baseDiskRef optDirPath
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (DiskCreated diskId) -> do
      emitOkWith fmt [("id", toJSON diskId)] $
        putStrLn $
          "Overlay created with ID: " ++ show diskId
      pure True
    Right DiskNotFound -> do
      emitError fmt "not_found" ("Base disk '" <> baseDiskRef <> "' not found") $
        putStrLn $
          "Base disk '" ++ T.unpack baseDiskRef ++ "' not found."
      pure False
    Right (DiskError msg) -> do
      emitError fmt "error" msg $ putStrLn $ "Error creating overlay: " ++ T.unpack msg
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle disk register command (registers local file in DB without copying)
handleDiskRegister :: OutputFormat -> Connection -> Text -> FilePath -> Maybe Text -> Maybe Text -> IO Bool
handleDiskRegister fmt conn name path mFormatStr mBackingRef = do
  exists <- doesFileExist path
  if not exists
    then do
      emitError fmt "file_not_found" (T.pack $ "File not found: " ++ path) $
        putStrLn $
          "Error: File not found: " ++ path
      pure False
    else do
      let mFormat = case mFormatStr of
            Just f -> case parseFormat f of
              Right fmt' -> Just (Just fmt')
              Left _err -> Nothing
            Nothing -> Just Nothing -- no format specified, let server auto-detect
      case mFormat of
        Nothing -> do
          let err = "Unknown format: " <> fromMaybe "" mFormatStr
          emitError fmt "invalid_format" err $ putStrLn $ "Error: " ++ T.unpack err
          pure False
        Just mFmt -> do
          absPath <- canonicalizePath path
          basePath <- getBaseImagesPath
          let storedPath =
                if basePath `isPrefixOfPath` absPath
                  then makeRelative basePath absPath
                  else absPath
          resp <- diskRegister conn name (T.pack storedPath) mFmt mBackingRef
          case resp of
            Left err -> do
              emitRpcError fmt err
              pure False
            Right (DiskCreated diskId) -> do
              emitOkWith fmt [("id", toJSON diskId)] $ do
                putStrLn $ "Disk image registered with ID: " ++ show diskId
                if storedPath /= absPath
                  then putStrLn $ "Stored as relative path: " ++ storedPath
                  else putStrLn $ "Stored as absolute path: " ++ storedPath
              pure True
            Right (DiskError msg) -> do
              emitError fmt "error" msg $ putStrLn $ "Error registering disk: " ++ T.unpack msg
              pure False
            Right other -> do
              emitError fmt "unexpected" (T.pack $ show other) $
                putStrLn $
                  "Unexpected response: " ++ show other
              pure False
  where
    isPrefixOfPath :: FilePath -> FilePath -> Bool
    isPrefixOfPath prefix path' =
      let prefixWithSlash = if last prefix == '/' then prefix else prefix ++ "/"
       in prefixWithSlash == take (length prefixWithSlash) path' || prefix == path'

-- | Handle disk import command (copies local file or downloads URL to destination)
handleDiskImport :: OutputFormat -> Connection -> Text -> Text -> Maybe Text -> Maybe Text -> WaitOptions -> IO Bool
handleDiskImport fmt conn name source mPath mFormatStr waitOpts = do
  let wait = woWait waitOpts
  unless (isStructured fmt) $
    when wait $
      putStrLn $
        "Importing disk image '" ++ T.unpack name ++ "' and waiting for completion..."
  resp <- diskImport conn name source mPath mFormatStr wait
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (DiskCreated diskId) -> do
      emitOkWith fmt [("id", toJSON diskId)] $
        putStrLn $
          "Disk image imported with ID: " ++ show diskId
      pure True
    Right (DiskImportStarted taskId) -> do
      emitOkWith fmt [("taskId", toJSON taskId)] $ do
        putStrLn $ "Import started (task ID: " ++ show taskId ++ ")"
        putStrLn $ "Use 'crv task wait " ++ show taskId ++ "' to wait for completion."
      pure True
    Right (DiskError msg) -> do
      emitError fmt "error" msg $ putStrLn $ "Error importing disk: " ++ T.unpack msg
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Get base images path ($HOME/VMs by default)
getBaseImagesPath :: IO FilePath
getBaseImagesPath = do
  mHome <- lookupEnv "HOME"
  pure $ fromMaybe "/var/lib/qemu" mHome </> "VMs"

-- | Handle disk delete command
handleDiskDelete :: OutputFormat -> Connection -> Text -> IO Bool
handleDiskDelete fmt conn diskRef = do
  resp <- diskDelete conn diskRef
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right DiskOk -> do
      emitOk fmt $ putStrLn "Disk image deleted."
      pure True
    Right DiskNotFound -> do
      emitError fmt "not_found" ("Disk '" <> diskRef <> "' not found") $
        putStrLn $
          "Disk '" ++ T.unpack diskRef ++ "' not found."
      pure False
    Right (DiskInUse vmPairs) -> do
      let vmNames = T.intercalate ", " (map snd vmPairs)
      emitError fmt "in_use" ("Disk is attached to VMs: " <> vmNames) $ do
        putStrLn $ "Disk is attached to VMs: " ++ T.unpack vmNames
        putStrLn "Detach the disk first before deleting."
      pure False
    Right (DiskHasOverlays overlayPairs) -> do
      let overlayNames = T.intercalate ", " (map snd overlayPairs)
      emitError fmt "has_overlays" ("Disk is used as backing image for overlays: " <> overlayNames) $ do
        putStrLn $ "Disk is used as backing image for overlays: " ++ T.unpack overlayNames
        putStrLn "Delete the overlay disks first."
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle disk resize command
handleDiskResize :: OutputFormat -> Connection -> Text -> Int64 -> IO Bool
handleDiskResize fmt conn diskRef newSizeMb = do
  resp <- diskResize conn diskRef newSizeMb
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right DiskOk -> do
      emitOk fmt $ putStrLn $ "Disk resized to " ++ show newSizeMb ++ " MB."
      pure True
    Right DiskNotFound -> do
      emitError fmt "not_found" ("Disk '" <> diskRef <> "' not found") $
        putStrLn $
          "Disk '" ++ T.unpack diskRef ++ "' not found."
      pure False
    Right VmMustBeStopped -> do
      emitError fmt "vm_must_be_stopped" "VM must be stopped for this operation" $
        putStrLn "Cannot resize disk while VM is running. Stop the VM first."
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle disk list command
handleDiskList :: OutputFormat -> Connection -> IO Bool
handleDiskList fmt conn = do
  resp <- diskList conn
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (DiskListResult disks) -> do
      emitResult fmt disks $
        if null disks
          then putStrLn "No disk images found."
          else do
            printTableHeader [("ID", -6), ("NAME", -20), ("FORMAT", -8), ("SIZE_MB", 10), ("ATTACHED_TO", -20)]
            mapM_ printDiskInfo disks
      pure True
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle disk show command
handleDiskShow :: OutputFormat -> Connection -> Text -> IO Bool
handleDiskShow fmt conn diskRef = do
  resp <- diskShow conn diskRef
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (DiskInfo info) -> do
      emitResult fmt info $ printDiskDetails info
      pure True
    Right DiskNotFound -> do
      emitError fmt "not_found" ("Disk '" <> diskRef <> "' not found") $
        putStrLn $
          "Disk '" ++ T.unpack diskRef ++ "' not found."
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle disk clone command
handleDiskClone :: OutputFormat -> Connection -> Text -> Text -> Maybe Text -> IO Bool
handleDiskClone fmt conn name baseDiskRef optionalPath = do
  resp <- diskClone conn name baseDiskRef optionalPath
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (DiskCreated diskId) -> do
      emitOkWith fmt [("id", toJSON diskId)] $
        putStrLn $
          "Disk cloned successfully. New disk ID: " ++ show diskId
      pure True
    Right DiskNotFound -> do
      emitError fmt "not_found" ("Base disk '" <> baseDiskRef <> "' not found") $
        putStrLn $
          "Base disk '" ++ T.unpack baseDiskRef ++ "' not found."
      pure False
    Right VmMustBeStopped -> do
      emitError fmt "vm_must_be_stopped" "VM must be stopped for this operation" $
        putStrLn "Error: Disk is attached to a running VM. Please stop the VM before cloning."
      pure False
    Right (DiskError msg) -> do
      emitError fmt "error" msg $ putStrLn $ "Error cloning disk: " ++ T.unpack msg
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle disk rebase command
handleDiskRebase :: OutputFormat -> Connection -> Text -> Maybe Text -> Bool -> IO Bool
handleDiskRebase fmt conn diskRef mNewBacking unsafe = do
  resp <- diskRebase conn diskRef mNewBacking unsafe
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right DiskOk -> do
      let msg = case mNewBacking of
            Nothing -> "Disk flattened (backing merged into overlay)."
            Just _ -> "Disk rebased to new backing image."
      emitOk fmt $ putStrLn msg
      pure True
    Right DiskNotFound -> do
      emitError fmt "not_found" ("Disk '" <> diskRef <> "' not found") $
        putStrLn $
          "Disk '" ++ T.unpack diskRef ++ "' not found."
      pure False
    Right VmMustBeStopped -> do
      emitError fmt "vm_must_be_stopped" "VM must be stopped for this operation" $
        putStrLn "Error: Disk is attached to a running VM. Please stop the VM first."
      pure False
    Right (FormatNotSupported msg) -> do
      emitError fmt "format_not_supported" msg $ putStrLn $ "Error: " ++ T.unpack msg
      pure False
    Right (DiskError msg) -> do
      emitError fmt "error" msg $ putStrLn $ "Error: " ++ T.unpack msg
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle disk refresh command
handleDiskRefresh :: OutputFormat -> Connection -> Text -> IO Bool
handleDiskRefresh fmt conn diskRef = do
  resp <- diskRefresh conn diskRef
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right DiskOk -> do
      emitOk fmt $ putStrLn "Disk size refreshed"
      pure True
    Right DiskNotFound -> do
      emitError fmt "not_found" "Disk not found" $ putStrLn "Disk not found"
      pure False
    Right (DiskError msg) -> do
      emitError fmt "error" msg $ putStrLn $ "Error: " ++ T.unpack msg
      pure False
    Right other -> do
      emitError fmt "error" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle disk attach command
handleDiskAttach
  :: OutputFormat
  -> Connection
  -> Text
  -> Text
  -> DriveInterface
  -> Maybe DriveMedia
  -> Bool
  -> Bool
  -> CacheType
  -> IO Bool
handleDiskAttach fmt conn vmRef diskRef iface media readOnly discard cache = do
  resp <- diskAttach conn vmRef diskRef iface media readOnly discard cache
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (DriveAttached driveId) -> do
      emitOkWith fmt [("id", toJSON driveId)] $
        putStrLn $
          "Disk attached. Drive ID: " ++ show driveId
      pure True
    Right DiskNotFound -> do
      emitError fmt "not_found" ("Disk '" <> diskRef <> "' not found") $
        putStrLn $
          "Disk '" ++ T.unpack diskRef ++ "' not found."
      pure False
    Right DiskVmNotFound -> do
      emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
        putStrLn $
          "VM '" ++ T.unpack vmRef ++ "' not found."
      pure False
    Right (DiskHasOverlays overlayPairs) -> do
      let overlayNames = T.intercalate ", " (map snd overlayPairs)
      emitError fmt "has_overlays" ("Disk is used as backing image for overlays: " <> overlayNames) $ do
        putStrLn $ "Error: Disk is used as backing image for overlays: " ++ T.unpack overlayNames
        putStrLn "Base images must be attached in read-only mode using --read-only."
      pure False
    Right (DiskError msg) -> do
      emitError fmt "error" msg $ putStrLn $ "Error attaching disk: " ++ T.unpack msg
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle disk detach command
handleDiskDetach :: OutputFormat -> Connection -> Text -> Text -> IO Bool
handleDiskDetach fmt conn vmRef diskRef = do
  resp <- diskDetach conn vmRef diskRef
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right DiskOk -> do
      emitOk fmt $ putStrLn "Disk detached."
      pure True
    Right DriveNotFound -> do
      emitError fmt "not_found" ("Disk '" <> diskRef <> "' not found on VM") $
        putStrLn $
          "Disk '" ++ T.unpack diskRef ++ "' not found on VM."
      pure False
    Right DiskVmNotFound -> do
      emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
        putStrLn $
          "VM '" ++ T.unpack vmRef ++ "' not found."
      pure False
    Right (DiskError msg) -> do
      emitError fmt "error" msg $ putStrLn $ "Error detaching disk: " ++ T.unpack msg
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

--------------------------------------------------------------------------------
-- Snapshot Command Handlers
--------------------------------------------------------------------------------

-- | Handle snapshot create command
handleSnapshotCreate :: OutputFormat -> Connection -> Text -> Text -> IO Bool
handleSnapshotCreate fmt conn diskRef name = do
  resp <- snapshotCreate conn diskRef name
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (SnapshotCreated snapId) -> do
      emitOkWith fmt [("id", toJSON snapId)] $
        putStrLn $
          "Snapshot created with ID: " ++ show snapId
      pure True
    Right SnapshotDiskNotFound -> do
      emitError fmt "not_found" ("Disk '" <> diskRef <> "' not found") $
        putStrLn $
          "Disk '" ++ T.unpack diskRef ++ "' not found."
      pure False
    Right (SnapshotFormatNotSupported msg) -> do
      emitError fmt "format_not_supported" msg $ putStrLn $ "Error: " ++ T.unpack msg
      pure False
    Right SnapshotVmMustBeStopped -> do
      emitError fmt "vm_must_be_stopped" "VM must be stopped for this operation" $
        putStrLn "Cannot create snapshot while VM is running. Stop the VM first."
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle snapshot delete command
handleSnapshotDelete :: OutputFormat -> Connection -> Text -> Text -> IO Bool
handleSnapshotDelete fmt conn diskRef snapshotRef = do
  resp <- snapshotDelete conn diskRef snapshotRef
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right SnapshotOk -> do
      emitOk fmt $ putStrLn "Snapshot deleted."
      pure True
    Right SnapshotNotFound -> do
      emitError fmt "not_found" ("Snapshot '" <> snapshotRef <> "' not found") $
        putStrLn $
          "Snapshot '" ++ T.unpack snapshotRef ++ "' not found."
      pure False
    Right SnapshotDiskNotFound -> do
      emitError fmt "not_found" ("Disk '" <> diskRef <> "' not found") $
        putStrLn $
          "Disk '" ++ T.unpack diskRef ++ "' not found."
      pure False
    Right SnapshotVmMustBeStopped -> do
      emitError fmt "vm_must_be_stopped" "VM must be stopped for this operation" $
        putStrLn "Cannot delete snapshot while VM is running. Stop the VM first."
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle snapshot rollback command
handleSnapshotRollback :: OutputFormat -> Connection -> Text -> Text -> IO Bool
handleSnapshotRollback fmt conn diskRef snapshotRef = do
  resp <- snapshotRollback conn diskRef snapshotRef
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right SnapshotOk -> do
      emitOk fmt $ putStrLn "Rollback complete."
      pure True
    Right SnapshotNotFound -> do
      emitError fmt "not_found" ("Snapshot '" <> snapshotRef <> "' not found") $
        putStrLn $
          "Snapshot '" ++ T.unpack snapshotRef ++ "' not found."
      pure False
    Right SnapshotDiskNotFound -> do
      emitError fmt "not_found" ("Disk '" <> diskRef <> "' not found") $
        putStrLn $
          "Disk '" ++ T.unpack diskRef ++ "' not found."
      pure False
    Right SnapshotVmMustBeStopped -> do
      emitError fmt "vm_must_be_stopped" "VM must be stopped for this operation" $
        putStrLn "Cannot rollback while VM is running. Stop the VM first."
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle snapshot merge command
handleSnapshotMerge :: OutputFormat -> Connection -> Text -> Text -> IO Bool
handleSnapshotMerge fmt conn diskRef snapshotRef = do
  resp <- snapshotMerge conn diskRef snapshotRef
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right SnapshotOk -> do
      emitOk fmt $ putStrLn "Snapshot merged."
      pure True
    Right SnapshotNotFound -> do
      emitError fmt "not_found" ("Snapshot '" <> snapshotRef <> "' not found") $
        putStrLn $
          "Snapshot '" ++ T.unpack snapshotRef ++ "' not found."
      pure False
    Right SnapshotDiskNotFound -> do
      emitError fmt "not_found" ("Disk '" <> diskRef <> "' not found") $
        putStrLn $
          "Disk '" ++ T.unpack diskRef ++ "' not found."
      pure False
    Right SnapshotVmMustBeStopped -> do
      emitError fmt "vm_must_be_stopped" "VM must be stopped for this operation" $
        putStrLn "Cannot merge while VM is running. Stop the VM first."
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
      pure False

-- | Handle snapshot list command
handleSnapshotList :: OutputFormat -> Connection -> Text -> IO Bool
handleSnapshotList fmt conn diskRef = do
  resp <- snapshotList conn diskRef
  case resp of
    Left err -> do
      emitRpcError fmt err
      pure False
    Right (SnapshotListResult snaps) -> do
      emitResult fmt snaps $
        if null snaps
          then putStrLn "No snapshots found."
          else do
            printTableHeader [("ID", -6), ("NAME", -30), ("CREATED", -20), ("SIZE_MB", 10)]
            mapM_ printSnapshotInfo snaps
      pure True
    Right SnapshotDiskNotFound -> do
      emitError fmt "not_found" ("Disk '" <> diskRef <> "' not found") $
        putStrLn $
          "Disk '" ++ T.unpack diskRef ++ "' not found."
      pure False
    Right other -> do
      emitError fmt "unexpected" (T.pack $ show other) $
        putStrLn $
          "Unexpected response: " ++ show other
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
      (if null (diiAttachedTo d) then "-" else T.unpack (T.intercalate ", " (map snd (diiAttachedTo d))))

-- | Print disk image details
printDiskDetails :: DiskImageInfo -> IO ()
printDiskDetails d = do
  printField "Disk ID" (show (diiId d))
  printField "Name" (T.unpack (diiName d))
  printField "File Path" (T.unpack (diiFilePath d))
  printField "Format" (T.unpack (enumToText $ diiFormat d))
  printField "Size (MB)" (maybe "(unknown)" show (diiSizeMb d))
  printField "Created" (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (diiCreatedAt d))
  printField "Attached to" (if null (diiAttachedTo d) then "(none)" else T.unpack (T.intercalate ", " (map snd (diiAttachedTo d))))
  case diiBackingImageName d of
    Nothing -> pure ()
    Just backingName ->
      printField "Backing" (T.unpack backingName ++ " (ID: " ++ maybe "?" show (diiBackingImageId d) ++ ")")

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
