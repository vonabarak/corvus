{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
  , handleDiskCopy
  , handleDiskMove

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
  , diskColumns
  , snapshotColumns
  )
where

import qualified Capnp.Gen.Enums as CGEnums
import Control.Exception (SomeException, try)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Output (Align (..), Column (..), TableOpts, emitError, emitOk, emitOkWith, emitResult, printField, printTable)
import Corvus.Client.Types (OutputFormat, WaitOptions (..))
import Corvus.Model (CacheType, DriveFormat, DriveInterface, DriveMedia, EnumText (..))
import Corvus.Protocol (DiskImageInfo (..), DiskImagePlacement (..), NamedRef (..), SnapshotInfo (..))
import Corvus.Wire.Common (entityRefFromText)
import Corvus.Wire.Enums (toCapnpDriveFormat)
import Data.Aeson (toJSON)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import System.Directory (doesFileExist)

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

parseFormat :: Text -> Either Text DriveFormat
parseFormat = enumFromText

parseInterface :: Text -> Either Text DriveInterface
parseInterface = enumFromText

parseCacheType :: Text -> Either Text CacheType
parseCacheType = enumFromText

parseMedia :: Text -> Either Text DriveMedia
parseMedia = enumFromText

--------------------------------------------------------------------------------
-- Disk Command Handlers
--------------------------------------------------------------------------------

-- | Handle disk create command. The Cap'n Proto schema's
-- @DiskManager.create@ does not accept a custom @path@ — the daemon
-- always allocates a path under the configured base. We accept the
-- legacy @mPath@ argument and silently ignore it for backwards
-- compatibility with the CLI parser; an explicit path will be
-- reintroduced when @DiskCreateParams@ gains a @path@ field.
handleDiskCreate :: OutputFormat -> CapnpConnection -> Text -> DriveFormat -> Int64 -> Maybe Text -> Bool -> Text -> IO Bool
handleDiskCreate fmt conn name format sizeMb _mPath ephemeral nodeRef = do
  r <- try @SomeException (CR.rpcDiskCreate conn name sizeMb (toCapnpDriveFormat format) ephemeral (entityRefFromText nodeRef))
  case r of
    Right diskId -> do
      emitOkWith fmt [("id", toJSON diskId)] $
        putStrLn $
          "Disk image created with ID: " ++ show diskId
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error creating disk: " ++ show e)
      pure False

-- | Handle disk overlay command
handleDiskCreateOverlay :: OutputFormat -> CapnpConnection -> Text -> Text -> Maybe Text -> Bool -> IO Bool
handleDiskCreateOverlay fmt conn name baseDiskRef _optDirPath ephemeral = do
  r <- try @SomeException (CR.rpcDiskCreateOverlay conn name (entityRefFromText baseDiskRef) ephemeral)
  case r of
    Right diskId -> do
      emitOkWith fmt [("id", toJSON diskId)] $
        putStrLn $
          "Overlay created with ID: " ++ show diskId
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error creating overlay: " ++ show e)
      pure False

-- | Handle disk register command (registers local file in DB without
-- copying). The format must be supplied since the Cap'n Proto schema
-- doesn't carry the auto-detect path yet.
handleDiskRegister :: OutputFormat -> CapnpConnection -> Text -> FilePath -> Maybe Text -> Maybe Text -> Bool -> Text -> IO Bool
handleDiskRegister fmt conn name path mFormatStr _mBackingRef ephemeral nodeRef = do
  exists <- doesFileExist path
  if not exists
    then do
      emitError fmt "file_not_found" (T.pack $ "File not found: " ++ path) $
        putStrLn $
          "Error: File not found: " ++ path
      pure False
    else case mFormatStr >>= eitherToMaybe . parseFormat of
      Nothing -> do
        emitError
          fmt
          "invalid_format"
          "--format must be supplied (Cap'n Proto schema mandates it)"
          (putStrLn "Error: --format must be supplied for disk register.")
        pure False
      Just fmt' -> do
        r <- try @SomeException (CR.rpcDiskRegister conn name (T.pack path) fmt' ephemeral (entityRefFromText nodeRef))
        case r of
          Right diskId -> do
            emitOkWith fmt [("id", toJSON diskId)] $
              putStrLn $
                "Disk image registered with ID: " ++ show diskId
            pure True
          Left e -> do
            emitError fmt "rpc_error" (T.pack (show e)) $
              putStrLn ("Error registering disk: " ++ show e)
            pure False

-- | Handle disk import command. The Cap'n Proto schema currently
-- only supports synchronous import from a local source; URL imports
-- and @--wait@ are not threaded through the wrapper. The legacy
-- 'WaitOptions' / format arguments are accepted to keep the CLI
-- parser happy.
handleDiskImport :: OutputFormat -> CapnpConnection -> Text -> Text -> Maybe Text -> Maybe Text -> Bool -> Text -> WaitOptions -> IO Bool
handleDiskImport fmt conn name source _mPath mFormatStr ephemeral nodeRef _waitOpts = do
  case mFormatStr >>= eitherToMaybe . parseFormat of
    Nothing -> do
      emitError
        fmt
        "invalid_format"
        "--format must be supplied (Cap'n Proto schema mandates it)"
        (putStrLn "Error: --format must be supplied for disk import.")
      pure False
    Just fmt' -> do
      r <- try @SomeException (CR.rpcDiskImport conn name source fmt' ephemeral (entityRefFromText nodeRef))
      case r of
        Right diskId -> do
          emitOkWith fmt [("id", toJSON diskId)] $
            putStrLn $
              "Disk image imported with ID: " ++ show diskId
          pure True
        Left e -> do
          emitError fmt "rpc_error" (T.pack (show e)) $
            putStrLn ("Error importing disk: " ++ show e)
          pure False

-- | Handle disk delete command
handleDiskDelete :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleDiskDelete fmt conn diskRef = do
  r <- try (CR.rpcDiskDelete conn (entityRefFromText diskRef)) :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $ putStrLn "Disk image deleted."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Handle disk resize command
handleDiskResize :: OutputFormat -> CapnpConnection -> Text -> Int64 -> IO Bool
handleDiskResize fmt conn diskRef newSizeMb = do
  r <- try (CR.rpcDiskResize conn (entityRefFromText diskRef) newSizeMb) :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $ putStrLn $ "Disk resized to " ++ show newSizeMb ++ " MB."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Handle disk list command
handleDiskList :: OutputFormat -> TableOpts -> CapnpConnection -> IO Bool
handleDiskList fmt tableOpts conn = do
  r <- try @SomeException (CR.rpcDiskList conn)
  case r of
    Right disks -> do
      emitResult fmt disks $
        if null disks
          then putStrLn "No disk images found."
          else printTable tableOpts diskColumns disks
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Handle disk show command
handleDiskShow :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleDiskShow fmt conn diskRef = do
  r <- try @SomeException (CR.rpcDiskShow conn (entityRefFromText diskRef))
  case r of
    Right info -> do
      emitResult fmt info $ printDiskDetails info
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Handle disk clone command
handleDiskClone :: OutputFormat -> CapnpConnection -> Text -> Text -> Maybe Text -> Bool -> IO Bool
handleDiskClone fmt conn name baseDiskRef _optionalPath ephemeral = do
  r <- try @SomeException (CR.rpcDiskClone conn (entityRefFromText baseDiskRef) name ephemeral)
  case r of
    Right diskId -> do
      emitOkWith fmt [("id", toJSON diskId)] $
        putStrLn $
          "Disk cloned successfully. New disk ID: " ++ show diskId
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error cloning disk: " ++ show e)
      pure False

-- | Handle disk rebase command. The Cap'n Proto schema currently
-- requires a new backing reference (flatten/unsafe are not threaded
-- through the wrapper yet); accept the legacy 'unsafe' argument as a
-- no-op for backwards compatibility.
handleDiskRebase :: OutputFormat -> CapnpConnection -> Text -> Maybe Text -> Bool -> IO Bool
handleDiskRebase fmt conn diskRef mNewBacking _unsafe = case mNewBacking of
  Nothing -> do
    emitError
      fmt
      "not_implemented"
      "disk rebase --flatten requires Cap'n Proto schema support (Phase 6, not yet implemented)"
      (putStrLn "Error: --flatten not yet supported over Cap'n Proto.")
    pure False
  Just newBacking -> do
    r <-
      try
        ( CR.rpcDiskRebase
            conn
            (entityRefFromText diskRef)
            (entityRefFromText newBacking)
        )
        :: IO (Either SomeException ())
    case r of
      Right () -> do
        emitOk fmt $ putStrLn "Disk rebased to new backing image."
        pure True
      Left e -> do
        emitError fmt "rpc_error" (T.pack (show e)) $
          putStrLn ("Error: " ++ show e)
        pure False

-- | Handle disk refresh command
handleDiskRefresh :: OutputFormat -> CapnpConnection -> Text -> IO Bool
handleDiskRefresh fmt conn diskRef = do
  r <- try @SomeException (CR.rpcDiskRefresh conn (entityRefFromText diskRef))
  case r of
    Right _ -> do
      emitOk fmt $ putStrLn "Disk size refreshed"
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

-- | Handle disk attach command
handleDiskAttach
  :: OutputFormat
  -> CapnpConnection
  -> Text
  -> Text
  -> DriveInterface
  -> Maybe DriveMedia
  -> Bool
  -> Bool
  -> CacheType
  -> IO Bool
handleDiskAttach fmt conn vmRef diskRef iface media readOnly discard cache = do
  r <-
    try @SomeException
      ( CR.rpcDiskAttach
          conn
          (entityRefFromText vmRef)
          (entityRefFromText diskRef)
          iface
          media
          readOnly
          discard
          cache
      )
  case r of
    Right driveId -> do
      emitOkWith fmt [("id", toJSON driveId)] $
        putStrLn $
          "Disk attached. Drive ID: " ++ show driveId
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error attaching disk: " ++ show e)
      pure False

-- | Handle disk detach command. The Cap'n Proto Vm cap's
-- detachDisk takes a drive id (Int64). We treat 'diskRef' as the
-- drive id when it parses as an integer, otherwise we fail with a
-- clear error since the schema no longer accepts a disk-name lookup
-- here.
handleDiskDetach :: OutputFormat -> CapnpConnection -> Text -> Text -> IO Bool
handleDiskDetach fmt conn vmRef diskRef = do
  r <-
    try (CR.rpcDiskDetachByDisk conn (entityRefFromText vmRef) (entityRefFromText diskRef))
      :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $ putStrLn "Disk detached."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

--------------------------------------------------------------------------------
-- Snapshot Command Handlers
--------------------------------------------------------------------------------

handleSnapshotCreate
  :: OutputFormat -> CapnpConnection -> Text -> Text -> CGEnums.QuiesceMode -> IO Bool
handleSnapshotCreate fmt conn diskRef name quiesce = do
  r <-
    try @SomeException
      (CR.rpcSnapshotCreate conn (entityRefFromText diskRef) name quiesce)
  case r of
    Right snapId -> do
      emitOkWith fmt [("id", toJSON snapId)] $
        putStrLn $
          "Snapshot created with ID: " ++ show snapId
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

handleSnapshotDelete :: OutputFormat -> CapnpConnection -> Text -> Text -> IO Bool
handleSnapshotDelete fmt conn diskRef snapshotRef = do
  r <-
    try
      (CR.rpcSnapshotDelete conn (entityRefFromText diskRef) (entityRefFromText snapshotRef))
      :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $ putStrLn "Snapshot deleted."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

handleSnapshotRollback :: OutputFormat -> CapnpConnection -> Text -> Text -> Bool -> IO Bool
handleSnapshotRollback fmt conn diskRef snapshotRef autoStop = do
  r <-
    try
      ( CR.rpcSnapshotRollback
          conn
          (entityRefFromText diskRef)
          (entityRefFromText snapshotRef)
          autoStop
      )
      :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $ putStrLn "Rollback complete."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

handleSnapshotMerge :: OutputFormat -> CapnpConnection -> Text -> Text -> IO Bool
handleSnapshotMerge fmt conn diskRef snapshotRef = do
  r <-
    try
      (CR.rpcSnapshotMerge conn (entityRefFromText diskRef) (entityRefFromText snapshotRef))
      :: IO (Either SomeException ())
  case r of
    Right () -> do
      emitOk fmt $ putStrLn "Snapshot merged."
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

handleSnapshotList :: OutputFormat -> TableOpts -> CapnpConnection -> Text -> IO Bool
handleSnapshotList fmt tableOpts conn diskRef = do
  r <- try @SomeException (CR.rpcSnapshotList conn (entityRefFromText diskRef))
  case r of
    Right snaps -> do
      emitResult fmt snaps $
        if null snaps
          then putStrLn "No snapshots found."
          else printTable tableOpts snapshotColumns snaps
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error: " ++ show e)
      pure False

--------------------------------------------------------------------------------
-- Printers
--------------------------------------------------------------------------------

-- | Column definitions for the @disk list@ table.
diskColumns :: [Column DiskImageInfo]
diskColumns =
  [ Column "ID" RightAlign (show . diiId)
  , Column "NAME" LeftAlign (T.unpack . diiName)
  , Column "FORMAT" LeftAlign (T.unpack . enumToText . diiFormat)
  , Column "SIZE_MB" RightAlign (maybe "-" show . diiSizeMb)
  , Column "EPH" LeftAlign (\d -> if diiEphemeral d then "yes" else "-")
  , Column "ATTACHED_TO" LeftAlign formatAttached
  ]
  where
    formatAttached d
      | null (diiAttachedTo d) = "-"
      | otherwise = T.unpack (T.intercalate ", " (map nrName (diiAttachedTo d)))

-- | Print disk image details
printDiskDetails :: DiskImageInfo -> IO ()
printDiskDetails d = do
  printField "Disk ID" (show (diiId d))
  printField "Name" (T.unpack (diiName d))
  -- Per-node placements: render one "<node>: <path>" line per
  -- placement, or @(none)@ when an image has been registered
  -- without an on-disk file yet.
  printField
    "Placements"
    ( if null (diiPlacements d)
        then "(none)"
        else
          T.unpack $
            T.intercalate "; " $
              map
                (\p -> nrName (dipNode p) <> ": " <> dipFilePath p)
                (diiPlacements d)
    )
  printField "Format" (T.unpack (enumToText $ diiFormat d))
  printField "Size (MB)" (maybe "(unknown)" show (diiSizeMb d))
  printField "Ephemeral" (if diiEphemeral d then "true" else "false")
  printField "Created" (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (diiCreatedAt d))
  printField "Attached to" (if null (diiAttachedTo d) then "(none)" else T.unpack (T.intercalate ", " (map nrName (diiAttachedTo d))))
  case diiBackingImage d of
    Nothing -> pure ()
    Just backing ->
      printField "Backing" (T.unpack (nrName backing) ++ " (ID: " ++ show (nrId backing) ++ ")")

-- | Column definitions for the @disk snapshot list@ table.
--
-- LIVE / Q are boolean badges:
--
--   * LIVE = "+" → snapshot was taken via QMP on a running VM.
--     LIVE = "-" → offline path (@qemu-img snapshot -c@).
--   * Q    = "+" → QGA @guest-fsfreeze-freeze@ was active when
--     the snapshot was stamped (filesystem-consistent).
--     Q = "-" → no quiesce (hard-reset-equivalent for unflushed
--     in-guest writes).
snapshotColumns :: [Column SnapshotInfo]
snapshotColumns =
  [ Column "ID" RightAlign (show . sniId)
  , Column "NAME" LeftAlign (T.unpack . sniName)
  , Column "CREATED" LeftAlign (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" . sniCreatedAt)
  , Column "SIZE_MB" RightAlign (maybe "-" show . sniSizeMb)
  , Column "LIVE" LeftAlign (boolBadge . sniLive)
  , Column "Q" LeftAlign (boolBadge . sniQuiesced)
  ]
  where
    boolBadge True = "+"
    boolBadge False = "-"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _) = Nothing

readMaybeInt64 :: String -> Maybe Int64
readMaybeInt64 s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

-- | Handle @crv disk copy <DISK> --to-node <NODE> [--to-path PATH]
-- [--with-backing-chain]@.
handleDiskCopy
  :: OutputFormat
  -> CapnpConnection
  -> Text
  -> Text
  -> Maybe Text
  -> Bool
  -> IO Bool
handleDiskCopy fmt conn diskRef toNodeRef mToPath withBackingChain = do
  r <-
    try @SomeException $
      CR.rpcDiskCopy
        conn
        (entityRefFromText diskRef)
        (entityRefFromText toNodeRef)
        mToPath
        withBackingChain
  case r of
    Right tid -> do
      emitOkWith fmt [("taskId", toJSON tid)] $
        putStrLn $
          "Disk copy started. Task ID: " ++ show tid
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error copying disk: " ++ show e)
      pure False

-- | Handle @crv disk move <DISK> --to-node <NODE> [--to-path PATH]
-- [--with-backing-chain]@.
handleDiskMove
  :: OutputFormat
  -> CapnpConnection
  -> Text
  -> Text
  -> Maybe Text
  -> Bool
  -> IO Bool
handleDiskMove fmt conn diskRef toNodeRef mToPath withBackingChain = do
  r <-
    try @SomeException $
      CR.rpcDiskMove
        conn
        (entityRefFromText diskRef)
        (entityRefFromText toNodeRef)
        mToPath
        withBackingChain
  case r of
    Right tid -> do
      emitOkWith fmt [("taskId", toJSON tid)] $
        putStrLn $
          "Disk move started. Task ID: " ++ show tid
      pure True
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error moving disk: " ++ show e)
      pure False
