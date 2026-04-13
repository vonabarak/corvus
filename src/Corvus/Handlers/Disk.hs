{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Disk image management handlers.
-- Handles disk image CRUD operations, snapshots, and attach/detach.
module Corvus.Handlers.Disk
  ( -- * Action types
    DiskCreate (..)
  , DiskCreateOverlay (..)
  , DiskRegister (..)
  , DiskImportUrl (..)
  , DiskDelete (..)
  , DiskResize (..)
  , DiskClone (..)
  , DiskRefresh (..)
  , SnapshotCreate (..)
  , SnapshotDelete (..)
  , SnapshotRollback (..)
  , SnapshotMerge (..)
  , DiskAttach (..)
  , DiskDetachByDisk (..)
  , DiskRebase (..)
  , DiskImportAction (..)

    -- * Disk image handlers
  , handleDiskCreate
  , handleDiskCreateOverlay
  , handleDiskRegister
  , handleDiskImportUrl
  , handleDiskImportCopy
  , handleDiskDelete
  , handleDiskResize
  , handleDiskList
  , handleDiskShow
  , handleDiskClone
  , handleDiskRebase
  , handleDiskRefresh

    -- * Snapshot handlers
  , handleSnapshotCreate
  , handleSnapshotDelete
  , handleSnapshotRollback
  , handleSnapshotMerge
  , handleSnapshotList

    -- * Attach/detach handlers
  , handleDiskAttach
  , handleDiskDetach

    -- * Helpers
  , sanitizeDiskName
  , resolveDiskPath
  , resolveDiskFilePathPure
  , makeRelativeToBase
  )
where

import Corvus.Action

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebugN, logInfoN, logWarnN)
import Corvus.Handlers.Resolve (resolveSnapshot, validateName)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Qemu.Config (QemuConfig, getEffectiveBasePath)
import Corvus.Qemu.Image
import Corvus.Qemu.Qmp
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Int (Int64)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlPersistT)
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing, doesFileExist)
import System.FilePath (isRelative, takeDirectory, takeExtension, takeFileName, (</>))

--------------------------------------------------------------------------------
-- Path Utilities
--------------------------------------------------------------------------------

-- | Sanitize a disk image name to prevent path traversal attacks.
-- First removes dangerous characters (path separators, null bytes), then
-- removes parent directory references to handle cases like ".\0." -> ".."
-- Also rejects all-digit names to prevent ambiguity with numeric IDs.
sanitizeDiskName :: Text -> Either Text Text
sanitizeDiskName name
  | T.null sanitized = Left "Invalid disk name: name is empty after sanitization"
  | otherwise = case validateName "Disk image" sanitized of
      Left err -> Left err
      Right () -> Right sanitized
  where
    sanitized =
      T.replace ".." "" $
        T.filter (`notElem` ['/', '\\', '\0']) name

-- | Resolve a disk image file path, handling relative paths.
-- Relative paths (not starting with /) are resolved against the base path.
resolveDiskPath :: QemuConfig -> DiskImage -> IO FilePath
resolveDiskPath config disk = do
  basePath <- getEffectiveBasePath config
  let rawPath = T.unpack $ diskImageFilePath disk
  pure $
    if "/" `T.isPrefixOf` diskImageFilePath disk
      then rawPath
      else basePath </> rawPath

-- | Convert an absolute file path to a relative path if it falls within the base directory.
-- Paths outside the base directory are returned as-is.
makeRelativeToBase :: FilePath -> FilePath -> Text
makeRelativeToBase basePath filePath
  | (basePath ++ "/") `isPrefixOf` filePath = T.pack $ drop (length basePath + 1) filePath
  | otherwise = T.pack filePath

-- | Resolve an optional path for a disk image file.
--
-- Path interpretation rules:
--   * No path given: uses @basePath\/fileName@
--   * Starts with @\/@: absolute path
--   * Otherwise: relative to @basePath@
--   * Ends with @\/@: treated as a directory — @fileName@ is appended
--   * Does not end with @\/@: treated as the full file path
--
-- The parent directory is created if it does not exist.
resolveDiskFilePath :: FilePath -> Maybe Text -> FilePath -> IO FilePath
resolveDiskFilePath basePath mPath fileName = do
  let result = resolveDiskFilePathPure basePath mPath fileName
  createDirectoryIfMissing True (takeDirectory result)
  pure result

-- | Pure path resolution logic (no IO). See 'resolveDiskFilePath' for rules.
resolveDiskFilePathPure :: FilePath -> Maybe Text -> FilePath -> FilePath
resolveDiskFilePathPure basePath mPath fileName = case mPath of
  Nothing -> basePath </> fileName
  Just p ->
    let raw = T.unpack p
        resolved
          | isRelative raw = basePath </> raw
          | otherwise = raw
     in if "/" `isSuffixOf` raw
          then resolved </> fileName
          else resolved

--------------------------------------------------------------------------------
-- Disk Image Handlers
--------------------------------------------------------------------------------

-- | Create a new disk image
handleDiskCreate :: ServerState -> Text -> DriveFormat -> Int64 -> Maybe Text -> IO Response
handleDiskCreate state name format sizeMb mPath = runServerLogging state $ do
  logInfoN $ "Creating disk image: " <> name <> " (" <> T.pack (show sizeMb) <> " MB)"

  -- Sanitize the name to prevent path traversal attacks
  case sanitizeDiskName name of
    Left err -> do
      logWarnN $ "Invalid disk name: " <> err
      pure $ RespError err
    Right safeName -> do
      -- Generate file path using sanitized name
      basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
      let fileName = T.unpack safeName <> "." <> T.unpack (enumToText format)
      filePath <- liftIO $ resolveDiskFilePath basePath mPath fileName

      -- Create the actual image file
      result <- liftIO $ createImage filePath format sizeMb
      case result of
        ImageError err -> do
          logWarnN $ "Failed to create image: " <> err
          pure $ RespError err
        ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
        ImageNotFound -> pure $ RespError "Unexpected error during creation"
        ImageSuccess -> do
          -- Store in database with original name for display, but sanitized path
          now <- liftIO getCurrentTime
          diskId <-
            liftIO $
              runSqlPool
                ( insert
                    DiskImage
                      { diskImageName = safeName
                      , diskImageFilePath = makeRelativeToBase basePath filePath
                      , diskImageFormat = format
                      , diskImageSizeMb = Just (fromIntegral sizeMb)
                      , diskImageCreatedAt = now
                      , diskImageBackingImageId = Nothing
                      }
                )
                (ssDbPool state)
          logInfoN $ "Created disk image with ID: " <> T.pack (show $ fromSqlKey diskId)
          pure $ RespDiskCreated $ fromSqlKey diskId

-- | Register an existing disk image file.
-- Format and size are auto-detected via qemu-img info.
-- If format is provided, it is used instead of auto-detection.
handleDiskRegister
  :: ServerState
  -> Text
  -> Text
  -> Maybe DriveFormat
  -> Maybe Int64
  -> IO Response
handleDiskRegister state name filePath mFormat mBackingDiskId =
  case validateName "Disk image" name of
    Left err -> pure $ RespError err
    Right () -> runServerLogging state $ do
      logInfoN $ "Registering disk image: " <> name <> " at " <> filePath

      -- Normalize path: strip base directory prefix if applicable
      basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
      let storedPath = makeRelativeToBase basePath (T.unpack filePath)
          resolvedPath =
            if "/" `isPrefixOf` T.unpack storedPath
              then T.unpack storedPath
              else basePath </> T.unpack storedPath

      -- Detect format if not provided
      format <- case mFormat of
        Just f -> pure f
        Nothing -> do
          mInfo <- liftIO $ getImageInfo resolvedPath
          case mInfo of
            Right info -> pure $ iiFormat info
            Left err -> do
              -- Fall back to extension-based detection
              case detectFormatFromPath (T.pack resolvedPath) of
                Just f -> pure f
                Nothing -> do
                  logWarnN $ "Could not detect format for " <> T.pack resolvedPath <> ": " <> err
                  pure FormatRaw -- safe default

      -- Auto-detect size
      sizeMb <- liftIO $ getImageSizeMb resolvedPath

      -- Store in database
      now <- liftIO getCurrentTime
      mExisting <-
        liftIO $
          runSqlPool
            ( getBy (UniqueImagePath storedPath)
            )
            (ssDbPool state)

      case mExisting of
        Just (Entity diskId _) -> do
          logInfoN $ "Disk image already registered with ID: " <> T.pack (show $ fromSqlKey diskId)
          pure $ RespDiskCreated $ fromSqlKey diskId
        Nothing -> do
          result <-
            liftIO $
              try $
                runSqlPool
                  ( insert
                      DiskImage
                        { diskImageName = name
                        , diskImageFilePath = storedPath
                        , diskImageFormat = format
                        , diskImageSizeMb = sizeMb
                        , diskImageCreatedAt = now
                        , diskImageBackingImageId = fmap toSqlKey mBackingDiskId
                        }
                  )
                  (ssDbPool state)
          case result of
            Right diskId -> do
              logInfoN $ "Registered disk image with ID: " <> T.pack (show $ fromSqlKey diskId)
              pure $ RespDiskCreated $ fromSqlKey diskId
            Left (_err :: SomeException) -> do
              -- Race condition: another thread inserted first. Retry lookup.
              mRetry <-
                liftIO $
                  runSqlPool (getBy (UniqueImagePath storedPath)) (ssDbPool state)
              case mRetry of
                Just (Entity diskId _) -> do
                  logInfoN $ "Disk image registered concurrently with ID: " <> T.pack (show $ fromSqlKey diskId)
                  pure $ RespDiskCreated $ fromSqlKey diskId
                Nothing -> pure $ RespError $ "Failed to register disk image: " <> name

-- | Import a disk image from an HTTP/HTTPS URL.
-- Downloads the file to the base images directory, decompresses .xz if needed,
-- and registers it in the database.
handleDiskImportUrl :: ServerState -> Text -> Text -> Maybe Text -> IO Response
handleDiskImportUrl state name url mFormatStr =
  case validateName "Disk image" name of
    Left err -> pure $ RespError err
    Right () -> runServerLogging state $ do
      logInfoN $ "Importing disk image from URL: " <> name <> " <- " <> url
      let mExplicitFmt = mFormatStr >>= either (const Nothing) Just . enumFromText
      result <- liftIO $ importDiskFromUrlIO state name url mExplicitFmt
      case result of
        Left err -> do
          logWarnN $ "URL import failed: " <> err
          pure $ RespError err
        Right diskId -> do
          logInfoN $ "Imported disk image with ID: " <> T.pack (show diskId)
          pure $ RespDiskCreated diskId

-- | Import a disk image by copying from a local file or downloading from a URL.
-- The file is placed in the daemon's base images directory (or a custom path)
-- and registered in the database.
handleDiskImportCopy :: ServerState -> Text -> Text -> Maybe Text -> Maybe Text -> IO Response
handleDiskImportCopy state name source mDestPath mFormatStr =
  case validateName "Disk image" name of
    Left err -> pure $ RespError err
    Right () -> runServerLogging state $ do
      logInfoN $ "Importing disk image: " <> name <> " from " <> source

      case sanitizeDiskName name of
        Left err -> do
          logWarnN $ "Invalid disk name: " <> err
          pure $ RespError err
        Right safeName -> do
          basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)

          -- Detect format
          let mExplicitFmt = mFormatStr >>= either (const Nothing) Just . enumFromText
              mDetectedFmt =
                if isHttpUrl source
                  then detectFormatFromUrl source
                  else detectFormatFromPath source
          case mExplicitFmt <|> mDetectedFmt of
            Nothing -> pure $ RespError "Cannot detect disk format. Use --format to specify."
            Just format -> do
              let fmtExt = T.unpack (enumToText format)
                  destFileName = T.unpack safeName <> "." <> fmtExt
              if isHttpUrl source
                then do
                  -- URL download
                  let isXz = ".xz" `isSuffixOf` T.unpack source || isXzUrl source
                      downloadFileName = destFileName <> if isXz then ".xz" else ""
                  downloadDest <- liftIO $ resolveDiskFilePath basePath mDestPath downloadFileName
                  logInfoN $ "Downloading to: " <> T.pack downloadDest
                  dlResult <- liftIO $ downloadImage downloadDest source
                  case dlResult of
                    ImageError err -> do
                      logWarnN $ "Download failed: " <> err
                      pure $ RespError $ "Download failed: " <> err
                    _ -> do
                      -- Decompress .xz if needed
                      actualPath <-
                        if isXz
                          then liftIO $ decompressXz downloadDest
                          else do
                            let finalDest = resolveDiskFilePathPure basePath mDestPath destFileName
                            pure $ Right finalDest
                      case actualPath of
                        Left err -> pure $ RespError err
                        Right diskPath -> registerImportedFile state basePath safeName format diskPath
                else do
                  -- Local file copy
                  let srcPath = T.unpack source
                  srcExists <- liftIO $ doesFileExist srcPath
                  if not srcExists
                    then pure $ RespError $ "Source file not found: " <> source
                    else do
                      destPath <- liftIO $ resolveDiskFilePath basePath mDestPath destFileName
                      -- Check same-path: canonicalize source, and for dest
                      -- canonicalize the parent dir + filename (dest file may not exist yet)
                      canonSrc <- liftIO $ canonicalizePath srcPath
                      canonDestDir <- liftIO $ canonicalizePath (takeDirectory destPath)
                      let canonDest = canonDestDir </> takeFileName destPath
                      if canonSrc == canonDest
                        then pure $ RespError "Source and destination paths are the same"
                        else do
                          logInfoN $ "Copying " <> T.pack canonSrc <> " to " <> T.pack canonDest
                          copyResult <- liftIO $ try $ copyFile canonSrc canonDest
                          case copyResult of
                            Left (err :: SomeException) -> do
                              logWarnN $ "Copy failed: " <> T.pack (show err)
                              pure $ RespError $ "Copy failed: " <> T.pack (show err)
                            Right () -> registerImportedFile state basePath safeName format canonDest
  where
    isXzUrl t = ".xz?" `T.isInfixOf` t

    registerImportedFile state' basePath safeName format diskPath = do
      sizeMb <- liftIO $ getImageSizeMb diskPath
      now <- liftIO getCurrentTime
      let storedPath = makeRelativeToBase basePath diskPath
      diskId <-
        liftIO $
          runSqlPool
            ( insert
                DiskImage
                  { diskImageName = safeName
                  , diskImageFilePath = storedPath
                  , diskImageFormat = format
                  , diskImageSizeMb = sizeMb
                  , diskImageCreatedAt = now
                  , diskImageBackingImageId = Nothing
                  }
            )
            (ssDbPool state')
      logInfoN $ "Imported disk image with ID: " <> T.pack (show $ fromSqlKey diskId)
      pure $ RespDiskCreated $ fromSqlKey diskId

-- | Create a qcow2 overlay backed by an existing disk image
handleDiskCreateOverlay :: ServerState -> T.Text -> Int64 -> Maybe Int -> Maybe T.Text -> IO Response
handleDiskCreateOverlay state name baseDiskId mResizeMb optDirPath = runServerLogging state $ do
  logInfoN $ "Creating overlay '" <> name <> "' backed by disk " <> T.pack (show baseDiskId)

  case sanitizeDiskName name of
    Left err -> do
      logWarnN $ "Invalid overlay name: " <> err
      pure $ RespError err
    Right safeName -> do
      mBaseDisk <- liftIO $ runSqlPool (get (toSqlKey baseDiskId :: DiskImageId)) (ssDbPool state)
      case mBaseDisk of
        Nothing -> pure RespDiskNotFound
        Just baseDisk -> do
          rwDrives <-
            liftIO $
              runSqlPool
                ( selectList
                    [ M.DriveDiskImageId ==. toSqlKey baseDiskId
                    , M.DriveReadOnly ==. False
                    ]
                    []
                )
                (ssDbPool state)
          if not (null rwDrives)
            then do
              let vmIds = map (fromSqlKey . driveVmId . entityVal) rwDrives
              logWarnN $ "Base image is attached read-write to VMs: " <> T.pack (show vmIds)
              pure $ RespError "Cannot use as base: image is attached read-write to VM(s)"
            else do
              basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
              let overlayFileName = T.unpack safeName <> ".qcow2"
              overlayFilePath <- liftIO $ resolveDiskFilePath basePath optDirPath overlayFileName
              baseFilePath <- liftIO $ resolveDiskPath (ssQemuConfig state) baseDisk
              result <- liftIO $ createOverlay overlayFilePath baseFilePath (diskImageFormat baseDisk)
              case result of
                ImageError err -> do
                  logWarnN $ "Failed to create overlay: " <> err
                  pure $ RespError err
                _ -> do
                  now <- liftIO getCurrentTime
                  diskId <-
                    liftIO $
                      runSqlPool
                        ( insert
                            DiskImage
                              { diskImageName = safeName
                              , diskImageFilePath = makeRelativeToBase basePath overlayFilePath
                              , diskImageFormat = FormatQcow2
                              , diskImageSizeMb = diskImageSizeMb baseDisk
                              , diskImageCreatedAt = now
                              , diskImageBackingImageId = Just (toSqlKey baseDiskId)
                              }
                        )
                        (ssDbPool state)
                  -- Resize if requested
                  case mResizeMb of
                    Just newSize -> do
                      res <- liftIO $ resizeImage overlayFilePath (fromIntegral newSize)
                      case res of
                        ImageSuccess ->
                          liftIO $ runSqlPool (update diskId [DiskImageSizeMb =. Just newSize]) (ssDbPool state)
                        _ -> logWarnN "Failed to resize overlay after creation"
                    Nothing -> pure ()
                  logInfoN $ "Created overlay with ID: " <> T.pack (show $ fromSqlKey diskId)
                  pure $ RespDiskCreated $ fromSqlKey diskId

-- | Clone a disk image
handleDiskClone :: ServerState -> Text -> Int64 -> Maybe Int -> Maybe Text -> IO Response
handleDiskClone state name baseDiskId mResizeMb optionalPath = runServerLogging state $ do
  logInfoN $ "Cloning disk image " <> T.pack (show baseDiskId) <> " to '" <> name <> "'"

  case sanitizeDiskName name of
    Left err -> do
      logWarnN $ "Invalid disk name: " <> err
      pure $ RespError err
    Right safeName -> do
      mBaseDisk <- liftIO $ runSqlPool (get (toSqlKey baseDiskId :: DiskImageId)) (ssDbPool state)
      case mBaseDisk of
        Nothing -> pure RespDiskNotFound
        Just baseDisk -> do
          -- Check if any attached VM is running or paused
          runningVms <- liftIO $ runSqlPool (getRunningAttachedVms baseDiskId) (ssDbPool state)
          if not (null runningVms)
            then pure RespVmMustBeStopped
            else do
              basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
              srcPath <- liftIO $ resolveDiskPath (ssQemuConfig state) baseDisk
              let srcFileName = takeFileName srcPath
                  ext = takeExtension srcFileName
                  cloneFileName = T.unpack safeName <> ext
              destPath <- liftIO $ resolveDiskFilePath basePath optionalPath cloneFileName
              result <- liftIO $ cloneImage srcPath destPath
              case result of
                ImageError err -> do
                  logWarnN $ "Failed to clone image: " <> err
                  pure $ RespError err
                ImageNotFound -> pure $ RespError "Source image file not found"
                _ -> do
                  now <- liftIO getCurrentTime
                  newDiskId <-
                    liftIO $
                      runSqlPool
                        ( do
                            dId <-
                              insert
                                DiskImage
                                  { diskImageName = safeName
                                  , diskImageFilePath = makeRelativeToBase basePath destPath
                                  , diskImageFormat = diskImageFormat baseDisk
                                  , diskImageSizeMb = diskImageSizeMb baseDisk
                                  , diskImageCreatedAt = now
                                  , diskImageBackingImageId = diskImageBackingImageId baseDisk
                                  }
                            -- Clone snapshots as well
                            baseSnapshots <- selectList [SnapshotDiskImageId ==. toSqlKey baseDiskId] []
                            forM_ baseSnapshots $ \snapEntity -> do
                              let snap = entityVal snapEntity
                              insert snap {snapshotDiskImageId = dId}
                            pure dId
                        )
                        (ssDbPool state)
                  -- Resize if requested
                  case mResizeMb of
                    Just newSize -> do
                      res <- liftIO $ resizeImage destPath (fromIntegral newSize)
                      case res of
                        ImageSuccess ->
                          liftIO $ runSqlPool (update newDiskId [DiskImageSizeMb =. Just newSize]) (ssDbPool state)
                        _ -> logWarnN "Failed to resize clone after creation"
                    Nothing -> pure ()
                  logInfoN $ "Cloned disk image with ID: " <> T.pack (show $ fromSqlKey newDiskId)
                  pure $ RespDiskCreated $ fromSqlKey newDiskId

-- | Rebase an overlay to a different backing image, or flatten (remove backing).
handleDiskRebase :: ServerState -> Int64 -> Maybe Int64 -> Bool -> IO Response
handleDiskRebase state diskId mNewBackingId unsafe = runServerLogging state $ do
  logInfoN $ "Rebasing disk image: " <> T.pack (show diskId)

  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      -- Must be qcow2
      if diskImageFormat disk /= FormatQcow2
        then pure $ RespFormatNotSupported "Rebase requires qcow2 format"
        else case diskImageBackingImageId disk of
          Nothing -> pure $ RespError "Disk is not an overlay"
          Just _ -> do
            -- Must not be attached to running/paused VMs
            runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
            if not (null runningVms)
              then pure RespVmMustBeStopped
              else do
                overlayPath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
                case mNewBackingId of
                  -- Flatten: remove backing
                  Nothing -> do
                    logInfoN "Flattening overlay (removing backing)"
                    result <- liftIO $ rebaseImage overlayPath Nothing unsafe
                    case result of
                      ImageSuccess -> do
                        liftIO $
                          runSqlPool
                            (update (toSqlKey diskId :: DiskImageId) [DiskImageBackingImageId =. Nothing])
                            (ssDbPool state)
                        -- Refresh size since flatten merges backing data
                        mSize <- liftIO $ getImageSizeMb overlayPath
                        case mSize of
                          Just newSize ->
                            liftIO $
                              runSqlPool
                                (update (toSqlKey diskId :: DiskImageId) [DiskImageSizeMb =. Just newSize])
                                (ssDbPool state)
                          Nothing -> pure ()
                        logInfoN "Overlay flattened successfully"
                        pure RespDiskOk
                      ImageNotFound -> pure $ RespError "Overlay file not found on disk"
                      ImageError err -> do
                        logWarnN $ "Failed to flatten overlay: " <> err
                        pure $ RespError err
                      _ -> pure $ RespError "Unexpected rebase result"
                  -- Rebase to new backing
                  Just newBackingId -> do
                    mNewBacking <- liftIO $ runSqlPool (get (toSqlKey newBackingId :: DiskImageId)) (ssDbPool state)
                    case mNewBacking of
                      Nothing -> pure $ RespError "New backing image not found"
                      Just newBacking -> do
                        -- Check circular dependency
                        circular <- liftIO $ runSqlPool (isCircularBacking diskId newBackingId) (ssDbPool state)
                        if circular
                          then pure $ RespError "Circular backing dependency detected"
                          else do
                            newBackingPath <- liftIO $ resolveDiskPath (ssQemuConfig state) newBacking
                            logInfoN $ "Rebasing to new backing: " <> T.pack newBackingPath
                            result <- liftIO $ rebaseImage overlayPath (Just (newBackingPath, diskImageFormat newBacking)) unsafe
                            case result of
                              ImageSuccess -> do
                                liftIO $
                                  runSqlPool
                                    (update (toSqlKey diskId :: DiskImageId) [DiskImageBackingImageId =. Just (toSqlKey newBackingId)])
                                    (ssDbPool state)
                                -- Refresh size
                                mSize <- liftIO $ getImageSizeMb overlayPath
                                case mSize of
                                  Just newSize ->
                                    liftIO $
                                      runSqlPool
                                        (update (toSqlKey diskId :: DiskImageId) [DiskImageSizeMb =. Just newSize])
                                        (ssDbPool state)
                                  Nothing -> pure ()
                                logInfoN "Overlay rebased successfully"
                                pure RespDiskOk
                              ImageNotFound -> pure $ RespError "Overlay file not found on disk"
                              ImageError err -> do
                                logWarnN $ "Failed to rebase overlay: " <> err
                                pure $ RespError err
                              _ -> pure $ RespError "Unexpected rebase result"

-- | Refresh a disk image's size by querying qemu-img info
handleDiskRefresh :: ServerState -> Int64 -> IO Response
handleDiskRefresh state diskId = runServerLogging state $ do
  logInfoN $ "Refreshing disk image size: " <> T.pack (show diskId)
  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      resolvedPath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
      mSize <- liftIO $ getImageSizeMb resolvedPath
      case mSize of
        Nothing -> pure $ RespError "Could not determine disk image size"
        Just newSize -> do
          liftIO $
            runSqlPool
              (update (toSqlKey diskId :: DiskImageId) [DiskImageSizeMb =. Just newSize])
              (ssDbPool state)
          logInfoN $ "Updated size to " <> T.pack (show newSize) <> " MB"
          pure RespDiskOk

-- | Delete a disk image
handleDiskDelete :: ServerState -> Int64 -> IO Response
handleDiskDelete state diskId = runServerLogging state $ do
  logInfoN $ "Deleting disk image: " <> T.pack (show diskId)

  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      -- Check if disk is attached to any VMs
      attachedVms <- liftIO $ runSqlPool (getAttachedVms diskId) (ssDbPool state)
      if not (null attachedVms)
        then pure $ RespDiskInUse attachedVms
        else do
          -- Check if disk is used as backing image for overlays
          overlayIds <- liftIO $ runSqlPool (getOverlayIds diskId) (ssDbPool state)
          if not (null overlayIds)
            then pure $ RespDiskHasOverlays overlayIds
            else do
              -- Delete the file (resolve relative path against base path)
              filePath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
              result <- liftIO $ deleteImage filePath
              case result of
                ImageError err -> do
                  logWarnN $ "Failed to delete image file: " <> err
                  pure $ RespError err
                ImageNotFound -> do
                  logWarnN "Image file not found, removing from database anyway"
                  liftIO $ runSqlPool (deleteDiskAndSnapshots diskId) (ssDbPool state)
                  pure RespDiskOk
                _ -> do
                  -- Delete from database
                  liftIO $ runSqlPool (deleteDiskAndSnapshots diskId) (ssDbPool state)
                  logInfoN $ "Deleted disk image: " <> T.pack (show diskId)
                  pure RespDiskOk

-- | Resize a disk image (VM must be stopped)
handleDiskResize :: ServerState -> Int64 -> Int64 -> IO Response
handleDiskResize state diskId newSizeMb = runServerLogging state $ do
  logInfoN $ "Resizing disk image " <> T.pack (show diskId) <> " to " <> T.pack (show newSizeMb) <> " MB"

  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      -- Check if any attached VM is running
      runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
      if not (null runningVms)
        then pure RespVmMustBeStopped
        else do
          -- Check if disk is used as backing image for overlays
          overlayIds <- liftIO $ runSqlPool (getOverlayIds diskId) (ssDbPool state)
          if not (null overlayIds)
            then pure $ RespDiskHasOverlays overlayIds
            else do
              filePath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
              result <- liftIO $ resizeImage filePath newSizeMb
              case result of
                ImageSuccess -> do
                  -- Update size in database
                  liftIO $
                    runSqlPool
                      (update (toSqlKey diskId :: DiskImageId) [M.DiskImageSizeMb =. Just (fromIntegral newSizeMb)])
                      (ssDbPool state)
                  logInfoN "Disk resized successfully"
                  pure RespDiskOk
                ImageError err -> do
                  logWarnN $ "Failed to resize: " <> err
                  pure $ RespError err
                ImageNotFound -> pure RespDiskNotFound
                ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg

-- | List all disk images
handleDiskList :: ServerState -> IO Response
handleDiskList state = do
  disks <- runSqlPool listDiskImages (ssDbPool state)
  pure $ RespDiskList disks

-- | Show disk image details
handleDiskShow :: ServerState -> Int64 -> IO Response
handleDiskShow state diskId = do
  mInfo <- runSqlPool (getDiskImageInfo diskId) (ssDbPool state)
  case mInfo of
    Nothing -> pure RespDiskNotFound
    Just info -> pure $ RespDiskInfo info

--------------------------------------------------------------------------------
-- Snapshot Handlers
--------------------------------------------------------------------------------

-- | Create a snapshot
handleSnapshotCreate :: ServerState -> Int64 -> Text -> IO Response
handleSnapshotCreate state diskId snapshotName =
  case validateName "Snapshot" snapshotName of
    Left err -> pure $ RespError err
    Right () -> runServerLogging state $ do
      logInfoN $ "Creating snapshot '" <> snapshotName <> "' for disk " <> T.pack (show diskId)

      mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
      case mDisk of
        Nothing -> pure RespDiskNotFound
        Just disk -> do
          -- Check format - only qcow2 supports snapshots
          if diskImageFormat disk /= FormatQcow2
            then do
              logWarnN "Snapshot requested on non-qcow2 image"
              pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
            else do
              -- Check if any attached VM is running or paused
              runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
              if not (null runningVms)
                then pure RespVmMustBeStopped
                else do
                  filePath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
                  result <- liftIO $ createSnapshot filePath snapshotName
                  case result of
                    ImageSuccess -> do
                      now <- liftIO getCurrentTime
                      snapshotId <-
                        liftIO $
                          runSqlPool
                            ( insert
                                Snapshot
                                  { snapshotDiskImageId = toSqlKey diskId
                                  , snapshotName = snapshotName
                                  , snapshotCreatedAt = now
                                  , snapshotSizeMb = Nothing
                                  }
                            )
                            (ssDbPool state)
                      logInfoN $ "Created snapshot with ID: " <> T.pack (show $ fromSqlKey snapshotId)
                      pure $ RespSnapshotCreated $ fromSqlKey snapshotId
                    ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
                    ImageError err -> pure $ RespError err
                    ImageNotFound -> pure RespDiskNotFound

-- | Delete a snapshot
handleSnapshotDelete :: ServerState -> Int64 -> Ref -> IO Response
handleSnapshotDelete state diskId snapRef = runServerLogging state $ do
  logInfoN $ "Deleting snapshot '" <> unRef snapRef <> "' from disk " <> T.pack (show diskId)

  -- First check disk exists
  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      -- Check format supports snapshots - only qcow2 supports snapshots
      if diskImageFormat disk /= FormatQcow2
        then pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
        else do
          -- Check if any attached VM is running or paused
          runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
          if not (null runningVms)
            then pure RespVmMustBeStopped
            else do
              -- Resolve snapshot ref
              mSnapId <- liftIO $ resolveSnapshot snapRef diskId (ssDbPool state)
              case mSnapId of
                Left _ -> pure RespSnapshotNotFound
                Right snapshotId -> do
                  mSnapshot <- liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
                  case mSnapshot of
                    Nothing -> pure RespSnapshotNotFound
                    Just snapshot -> do
                      filePath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
                      result <- liftIO $ deleteSnapshot filePath (snapshotName snapshot)
                      case result of
                        ImageSuccess -> do
                          liftIO $ runSqlPool (delete (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
                          logInfoN "Snapshot deleted"
                          pure RespSnapshotOk
                        ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
                        ImageError err -> pure $ RespError err
                        ImageNotFound -> pure RespDiskNotFound

-- | Rollback to a snapshot (VM must be stopped)
handleSnapshotRollback :: ServerState -> Int64 -> Ref -> IO Response
handleSnapshotRollback state diskId snapRef = runServerLogging state $ do
  logInfoN $ "Rolling back disk " <> T.pack (show diskId) <> " to snapshot '" <> unRef snapRef <> "'"

  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      if diskImageFormat disk /= FormatQcow2
        then pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
        else do
          runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
          if not (null runningVms)
            then pure RespVmMustBeStopped
            else do
              mSnapId <- liftIO $ resolveSnapshot snapRef diskId (ssDbPool state)
              case mSnapId of
                Left _ -> pure RespSnapshotNotFound
                Right snapshotId -> do
                  mSnapshot <- liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
                  case mSnapshot of
                    Nothing -> pure RespSnapshotNotFound
                    Just snapshot -> do
                      filePath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
                      result <- liftIO $ rollbackSnapshot filePath (snapshotName snapshot)
                      case result of
                        ImageSuccess -> do
                          logInfoN "Rollback complete"
                          pure RespSnapshotOk
                        ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
                        ImageError err -> pure $ RespError err
                        ImageNotFound -> pure RespDiskNotFound

-- | Merge a snapshot (VM must be stopped)
handleSnapshotMerge :: ServerState -> Int64 -> Ref -> IO Response
handleSnapshotMerge state diskId snapRef = runServerLogging state $ do
  logInfoN $ "Merging snapshot '" <> unRef snapRef <> "' for disk " <> T.pack (show diskId)

  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      if diskImageFormat disk /= FormatQcow2
        then pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
        else do
          runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
          if not (null runningVms)
            then pure RespVmMustBeStopped
            else do
              mSnapId <- liftIO $ resolveSnapshot snapRef diskId (ssDbPool state)
              case mSnapId of
                Left _ -> pure RespSnapshotNotFound
                Right snapshotId -> do
                  mSnapshot <- liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
                  case mSnapshot of
                    Nothing -> pure RespSnapshotNotFound
                    Just snapshot -> do
                      filePath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
                      result <- liftIO $ mergeSnapshot filePath (snapshotName snapshot)
                      case result of
                        ImageSuccess -> do
                          liftIO $ runSqlPool (delete (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
                          logInfoN "Merge complete"
                          pure RespSnapshotOk
                        ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
                        ImageError err -> pure $ RespError err
                        ImageNotFound -> pure RespDiskNotFound

-- | List snapshots for a disk image
handleSnapshotList :: ServerState -> Int64 -> IO Response
handleSnapshotList state diskId = do
  mDisk <- runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just _ -> do
      snapshots <- runSqlPool (getSnapshots diskId) (ssDbPool state)
      pure $ RespSnapshotList snapshots

--------------------------------------------------------------------------------
-- Attach/Detach Handlers
--------------------------------------------------------------------------------

-- | Attach a disk to a VM
handleDiskAttach
  :: ServerState
  -> Int64
  -> Int64
  -> DriveInterface
  -> Maybe DriveMedia
  -> Bool
  -> Bool
  -> CacheType
  -> IO Response
handleDiskAttach state vmId diskId interface media readOnly discard cache = runServerLogging state $ do
  logInfoN $
    "Attaching disk "
      <> T.pack (show diskId)
      <> " to VM "
      <> T.pack (show vmId)
      <> if readOnly then " (read-only)" else ""

  -- Check disk exists
  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      -- Check VM exists and get status
      mVm <- liftIO $ runSqlPool (get (toSqlKey vmId :: VmId)) (ssDbPool state)
      case mVm of
        Nothing -> pure RespVmNotFound
        Just vm -> do
          -- Check if disk is used as backing image for overlays
          -- We only allow attaching base images as read-only.
          overlayIds <- liftIO $ runSqlPool (getOverlayIds diskId) (ssDbPool state)
          if not (null overlayIds) && not readOnly
            then pure $ RespDiskHasOverlays overlayIds
            else do
              -- Create drive record (unique constraint: one drive per VM+disk pair)
              mDriveId <-
                liftIO $
                  runSqlPool
                    ( insertUnique
                        Drive
                          { driveVmId = toSqlKey vmId
                          , driveDiskImageId = toSqlKey diskId
                          , driveInterface = interface
                          , driveMedia = media
                          , driveReadOnly = readOnly
                          , driveCacheType = cache
                          , driveDiscard = discard
                          }
                    )
                    (ssDbPool state)
              case mDriveId of
                Nothing -> pure $ RespError "Disk is already attached to this VM"
                Just driveId -> do
                  -- If VM is running or paused, hot-plug via QMP (both have live QEMU process)
                  if vmStatus vm `elem` [VmRunning, VmPaused]
                    then do
                      logInfoN $ "VM is " <> enumToText (vmStatus vm) <> ", performing hot-plug"
                      let nodeName = "drive-" <> T.pack (show $ fromSqlKey driveId)
                          deviceId = "device-" <> T.pack (show $ fromSqlKey driveId)
                          format = diskImageFormat disk
                      filePath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk

                      -- Add block device
                      let qemuCfg = ssQemuConfig state
                      blockResult <- liftIO $ qmpBlockdevAdd qemuCfg vmId nodeName filePath format readOnly
                      case blockResult of
                        QmpSuccess -> do
                          -- Add device
                          deviceResult <- liftIO $ qmpDeviceAddDrive qemuCfg vmId deviceId nodeName interface
                          case deviceResult of
                            QmpSuccess -> do
                              logInfoN "Hot-plug successful"
                              pure $ RespDiskAttached $ fromSqlKey driveId
                            QmpError err -> do
                              logWarnN $ "Device add failed: " <> err
                              -- Try to clean up blockdev
                              _ <- liftIO $ qmpBlockdevDel qemuCfg vmId nodeName
                              -- Remove drive record
                              liftIO $ runSqlPool (delete driveId) (ssDbPool state)
                              pure $ RespError $ "Device add failed: " <> err
                            QmpConnectionFailed err -> do
                              liftIO $ runSqlPool (delete driveId) (ssDbPool state)
                              pure $ RespError $ "QMP connection failed: " <> err
                        QmpError err -> do
                          logWarnN $ "Blockdev add failed: " <> err
                          liftIO $ runSqlPool (delete driveId) (ssDbPool state)
                          pure $ RespError $ "Blockdev add failed: " <> err
                        QmpConnectionFailed err -> do
                          liftIO $ runSqlPool (delete driveId) (ssDbPool state)
                          pure $ RespError $ "QMP connection failed: " <> err
                    else do
                      logInfoN "VM is not active, disk attached to database only"
                      pure $ RespDiskAttached $ fromSqlKey driveId

-- | Detach a disk from a VM by disk image ID (looks up drive via UniqueDrive constraint)
handleDiskDetach :: ServerState -> Int64 -> Int64 -> IO Response
handleDiskDetach state vmId diskId = runServerLogging state $ do
  logInfoN $
    "Detaching disk image "
      <> T.pack (show diskId)
      <> " from VM "
      <> T.pack (show vmId)

  -- Look up drive by (vmId, diskImageId) unique constraint
  mDrive <- liftIO $ runSqlPool (getBy (UniqueDrive (toSqlKey vmId) (toSqlKey diskId))) (ssDbPool state)
  case mDrive of
    Nothing -> pure RespDriveNotFound
    Just (Entity driveKey drive) -> do
      let driveId = fromSqlKey driveKey
      -- Check VM status
      mVm <- liftIO $ runSqlPool (get (driveVmId drive)) (ssDbPool state)
      case mVm of
        Nothing -> pure RespVmNotFound
        Just vm -> do
          -- If VM is running or paused, hot-unplug via QMP (both have live QEMU process)
          if vmStatus vm `elem` [VmRunning, VmPaused]
            then do
              logInfoN $ "VM is " <> enumToText (vmStatus vm) <> ", performing hot-unplug"
              let deviceId = "device-" <> T.pack (show driveId)
                  nodeName = "drive-" <> T.pack (show driveId)

              -- Remove device first (device_del is async — QEMU returns
              -- immediately but the device is removed in the background)
              let qemuCfg = ssQemuConfig state
              deviceResult <- liftIO $ qmpDeviceDel qemuCfg vmId deviceId
              case deviceResult of
                QmpSuccess -> do
                  -- Remove block device, retrying if device_del hasn't completed yet
                  blockResult <- liftIO $ retryBlockdevDel qemuCfg vmId nodeName
                  case blockResult of
                    QmpSuccess -> do
                      liftIO $ runSqlPool (delete driveKey) (ssDbPool state)
                      logInfoN "Hot-unplug successful"
                      pure RespDiskOk
                    QmpError err -> do
                      logWarnN $ "Blockdev remove failed (device already removed): " <> err
                      -- Still remove from DB since device is gone
                      liftIO $ runSqlPool (delete driveKey) (ssDbPool state)
                      pure RespDiskOk
                    QmpConnectionFailed err ->
                      pure $ RespError $ "QMP connection failed: " <> err
                QmpError err -> do
                  logWarnN $ "Device remove failed: " <> err
                  pure $ RespError $ "Device remove failed: " <> err
                QmpConnectionFailed err ->
                  pure $ RespError $ "QMP connection failed: " <> err
            else do
              -- Just remove from database
              liftIO $ runSqlPool (delete driveKey) (ssDbPool state)
              logInfoN "Drive detached from database"
              pure RespDiskOk

-- | Retry blockdev-del up to 5 times with 200ms delay between attempts.
-- QEMU's device_del is asynchronous — the device may still be in use
-- when we try to remove its block backend immediately after device_del returns.
retryBlockdevDel :: QemuConfig -> Int64 -> T.Text -> IO QmpResult
retryBlockdevDel config vmId nodeName = go (5 :: Int)
  where
    go 0 = qmpBlockdevDel config vmId nodeName
    go n = do
      result <- qmpBlockdevDel config vmId nodeName
      case result of
        QmpSuccess -> pure QmpSuccess
        QmpError err
          | "in use" `T.isInfixOf` err -> do
              threadDelay 200000
              go (n - 1)
          | otherwise -> pure result
        other -> pure other

--------------------------------------------------------------------------------
-- Database Operations
--------------------------------------------------------------------------------

-- | Get VMs that have this disk attached (ID + name pairs)
getAttachedVms :: Int64 -> SqlPersistT IO [(Int64, T.Text)]
getAttachedVms diskId = do
  drives <- selectList [M.DriveDiskImageId ==. toSqlKey diskId] []
  let vmKeys = map (driveVmId . entityVal) drives
  forM vmKeys $ \vmKey -> do
    mVm <- get vmKey
    let name = maybe "(deleted)" vmName mVm
    pure (fromSqlKey vmKey, name)

-- | Get running VMs that have this disk attached
-- | Get VMs with active QEMU processes that have this disk attached
-- Both running and paused VMs have live QEMU processes holding disk files open
getRunningAttachedVms :: Int64 -> SqlPersistT IO [Int64]
getRunningAttachedVms diskId = do
  drives <- selectList [M.DriveDiskImageId ==. toSqlKey diskId] []
  let vmKeys = map (driveVmId . entityVal) drives
  activeVms <- selectList [M.VmId <-. vmKeys, M.VmStatus <-. [VmRunning, VmPaused]] []
  pure $ map (fromSqlKey . entityKey) activeVms

-- | Get overlay disk (ID, name) pairs that reference this disk as a backing image
getOverlayIds :: Int64 -> SqlPersistT IO [(Int64, T.Text)]
getOverlayIds diskId = do
  overlays <- selectList [M.DiskImageBackingImageId ==. Just (toSqlKey diskId)] []
  pure $ map (\(Entity key d) -> (fromSqlKey key, diskImageName d)) overlays

-- | Check if newBackingId transitively depends on diskId via the backing chain.
-- Returns True if making diskId backed by newBackingId would create a cycle.
isCircularBacking :: Int64 -> Int64 -> SqlPersistT IO Bool
isCircularBacking diskId newBackingId
  | diskId == newBackingId = pure True
  | otherwise = walk newBackingId
  where
    walk currentId = do
      mDisk <- get (toSqlKey currentId :: DiskImageId)
      case mDisk of
        Nothing -> pure False
        Just disk -> case diskImageBackingImageId disk of
          Nothing -> pure False
          Just parentKey ->
            let parentId = fromSqlKey parentKey
             in if parentId == diskId then pure True else walk parentId

-- | Delete disk and its snapshots
deleteDiskAndSnapshots :: Int64 -> SqlPersistT IO ()
deleteDiskAndSnapshots diskId = do
  deleteWhere [M.SnapshotDiskImageId ==. toSqlKey diskId]
  delete (toSqlKey diskId :: DiskImageId)

-- | Resolve backing image name from optional key
getBackingImageName :: Maybe DiskImageId -> SqlPersistT IO (Maybe T.Text)
getBackingImageName Nothing = pure Nothing
getBackingImageName (Just backingKey) = do
  mBacking <- get backingKey
  pure $ fmap diskImageName mBacking

-- | List all disk images with attachment info
listDiskImages :: SqlPersistT IO [DiskImageInfo]
listDiskImages = do
  disks <- selectList [] [Asc M.DiskImageName]
  forM disks $ \(Entity key disk) -> do
    attachedVms <- getAttachedVms (fromSqlKey key)
    backingName <- getBackingImageName (diskImageBackingImageId disk)
    pure $
      DiskImageInfo
        { diiId = fromSqlKey key
        , diiName = diskImageName disk
        , diiFilePath = diskImageFilePath disk
        , diiFormat = diskImageFormat disk
        , diiSizeMb = diskImageSizeMb disk
        , diiCreatedAt = diskImageCreatedAt disk
        , diiAttachedTo = attachedVms
        , diiBackingImageId = fmap fromSqlKey (diskImageBackingImageId disk)
        , diiBackingImageName = backingName
        }

-- | Get disk image info
getDiskImageInfo :: Int64 -> SqlPersistT IO (Maybe DiskImageInfo)
getDiskImageInfo diskId = do
  mDisk <- get (toSqlKey diskId :: DiskImageId)
  case mDisk of
    Nothing -> pure Nothing
    Just disk -> do
      attachedVms <- getAttachedVms diskId
      backingName <- getBackingImageName (diskImageBackingImageId disk)
      pure $
        Just
          DiskImageInfo
            { diiId = diskId
            , diiName = diskImageName disk
            , diiFilePath = diskImageFilePath disk
            , diiFormat = diskImageFormat disk
            , diiSizeMb = diskImageSizeMb disk
            , diiCreatedAt = diskImageCreatedAt disk
            , diiAttachedTo = attachedVms
            , diiBackingImageId = fmap fromSqlKey (diskImageBackingImageId disk)
            , diiBackingImageName = backingName
            }

-- | Get snapshots for a disk
getSnapshots :: Int64 -> SqlPersistT IO [SnapshotInfo]
getSnapshots diskId = do
  snapshots <- selectList [M.SnapshotDiskImageId ==. toSqlKey diskId] [Asc M.SnapshotCreatedAt]
  pure $
    map
      ( \(Entity key snap) ->
          SnapshotInfo
            { sniId = fromSqlKey key
            , sniName = snapshotName snap
            , sniCreatedAt = snapshotCreatedAt snap
            , sniSizeMb = snapshotSizeMb snap
            }
      )
      snapshots

--------------------------------------------------------------------------------
-- Core Operations
-- These return Either Text Int64 (error or disk ID) for reuse by Apply handler.
--------------------------------------------------------------------------------

-- | Import a disk image from an HTTP/HTTPS URL and return its ID.
-- Downloads to the base images directory, decompresses .xz if needed.
importDiskFromUrlIO :: ServerState -> Text -> Text -> Maybe DriveFormat -> IO (Either Text Int64)
importDiskFromUrlIO state name url mFormat = do
  case sanitizeDiskName name of
    Left err -> pure $ Left err
    Right safeName -> do
      let mUrlFmt = detectFormatFromUrl url
      case mFormat <|> mUrlFmt of
        Nothing -> pure $ Left "Cannot detect disk format from URL. Use --format to specify."
        Just format -> do
          basePath <- getEffectiveBasePath (ssQemuConfig state)
          let urlStr = T.unpack url
              isXz = ".xz" `isSuffixOf` urlStr || ".xz?" `isInfixOf'` urlStr
              fmtExt = T.unpack (enumToText format)
              downloadFileName = T.unpack safeName <> "." <> fmtExt <> if isXz then ".xz" else ""
              downloadPath = basePath </> downloadFileName
              finalFileName = T.unpack safeName <> "." <> fmtExt
              finalPath = basePath </> finalFileName
          dlResult <- downloadImage downloadPath url
          case dlResult of
            ImageError err -> pure $ Left $ "Download failed: " <> err
            _ -> do
              actualPath <-
                if isXz
                  then decompressXz downloadPath
                  else pure $ Right finalPath
              case actualPath of
                Left err -> pure $ Left err
                Right diskPath -> do
                  let storedPath = makeRelativeToBase basePath diskPath
                  sizeMb <- getImageSizeMb diskPath
                  now <- getCurrentTime
                  diskId <-
                    runSqlPool
                      ( insert
                          DiskImage
                            { diskImageName = safeName
                            , diskImageFilePath = storedPath
                            , diskImageFormat = format
                            , diskImageSizeMb = sizeMb
                            , diskImageCreatedAt = now
                            , diskImageBackingImageId = Nothing
                            }
                      )
                      (ssDbPool state)
                  pure $ Right $ fromSqlKey diskId
  where
    isInfixOf' needle haystack = needle `T.isInfixOf` T.pack haystack

--------------------------------------------------------------------------------
-- Action Types
--------------------------------------------------------------------------------

data DiskCreate = DiskCreate
  { dcrName :: Text
  , dcrFormat :: DriveFormat
  , dcrSizeMb :: Int64
  , dcrPath :: Maybe Text
  }

instance Action DiskCreate where
  actionSubsystem _ = SubDisk
  actionCommand _ = "create"
  actionEntityName = Just . dcrName
  actionExecute ctx a = handleDiskCreate (acState ctx) (dcrName a) (dcrFormat a) (dcrSizeMb a) (dcrPath a)

data DiskCreateOverlay = DiskCreateOverlay
  { dcoName :: Text
  , dcoBaseDiskId :: Int64
  , dcoResizeMb :: Maybe Int
  , dcoPath :: Maybe Text
  }

instance Action DiskCreateOverlay where
  actionSubsystem _ = SubDisk
  actionCommand _ = "overlay"
  actionEntityName = Just . dcoName
  actionExecute ctx a = handleDiskCreateOverlay (acState ctx) (dcoName a) (dcoBaseDiskId a) (dcoResizeMb a) (dcoPath a)

data DiskRegister = DiskRegister
  { drgName :: Text
  , drgPath :: Text
  , drgFormat :: Maybe DriveFormat
  , drgBackingDiskId :: Maybe Int64
  }

instance Action DiskRegister where
  actionSubsystem _ = SubDisk
  actionCommand _ = "register"
  actionEntityName = Just . drgName
  actionExecute ctx a = handleDiskRegister (acState ctx) (drgName a) (drgPath a) (drgFormat a) (drgBackingDiskId a)

data DiskImportAction = DiskImportAction
  { diaName :: Text
  , diaSource :: Text
  , diaDestPath :: Maybe Text
  , diaFormat :: Maybe Text
  }

instance Action DiskImportAction where
  actionSubsystem _ = SubDisk
  actionCommand _ = "import"
  actionEntityName = Just . diaName
  actionExecute ctx a = handleDiskImportCopy (acState ctx) (diaName a) (diaSource a) (diaDestPath a) (diaFormat a)

data DiskImportUrl = DiskImportUrl
  { diuName :: Text
  , diuUrl :: Text
  , diuFormat :: Maybe Text
  }

instance Action DiskImportUrl where
  actionSubsystem _ = SubDisk
  actionCommand _ = "import-url"
  actionEntityName = Just . diuName
  actionExecute ctx a = handleDiskImportUrl (acState ctx) (diuName a) (diuUrl a) (diuFormat a)

newtype DiskDelete = DiskDelete {ddelDiskId :: Int64}

instance Action DiskDelete where
  actionSubsystem _ = SubDisk
  actionCommand _ = "delete"
  actionEntityId = Just . fromIntegral . ddelDiskId
  actionExecute ctx a = handleDiskDelete (acState ctx) (ddelDiskId a)

data DiskResize = DiskResize
  { drzDiskId :: Int64
  , drzNewSizeMb :: Int64
  }

instance Action DiskResize where
  actionSubsystem _ = SubDisk
  actionCommand _ = "resize"
  actionEntityId = Just . fromIntegral . drzDiskId
  actionExecute ctx a = handleDiskResize (acState ctx) (drzDiskId a) (drzNewSizeMb a)

data DiskClone = DiskClone
  { dclName :: Text
  , dclBaseDiskId :: Int64
  , dclResizeMb :: Maybe Int
  , dclPath :: Maybe Text
  }

instance Action DiskClone where
  actionSubsystem _ = SubDisk
  actionCommand _ = "clone"
  actionEntityName = Just . dclName
  actionExecute ctx a = handleDiskClone (acState ctx) (dclName a) (dclBaseDiskId a) (dclResizeMb a) (dclPath a)

data DiskRebase = DiskRebase
  { drbDiskId :: Int64
  , drbNewBackingId :: Maybe Int64
  , drbUnsafe :: Bool
  }

instance Action DiskRebase where
  actionSubsystem _ = SubDisk
  actionCommand (DiskRebase _ Nothing _) = "flatten"
  actionCommand (DiskRebase _ (Just _) _) = "rebase"
  actionEntityId = Just . fromIntegral . drbDiskId
  actionExecute ctx a = handleDiskRebase (acState ctx) (drbDiskId a) (drbNewBackingId a) (drbUnsafe a)

newtype DiskRefresh = DiskRefresh {drfDiskId :: Int64}

instance Action DiskRefresh where
  actionSubsystem _ = SubDisk
  actionCommand _ = "refresh"
  actionEntityId = Just . fromIntegral . drfDiskId
  actionExecute ctx a = handleDiskRefresh (acState ctx) (drfDiskId a)

data SnapshotCreate = SnapshotCreate
  { scrDiskId :: Int64
  , scrName :: Text
  }

instance Action SnapshotCreate where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "create"
  actionEntityId = Just . fromIntegral . scrDiskId
  actionExecute ctx a = handleSnapshotCreate (acState ctx) (scrDiskId a) (scrName a)

data SnapshotDelete = SnapshotDelete
  { sdelDiskId :: Int64
  , sdelSnapRef :: Ref
  }

instance Action SnapshotDelete where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "delete"
  actionEntityId = Just . fromIntegral . sdelDiskId
  actionExecute ctx a = handleSnapshotDelete (acState ctx) (sdelDiskId a) (sdelSnapRef a)

data SnapshotRollback = SnapshotRollback
  { srlDiskId :: Int64
  , srlSnapRef :: Ref
  }

instance Action SnapshotRollback where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "rollback"
  actionEntityId = Just . fromIntegral . srlDiskId
  actionExecute ctx a = handleSnapshotRollback (acState ctx) (srlDiskId a) (srlSnapRef a)

data SnapshotMerge = SnapshotMerge
  { smrDiskId :: Int64
  , smrSnapRef :: Ref
  }

instance Action SnapshotMerge where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "merge"
  actionEntityId = Just . fromIntegral . smrDiskId
  actionExecute ctx a = handleSnapshotMerge (acState ctx) (smrDiskId a) (smrSnapRef a)

data DiskAttach = DiskAttach
  { datVmId :: Int64
  , datDiskId :: Int64
  , datInterface :: DriveInterface
  , datMedia :: Maybe DriveMedia
  , datReadOnly :: Bool
  , datDiscard :: Bool
  , datCache :: CacheType
  }

instance Action DiskAttach where
  actionSubsystem _ = SubDisk
  actionCommand _ = "attach"
  actionEntityId = Just . fromIntegral . datVmId
  actionExecute ctx a = handleDiskAttach (acState ctx) (datVmId a) (datDiskId a) (datInterface a) (datMedia a) (datReadOnly a) (datDiscard a) (datCache a)

data DiskDetachByDisk = DiskDetachByDisk
  { ddbVmId :: Int64
  , ddbDiskId :: Int64
  }

instance Action DiskDetachByDisk where
  actionSubsystem _ = SubDisk
  actionCommand _ = "detach"
  actionEntityId = Just . fromIntegral . ddbVmId
  actionExecute ctx a = handleDiskDetach (acState ctx) (ddbVmId a) (ddbDiskId a)
