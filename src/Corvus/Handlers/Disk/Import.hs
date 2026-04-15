{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Import disk images from local files or HTTP(S) URLs.
--
-- Two entry points:
--
-- * 'handleDiskImportCopy' — copies a local file or downloads a URL to
--   the base images directory (with optional destination path override)
--   and registers the result. Also handles @.xz@ decompression.
--
-- * 'handleDiskImportUrl' — specialised URL-only import, kept for the
--   legacy @ReqDiskImportUrl@ protocol message.
--
-- The pure helper 'importDiskFromUrlIO' is exported for reuse from the
-- @crv apply@ handler so it can download URLs declared in apply configs
-- without going through the protocol layer.
module Corvus.Handlers.Disk.Import
  ( -- * Action types
    DiskImportAction (..)
  , DiskImportUrl (..)

    -- * Handlers
  , handleDiskImportCopy
  , handleDiskImportUrl

    -- * Reusable core for Apply
  , importDiskFromUrlIO
  )
where

import Corvus.Action
import Corvus.Handlers.Disk.Path (makeRelativeToBase, resolveDiskFilePath, resolveDiskFilePathPure, sanitizeDiskName)
import Corvus.Handlers.Resolve (validateName)

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Model
import Corvus.Protocol
import Corvus.Qemu.Config (getEffectiveBasePath)
import Corvus.Qemu.Image
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Int (Int64)
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import System.Directory (canonicalizePath, copyFile, doesFileExist)
import System.FilePath (takeDirectory, takeFileName, (</>))

-- | Import a disk image from an HTTP/HTTPS URL.
-- Downloads the file to the base images directory, decompresses @.xz@ if
-- needed, and registers it in the database.
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

-- | Import a disk image by copying from a local file or downloading from
-- a URL. The file is placed in the daemon's base images directory (or a
-- custom path) and registered in the database.
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

-- | Import a disk image from an HTTP/HTTPS URL and return its ID.
--
-- Downloads to the base images directory, decompresses @.xz@ if needed.
-- Exposed outside the Action typeclass so @crv apply@ can reuse it
-- directly when walking a YAML config.
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
