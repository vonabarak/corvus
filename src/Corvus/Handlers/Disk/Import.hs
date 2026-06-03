{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
module Corvus.Handlers.Disk.Import
  ( -- * Action types
    DiskImportAction (..)
  , DiskImportUrl (..)

    -- * Handlers
  , handleDiskImportCopy
  , handleDiskImportUrl
  )
where

import Corvus.Action
import Corvus.Handlers.Disk.Agent
  ( cloneImageViaAgent
  , decompressXzViaAgent
  , deleteImageViaAgent
  , downloadImageViaAgent
  , getImageSizeMbViaAgent
  , md5HashFileViaAgent
  )
import Corvus.Handlers.Disk.Db (recordDiskImageNode)
import Corvus.Handlers.Disk.Path (makeRelativeToBase, resolveDiskFilePath, resolveDiskFilePathPure, sanitizeDiskName)
import Corvus.Handlers.Resolve (resolveNode, validateName)

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Handlers.Scheduler (pickNodeForDisk)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Node.Image (ImageResult (..), detectFormatFromPath, detectFormatFromUrl, isHttpUrl)
import Corvus.Protocol
import Corvus.Qemu.Config (getEffectiveBasePath)
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Int (Int64)
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (takeDirectory, takeFileName, (</>))

-- | Resolve a (possibly empty) node-ref text used by every
-- import path. Empty / @"0"@ defers to 'pickNodeForDisk';
-- everything else parses as a 'Ref' and looks up via 'resolveNode'.
resolveImportTargetNode :: ServerState -> Text -> IO (Either Text M.NodeId)
resolveImportTargetNode state nodeRefText
  | T.null nodeRefText || nodeRefText == "0" = pickNodeForDisk state
  | otherwise = do
      r <- resolveNode (Ref nodeRefText) (ssDbPool state)
      pure $ fmap (M.toSqlKey :: Int64 -> M.NodeId) r

-- | Import a disk image from an HTTP/HTTPS URL.
-- Downloads the file to the base images directory, decompresses @.xz@ if
-- needed, and registers it in the database.
handleDiskImportUrl :: ServerState -> ApplySink -> Text -> Text -> Maybe Text -> Bool -> Text -> IO Response
handleDiskImportUrl state sink name url mFormatStr ephemeral nodeRefText =
  case validateName "Disk image" name of
    Left err -> pure $ RespError err
    Right () -> runServerLogging state $ do
      logInfoN $ "Importing disk image from URL: " <> name <> " <- " <> url
      let mExplicitFmt = mFormatStr >>= either (const Nothing) Just . enumFromText
      result <- liftIO $ importDiskFromUrlIO state sink name url mExplicitFmt Nothing ephemeral nodeRefText
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
--
-- The optional @mMd5@ argument enables integrity-checking on URL
-- imports: if the destination file already exists and its MD5 matches,
-- the download is skipped; on mismatch the import fails without
-- overwriting. Fresh downloads are retried up to 'maxImportAttempts'
-- times on hash mismatch.
handleDiskImportCopy :: ServerState -> ApplySink -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Bool -> Text -> IO Response
handleDiskImportCopy state sink name source mDestPath mFormatStr mMd5 ephemeral nodeRefText =
  case validateName "Disk image" name of
    Left err -> pure $ RespError err
    Right () -> runServerLogging state $ do
      logInfoN $ "Importing disk image: " <> name <> " from " <> source

      case sanitizeDiskName name of
        Left err -> do
          logWarnN $ "Invalid disk name: " <> err
          pure $ RespError err
        Right safeName -> do
          mNid <- liftIO $ resolveImportTargetNode state nodeRefText
          case mNid of
            Left err -> pure $ RespError err
            Right nid -> do
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
                      let finalDest = resolveDiskFilePathPure basePath mDestPath destFileName
                      fetchResult <-
                        liftIO $
                          fetchAndVerify
                            state
                            nid
                            sink
                            name
                            FetchSpec
                              { fsUrl = source
                              , fsDownloadPath = downloadDest
                              , fsFinalPath = finalDest
                              , fsIsXz = isXz
                              , fsExpectedMd5 = mMd5
                              }
                      case fetchResult of
                        Left err -> do
                          logWarnN err
                          pure $ RespError err
                        Right diskPath -> registerImportedFile state nid basePath safeName format diskPath ephemeral
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
                              copyResult <- liftIO $ cloneImageViaAgent state nid canonSrc canonDest
                              case copyResult of
                                ImageSuccess -> registerImportedFile state nid basePath safeName format canonDest ephemeral
                                ImageError err -> do
                                  logWarnN $ "Copy failed: " <> err
                                  pure $ RespError $ "Copy failed: " <> err
                                ImageNotFound -> pure $ RespError "Source file not found"
                                ImageFormatNotSupported msg -> pure $ RespError msg
  where
    isXzUrl t = ".xz?" `T.isInfixOf` t

    registerImportedFile state' nid basePath safeName format diskPath ephem = do
      sizeMb <- liftIO $ getImageSizeMbViaAgent state' nid diskPath
      now <- liftIO getCurrentTime
      let storedPath = makeRelativeToBase basePath diskPath
      diskId <-
        liftIO $
          runSqlPool
            ( do
                dkey <-
                  insert
                    DiskImage
                      { diskImageName = safeName
                      , diskImageFormat = format
                      , diskImageSizeMb = sizeMb
                      , diskImageCreatedAt = now
                      , diskImageBackingImageId = Nothing
                      , diskImageEphemeral = ephem
                      }
                recordDiskImageNode dkey nid storedPath
                pure dkey
            )
            (ssDbPool state')
      logInfoN $ "Imported disk image with ID: " <> T.pack (show $ fromSqlKey diskId)
      pure $ RespDiskCreated $ fromSqlKey diskId

-- | Import a disk image from an HTTP/HTTPS URL and return its ID.
--
-- Downloads to the base images directory, decompresses @.xz@ if
-- needed. The optional @mMd5@ argument enables integrity-checking:
-- if the destination file already exists and its MD5 matches the
-- expected hash, the download is skipped; on a fresh download an
-- MD5 mismatch triggers up to 'maxImportAttempts' retries, after
-- which the import fails. On mismatch against a pre-existing file
-- the import fails without overwriting.
--
-- Exposed outside the Action typeclass so @crv apply@ can reuse it
-- directly when walking a YAML config.
importDiskFromUrlIO :: ServerState -> ApplySink -> Text -> Text -> Maybe DriveFormat -> Maybe Text -> Bool -> Text -> IO (Either Text Int64)
importDiskFromUrlIO state sink name url mFormat mMd5 ephemeral nodeRefText = do
  case sanitizeDiskName name of
    Left err -> pure $ Left err
    Right safeName -> do
      mNid <- resolveImportTargetNode state nodeRefText
      case mNid of
        Left err -> pure $ Left err
        Right nid -> importOnNode safeName nid
  where
    isInfixOf' needle haystack = needle `T.isInfixOf` T.pack haystack

    importOnNode :: Text -> M.NodeId -> IO (Either Text Int64)
    importOnNode safeName nid = do
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
          fetchResult <-
            fetchAndVerify
              state
              nid
              sink
              name
              FetchSpec
                { fsUrl = url
                , fsDownloadPath = downloadPath
                , fsFinalPath = finalPath
                , fsIsXz = isXz
                , fsExpectedMd5 = mMd5
                }
          case fetchResult of
            Left err -> pure $ Left err
            Right diskPath -> do
              let storedPath = makeRelativeToBase basePath diskPath
              sizeMb <- getImageSizeMbViaAgent state nid diskPath
              now <- getCurrentTime
              diskId <-
                runSqlPool
                  ( do
                      dkey <-
                        insert
                          DiskImage
                            { diskImageName = safeName
                            , diskImageFormat = format
                            , diskImageSizeMb = sizeMb
                            , diskImageCreatedAt = now
                            , diskImageBackingImageId = Nothing
                            , diskImageEphemeral = ephemeral
                            }
                      recordDiskImageNode dkey nid storedPath
                      pure dkey
                  )
                  (ssDbPool state)
              pure $ Right $ fromSqlKey diskId

--------------------------------------------------------------------------------
-- Fetch + verify
--------------------------------------------------------------------------------

-- | Inputs for 'fetchAndVerify'. Computed once upstream (knowing the
-- final destination path before downloading is needed for the
-- skip-if-exists check).
data FetchSpec = FetchSpec
  { fsUrl :: Text
  , fsDownloadPath :: FilePath
  -- ^ Where the bytes land on first write (e.g. @<base>/foo.qcow2.xz@).
  , fsFinalPath :: FilePath
  -- ^ Final on-disk file (post-decompression for @.xz@ URLs).
  , fsIsXz :: Bool
  , fsExpectedMd5 :: Maybe Text
  -- ^ When 'Just', the post-decompression file's MD5 must match.
  }

-- | Maximum number of download attempts when the MD5 doesn't match
-- the supplied hash. The user-facing wording was "retry up to three
-- times" which we interpret here as a 3-attempt cap (initial + 2
-- retries on mismatch).
maxImportAttempts :: Int
maxImportAttempts = 3

-- | Drive the download / decompression / hash-verify state machine,
-- returning the absolute path of the final on-disk artifact (the
-- @.qcow2@ for @.xz@ URLs, the downloaded file otherwise) on
-- success.
--
-- When 'fsExpectedMd5' is set:
--
--   * If the final file already exists, hash it; matching → success
--     (no download, no overwrite). Mismatch → fail without touching
--     the file.
--   * Otherwise, download and (optionally) decompress, then hash.
--     On mismatch, delete the result and retry, up to
--     'maxImportAttempts' total attempts.
--
-- When 'fsExpectedMd5' is 'Nothing', the existing-file behaviour
-- preserves the no-clobber default ("Destination file already
-- exists" error) and a single download attempt is made.
--
-- The @sink@ + @diskName@ pair, when non-silent, brackets each
-- download attempt with 'DownloadStart' / 'DownloadEnd' and pumps
-- byte-counted 'DownloadProgress' events fed from the node agent's
-- 'DiskDownloadSink'. Pass 'silentApplySink' for legacy callers
-- that don't stream.
fetchAndVerify
  :: ServerState
  -> M.NodeId
  -> ApplySink
  -> Text
  -- ^ Disk name (used as the @name@ field on download events).
  -> FetchSpec
  -> IO (Either Text FilePath)
fetchAndVerify state nid sink diskName FetchSpec {..} = do
  finalExists <- doesFileExist fsFinalPath
  case (finalExists, fsExpectedMd5) of
    (True, Just expected) -> do
      let expectedLc = T.toLower expected
      hashResult <- md5HashFileViaAgent state nid fsFinalPath
      case hashResult of
        Left err -> pure $ Left $ "verify existing " <> T.pack fsFinalPath <> ": " <> err
        Right actual
          | actual == expectedLc -> pure $ Right fsFinalPath
          | otherwise ->
              pure $
                Left $
                  "md5 of existing file "
                    <> T.pack fsFinalPath
                    <> " does not match (got "
                    <> actual
                    <> ", expected "
                    <> expectedLc
                    <> "); refusing to overwrite"
    (True, Nothing) -> pure $ Left "Destination file already exists"
    (False, _) -> downloadLoop 1
  where
    onProgress d t = sink (DownloadProgress diskName d t)

    downloadLoop n = do
      sink (DownloadStart diskName fsUrl)
      dlResult <- downloadImageViaAgent state nid fsDownloadPath fsUrl (Just onProgress)
      case dlResult of
        ImageError err -> do
          sink (DownloadEnd diskName False err)
          pure $ Left $ "Download failed: " <> err
        _ -> do
          sink (DownloadEnd diskName True "")
          decompressedResult <-
            if fsIsXz
              then decompressXzViaAgent state nid fsDownloadPath
              else pure $ Right fsFinalPath
          case decompressedResult of
            Left err -> pure $ Left err
            Right diskPath -> case fsExpectedMd5 of
              Nothing -> pure $ Right diskPath
              Just expected -> do
                let expectedLc = T.toLower expected
                hashResult <- md5HashFileViaAgent state nid diskPath
                case hashResult of
                  Left err -> pure $ Left $ "verify download: " <> err
                  Right actual
                    | actual == expectedLc -> pure $ Right diskPath
                    | n >= maxImportAttempts ->
                        pure $
                          Left $
                            "md5 mismatch after "
                              <> T.pack (show maxImportAttempts)
                              <> " attempts (got "
                              <> actual
                              <> ", expected "
                              <> expectedLc
                              <> ")"
                    | otherwise -> do
                        -- Drop the bad artifact via the agent and try again.
                        _ <- deleteImageViaAgent state nid diskPath
                        downloadLoop (n + 1)

--------------------------------------------------------------------------------
-- Action Types
--------------------------------------------------------------------------------

data DiskImportAction = DiskImportAction
  { diaName :: Text
  , diaSource :: Text
  , diaDestPath :: Maybe Text
  , diaFormat :: Maybe Text
  , diaMd5 :: Maybe Text
  , diaEphemeral :: Bool
  , diaNodeRef :: Text
  -- ^ Node where the import lands. Empty / @"0"@ defers to
  -- 'Corvus.Handlers.Scheduler.pickNodeForDisk'.
  }

instance Action DiskImportAction where
  actionSubsystem _ = SubDisk
  actionCommand _ = "import"
  actionEntityName = Just . diaName
  actionExecute ctx a = handleDiskImportCopy (acState ctx) (acApplySink ctx) (diaName a) (diaSource a) (diaDestPath a) (diaFormat a) (diaMd5 a) (diaEphemeral a) (diaNodeRef a)

data DiskImportUrl = DiskImportUrl
  { diuName :: Text
  , diuUrl :: Text
  , diuFormat :: Maybe Text
  , diuEphemeral :: Bool
  , diuNodeRef :: Text
  -- ^ Node where the import lands. Empty / @"0"@ defers to
  -- 'Corvus.Handlers.Scheduler.pickNodeForDisk'.
  }

instance Action DiskImportUrl where
  actionSubsystem _ = SubDisk
  actionCommand _ = "import-url"
  actionEntityName = Just . diuName
  actionExecute ctx a = handleDiskImportUrl (acState ctx) (acApplySink ctx) (diuName a) (diuUrl a) (diuFormat a) (diuEphemeral a) (diuNodeRef a)
