{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | QEMU disk image management using qemu-img.
module Corvus.Qemu.Image
  ( -- * Image operations
    createImage
  , createOverlay
  , deleteImage
  , resizeImage
  , rebaseImage
  , getImageInfo
  , getImageSizeMb
  , parseImageInfo

    -- * Snapshot operations
  , createSnapshot
  , deleteSnapshot
  , rollbackSnapshot
  , mergeSnapshot
  , listSnapshots
  , cloneImage

    -- * Download operations
  , downloadImage
  , decompressXz

    -- * URL utilities
  , isHttpUrl
  , detectFormatFromUrl

    -- * Path utilities
  , detectFormatFromPath

    -- * Types
  , ImageInfo (..)
  , SnapshotData (..)
  , ImageResult (..)
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Corvus.Model (DriveFormat (..), EnumText (..))
import Data.Aeson (FromJSON (..), eitherDecodeStrict, withObject, (.:), (.:?))
import qualified Data.ByteString.Char8 as BS8
import Data.Int (Int64)
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import System.Directory (copyFile, doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension)
import System.Process (readProcessWithExitCode)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Result of an image operation
data ImageResult
  = ImageSuccess
  | ImageError !Text
  | ImageNotFound
  | ImageFormatNotSupported !Text
  deriving (Eq, Show)

-- | Information about a disk image
data ImageInfo = ImageInfo
  { iiFormat :: !DriveFormat
  , iiVirtualSizeMb :: !Int64
  , iiActualSizeMb :: !(Maybe Int64)
  , iiSnapshots :: ![SnapshotData]
  }
  deriving (Eq, Show)

-- | Snapshot data from qemu-img info
data SnapshotData = SnapshotData
  { sdId :: !Text
  , sdName :: !Text
  , sdSizeMb :: !(Maybe Int)
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Path to qemu-img binary
qemuImgBinary :: FilePath
qemuImgBinary = "qemu-img"

--------------------------------------------------------------------------------
-- Image Operations
--------------------------------------------------------------------------------

-- | Create a new disk image
createImage
  :: FilePath
  -- ^ File path
  -> DriveFormat
  -- ^ Format
  -> Int64
  -- ^ Size in MB
  -> IO ImageResult
createImage path format sizeMb = do
  exists <- doesFileExist path
  if exists
    then pure $ ImageError "File already exists"
    else do
      let formatStr = T.unpack $ enumToText format
          sizeStr = show sizeMb ++ "M"
          args = ["create", "-f", formatStr, path, sizeStr]
      (exitCode, _, stderr) <- readProcessWithExitCode qemuImgBinary args ""
      case exitCode of
        ExitSuccess -> pure ImageSuccess
        ExitFailure _ -> pure $ ImageError $ T.pack stderr

-- | Create a qcow2 overlay backed by an existing image
createOverlay
  :: FilePath
  -- ^ Overlay file path
  -> FilePath
  -- ^ Backing file path
  -> DriveFormat
  -- ^ Backing file format
  -> IO ImageResult
createOverlay overlayPath backingPath backingFormat = do
  exists <- doesFileExist overlayPath
  if exists
    then pure $ ImageError "Overlay file already exists"
    else do
      backingExists <- doesFileExist backingPath
      if not backingExists
        then pure $ ImageError $ "Backing file not found: " <> T.pack backingPath
        else do
          let formatStr = T.unpack $ enumToText backingFormat
              args = ["create", "-f", "qcow2", "-b", backingPath, "-F", formatStr, overlayPath]
          (exitCode, _, stderr) <- readProcessWithExitCode qemuImgBinary args ""
          case exitCode of
            ExitSuccess -> pure ImageSuccess
            ExitFailure _ -> pure $ ImageError $ T.pack stderr

-- | Delete a disk image
deleteImage
  :: FilePath
  -- ^ File path
  -> IO ImageResult
deleteImage path = do
  exists <- doesFileExist path
  if not exists
    then pure ImageNotFound
    else do
      result <- try $ removeFile path
      case result of
        Left (e :: SomeException) -> pure $ ImageError $ T.pack $ show e
        Right () -> pure ImageSuccess

-- | Resize a disk image (only works when VM is stopped)
resizeImage
  :: FilePath
  -- ^ File path
  -> Int64
  -- ^ New size in MB
  -> IO ImageResult
resizeImage path newSizeMb = do
  exists <- doesFileExist path
  if not exists
    then pure ImageNotFound
    else do
      let sizeStr = show newSizeMb ++ "M"
          args = ["resize", path, sizeStr]
      (exitCode, _, stderr) <- readProcessWithExitCode qemuImgBinary args ""
      case exitCode of
        ExitSuccess -> pure ImageSuccess
        ExitFailure _ -> pure $ ImageError $ T.pack stderr

-- | Rebase an overlay image to a different backing file, or flatten (remove backing).
-- Safe rebase (default) rewrites the overlay so reads produce the same result with the new backing.
-- Unsafe rebase (-u) only updates the backing pointer without data transformation.
rebaseImage
  :: FilePath
  -- ^ Overlay file path
  -> Maybe (FilePath, DriveFormat)
  -- ^ New backing (path, format). Nothing = flatten (remove backing).
  -> Bool
  -- ^ Unsafe mode
  -> IO ImageResult
rebaseImage overlayPath mNewBacking unsafe = do
  exists <- doesFileExist overlayPath
  if not exists
    then pure ImageNotFound
    else do
      let unsafeFlag = ["-u" | unsafe]
          backingArgs = case mNewBacking of
            Nothing -> ["-b", ""]
            Just (backingPath, backingFormat) ->
              ["-b", backingPath, "-F", T.unpack (enumToText backingFormat)]
          args = ["rebase"] ++ unsafeFlag ++ backingArgs ++ [overlayPath]
      (exitCode, _, stderr) <- readProcessWithExitCode qemuImgBinary args ""
      case exitCode of
        ExitSuccess -> pure ImageSuccess
        ExitFailure _ -> pure $ ImageError $ T.pack stderr

-- | Get information about a disk image.
--
-- Runs @qemu-img info --output=json@ and decodes the resulting JSON.
-- JSON output is stable across qemu versions (it tracks the QAPI
-- @ImageInfo@ schema); the older human-readable output changed format
-- between releases and required fragile line-grepping.
getImageInfo
  :: FilePath
  -- ^ File path
  -> IO (Either Text ImageInfo)
getImageInfo path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left "Image file not found"
    else do
      let args = ["info", "--output=json", path]
      (exitCode, stdout, stderr) <- readProcessWithExitCode qemuImgBinary args ""
      case exitCode of
        ExitFailure _ -> pure $ Left $ T.pack stderr
        ExitSuccess -> pure $ parseImageInfo stdout

-- | Get the virtual size of a disk image in MB, returning Nothing on any failure.
getImageSizeMb :: FilePath -> IO (Maybe Int)
getImageSizeMb path = do
  result <- getImageInfo path
  pure $ case result of
    Right info -> Just (fromIntegral $ iiVirtualSizeMb info)
    Left _ -> Nothing

--------------------------------------------------------------------------------
-- JSON wire types for @qemu-img info --output=json@.
--
-- These mirror the subset of the QAPI @ImageInfo@ schema we care about;
-- fields like @cluster-size@, @format-specific@, @dirty-flag@, and the
-- @children@ array are ignored.
--------------------------------------------------------------------------------

data QemuImgInfo = QemuImgInfo
  { qimFormat :: !Text
  , qimVirtualSize :: !Int64
  , qimActualSize :: !(Maybe Int64)
  , qimSnapshots :: !(Maybe [QemuImgSnapshot])
  }

instance FromJSON QemuImgInfo where
  parseJSON = withObject "QemuImgInfo" $ \o ->
    QemuImgInfo
      <$> o .: "format"
      <*> o .: "virtual-size"
      <*> o .:? "actual-size"
      <*> o .:? "snapshots"

data QemuImgSnapshot = QemuImgSnapshot
  { qisId :: !Text
  , qisName :: !Text
  , qisVmStateSize :: !(Maybe Int64)
  }

instance FromJSON QemuImgSnapshot where
  parseJSON = withObject "QemuImgSnapshot" $ \o ->
    QemuImgSnapshot
      <$> o .: "id"
      <*> o .: "name"
      <*> o .:? "vm-state-size"

-- | Parse the JSON payload produced by @qemu-img info --output=json@.
parseImageInfo :: String -> Either Text ImageInfo
parseImageInfo output =
  case eitherDecodeStrict (BS8.pack output) of
    Left err -> Left $ "Failed to parse qemu-img JSON: " <> T.pack err
    Right qim -> do
      format <- case enumFromText (qimFormat qim) of
        Right f -> Right f
        Left _ -> Left $ "Unknown format: " <> qimFormat qim
      Right
        ImageInfo
          { iiFormat = format
          , iiVirtualSizeMb = qimVirtualSize qim `div` (1024 * 1024)
          , iiActualSizeMb = fmap (`div` (1024 * 1024)) (qimActualSize qim)
          , iiSnapshots = maybe [] (map snapToInfo) (qimSnapshots qim)
          }
  where
    snapToInfo qs =
      SnapshotData
        { sdId = qisId qs
        , sdName = qisName qs
        , sdSizeMb = fmap (fromIntegral . (`div` (1024 * 1024))) (qisVmStateSize qs)
        }

--------------------------------------------------------------------------------
-- Snapshot Operations
--------------------------------------------------------------------------------

-- | Create a snapshot (qcow2 only)
createSnapshot
  :: FilePath
  -- ^ File path
  -> Text
  -- ^ Snapshot name
  -> IO ImageResult
createSnapshot path name = do
  exists <- doesFileExist path
  if not exists
    then pure ImageNotFound
    else do
      let args = ["snapshot", "-c", T.unpack name, path]
      (exitCode, _, stderr) <- readProcessWithExitCode qemuImgBinary args ""
      case exitCode of
        ExitSuccess -> pure ImageSuccess
        ExitFailure _ ->
          if "does not support" `T.isInfixOf` T.pack stderr
            then pure $ ImageFormatNotSupported "Snapshots require qcow2 format"
            else pure $ ImageError $ T.pack stderr

-- | Delete a snapshot (qcow2 only)
deleteSnapshot
  :: FilePath
  -- ^ File path
  -> Text
  -- ^ Snapshot name
  -> IO ImageResult
deleteSnapshot path name = do
  exists <- doesFileExist path
  if not exists
    then pure ImageNotFound
    else do
      let args = ["snapshot", "-d", T.unpack name, path]
      (exitCode, _, stderr) <- readProcessWithExitCode qemuImgBinary args ""
      case exitCode of
        ExitSuccess -> pure ImageSuccess
        ExitFailure _ ->
          if "does not support" `T.isInfixOf` T.pack stderr
            then pure $ ImageFormatNotSupported "Snapshots require qcow2 format"
            else pure $ ImageError $ T.pack stderr

-- | Rollback to a snapshot (qcow2 only, VM must be stopped)
rollbackSnapshot
  :: FilePath
  -- ^ File path
  -> Text
  -- ^ Snapshot name
  -> IO ImageResult
rollbackSnapshot path name = do
  exists <- doesFileExist path
  if not exists
    then pure ImageNotFound
    else do
      let args = ["snapshot", "-a", T.unpack name, path]
      (exitCode, _, stderr) <- readProcessWithExitCode qemuImgBinary args ""
      case exitCode of
        ExitSuccess -> pure ImageSuccess
        ExitFailure _ ->
          if "does not support" `T.isInfixOf` T.pack stderr
            then pure $ ImageFormatNotSupported "Snapshots require qcow2 format"
            else pure $ ImageError $ T.pack stderr

-- | Merge a snapshot (deletes snapshot metadata, preserving current disk state)
-- For internal qcow2 snapshots, merging simply removes the snapshot record
-- while keeping all current data intact. This is the opposite of rollback.
mergeSnapshot
  :: FilePath
  -- ^ File path
  -> Text
  -- ^ Snapshot name to merge (delete)
  -> IO ImageResult
mergeSnapshot path name = do
  exists <- doesFileExist path
  if not exists
    then pure ImageNotFound
    else deleteSnapshot path name

-- | List snapshots in an image (qcow2 only)
listSnapshots
  :: FilePath
  -- ^ File path
  -> IO (Either Text [SnapshotData])
listSnapshots path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left "Image file not found"
    else do
      infoResult <- getImageInfo path
      case infoResult of
        Left err -> pure $ Left err
        Right info -> pure $ Right $ iiSnapshots info

-- | Clone a disk image file
cloneImage :: FilePath -> FilePath -> IO ImageResult
cloneImage src dest = do
  exists <- doesFileExist src
  if not exists
    then pure ImageNotFound
    else do
      destExists <- doesFileExist dest
      if destExists
        then pure $ ImageError "Destination file already exists"
        else do
          result <- try $ copyFile src dest
          case result of
            Left (e :: SomeException) -> pure $ ImageError $ T.pack $ show e
            Right () -> pure ImageSuccess

--------------------------------------------------------------------------------
-- Download Operations
--------------------------------------------------------------------------------

-- | Download a file from an HTTP/HTTPS URL to the given destination path.
-- Tries curl first, falls back to wget.
downloadImage
  :: FilePath
  -- ^ Destination file path
  -> Text
  -- ^ URL to download from
  -> IO ImageResult
downloadImage destPath url = do
  exists <- doesFileExist destPath
  if exists
    then pure $ ImageError "Destination file already exists"
    else do
      let urlStr = T.unpack url
      -- Try curl first
      curlResult <- try $ readProcessWithExitCode "curl" ["-L", "-o", destPath, "-s", "-S", urlStr] ""
      case curlResult of
        Left (_ :: SomeException) -> do
          -- curl not available, try wget
          wgetResult <- try $ readProcessWithExitCode "wget" ["-O", destPath, "-q", urlStr] ""
          case wgetResult of
            Left (_ :: SomeException) ->
              pure $ ImageError "Neither curl nor wget is available for downloading images"
            Right (ExitSuccess, _, _) -> pure ImageSuccess
            Right (ExitFailure n, _, stderr) ->
              pure $ ImageError $ "wget failed (exit " <> T.pack (show n) <> "): " <> T.pack stderr
        Right (ExitSuccess, _, _) -> pure ImageSuccess
        Right (ExitFailure n, _, stderr) ->
          pure $ ImageError $ "curl failed (exit " <> T.pack (show n) <> "): " <> T.pack stderr

-- | Decompress an .xz file in place. Returns the path to the decompressed file.
-- The .xz file is removed after successful decompression.
decompressXz
  :: FilePath
  -- ^ Path to .xz file
  -> IO (Either Text FilePath)
decompressXz xzPath = do
  let finalPath = take (length xzPath - 3) xzPath -- strip .xz
  result <- try $ readProcessWithExitCode "xz" ["-d", xzPath] ""
  case result of
    Left (_ :: SomeException) ->
      pure $ Left "xz command not found for decompressing image"
    Right (ExitSuccess, _, _) ->
      pure $ Right finalPath
    Right (ExitFailure n, _, stderr) ->
      pure $ Left $ "xz decompression failed (exit " <> T.pack (show n) <> "): " <> T.pack stderr

--------------------------------------------------------------------------------
-- URL Utilities
--------------------------------------------------------------------------------

-- | Check if a string looks like an HTTP/HTTPS URL
isHttpUrl :: Text -> Bool
isHttpUrl t = "http://" `T.isPrefixOf` t || "https://" `T.isPrefixOf` t

-- | Detect disk format from a URL's file extension.
-- Strips .xz suffix first if present, then checks the inner extension.
detectFormatFromUrl :: Text -> Maybe DriveFormat
detectFormatFromUrl url =
  let -- Extract filename portion (after last /)
      filename = T.unpack $ snd $ T.breakOnEnd "/" url
      -- Strip query string
      base = takeWhile (/= '?') filename
      -- Strip .xz if present
      inner = if ".xz" `isSuffixOf` base then take (length base - 3) base else base
      ext = takeExtension inner
   in case ext of
        ".qcow2" -> Just FormatQcow2
        ".raw" -> Just FormatRaw
        ".img" -> Just FormatRaw
        ".vmdk" -> Just FormatVmdk
        ".vdi" -> Just FormatVdi
        ".vpc" -> Just FormatVpc
        ".vhd" -> Just FormatVpc
        ".vhdx" -> Just FormatVhdx
        _ -> Nothing

-- | Detect disk format from a file path extension.
detectFormatFromPath :: Text -> Maybe DriveFormat
detectFormatFromPath path =
  case takeExtension (T.unpack path) of
    ".qcow2" -> Just FormatQcow2
    ".raw" -> Just FormatRaw
    ".img" -> Just FormatRaw
    ".vmdk" -> Just FormatVmdk
    ".vdi" -> Just FormatVdi
    ".vpc" -> Just FormatVpc
    ".vhd" -> Just FormatVpc
    ".vhdx" -> Just FormatVhdx
    _ -> Nothing
