{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | QEMU disk image management using qemu-img.
module Corvus.Qemu.Image
  ( -- * Image operations
    createImage,
    createOverlay,
    deleteImage,
    resizeImage,
    getImageInfo,

    -- * Snapshot operations
    createSnapshot,
    deleteSnapshot,
    rollbackSnapshot,
    mergeSnapshot,
    listSnapshots,
    cloneImage,

    -- * Types
    ImageInfo (..),
    SnapshotData (..),
    ImageResult (..),
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Corvus.Model (DriveFormat (..), EnumText (..))
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import System.Directory (copyFile, doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

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
  { iiFormat :: !DriveFormat,
    iiVirtualSizeMb :: !Int64,
    iiActualSizeMb :: !(Maybe Int64),
    iiSnapshots :: ![SnapshotData]
  }
  deriving (Eq, Show)

-- | Snapshot data from qemu-img info
data SnapshotData = SnapshotData
  { sdId :: !Text,
    sdName :: !Text,
    sdSizeMb :: !(Maybe Int)
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
createImage ::
  -- | File path
  FilePath ->
  -- | Format
  DriveFormat ->
  -- | Size in MB
  Int64 ->
  IO ImageResult
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
createOverlay ::
  -- | Overlay file path
  FilePath ->
  -- | Backing file path
  FilePath ->
  -- | Backing file format
  DriveFormat ->
  IO ImageResult
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
deleteImage ::
  -- | File path
  FilePath ->
  IO ImageResult
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
resizeImage ::
  -- | File path
  FilePath ->
  -- | New size in MB
  Int64 ->
  IO ImageResult
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

-- | Get information about a disk image
getImageInfo ::
  -- | File path
  FilePath ->
  IO (Either Text ImageInfo)
getImageInfo path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left "Image file not found"
    else do
      let args = ["info", "--output=human", path]
      (exitCode, stdout, stderr) <- readProcessWithExitCode qemuImgBinary args ""
      case exitCode of
        ExitFailure _ -> pure $ Left $ T.pack stderr
        ExitSuccess -> pure $ parseImageInfo stdout

-- | Parse qemu-img info output
parseImageInfo :: String -> Either Text ImageInfo
parseImageInfo output = do
  let ls = lines output
  format <- parseFormat ls
  virtualSize <- parseVirtualSize ls
  let actualSize = parseActualSize ls
      snapshots = parseSnapshots ls
  Right $
    ImageInfo
      { iiFormat = format,
        iiVirtualSizeMb = virtualSize,
        iiActualSizeMb = actualSize,
        iiSnapshots = snapshots
      }
  where
    parseFormat ls =
      case filter ("file format:" `T.isPrefixOf`) (map T.pack ls) of
        (line : _) ->
          let formatStr = T.strip $ T.drop (T.length "file format:") line
           in case enumFromText formatStr of
                Right f -> Right f
                Left _ -> Left $ "Unknown format: " <> formatStr
        [] -> Left "Could not find format in image info"

    parseVirtualSize ls =
      case filter ("virtual size:" `T.isPrefixOf`) (map T.pack ls) of
        (line : _) -> extractSizeMb line
        [] -> Left "Could not find virtual size in image info"

    parseActualSize ls =
      case filter ("disk size:" `T.isPrefixOf`) (map T.pack ls) of
        (line : _) -> either (const Nothing) Just $ extractSizeMb line
        [] -> Nothing

    extractSizeMb line =
      -- qemu-img info output: "virtual size: 10 GiB (10737418240 bytes)"
      -- Extract the byte count from inside parentheses
      case T.breakOn "(" line of
        (_, rest)
          | not (T.null rest) ->
              -- rest is "(10737418240 bytes)" - extract the number
              let numStr = T.takeWhile (`elem` ['0' .. '9']) $ T.drop 1 rest
               in case readMaybe (T.unpack numStr) of
                    Just n -> Right $ n `div` (1024 * 1024)
                    Nothing -> Left "Could not parse byte count from parentheses"
        _ -> Left "Could not find byte count in parentheses"

    parseSnapshots ls =
      let textLines = map T.pack ls
          snapshotLines = dropWhile (not . ("Snapshot list:" `T.isPrefixOf`)) textLines
       in case snapshotLines of
            [] -> []
            (_ : _ : rest) -> mapMaybe parseSnapshotLine $ takeWhile (not . T.null) rest
            _ -> []

    parseSnapshotLine line =
      let parts = T.words line
       in case parts of
            (snapId : name : _) ->
              Just $
                SnapshotData
                  { sdId = snapId,
                    sdName = name,
                    sdSizeMb = Nothing
                  }
            _ -> Nothing

--------------------------------------------------------------------------------
-- Snapshot Operations
--------------------------------------------------------------------------------

-- | Create a snapshot (qcow2 only)
createSnapshot ::
  -- | File path
  FilePath ->
  -- | Snapshot name
  Text ->
  IO ImageResult
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
deleteSnapshot ::
  -- | File path
  FilePath ->
  -- | Snapshot name
  Text ->
  IO ImageResult
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
rollbackSnapshot ::
  -- | File path
  FilePath ->
  -- | Snapshot name
  Text ->
  IO ImageResult
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
mergeSnapshot ::
  -- | File path
  FilePath ->
  -- | Snapshot name to merge (delete)
  Text ->
  IO ImageResult
mergeSnapshot path name = do
  exists <- doesFileExist path
  if not exists
    then pure ImageNotFound
    else deleteSnapshot path name

-- | List snapshots in an image (qcow2 only)
listSnapshots ::
  -- | File path
  FilePath ->
  IO (Either Text [SnapshotData])
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
