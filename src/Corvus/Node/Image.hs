{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | QEMU disk image management using qemu-img.
--
-- Phase 2: lives under "Corvus.Node" because every function in it
-- shells out to a host-side tool (@qemu-img@, @cp@, @curl@/@wget@,
-- @xz@) and writes to the local disk. Invoked by the
-- agent's 'Corvus.Node.Caps.Session'. The daemon reaches it
-- exclusively through the Cap'n Proto wire (see
-- "Corvus.NodeAgentClient").
--
-- Pure helpers ('isHttpUrl', 'detectFormatFromUrl',
-- 'detectFormatFromPath', 'parseImageInfo') stay in this module
-- and are imported daemon-side from here for path/format
-- classification.
module Corvus.Node.Image
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

    -- * Hashing
  , hashFile
  , md5HashFile

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

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Corvus.Model (DriveFormat (..), EnumText (..))
import qualified Crypto.Hash as Hash
import Data.Aeson (FromJSON (..), eitherDecodeStrict, withObject, (.:), (.:?))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit, toLower)
import Data.Int (Int64)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import System.Directory (copyFile, doesFileExist, removeFile)
import qualified System.Directory as D
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension)
import qualified System.Posix.Files as Posix
import System.Process
  ( ProcessHandle
  , StdStream (CreatePipe)
  , createProcess
  , getProcessExitCode
  , proc
  , readProcessWithExitCode
  , std_err
  , std_out
  , waitForProcess
  )

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

-- | Clone a disk image file via @qemu-img convert -O <destFormat>@.
--
-- Uses 'convert' rather than a flat byte copy so:
--
-- 1. The destination contains only the source's active state — no
--    internal snapshots, no vmstate. The
--    'Corvus.Handlers.Build.publishArtifactByClone' caller relies
--    on this: a published artifact must be a flat standalone image
--    with an empty snapshot table (@qemu-img snapshot -l@ returning
--    empty).
-- 2. The output format is selectable. A @qcow2 -> raw@ clone (e.g.
--    the installer-strategy bake whose target is @format: raw@)
--    physically converts the bytes; the destination file then
--    matches the format the daemon's 'DiskImage' row will record.
--    Hardcoding @-O qcow2@ left raw targets with qcow2-magic bytes
--    on disk, which then surfaced to verify-side guests as the
--    qcow2 header instead of the installer's payload.
--
-- The previous flat-copyFile implementation worked while disk-mode
-- cache was the default — the source qcow2 only had small per-step
-- block snapshots in its metadata table — but as soon as memory-
-- mode cache landed each per-step snapshot included full RAM
-- contents, ballooning the source qcow2 by RAM-size per step
-- (~18 GB after a 9-step 2-GB bake). A flat copy then needed
-- 2× that to land the destination atomically and routinely hit
-- @ENOSPC@ on the publish step even when the active state was
-- only a few GB.
cloneImage :: FilePath -> FilePath -> Text -> IO ImageResult
cloneImage src dest destFormat = do
  exists <- doesFileExist src
  if not exists
    then pure ImageNotFound
    else do
      destExists <- doesFileExist dest
      if destExists
        then pure $ ImageError "Destination file already exists"
        else do
          let args = ["convert", "-O", T.unpack destFormat, src, dest]
          result <-
            try $ readProcessWithExitCode qemuImgBinary args ""
          case result of
            Left (e :: SomeException) ->
              pure $ ImageError $ T.pack (show e)
            Right (ExitSuccess, _, _) -> pure ImageSuccess
            Right (ExitFailure _, _, stderr) ->
              pure $ ImageError $ T.pack stderr

--------------------------------------------------------------------------------
-- Download Operations
--------------------------------------------------------------------------------

-- | Download a file from an HTTP/HTTPS URL to the given destination
-- path. Tries curl first, falls back to wget.
--
-- The @onProgress@ callback is invoked every ~250 ms with the
-- current size of the destination file and the total transfer
-- length (probed once via HEAD, or @0@ when Content-Length is
-- unknown / the HEAD request fails). Exactly one extra @onProgress@
-- call fires after the download finishes carrying the final file
-- size. The callback's exceptions are swallowed so a misbehaving
-- sink can't kill the transfer.
downloadImage
  :: FilePath
  -- ^ Destination file path
  -> Text
  -- ^ URL to download from
  -> (Int64 -> Int64 -> IO ())
  -- ^ Progress callback: @(downloaded, total)@. Pass
  -- @\_ _ -> pure ()@ when no progress reporting is wanted.
  -> IO ImageResult
downloadImage destPath url onProgress = do
  exists <- doesFileExist destPath
  if exists
    then pure $ ImageError "Destination file already exists"
    else do
      total <- probeContentLength url
      safeProgress 0 total
      let urlStr = T.unpack url
          curlProc =
            (proc "curl" ["-L", "-o", destPath, "-s", "-S", urlStr])
              { std_out = CreatePipe
              , std_err = CreatePipe
              }
      curlAttempt <- try (createProcess curlProc)
      case curlAttempt of
        Left (_ :: SomeException) ->
          runWget total urlStr
        Right (_, _, mStderr, ph) -> do
          finalize
            ph
            mStderr
            total
            "curl"
            ( \case
                ExitSuccess -> pure ImageSuccess
                ExitFailure n -> do
                  -- A non-zero exit may be "curl: command not found"
                  -- (treated as exit 127 when started via a shell, but
                  -- via 'proc' the spawn itself succeeded — so a real
                  -- 127 here means curl ran and failed). Either way
                  -- fall back to wget on missing-binary signals.
                  if n == 127
                    then runWget total urlStr
                    else readErrText mStderr >>= \err -> pure $ ImageError $ "curl failed (exit " <> T.pack (show n) <> "): " <> err
            )
  where
    safeProgress :: Int64 -> Int64 -> IO ()
    safeProgress d t = do
      _ <- try (onProgress d t) :: IO (Either SomeException ())
      pure ()

    -- Poll the destination file's size every 250 ms while the
    -- transfer process is alive, pushing each observed size
    -- through the progress callback.
    pollSize :: ProcessHandle -> Int64 -> IO ()
    pollSize ph total = loop
      where
        loop = do
          mExit <- getProcessExitCode ph
          case mExit of
            Just _ -> pure ()
            Nothing -> do
              size <- currentFileSize destPath
              safeProgress size total
              threadDelay 250_000
              loop

    finalize ph mStderr total binaryName onExit = do
      poll <- async (pollSize ph total)
      exit <- waitForProcess ph
      cancel poll
      finalSize <- currentFileSize destPath
      safeProgress finalSize total
      result <- onExit exit
      -- Touch the binaryName so it appears in the closure's free
      -- vars (silences -Wunused-matches when both transports succeed
      -- without ever stringifying the name).
      _ <- pure (binaryName :: String)
      pure result

    runWget total urlStr = do
      let wgetProc =
            (proc "wget" ["-O", destPath, "-q", urlStr])
              { std_out = CreatePipe
              , std_err = CreatePipe
              }
      wgetAttempt <- try (createProcess wgetProc)
      case wgetAttempt of
        Left (_ :: SomeException) ->
          pure $ ImageError "Neither curl nor wget is available for downloading images"
        Right (_, _, mStderr, ph) ->
          finalize
            ph
            mStderr
            total
            "wget"
            ( \case
                ExitSuccess -> pure ImageSuccess
                ExitFailure n -> readErrText mStderr >>= \err -> pure $ ImageError $ "wget failed (exit " <> T.pack (show n) <> "): " <> err
            )

    readErrText mh = case mh of
      Nothing -> pure T.empty
      Just h -> do
        result <- try (BS8.hGetContents h)
        pure $ case result of
          Left (_ :: SomeException) -> T.empty
          Right bs -> T.pack (BS8.unpack bs)

-- | Stat the destination file. Returns 0 on any error (file not
-- yet created, agent permissions, etc.).
currentFileSize :: FilePath -> IO Int64
currentFileSize path = do
  exists <- D.doesFileExist path
  if not exists
    then pure 0
    else do
      result <- try (Posix.getFileStatus path)
      pure $ case result of
        Left (_ :: SomeException) -> 0
        Right st -> fromIntegral (Posix.fileSize st)

-- | Issue a HEAD request via curl to discover the total transfer
-- length. Returns @0@ on any failure (network issues, the server
-- doesn't support HEAD, no @Content-Length@ in the response, …) so
-- the caller falls back to a counter-only display.
probeContentLength :: Text -> IO Int64
probeContentLength url = do
  let args = ["-sIL", "--max-time", "10", T.unpack url]
  result <- try (readProcessWithExitCode "curl" args "")
  pure $ case result of
    Left (_ :: SomeException) -> 0
    Right (ExitSuccess, out, _) -> parseLastContentLength out
    Right _ -> 0

-- | Pick the last @Content-Length: NNNN@ header from a HEAD
-- response transcript. The "last" preference is intentional — when
-- curl follows redirects (@-L@) each hop's headers are printed; we
-- care about the final target's length.
parseLastContentLength :: String -> Int64
parseLastContentLength = go 0 . lines
  where
    go acc [] = acc
    go acc (l : rest) = case stripPrefixCI "content-length:" l of
      Nothing -> go acc rest
      Just rest' ->
        let value = takeWhile isDigit (dropWhile (== ' ') rest')
         in if null value
              then go acc rest
              else go (read value) rest

    stripPrefixCI :: String -> String -> Maybe String
    stripPrefixCI prefix s
      | map toLower prefix `isPrefixOf` map toLower s =
          Just (drop (length prefix) s)
      | otherwise = Nothing

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
-- Hashing
--------------------------------------------------------------------------------

-- | Compute a hex digest of a file. Supported algorithm names are
-- @md5@, @sha1@, @sha256@, @sha512@, and @blake2b@.
hashFile :: Text -> FilePath -> IO (Either Text Text)
hashFile algorithm path = do
  result <- try $ LBS.readFile path
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ "hashFile: cannot read " <> T.pack path <> ": " <> T.pack (show e)
    Right bytes -> case T.toLower algorithm of
      "md5" -> pure $ Right $ digestHex (Hash.hashlazy bytes :: Hash.Digest Hash.MD5)
      "sha1" -> pure $ Right $ digestHex (Hash.hashlazy bytes :: Hash.Digest Hash.SHA1)
      "sha256" -> pure $ Right $ digestHex (Hash.hashlazy bytes :: Hash.Digest Hash.SHA256)
      "sha512" -> pure $ Right $ digestHex (Hash.hashlazy bytes :: Hash.Digest Hash.SHA512)
      "blake2b" -> pure $ Right $ digestHex (Hash.hashlazy bytes :: Hash.Digest Hash.Blake2b_512)
      other -> pure $ Left $ "unsupported hash algorithm: " <> other
  where
    digestHex :: (Show a) => a -> Text
    digestHex = T.toLower . T.pack . show

-- | Compute the MD5 hash of a file.
md5HashFile :: FilePath -> IO (Either Text Text)
md5HashFile = hashFile "md5"

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
