{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | VM image management for integration tests.
-- Downloads and caches cloud images, creates overlays for test isolation.
module Test.VM.Image
  ( -- * Configuration
    ImageConfig (..),
    defaultImageConfig,

    -- * Image operations
    ensureBaseImage,
    createOverlay,
    removeOverlay,

    -- * Utilities
    getCacheDir,
  )
where

import Control.Exception (try)
import Control.Monad (unless, when)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    getHomeDirectory,
    removeFile,
  )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

-- | Configuration for VM images
data ImageConfig = ImageConfig
  { -- | URL to download the base image from
    icImageUrl :: !Text,
    -- | Filename for the cached image
    icImageName :: !Text,
    -- | Cache directory (relative to home)
    icCacheDir :: !FilePath
  }
  deriving (Show, Eq)

-- | Default configuration using AlmaLinux 10 cloud image
defaultImageConfig :: ImageConfig
defaultImageConfig =
  ImageConfig
    { icImageUrl = "https://repo.almalinux.org/almalinux/10/cloud/x86_64_v2/images/AlmaLinux-10-GenericCloud-10.1-20251125.0.x86_64_v2.qcow2",
      icImageName = "AlmaLinux-10-GenericCloud-10.1-20251125.0.x86_64_v2.qcow2",
      icCacheDir = ".cache/corvus-test"
    }

-- | Get the cache directory path
getCacheDir :: ImageConfig -> IO FilePath
getCacheDir config = do
  home <- getHomeDirectory
  let cacheDir = home </> icCacheDir config
  createDirectoryIfMissing True cacheDir
  pure cacheDir

-- | Ensure the base image is downloaded and cached.
-- Returns the path to the cached image.
ensureBaseImage :: ImageConfig -> IO (Either Text FilePath)
ensureBaseImage config = do
  cacheDir <- getCacheDir config
  let imagePath = cacheDir </> T.unpack (icImageName config)

  exists <- doesFileExist imagePath
  if exists
    then pure $ Right imagePath
    else downloadImage config imagePath

-- | Download the base image to the specified path
downloadImage :: ImageConfig -> FilePath -> IO (Either Text FilePath)
downloadImage config destPath = do
  putStrLn $ "Downloading VM image from: " ++ T.unpack (icImageUrl config)
  putStrLn $ "This may take a few minutes..."

  let url = T.unpack (icImageUrl config)

  -- Try curl first, fall back to wget
  result <- try $ readProcessWithExitCode "curl" ["-L", "-o", destPath, "-#", url] ""

  case result of
    Left (_ :: IOError) -> do
      -- curl not available, try wget
      wgetResult <- try $ readProcessWithExitCode "wget" ["-O", destPath, url] ""
      case wgetResult of
        Left (_ :: IOError) ->
          pure $ Left "Neither curl nor wget is available for downloading images"
        Right (code, _, stderr) ->
          handleDownloadResult code stderr destPath
    Right (code, _, stderr) ->
      handleDownloadResult code stderr destPath
  where
    handleDownloadResult ExitSuccess _ path = do
      putStrLn "Download complete."
      pure $ Right path
    handleDownloadResult (ExitFailure n) stderr _ =
      pure $ Left $ "Download failed with exit code " <> T.pack (show n) <> ": " <> T.pack stderr

-- | Create a copy-on-write overlay image backed by the base image.
-- Each test should use its own overlay to ensure isolation.
createOverlay :: FilePath -> FilePath -> IO (Either Text FilePath)
createOverlay baseImage overlayPath = do
  -- Check base image exists
  baseExists <- doesFileExist baseImage
  unless baseExists $
    error $
      "Base image does not exist: " ++ baseImage

  -- Remove existing overlay if present
  overlayExists <- doesFileExist overlayPath
  when overlayExists $ removeFile overlayPath

  -- Create overlay using qemu-img
  (code, _, stderr) <-
    readProcessWithExitCode
      "qemu-img"
      [ "create",
        "-f",
        "qcow2",
        "-F",
        "qcow2",
        "-b",
        baseImage,
        overlayPath
      ]
      ""

  case code of
    ExitSuccess -> pure $ Right overlayPath
    ExitFailure n ->
      pure $
        Left $
          "Failed to create overlay (exit " <> T.pack (show n) <> "): " <> T.pack stderr

-- | Remove an overlay image
removeOverlay :: FilePath -> IO ()
removeOverlay overlayPath = do
  exists <- doesFileExist overlayPath
  when exists $ removeFile overlayPath
