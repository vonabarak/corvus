{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | VM image management for integration tests.
-- Downloads and caches cloud images, creates overlays for test isolation.
module Test.VM.Image
  ( -- * Configuration
    ImageConfig (..),
    getImageConfig,

    -- * Image operations
    ensureBaseImage,

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
    getCurrentDirectory,
    removeFile,
  )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Settings

-- | Get the cache directory path
getCacheDir :: ImageConfig -> IO FilePath
getCacheDir config = do
  projectRoot <- getCurrentDirectory
  let cacheDir = projectRoot </> ".test-images"
  createDirectoryIfMissing True cacheDir
  pure cacheDir

-- | Ensure the base image is downloaded and cached.
-- Returns the path to the cached image.
ensureBaseImage :: Text -> IO (Either Text FilePath)
ensureBaseImage osName = case getImageConfig osName of
  Nothing -> pure $ Left $ "Unsupported OS: " <> osName
  Just config -> do
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
  putStrLn "This may take a few minutes..."

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

