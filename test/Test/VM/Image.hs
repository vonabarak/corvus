{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | VM image management for integration tests.
-- Downloads and caches cloud images, creates overlays for test isolation.
module Test.VM.Image
  ( -- * Image operations
    ensureBaseImage
  )
where

import Test.VM.Types (prebakedImageName)

import Control.Exception (try)
import Control.Monad (unless, when)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getCurrentDirectory
  , removeFile
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
-- Handles .xz compressed images by decompressing after download.
-- For "corvus-test", returns the local pre-baked image path (no download).
ensureBaseImage :: Text -> IO (Either Text FilePath)
ensureBaseImage osName
  | osName == prebakedImageName = do
      projectRoot <- getCurrentDirectory
      let imagePath = projectRoot </> ".test-images" </> "corvus-test.qcow2"
      exists <- doesFileExist imagePath
      if exists
        then pure $ Right imagePath
        else
          pure $
            Left $
              "Pre-baked test image not found: "
                <> T.pack imagePath
                <> ". Run 'make test-image' to build it."
  | otherwise = case getImageConfig osName of
      Nothing -> pure $ Left $ "Unsupported OS: " <> osName
      Just config -> do
        cacheDir <- getCacheDir config
        let imageName = T.unpack (icImageName config)
            isXz = ".xz" `isSuffixOf` imageName
            finalPath = if isXz then cacheDir </> dropXzExt imageName else cacheDir </> imageName
            downloadPath = cacheDir </> imageName

        exists <- doesFileExist finalPath
        if exists
          then pure $ Right finalPath
          else do
            result <- downloadImage config downloadPath
            case result of
              Left err -> pure $ Left err
              Right path
                | isXz -> decompressXz path finalPath
                | otherwise -> pure $ Right path

-- | Drop .xz extension from a filename
dropXzExt :: String -> String
dropXzExt name
  | ".xz" `isSuffixOf` name = take (length name - 3) name
  | otherwise = name

-- | Check if a string ends with a suffix
isSuffixOf :: String -> String -> Bool
isSuffixOf suffix str = drop (length str - length suffix) str == suffix

-- | Decompress an .xz file and remove the compressed original
decompressXz :: FilePath -> FilePath -> IO (Either Text FilePath)
decompressXz xzPath finalPath = do
  putStrLn $ "Decompressing " ++ xzPath ++ "..."
  result <- try $ readProcessWithExitCode "xz" ["-d", "-k", xzPath] ""
  case result of
    Left (_ :: IOError) ->
      pure $ Left "xz command not found for decompressing image"
    Right (ExitSuccess, _, _) -> do
      -- xz -d -k decompresses in place (removes .xz, keeps original with -k)
      -- The output file is the input without .xz extension
      let decompressedPath = dropXzExt xzPath
      if decompressedPath /= finalPath
        then do
          -- Rename to expected path if different
          renameResult <- try $ readProcessWithExitCode "mv" [decompressedPath, finalPath] ""
          case renameResult of
            Left (_ :: IOError) -> pure $ Left "Failed to move decompressed image"
            Right (ExitSuccess, _, _) -> do
              removeFile xzPath
              putStrLn "Decompression complete."
              pure $ Right finalPath
            Right (ExitFailure n, _, stderr) ->
              pure $ Left $ "mv failed: " <> T.pack (show n) <> " " <> T.pack stderr
        else do
          removeFile xzPath
          putStrLn "Decompression complete."
          pure $ Right finalPath
    Right (ExitFailure n, _, stderr) ->
      pure $ Left $ "xz decompression failed: " <> T.pack (show n) <> " " <> T.pack stderr

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
