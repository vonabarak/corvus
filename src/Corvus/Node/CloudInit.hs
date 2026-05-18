{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cloud-init NoCloud ISO assembly — the host-side I/O half of
-- "Corvus.CloudInit".
--
-- The daemon composes @user-data@, @meta-data@, and optional
-- @network-config@ text (from DB rows + SSH key set) and ships
-- them across the wire. This module receives them on the agent,
-- writes the three files into @targetDir@, and shells out to
-- @genisoimage@ (falls back to @mkisofs@) to produce
-- @cloud-init.iso@.
--
-- Symmetric with how the daemon's old @generateCloudInitIso@
-- worked, just split across the wire: the daemon keeps the
-- compositional logic ("Corvus.CloudInit"), the agent keeps the
-- subprocess + filesystem surface (this module).
module Corvus.Node.CloudInit
  ( assembleCloudInitIso
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

-- | Assemble a NoCloud cloud-init ISO from already-composed text
-- inputs.
--
-- Steps:
--
--   1. @mkdir -p targetDir@.
--   2. Write @user-data@, @meta-data@, and optionally
--      @network-config@ into @targetDir@.
--   3. Try @genisoimage@; on failure, fall back to @mkisofs@.
--   4. On success, remove the three input files and return
--      @targetDir\/cloud-init.iso@. On both-tools-failed, return
--      a 'Left' carrying the second tool's error.
assembleCloudInitIso
  :: FilePath
  -- ^ Directory to write inputs and the resulting ISO into.
  -> Text
  -- ^ Composed @user-data@ text.
  -> Text
  -- ^ Composed @meta-data@ text.
  -> Maybe Text
  -- ^ Optional @network-config@ text.
  -> IO (Either Text FilePath)
assembleCloudInitIso targetDir userData metaData mNetworkConfig = do
  createDirectoryIfMissing True targetDir

  let userDataPath = targetDir </> "user-data"
      metaDataPath = targetDir </> "meta-data"
      networkConfigPath = targetDir </> "network-config"
      isoPath = targetDir </> "cloud-init.iso"

  TIO.writeFile userDataPath userData
  TIO.writeFile metaDataPath metaData

  mNetworkConfigFile <- case mNetworkConfig of
    Just nc -> do
      TIO.writeFile networkConfigPath nc
      pure $ Just networkConfigPath
    Nothing -> pure Nothing

  let inputFiles = [userDataPath, metaDataPath] ++ catMaybes [mNetworkConfigFile]

  -- Try genisoimage first, fall back to mkisofs.
  result <- tryGenIsoImage inputFiles isoPath
  case result of
    Right _ -> do
      mapM_ removeIfExists inputFiles
      pure $ Right isoPath
    Left _ -> do
      mkResult <- tryMkIsofs inputFiles isoPath
      case mkResult of
        Right _ -> do
          mapM_ removeIfExists inputFiles
          pure $ Right isoPath
        Left err -> pure $ Left err

-- | Try to create the ISO using @genisoimage@.
tryGenIsoImage :: [FilePath] -> FilePath -> IO (Either Text ())
tryGenIsoImage inputFiles isoPath = do
  result <-
    try $
      readProcessWithExitCode
        "genisoimage"
        (["-output", isoPath, "-volid", "cidata", "-joliet", "-rock"] ++ inputFiles)
        ""
  case result of
    Left (_ :: SomeException) -> pure $ Left "genisoimage not found"
    Right (ExitSuccess, _, _) -> pure $ Right ()
    Right (ExitFailure n, _, stderr) ->
      pure $
        Left $
          "genisoimage failed (exit " <> T.pack (show n) <> "): " <> T.pack stderr

-- | Try to create the ISO using @mkisofs@.
tryMkIsofs :: [FilePath] -> FilePath -> IO (Either Text ())
tryMkIsofs inputFiles isoPath = do
  result <-
    try $
      readProcessWithExitCode
        "mkisofs"
        (["-output", isoPath, "-volid", "cidata", "-joliet", "-rock"] ++ inputFiles)
        ""
  case result of
    Left (_ :: SomeException) -> pure $ Left "mkisofs not found"
    Right (ExitSuccess, _, _) -> pure $ Right ()
    Right (ExitFailure n, _, stderr) ->
      pure $
        Left $
          "mkisofs failed (exit " <> T.pack (show n) <> "): " <> T.pack stderr

-- | Helper to remove a file if it exists. Used to clean up the
-- three temp input files after the ISO is built.
removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path
