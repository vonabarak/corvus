{-# LANGUAGE OverloadedStrings #-}

-- | Disk image path utilities.
--
-- Pure helpers shared across all disk-handling code. Kept separate from
-- 'Corvus.Handlers.Disk' so other subsystems (cloud-init ISO placement,
-- virtiofsd runtime paths) can reuse them without dragging in the full
-- disk-handler module.
module Corvus.Handlers.Disk.Path
  ( sanitizeDiskName
  , resolveDiskPath
  , makeRelativeToBase
  , resolveDiskFilePath
  , resolveDiskFilePathPure
  )
where

import Corvus.Handlers.Resolve (validateName)
import Corvus.Model
import Corvus.Qemu.Config (QemuConfig, getEffectiveBasePath)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath (isRelative, takeDirectory, (</>))

-- | Sanitize a disk image name to prevent path traversal attacks.
--
-- First removes dangerous characters (path separators, null bytes), then
-- removes parent directory references to handle cases like @".\0."@ →
-- @".."@. Also rejects all-digit names to prevent ambiguity with numeric
-- IDs (that check is enforced by 'validateName').
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
-- Relative paths (not starting with @\/@) are resolved against the base path.
resolveDiskPath :: QemuConfig -> DiskImage -> IO FilePath
resolveDiskPath config disk = do
  basePath <- getEffectiveBasePath config
  let rawPath = T.unpack $ diskImageFilePath disk
  pure $
    if "/" `T.isPrefixOf` diskImageFilePath disk
      then rawPath
      else basePath </> rawPath

-- | Convert an absolute file path to a relative path if it falls within
-- the base directory. Paths outside the base directory are returned as-is.
makeRelativeToBase :: FilePath -> FilePath -> Text
makeRelativeToBase basePath filePath
  | (basePath ++ "/") `isPrefixOf` filePath = T.pack $ drop (length basePath + 1) filePath
  | otherwise = T.pack filePath

-- | Resolve an optional path for a disk image file.
--
-- Path interpretation rules:
--
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
