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
import qualified Corvus.Model as M
import Corvus.Qemu.Config (QemuConfig, getEffectiveBasePath)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (Entity (..), getBy)
import Database.Persist.Postgresql (SqlBackend, runSqlPool)
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

-- | Resolve a disk image file path for a specific node by
-- reading the matching 'DiskImageNode' row. Relative paths
-- (not starting with @\/@) are resolved against the daemon's
-- base directory. Returns the empty string if no placement
-- exists for @(diskId, nodeId)@ — callers that hit this case
-- should treat it as a clear bug (the same-node invariant
-- check should have rejected the request earlier).
resolveDiskPath
  :: Pool SqlBackend -> QemuConfig -> DiskImageId -> NodeId -> IO FilePath
resolveDiskPath pool config diskId nodeId = do
  basePath <- getEffectiveBasePath config
  mRow <- runSqlPool (getBy (M.UniqueDiskImageOnNode diskId nodeId)) pool
  case mRow of
    Nothing -> pure ""
    Just (Entity _ row) ->
      let stored = T.unpack (diskImageNodeFilePath row)
       in pure $
            if "/" `isPrefixOf` stored
              then stored
              else basePath </> stored

-- | Convert an absolute file path to a relative path if it falls within
-- the base directory. Paths outside the base directory are returned as-is.
--
-- Trailing slashes on @basePath@ (e.g. when a node was
-- registered with @\/home\/kvm\/VMs\/@) are normalised away
-- first so the prefix check matches regardless of how the
-- caller wrote it.
makeRelativeToBase :: FilePath -> FilePath -> Text
makeRelativeToBase basePath filePath
  | (b ++ "/") `isPrefixOf` filePath = T.pack $ drop (length b + 1) filePath
  | otherwise = T.pack filePath
  where
    b = stripTrailingSlashes basePath
    stripTrailingSlashes :: FilePath -> FilePath
    stripTrailingSlashes [] = []
    stripTrailingSlashes s
      | last s == '/' = stripTrailingSlashes (init s)
      | otherwise = s

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
