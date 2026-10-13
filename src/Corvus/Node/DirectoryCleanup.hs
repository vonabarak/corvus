-- | Non-recursive cleanup of empty storage directories on the nodeagent.
module Corvus.Node.DirectoryCleanup (removeDirectoryIfEmpty) where

import Control.Monad (unless, when)
import GHC.IO.Exception (IOErrorType (UnsatisfiedConstraints))
import System.Directory (canonicalizePath, listDirectory, pathIsSymbolicLink, removeDirectory)
import System.FilePath (dropTrailingPathSeparator)
import System.IO.Error (catchIOError, ioeGetErrorType, isAlreadyExistsError, isDoesNotExistError)

-- | Remove only this directory, preserving symlinks and protected storage
-- roots. A missing directory or one that gains contents during cleanup is a
-- no-op. Other I/O errors are reported to the caller for best-effort logging.
removeDirectoryIfEmpty :: [FilePath] -> FilePath -> IO ()
removeDirectoryIfEmpty protectedPaths path =
  catchIOError cleanup $ \err ->
    unless (isDoesNotExistError err) (ioError err)
  where
    directory = dropTrailingPathSeparator path
    cleanup = do
      symbolic <- pathIsSymbolicLink directory
      unless symbolic $ do
        resolved <- canonicalizePath directory
        roots <- mapM canonicalizePath ("/" : protectedPaths)
        unless (resolved `elem` roots) $ do
          contents <- listDirectory directory
          when (null contents) $
            catchIOError (removeDirectory directory) $ \err ->
              unless
                ( isDoesNotExistError err
                    || isAlreadyExistsError err
                    || ioeGetErrorType err == UnsatisfiedConstraints
                )
                (ioError err)
