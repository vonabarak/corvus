{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Scoped file callbacks. Dropped/cancelled transfers reject later calls.
module Corvus.Rpc.FileTransfer (withSource, readSource, bytesSource, withDownload, withDownloadNoReplace) where

import qualified Capnp as C
import qualified Capnp.Gen.Streams as S
import Capnp.Rpc.Server (SomeServer)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception (SomeException, bracket, bracketOnError, catch, finally)
import Control.Monad (unless, when)
import Corvus.NodeAgentClient.Core (callOn)
import Corvus.Rpc.Common (handleParsed, handleParsedAsync)
import qualified Data.ByteString as BS
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Word (Word64)
import Supervisors (Supervisor)
import System.Directory (removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (Handle, hClose, openBinaryTempFile)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (createLink, getSymbolicLinkStatus)
import System.Timeout (timeout)

data Source = Source (MVar Bool) (Int -> IO (BS.ByteString, Bool))
instance SomeServer Source
instance S.ByteSource'server_ Source where
  byteSource'read (Source active readChunk) = handleParsedAsync $ \S.ByteSource'read'params {S.maxBytes = size} ->
    modifyMVar active $ \alive -> do
      unless alive $ ioError (userError "file source closed")
      when (size == 0 || size > 32768) $ ioError (userError "invalid file chunk size")
      reply <- timeout 60000000 (readChunk (fromIntegral size))
      (chunk, eof) <- maybe (ioError (userError "file source timed out")) pure reply
      when (BS.length chunk > fromIntegral size || (BS.null chunk && not eof)) $ ioError (userError "invalid file source reply")
      pure (not eof, S.ByteSource'read'results {S.chunk = chunk, S.eof = eof})

withSource :: Supervisor -> (Int -> IO (BS.ByteString, Bool)) -> (C.Client S.ByteSource -> IO a) -> IO a
withSource sup callback use = do
  active <- newMVar True
  cap <- C.export @S.ByteSource sup (Source active callback)
  use cap `finally` modifyMVar_ active (const (pure False))

readSource :: C.Client S.ByteSource -> Int -> IO (BS.ByteString, Bool)
readSource cap size = do
  S.ByteSource'read'results {S.chunk = chunk, S.eof = eof} <- callOn #read S.ByteSource'read'params {S.maxBytes = fromIntegral size} cap
  pure (chunk, eof)

bytesSource :: BS.ByteString -> IO (Int -> IO (BS.ByteString, Bool))
bytesSource bytes = do
  remaining <- newIORef bytes
  pure $ \size -> atomicModifyIORef' remaining $ \rest ->
    let (chunk, next) = BS.splitAt size rest
     in (next, (chunk, BS.null next))

data FileSink = FileSink Handle (MVar (Bool, Bool, Word64))
instance SomeServer FileSink
instance S.ByteSink'server_ FileSink where
  byteSink'write (FileSink handle state) = handleParsed $ \S.ByteSink'write'params {S.chunk = chunk} -> do
    modifyMVar_ state $ \(active, ended, count) -> do
      unless (active && not ended && BS.length chunk <= 32768) $ ioError (userError "file sink closed or oversized chunk")
      BS.hPut handle chunk
      pure (active, ended, count + fromIntegral (BS.length chunk))
    pure S.ByteSink'write'results
  byteSink'end (FileSink _ state) = handleParsed $ \_ -> do
    modifyMVar_ state $ \(active, ended, count) -> do
      unless (active && not ended) $ ioError (userError "file sink closed")
      pure (False, True, count)
    pure S.ByteSink'end'results
  byteSink'abort (FileSink _ state) = handleParsed $ \_ -> do
    modifyMVar_ state $ \(_, _, count) -> pure (False, False, count)
    pure S.ByteSink'abort'results

-- | Publish the sibling temporary file only after end() and RPC success.
withDownload :: Supervisor -> FilePath -> (C.Client S.ByteSink -> IO Word64) -> IO Word64
withDownload = downloadWith renameFile

-- | Reject existing destinations, including dangling symlinks. Hard-linking
-- the completed sibling file publishes it atomically without replacing a racer.
withDownloadNoReplace :: Supervisor -> FilePath -> (C.Client S.ByteSink -> IO Word64) -> IO Word64
withDownloadNoReplace sup destination transfer = do
  exists <-
    (getSymbolicLinkStatus destination >> pure True) `catch` \err ->
      if isDoesNotExistError err then pure False else ioError err
  when exists $ ioError (userError "screenshot destination already exists")
  downloadWith (\source target -> createLink source target >> removeFile source) sup destination transfer

downloadWith :: (FilePath -> FilePath -> IO ()) -> Supervisor -> FilePath -> (C.Client S.ByteSink -> IO Word64) -> IO Word64
downloadWith publish sup destination transfer = bracketOnError
  (openBinaryTempFile (takeDirectory destination) (takeFileName destination <> ".part"))
  (\(path, handle) -> ignore (hClose handle) >> ignore (removeFile path))
  $ \(path, handle) -> do
    state <- newMVar (True, False, 0)
    count <-
      bracket
        (C.export @S.ByteSink sup (FileSink handle state))
        (\_ -> modifyMVar_ state $ \(_, ended, bytes) -> pure (False, ended, bytes))
        transfer
    valid <- modifyMVar state $ \s@(_, ended, bytes) -> pure (s, ended && bytes == count)
    unless valid $ ioError (userError "incomplete file download")
    hClose handle
    publish path destination
    pure count
  where
    ignore action = action `catch` \(_ :: SomeException) -> pure ()
