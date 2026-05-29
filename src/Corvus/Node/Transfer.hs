{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Inter-agent disk transfer plumbing.
--
-- The daemon does not carry the bytes of a migrated disk image.
-- Instead:
--
--   1. The daemon asks the source agent to 'openReader' for a path
--      and receives a 'DiskReader' cap, an opaque token, the file
--      size, and its current md5.
--   2. The daemon asks the destination agent to start an import
--      from a peer host:port + that token. The destination opens
--      its own Cap'n Proto session to the source agent, claims the
--      'DiskReader' by token via @attachReader@, then runs
--      @reader.pipeInto(localFileSink)@ where the sink writes to a
--      @.part@ file on the destination.
--
-- The data path is destination↔source — daemon is out of the way.
--
-- This module provides:
--
--   * 'TokenRegistry'   — the source-side token → 'DiskReader' map
--                         the @diskOpenRead@ / @attachReader@
--                         handlers populate / consume.
--   * 'newFileReader'   — build a 'DiskReader' cap server that
--                         streams the file at @path@ to a
--                         caller-supplied 'ByteSink'.
--   * 'newFileWriterSink' — build a 'ByteSink' cap server the
--                         destination hands to a peer's 'pipeInto';
--                         it writes bytes to @destPath.part@ and
--                         signals completion via a 'MVar'.
--   * 'newToken'        — 128-bit hex-encoded single-use ticket.
--   * 'registerReader' / 'redeemReader' — registry operations the
--                         RPC handlers use.
--
-- All three helpers run inside the nodeagent process.
module Corvus.Node.Transfer
  ( -- * Token registry
    TokenRegistry
  , newTokenRegistry
  , newToken
  , registerReader
  , redeemReader

    -- * Server caps
  , FileReader (..)
  , newFileReader
  , FileWriterSink (..)
  , newFileWriterSink
  , FileWriterDone (..)
  , waitFileWriter
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
import qualified Capnp.Gen.Streams as CGS
import Capnp.Rpc.Server (SomeServer)
import Control.Concurrent.MVar
  ( MVar
  , modifyMVar
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , takeMVar
  , tryPutMVar
  )
import qualified Control.Exception as E
import Control.Monad (unless, void, when)
import Corvus.Rpc.Common (handleParsed)
import Corvus.Rpc.Streams (callSink)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)
import System.IO
  ( BufferMode (..)
  , Handle
  , IOMode (..)
  , hClose
  , hFileSize
  , hFlush
  , hSetBuffering
  , openBinaryFile
  )
import qualified System.Random as Random

-- ---------------------------------------------------------------------------
-- Configuration

-- | Streaming chunk size for inter-agent disk transfer.
--
-- 1 MiB. Source side is a strict per-chunk request/reply through
-- the Cap'n Proto RPC pipeline, so wall-clock throughput is
-- @chunkSize / per-RPC-latency@. With the bulk-memcpy
-- 'Capnp.Untyped.marshalBytes' patch making the @Data@ field
-- serialiser O(memcpy), the empirical sweet spot on a real network
-- is ~1 MiB: small enough that one buffer per direction is a few
-- MiB of overhead, large enough that per-RPC framing is fully
-- amortised. Throughput plateaus around here against the
-- haskell-capnp single-cap dispatch limit; smaller chunks (256
-- KiB) measured identical, larger chunks (4 MiB, 16 MiB) measured
-- progressively worse.
transferChunkBytes :: Int
transferChunkBytes = 1024 * 1024

-- ---------------------------------------------------------------------------
-- Token registry

-- | The source-side single-use ticket map. Keys are random hex
-- strings handed to the destination via @diskOpenRead@; values are
-- the 'FileReader' server impls behind them.
--
-- We deliberately store the bare server impl rather than the
-- exported Cap'n Proto 'C.Client' cap: a 'C.Client' is bound to
-- the connection it was originally exported on (level-1 RPC has
-- no three-party handoff), so returning the daemon's exported
-- cap from a method call on a different connection breaks. The
-- destination agent calls @attachReader@ on its own fresh
-- session to the source — that session must re-export the impl
-- on its own supervisor before handing it back.
newtype TokenRegistry = TokenRegistry (MVar (Map.Map Text FileReader))

newTokenRegistry :: IO TokenRegistry
newTokenRegistry = TokenRegistry <$> newMVar Map.empty

-- | Generate a 128-bit random hex token (32 hex chars). Used as
-- the bearer ticket the destination presents to @attachReader@.
newToken :: IO Text
newToken = do
  bytes <- mapM (\_ -> Random.randomRIO (0, 255 :: Int)) [1 .. (16 :: Int)]
  pure $ T.pack (concatMap byteToHex bytes)
  where
    byteToHex :: Int -> String
    byteToHex b =
      let h = showHex (b :: Int) ""
       in if length h == 1 then '0' : h else h

-- | Insert a token → reader binding.
registerReader :: TokenRegistry -> Text -> FileReader -> IO ()
registerReader (TokenRegistry mv) token reader =
  modifyMVar_ mv (pure . Map.insert token reader)

-- | Look up and consume a token. Returns 'Nothing' if the token
-- is unknown / already consumed.
redeemReader :: TokenRegistry -> Text -> IO (Maybe FileReader)
redeemReader (TokenRegistry mv) token =
  modifyMVar mv $ \m ->
    case Map.lookup token m of
      Nothing -> pure (m, Nothing)
      Just r -> pure (Map.delete token m, Just r)

-- ---------------------------------------------------------------------------
-- DiskReader server (source side)

-- | A 'DiskReader' server impl. Owns an open file handle and
-- streams its contents through a caller-supplied 'ByteSink' when
-- @pipeInto@ is invoked. Closing is idempotent.
data FileReader = FileReader
  { frPath :: !FilePath
  , frHandle :: !(MVar (Maybe Handle))
  -- ^ 'Nothing' once the file has been closed (after EOF or
  -- explicit cancel).
  }

instance SomeServer FileReader

instance CGNA.DiskReader'server_ FileReader where
  diskReader'pipeInto fr =
    handleParsed $ \CGNA.DiskReader'pipeInto'params {CGNA.sink = sink} -> do
      streamToSink fr sink
      pure CGNA.DiskReader'pipeInto'results

  diskReader'cancel fr =
    handleParsed $ \_ -> do
      closeReader fr
      pure CGNA.DiskReader'cancel'results

-- | Build a 'FileReader' for @path@. Opens the file in binary
-- read mode immediately; subsequent 'pipeInto' calls drain it.
newFileReader :: FilePath -> IO FileReader
newFileReader path = do
  h <- openBinaryFile path ReadMode
  hSetBuffering h (BlockBuffering (Just transferChunkBytes))
  mv <- newMVar (Just h)
  pure FileReader {frPath = path, frHandle = mv}

closeReader :: FileReader -> IO ()
closeReader fr =
  modifyMVar_ (frHandle fr) $ \case
    Nothing -> pure Nothing
    Just h -> do
      E.handle (\(_ :: E.SomeException) -> pure ()) (hClose h)
      pure Nothing

streamToSink :: FileReader -> C.Client CGS.ByteSink -> IO ()
streamToSink fr sink = do
  mh <- readMVar (frHandle fr)
  case mh of
    Nothing -> do
      -- File already closed. Behave as EOF.
      callSinkEnd sink
    Just h -> do
      r <-
        E.try @E.SomeException
          ( do
              loop h
              callSinkEnd sink
          )
      -- Regardless of outcome, drop the file handle.
      closeReader fr
      case r of
        Left e -> E.throwIO e
        Right () -> pure ()
  where
    loop h = do
      chunk <- BS.hGet h transferChunkBytes
      if BS.null chunk
        then pure ()
        else do
          callSink
            #write
            CGS.ByteSink'write'params {CGS.chunk = chunk}
            sink
          loop h

callSinkEnd :: C.Client CGS.ByteSink -> IO ()
callSinkEnd = callSink #end CGS.ByteSink'end'params

-- ---------------------------------------------------------------------------
-- ByteSink server (destination side)

-- | Completion signal from a 'FileWriterSink'. 'fwdSuccess' fires
-- with 'Nothing' on clean EOF or 'Just' an error message if a
-- write or close raised. Set exactly once.
newtype FileWriterDone = FileWriterDone {unFileWriterDone :: MVar (Maybe Text)}

-- | A 'ByteSink' server impl that writes bytes received over
-- 'write' into a local file handle, then signals completion via a
-- 'FileWriterDone' when 'end' is invoked (or when a write fails).
data FileWriterSink = FileWriterSink
  { fwsHandle :: !(MVar (Maybe Handle))
  , fwsBytes :: !(MVar Int)
  , fwsDone :: !FileWriterDone
  }

instance SomeServer FileWriterSink

instance CGS.ByteSink'server_ FileWriterSink where
  byteSink'write fws =
    handleParsed $ \CGS.ByteSink'write'params {CGS.chunk = chunk} -> do
      r <-
        E.try @E.SomeException $ writeChunk fws chunk
      case r of
        Left e -> do
          signalDone fws (Just (T.pack (show e)))
          E.throwIO e
        Right () -> pure ()
      pure CGS.ByteSink'write'results

  byteSink'end fws =
    handleParsed $ \_ -> do
      r <-
        E.try @E.SomeException $ closeWriter fws
      case r of
        Left e -> signalDone fws (Just (T.pack (show e)))
        Right () -> signalDone fws Nothing
      pure CGS.ByteSink'end'results

-- | Build a 'FileWriterSink' that writes to @path@. Caller fsyncs
-- + renames the resulting file after 'waitFileWriter' returns.
newFileWriterSink :: FilePath -> IO (FileWriterSink, FileWriterDone)
newFileWriterSink path = do
  h <- openBinaryFile path WriteMode
  hSetBuffering h (BlockBuffering (Just transferChunkBytes))
  hv <- newMVar (Just h)
  bv <- newMVar 0
  done <- FileWriterDone <$> newEmptyMVar
  let fws =
        FileWriterSink
          { fwsHandle = hv
          , fwsBytes = bv
          , fwsDone = done
          }
  pure (fws, done)

-- | Block until a sink reports completion. Returns 'Nothing' on
-- clean EOF, 'Just msg' on error.
waitFileWriter :: FileWriterDone -> IO (Maybe Text)
waitFileWriter (FileWriterDone mv) = readMVar mv

writeChunk :: FileWriterSink -> BS.ByteString -> IO ()
writeChunk fws chunk = do
  mh <- readMVar (fwsHandle fws)
  case mh of
    Nothing -> pure () -- already closed; drop late writes
    Just h -> do
      BS.hPut h chunk
      modifyMVar_ (fwsBytes fws) (\n -> pure (n + BS.length chunk))

closeWriter :: FileWriterSink -> IO ()
closeWriter fws =
  modifyMVar_ (fwsHandle fws) $ \case
    Nothing -> pure Nothing
    Just h -> do
      hFlush h
      hClose h
      pure Nothing

signalDone :: FileWriterSink -> Maybe Text -> IO ()
signalDone fws result = do
  let FileWriterDone mv = fwsDone fws
  void $ tryPutMVar mv result
