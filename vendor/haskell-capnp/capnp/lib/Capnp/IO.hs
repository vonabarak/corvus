{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Capnp.IO
-- Description: Utilities for reading and writing values to handles.
--
-- This module provides utilities for reading and writing values to and
-- from file 'Handle's.
module Capnp.IO
  ( sGetMsg
  , sPutMsg
  , M.hGetMsg
  , hGetParsed
  , getParsed
  , hPutParsed
  )
where

import Capnp.Bits (WordCount, wordsToBytes)
import Capnp.Classes (Parse)
import Capnp.Convert
  ( msgToLBS
  , msgToParsed
  , parsedToBuilder
  )
import Capnp.Message (Mutability (..))
import qualified Capnp.Message as M
import qualified Capnp.Repr as R
import Capnp.TraversalLimit (evalLimitT)
import Control.Exception (throwIO)
import Control.Monad.Trans.Class (lift)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import Network.Simple.TCP (Socket, recv, sendLazy)
import System.IO (Handle, stdin)
import System.IO.Error (eofErrorType, mkIOError)

-- | Like 'hGetMsg', except that it takes a socket instead of a 'Handle'.
sGetMsg :: Socket -> WordCount -> IO (M.Message 'Const)
sGetMsg socket limit =
  evalLimitT limit $ M.readMessage (lift read32) (lift . readSegment)
  where
    read32 = do
      bytes <- recvFull 4
      pure $
        (fromIntegral (bytes `BS.index` 0) `shiftL` 0)
          .|. (fromIntegral (bytes `BS.index` 1) `shiftL` 8)
          .|. (fromIntegral (bytes `BS.index` 2) `shiftL` 16)
          .|. (fromIntegral (bytes `BS.index` 3) `shiftL` 24)
    readSegment !wordCount =
      M.fromByteString <$> recvFull (fromIntegral $ wordsToBytes wordCount)

    -- \| Like recv, but (1) never returns less than `count` bytes, (2)
    -- uses `socket`, rather than taking the socket as an argument, and (3)
    -- throws an EOF exception when the connection is closed.
    --
    -- Implementation note: the previous version built a right-leaning
    -- chain of strict-ByteString @(<>)@ calls — each forced @<>@
    -- allocates a buffer the size of both operands and @memcpy@s
    -- both into it, so a 16 MiB segment arriving in N kernel chunks
    -- cost O(N²) memory traffic to assemble. This version collects
    -- the recv'd chunks in a list and concatenates once at the end
    -- via 'BS.concat', which is O(total bytes) — single allocation,
    -- single pass.
    recvFull :: Int -> IO BS.ByteString
    recvFull !total = go total []
      where
        go 0 acc = pure $! BS.concat (reverse acc)
        go !remaining acc = do
          maybeBytes <- recv socket remaining
          case maybeBytes of
            Nothing ->
              throwIO $ mkIOError eofErrorType "Remote socket closed" Nothing Nothing
            Just bytes ->
              go (remaining - BS.length bytes) (bytes : acc)

-- | Like 'hPutMsg', except that it takes a 'Socket' instead of a 'Handle'.
sPutMsg :: Socket -> M.Message 'Const -> IO ()
sPutMsg socket = sendLazy socket . msgToLBS

-- | Read a struct from the handle in its parsed form, using the supplied
-- read limit.
hGetParsed :: forall a pa. (R.IsStruct a, Parse a pa) => Handle -> WordCount -> IO pa
hGetParsed handle limit = do
  msg <- M.hGetMsg handle limit
  evalLimitT limit $ msgToParsed @a msg

-- | Read a struct from stdin in its parsed form, using the supplied
-- read limit.
getParsed :: (R.IsStruct a, Parse a pa) => WordCount -> IO pa
getParsed = hGetParsed stdin

-- | Write the parsed form of a struct to the handle
hPutParsed :: (R.IsStruct a, Parse a pa) => Handle -> pa -> IO ()
hPutParsed h value = do
  bb <- evalLimitT maxBound $ parsedToBuilder value
  BB.hPutBuilder h bb
