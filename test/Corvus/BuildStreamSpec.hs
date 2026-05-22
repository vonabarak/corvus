{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for build-pipeline helpers.
--
-- The 'BuildEvent' Binary round-trip tests are gone — Phase 5 ripped
-- the 'Data.Binary' wire out; build events stream via a
-- 'BuildEventSink' Cap'n Proto cap (Phase 6).
module Corvus.BuildStreamSpec (spec) where

import Corvus.Node.GuestAgent (splitLines)
import Corvus.Rpc.Streams
  ( feedLineBuffer
  , flushLineBuffer
  , newLineBufferSink
  )
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
  describe "splitLines (QGA tail buffer)" $ do
    it "splits a single newline-terminated line" $
      splitLines "hello\n" `shouldBe` (["hello"], "")

    it "leaves a partial trailing line in the buffer" $
      splitLines "hello\nwor"
        `shouldBe` (["hello"], "wor")

    it "handles multiple lines" $
      splitLines "a\nb\nc\n"
        `shouldBe` (["a", "b", "c"], "")

    it "empty input is empty output and empty buffer" $
      splitLines "" `shouldBe` ([], "")

    it "single partial line stays in the buffer" $
      splitLines "no newline yet"
        `shouldBe` ([], "no newline yet")

    it "blank lines are preserved as empty entries" $
      splitLines "a\n\nb\n"
        `shouldBe` (["a", "", "b"], "")

    it "binary bytes survive intact" $
      splitLines (BS.pack [0x68, 0x69, 0x0A, 0xFF, 0xFE, 0x0A, 0x71])
        `shouldBe` ([BS.pack [0x68, 0x69], BS.pack [0xFF, 0xFE]], BS.pack [0x71])

  describe "LineBufferSink" $ do
    it "emits each complete line, retains the trailing partial" $ do
      (sink, drainLines) <- newCollectingSink
      feedLineBuffer sink "foo\nbar\nba"
      feedLineBuffer sink "z\n"
      drainLines `shouldReturn` ["foo", "bar", "baz"]

    it "joins a fragment that arrives in two writes" $ do
      (sink, drainLines) <- newCollectingSink
      feedLineBuffer sink "hel"
      feedLineBuffer sink "lo\n"
      drainLines `shouldReturn` ["hello"]

    it "flushes a final partial line on end" $ do
      (sink, drainLines) <- newCollectingSink
      feedLineBuffer sink "first\ntrailing"
      flushLineBuffer sink
      drainLines `shouldReturn` ["first", "trailing"]

    it "is a no-op when end runs with no buffered bytes" $ do
      (sink, drainLines) <- newCollectingSink
      feedLineBuffer sink "x\n"
      flushLineBuffer sink
      flushLineBuffer sink
      drainLines `shouldReturn` ["x"]

    it "strips a trailing \\r from CRLF lines" $ do
      (sink, drainLines) <- newCollectingSink
      feedLineBuffer sink "alpha\r\nbeta\r\n"
      drainLines `shouldReturn` ["alpha", "beta"]

    it "preserves blank lines" $ do
      (sink, drainLines) <- newCollectingSink
      feedLineBuffer sink "a\n\nb\n"
      drainLines `shouldReturn` ["a", "", "b"]

    it "decodes UTF-8 lines that arrive split across writes" $ do
      -- "naïve\n" in UTF-8 is 6 bytes: n a 0xC3 0xAF v e \n
      let chunkA = BS.pack [0x6E, 0x61, 0xC3] -- "na" + first half of ï
          chunkB = BS.pack [0xAF, 0x76, 0x65, 0x0A] -- rest of ï + "ve\n"
      (sink, drainLines) <- newCollectingSink
      feedLineBuffer sink chunkA
      feedLineBuffer sink chunkB
      drainLines `shouldReturn` ["na\239ve"]

    it "tolerates invalid UTF-8 by inserting replacement chars" $ do
      (sink, drainLines) <- newCollectingSink
      feedLineBuffer sink (BS.pack [0xFF, 0xFE, 0x0A])
      drainLines `shouldReturn` ["\65533\65533"]
  where
    newCollectingSink = do
      ref <- newIORef ([] :: [Text])
      sink <- newLineBufferSink (\l -> modifyIORef' ref (l :))
      let drain = reverse <$> readIORef ref
      pure (sink, drain)
