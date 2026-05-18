{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for build-pipeline helpers.
--
-- The 'BuildEvent' Binary round-trip tests are gone — Phase 5 ripped
-- the 'Data.Binary' wire out; build events will be streamed via a
-- 'BuildEventSink' Cap'n Proto cap in Phase 6.
module Corvus.BuildStreamSpec (spec) where

import Corvus.Node.GuestAgent (splitLines)
import qualified Data.ByteString as BS
import Test.Hspec

spec :: Spec
spec =
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
