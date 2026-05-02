{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the build-streaming primitives:
--
--   * 'BuildEvent' Binary round-trip — every constructor decodes back
--     to the value the daemon serialised. If this regresses, the
--     wire format silently drifts and the client renders garbled
--     events.
--   * 'splitLines' boundary cases for the QGA log-tail line splitter
--     used by 'guestExecWithTail'.
module Corvus.BuildStreamSpec (spec) where

import Corvus.Model (TaskResult (..))
import Corvus.Protocol
  ( BuildEvent (..)
  , BuildOne (..)
  , BuildResult (..)
  )
import Corvus.Qemu.GuestAgent (splitLines)
import Data.Binary (decode, encode)
import qualified Data.ByteString as BS
import Test.Hspec

roundtrip :: BuildEvent -> Expectation
roundtrip ev = decode (encode ev) `shouldBe` ev

spec :: Spec
spec = do
  describe "BuildEvent Binary round-trip" $ do
    it "BuildLogLine" $
      roundtrip (BuildLogLine "starting build: gentoo-corvus-test")
    it "StepStart" $
      roundtrip (StepStart 7 "shell" "set -eux; emerge --noreplace ...")
    it "StepOutput with multibyte UTF-8" $
      roundtrip (StepOutput 2 "héllo \x1f600 wörld")
    it "StepEnd success" $
      roundtrip (StepEnd 3 TaskSuccess Nothing)
    it "StepEnd error with message" $
      roundtrip (StepEnd 4 TaskError (Just "exit code 2"))
    it "BuildEnd success" $
      roundtrip (BuildEnd (Right 42))
    it "BuildEnd failure" $
      roundtrip (BuildEnd (Left "shell: exit code 1"))
    it "PipelineEnd" $
      roundtrip
        ( PipelineEnd
            ( BuildResult
                [ BuildOne "ok-build" (Just 100) Nothing
                , BuildOne "fail-build" Nothing (Just "shell: exit code 1")
                ]
            )
        )

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
