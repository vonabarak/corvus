{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'Corvus.Qemu.Qmp.classifyQmpResponse'.
--
-- The QMP success/error classifier is a substring match on the literal
-- key @\"return\"@ rather than a JSON parser — see the Haddock on
-- 'classifyQmpResponse' for the rationale. These tests pin the current
-- behaviour (including the known substring-collision edge case) so any
-- future tightening is a deliberate change, not a silent regression.
module Corvus.QmpSpec (spec) where

import Corvus.Qemu.Qmp
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
  describe "classifyQmpResponse" $ do
    describe "success responses" $ do
      it "recognises {\"return\":{}}" $
        classifyQmpResponse "{\"return\":{}}" `shouldBe` QmpSuccess

      it "recognises {\"return\":{\"version\":{...}}}" $
        classifyQmpResponse "{\"return\":{\"version\":{\"qemu\":{\"major\":10}}}}"
          `shouldBe` QmpSuccess

      it "recognises a pretty-printed success response" $
        classifyQmpResponse "{\n  \"return\": {}\n}" `shouldBe` QmpSuccess

    describe "error responses" $ do
      it "recognises {\"error\":{\"class\":\"...\",\"desc\":\"...\"}} as error" $ do
        let body = "{\"error\":{\"class\":\"GenericError\",\"desc\":\"no such device\"}}"
        case classifyQmpResponse body of
          QmpError msg -> T.unpack msg `shouldBe` BS.unpack body
          other -> expectationFailure $ "expected QmpError, got " ++ show other

      it "carries the raw response bytes in the QmpError message" $ do
        let body = "{\"error\":{\"class\":\"CommandNotFound\",\"desc\":\"unknown cmd\"}}"
        case classifyQmpResponse body of
          QmpError msg -> T.unpack msg `shouldContain` "CommandNotFound"
          other -> expectationFailure $ "expected QmpError, got " ++ show other

      it "treats an empty payload as an error" $
        case classifyQmpResponse "" of
          QmpError _ -> pure ()
          other -> expectationFailure $ "expected QmpError, got " ++ show other

      it "treats a JSON array as an error (QMP never returns arrays at top level)" $
        case classifyQmpResponse "[\"error\",\"not an object\"]" of
          QmpError _ -> pure ()
          other -> expectationFailure $ "expected QmpError, got " ++ show other

      it "treats truncated JSON as an error" $
        case classifyQmpResponse "{\"error\":{\"class\"" of
          QmpError _ -> pure ()
          other -> expectationFailure $ "expected QmpError, got " ++ show other

    describe "robustness of the substring check" $ do
      -- An error body whose description contains the word "return"
      -- without surrounding quotes is still classified as error — the
      -- match is on the literal 8-byte sequence @"return"@, with the
      -- framing quotes included. Pinning this so a future switch to
      -- real JSON parsing has to preserve at least this guarantee.
      it "does not match the word 'return' without surrounding quotes" $ do
        let body = "{\"error\":{\"class\":\"GenericError\",\"desc\":\"could not return result\"}}"
        case classifyQmpResponse body of
          QmpError _ -> pure ()
          other -> expectationFailure $ "expected QmpError, got " ++ show other
