{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'Corvus.Node.Qmp.classifyQmpResponse'.
--
-- The QMP success/error classifier is a substring match on the literal
-- key @\"return\"@ rather than a JSON parser — see the Haddock on
-- 'classifyQmpResponse' for the rationale. These tests pin the current
-- behaviour (including the known substring-collision edge case) so any
-- future tightening is a deliberate change, not a silent regression.
module Corvus.QmpSpec (spec) where

import Corvus.Node.Qmp
import Data.Aeson (eitherDecode')
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
  describe "BlockEntry decode" $ do
    it "decodes a removable drive with inserted media" $ do
      let json =
            "{\"device\":\"drive-42\",\"removable\":true,\"tray_open\":false,\"inserted\":{\"file\":\"/iso.iso\",\"format\":\"raw\"}}"
      case eitherDecode' (BSL.fromStrict (BS.pack json)) of
        Right e -> do
          beDevice e `shouldBe` Just "drive-42"
          beRemovable e `shouldBe` True
          beTrayOpen e `shouldBe` False
          beInserted e `shouldSatisfy` (/= Nothing)
        Left e -> expectationFailure (show e)

    it "leaves inserted as Nothing when the tray is empty" $ do
      let json = "{\"device\":\"drive-42\",\"removable\":true,\"tray_open\":false}"
      eitherDecode' (BSL.fromStrict (BS.pack json))
        `shouldBe` Right
          BlockEntry
            { beDevice = Just "drive-42"
            , beRemovable = True
            , beTrayOpen = False
            , beInserted = Nothing
            }

    it "leaves device as Nothing when absent" $ do
      let json = "{\"removable\":true,\"tray_open\":false}"
      eitherDecode' (BSL.fromStrict (BS.pack json))
        `shouldBe` Right
          BlockEntry
            { beDevice = Nothing
            , beRemovable = True
            , beTrayOpen = False
            , beInserted = Nothing
            }

    it "defaults tray_open to False when absent (plain disk)" $ do
      -- QEMU only emits 'tray_open' for removable cdrom devices; a plain
      -- (non-removable) disk entry has no such key. Decoding must not
      -- fail on its absence.
      let json = "{\"device\":\"virtio1\",\"removable\":false,\"inserted\":{\"file\":\"/root.qcow2\",\"format\":\"qcow2\"}}"
      case eitherDecode' (BSL.fromStrict (BS.pack json)) of
        Right e -> do
          beDevice e `shouldBe` Just "virtio1"
          beRemovable e `shouldBe` False
          beTrayOpen e `shouldBe` False
          beInserted e `shouldSatisfy` (/= Nothing)
        Left e -> expectationFailure (show e)

  describe "extractReplyLine" $ do
    it "returns the reply line when it is the only line" $ do
      extractReplyLine "{\"return\":{}}" `shouldBe` Right "{\"return\":{}}"

    it "picks the last reply line, skipping async event lines" $ do
      let response =
            BS.concat
              [ "{\"return\":{\"qmp_capabilities\":true}}\n"
              , "{\"event\":\"SHUTDOWN\",\"timestamp\":{\"seconds\":1,\"microseconds\":2}}\n"
              , "{\"return\":{}}\n"
              ]
      extractReplyLine response `shouldBe` Right "{\"return\":{}}"

    it "returns Left when no reply line is present" $ do
      extractReplyLine "{\"event\":\"RESET\",\"timestamp\":{\"seconds\":1,\"microseconds\":2}}"
        `shouldBe` Left "no QMP reply line in response"

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
