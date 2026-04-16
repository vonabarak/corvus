{-# LANGUAGE OverloadedStrings #-}

-- | Wire-format tests for 'Corvus.Protocol.decodeMessage'.
--
-- The one backstop against a stale client talking to a new daemon (or
-- vice versa) is the version byte at offset 0. These tests pin that
-- behaviour with direct 'ByteString' fixtures rather than going through
-- the socket, so the rule is exercised every unit run.
module Corvus.ProtocolVersionSpec (spec) where

import Corvus.Protocol
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Word (Word8)
import Test.Hspec

spec :: Spec
spec = do
  describe "decodeMessage" $ do
    it "round-trips a message encoded at the current protocol version" $ do
      let original = ReqPing
          wire = encodeMessage original
      (decodeMessage wire :: Either String Request) `shouldBe` Right original

    it "round-trips a more complex request" $ do
      let original = ReqVmStart (Ref "my-vm") True
          wire = encodeMessage original
      (decodeMessage wire :: Either String Request) `shouldBe` Right original

    it "rejects a message with a lower version byte" $ do
      -- Hand-craft a wire-format message that *looks* valid except for
      -- the version byte: encode a real Request but overwrite byte 0.
      let wire = overwriteVersion 27 (encodeMessage ReqPing)
      (decodeMessage wire :: Either String Request)
        `shouldSatisfy` isLeftStartingWith "protocol version mismatch"

    it "rejects a message with a higher version byte" $ do
      let wire = overwriteVersion 29 (encodeMessage ReqPing)
      (decodeMessage wire :: Either String Request)
        `shouldSatisfy` isLeftStartingWith "protocol version mismatch"

    it "mentions the expected and received versions in the mismatch error" $ do
      let wire = overwriteVersion 7 (encodeMessage ReqPing)
      case decodeMessage wire :: Either String Request of
        Left err -> do
          err `shouldContain` "expected 28"
          err `shouldContain` "got 7"
        Right _ -> expectationFailure "expected a version mismatch, got a successful decode"

    it "rejects an empty byte string with a version decode error, not a crash" $
      (decodeMessage BL.empty :: Either String Request) `shouldSatisfy` isLeft

    it "rejects a message truncated after the version byte" $ do
      -- Just the version byte, no length prefix.
      let wire = encode (protocolVersion :: Word8)
      (decodeMessage wire :: Either String Request) `shouldSatisfy` isLeft

    it "rejects a message truncated mid-length-prefix" $ do
      -- Version byte + 4 bytes into what should be an 8-byte length prefix.
      let wire = encode (protocolVersion :: Word8) <> BL.replicate 4 0
      (decodeMessage wire :: Either String Request) `shouldSatisfy` isLeft

    it "rejects a message whose payload contains an unknown constructor tag" $ do
      -- Binary encoding of a sum type uses a tag byte/word to pick the
      -- constructor. 0xFF repeated forever is out of range for every
      -- nested sum in Request, so decoding must fail.
      let corruptPayload = BL.replicate 8 0xFF
          len = fromIntegral (BL.length corruptPayload) :: Int64
          wire = encode (protocolVersion :: Word8) <> encode len <> corruptPayload
      (decodeMessage wire :: Either String Request) `shouldSatisfy` isLeft

-- Note: we intentionally do not test "declared length > supplied bytes"
-- here. 'decodeMessage' is the pure codec; frame-length enforcement
-- lives in 'Server.recvExact', which guarantees the ByteString
-- 'decodeMessage' receives is exactly the advertised length. A test
-- for that framing rule would have to exercise the socket path.

-- | Replace the first byte of a wire message with a supplied version byte.
-- Panics if given an empty ByteString; the test inputs always have a
-- version byte at offset 0.
overwriteVersion :: Word8 -> BL.ByteString -> BL.ByteString
overwriteVersion v bs = case BL.uncons bs of
  Nothing -> error "overwriteVersion: empty input"
  Just (_, rest) -> BL.cons v rest

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- | Is this a Left whose message starts with the supplied prefix?
isLeftStartingWith :: String -> Either String b -> Bool
isLeftStartingWith prefix (Left msg) = prefix `isPrefixOf` msg
isLeftStartingWith _ _ = False
