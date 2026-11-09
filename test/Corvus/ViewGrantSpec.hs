{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the pure helpers that make up the SPICE view-grant
-- flow: password generation (daemon) and grant host resolution plus
-- JSON projection (client).
module Corvus.ViewGrantSpec (spec) where

import Corvus.Client.Commands (grantToJson, resolveGrantHost)
import Corvus.Handlers.Vm (generateSpicePassword)
import Corvus.Wire.Common (ViewGrant (..))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (isInfixOf)
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
  describe "generateSpicePassword" $ do
    it "returns a 24-character URL-safe base64 string" $ do
      pw <- generateSpicePassword
      T.length pw `shouldBe` 24
      T.all isUrlSafeBase64 pw `shouldBe` True

    it "returns a different password on each call" $ do
      pw1 <- generateSpicePassword
      pw2 <- generateSpicePassword
      pw1 `shouldNotBe` pw2

  describe "resolveGrantHost" $ do
    it "leaves a non-wildcard host untouched" $ do
      let grant = baseGrant {vgHost = "10.0.0.5"}
      vgHost (resolveGrantHost "10.1.1.1" grant) `shouldBe` "10.0.0.5"

    it "substitutes a wildcard host with the VM node address instead of the daemon host" $ do
      let grant = baseGrant {vgHost = "0.0.0.0"}
      vgHost (resolveGrantHost "node.example.com" grant) `shouldBe` "node.example.com"

    it "treats an empty host as wildcard and substitutes it" $ do
      let grant = baseGrant {vgHost = ""}
      vgHost (resolveGrantHost "10.0.0.9" grant) `shouldBe` "10.0.0.9"

    it "substitutes an IPv6 wildcard (::) with the VM node address" $ do
      let grant = baseGrant {vgHost = "::"}
      vgHost (resolveGrantHost "2001:db8::1" grant) `shouldBe` "2001:db8::1"

    it "substitutes wildcard hosts for Unix-socket clients too" $ do
      let grant = baseGrant {vgHost = "0.0.0.0"}
      vgHost (resolveGrantHost "node.example.com" grant) `shouldBe` "node.example.com"

  describe "grantToJson" $ do
    it "encodes all four fields with snake_case TTL key" $ do
      let grant = ViewGrant "0.0.0.0" 5904 "hunter2" 120
          resolvedGrant = resolveGrantHost "node.example.com" grant
          json = BL.unpack (encode (grantToJson resolvedGrant))
      json `shouldSatisfy` ("\"host\":\"node.example.com\"" `isInfixOf`)
      json `shouldSatisfy` ("\"port\":5904" `isInfixOf`)
      json `shouldSatisfy` ("\"password\":\"hunter2\"" `isInfixOf`)
      json `shouldSatisfy` ("\"ttl_seconds\":120" `isInfixOf`)
      json `shouldNotSatisfy` ("ttlSeconds" `isInfixOf`)

-- | True when @c@ is a character produced by URL-safe base64 without
-- padding: @A-Z@, @a-z@, @0-9@, @-@, or @_@.
isUrlSafeBase64 :: Char -> Bool
isUrlSafeBase64 c =
  isAsciiUpper c
    || isAsciiLower c
    || isDigit c
    || c == '-'
    || c == '_'

baseGrant :: ViewGrant
baseGrant = ViewGrant "127.0.0.1" 5900 "pw" 60
