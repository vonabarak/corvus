{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the pure helpers that make up the SPICE view-grant
-- flow: password generation (daemon) and grant host resolution plus
-- JSON projection (client).
module Corvus.ViewGrantSpec (spec) where

import Corvus.Client.Commands (grantToJson, resolveGrantHost)
import Corvus.Client.Rpc (SpiceGrant (..))
import Corvus.Client.Types
  ( BorderStyleOpt (..)
  , Command (..)
  , Options (..)
  , OutputFormat (..)
  )
import Corvus.Handlers.Vm (generateSpicePassword)
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
    it "leaves a non-wildcard host untouched even when the client is on TCP" $ do
      let grant = baseGrant {sgHost = "10.0.0.5"}
          opts = baseOpts {optTcp = True, optHost = "10.1.1.1"}
      sgHost (resolveGrantHost opts grant) `shouldBe` "10.0.0.5"

    it "substitutes a wildcard host with optHost when the client is on TCP" $ do
      let grant = baseGrant {sgHost = "0.0.0.0"}
          opts = baseOpts {optTcp = True, optHost = "daemon.example.com"}
      sgHost (resolveGrantHost opts grant) `shouldBe` "daemon.example.com"

    it "treats an empty host as wildcard and substitutes it" $ do
      let grant = baseGrant {sgHost = ""}
          opts = baseOpts {optTcp = True, optHost = "10.0.0.9"}
      sgHost (resolveGrantHost opts grant) `shouldBe` "10.0.0.9"

    it "substitutes an IPv6 wildcard (::) with optHost when on TCP" $ do
      let grant = baseGrant {sgHost = "::"}
          opts = baseOpts {optTcp = True, optHost = "2001:db8::1"}
      sgHost (resolveGrantHost opts grant) `shouldBe` "2001:db8::1"

    it "leaves the wildcard intact when the client is on a Unix socket" $ do
      let grant = baseGrant {sgHost = "0.0.0.0"}
          opts = baseOpts {optTcp = False, optHost = "ignored"}
      sgHost (resolveGrantHost opts grant) `shouldBe` "0.0.0.0"

  describe "grantToJson" $ do
    it "encodes all four fields with snake_case TTL key" $ do
      let grant = SpiceGrant "10.0.0.5" 5904 "hunter2" 120
          json = BL.unpack (encode (grantToJson grant))
      json `shouldSatisfy` ("\"host\":\"10.0.0.5\"" `isInfixOf`)
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

baseGrant :: SpiceGrant
baseGrant = SpiceGrant "127.0.0.1" 5900 "pw" 60

-- | A minimally-populated 'Options' suitable for exercising the host
-- resolver. The 'optCommand' field is required but not inspected, so
-- 'Ping' is used as a stand-in.
baseOpts :: Options
baseOpts =
  Options
    { optSocket = Nothing
    , optTcp = False
    , optHost = "127.0.0.1"
    , optPort = 9876
    , optOutput = TextOutput
    , optBorders = BordersUnicodeOpt
    , optTruncate = True
    , optColumns = []
    , optFitWidth = False
    , optCommand = Ping
    }
