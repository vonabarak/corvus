{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the @\/etc\/corvus-build-info@ renderer.
module Corvus.BuildProvenanceSpec (spec) where

import Corvus.Handlers.Build.Provenance (renderBuildInfo)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Test.Hspec

spec :: Spec
spec = describe "Build.Provenance.renderBuildInfo" $ do
  let buildTime = UTCTime (fromGregorian 2026 4 30) (secondsToDiffTime (15 * 3600 + 21 * 60 + 9))

  it "renders the expected key/value lines" $ do
    let out = renderBuildInfo "debian-12-nginx" buildTime "debian-12-cloud" "0.9.0.0"
    out
      `shouldBe` T.unlines
        [ "build_name: debian-12-nginx"
        , "build_date: 2026-04-30T15:21:09Z"
        , "source_template: debian-12-cloud"
        , "corvus_version: 0.9.0.0"
        ]

  it "ends with a newline" $ do
    let out = renderBuildInfo "x" buildTime "tpl" "0"
    T.takeEnd 1 out `shouldBe` "\n"
