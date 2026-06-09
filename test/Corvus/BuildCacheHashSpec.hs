{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'Corvus.Build.Cache.Hash'. Pure tests — no
-- database, no agent — exercising the chain-hash invariants the
-- build cache relies on.
module Corvus.BuildCacheHashSpec (spec) where

import Corvus.Build.Cache.Hash
  ( cacheSnapshotName
  , chainHashes
  , envelopeHash
  , shortChainHash
  )
import Corvus.Model (DriveFormat (..))
import Corvus.Schema.Apply (IfExists (..))
import Corvus.Schema.Build
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Test.Hspec

decodePipeline :: BS8.ByteString -> Either String PipelineConfig
decodePipeline bs = case Yaml.decodeEither' bs of
  Left e -> Left (show e)
  Right v -> Right v

firstBuild :: PipelineConfig -> Build
firstBuild cfg = case pcSteps cfg of
  (PipelineBuild b : _) -> b
  _ -> error "expected first step to be a build"

parse :: BS8.ByteString -> Build
parse yaml = case decodePipeline yaml of
  Right cfg -> firstBuild cfg
  Left e -> error e

withProvisioners :: BS8.ByteString -> Build
withProvisioners suffix =
  parse $
    BS8.unlines
      [ "pipeline:"
      , "  - build:"
      , "      name: x"
      , "      template: tpl"
      , "      target: {}"
      , "      provisioners:"
      , suffix
      ]

threeStepBuild :: Build
threeStepBuild =
  withProvisioners $
    BS8.unlines
      [ "        - shell: \"echo one\""
      , "        - shell: \"echo two\""
      , "        - shell: \"echo three\""
      ]

spec :: Spec
spec = do
  describe "envelopeHash" $ do
    it "is stable across runs of the same Build" $ do
      let b = threeStepBuild
      envelopeHash b `shouldBe` envelopeHash b

    it "ignores buildName (only buildName is operator label, not bake content)" $ do
      let b1 = threeStepBuild
          b2 = b1 {buildName = "different-label"}
      envelopeHash b1 `shouldBe` envelopeHash b2

    it "ignores buildDescription / buildNode / buildCleanup / buildWaitForShutdownSec" $ do
      let b1 = threeStepBuild
          b2 =
            b1
              { buildDescription = Just "x"
              , buildNode = "n"
              , buildCleanup = CleanupNever
              , buildWaitForShutdownSec = 999
              }
      envelopeHash b1 `shouldBe` envelopeHash b2

    it "ignores buildUseCache / buildBuildCache (operator policy, not bake content)" $ do
      let b1 = threeStepBuild
          b2 = b1 {buildUseCache = True, buildBuildCache = True}
      envelopeHash b1 `shouldBe` envelopeHash b2

    it "DOES change when buildTemplate changes" $ do
      let b1 = threeStepBuild
          b2 = b1 {buildTemplate = "other-template"}
      envelopeHash b1 `shouldNotBe` envelopeHash b2

    it "DOES change when target.format changes" $ do
      let b1 = threeStepBuild
          t = buildTarget b1
          b2 = b1 {buildTarget = t {btFormat = FormatRaw}}
      envelopeHash b1 `shouldNotBe` envelopeHash b2

    it "DOES change when target.sizeGb changes" $ do
      let b1 = threeStepBuild
          t = buildTarget b1
          b2 = b1 {buildTarget = t {btSizeGb = btSizeGb t + 5}}
      envelopeHash b1 `shouldNotBe` envelopeHash b2

    it "does NOT change when target.path changes (operator policy — where to publish)" $ do
      let b1 = threeStepBuild
          t = buildTarget b1
          b2 = b1 {buildTarget = t {btPath = Just "elsewhere/"}}
      envelopeHash b1 `shouldBe` envelopeHash b2

    it "does NOT change when target.ifExists changes (operator policy — collision handling)" $ do
      let b1 = threeStepBuild
          t = buildTarget b1
          b2 = b1 {buildTarget = t {btIfExists = IfExistsSkip}}
      envelopeHash b1 `shouldBe` envelopeHash b2

    it "does NOT change when target.compact changes (operator policy — post-bake size)" $ do
      let b1 = threeStepBuild
          t = buildTarget b1
          b2 = b1 {buildTarget = t {btCompact = not (btCompact t)}}
      envelopeHash b1 `shouldBe` envelopeHash b2

    it "DOES change when strategy changes" $ do
      let b1 = threeStepBuild
          b2 = b1 {buildStrategy = BuildStrategyFromScratch}
      envelopeHash b1 `shouldNotBe` envelopeHash b2

    it "DOES change when bvm cpu/ram changes" $ do
      let b1 = threeStepBuild
          v = buildVm b1
          b2 = b1 {buildVm = v {bvmCpuCount = bvmCpuCount v + 1}}
      envelopeHash b1 `shouldNotBe` envelopeHash b2

  describe "chainHashes" $ do
    it "returns one (stepHash, chainHash) pair per provisioner" $ do
      let b = threeStepBuild
          pairs = chainHashes b
      length pairs `shouldBe` 3

    it "the chain hashes are all distinct" $ do
      let chains = map snd (chainHashes threeStepBuild)
      Set.size (Set.fromList chains) `shouldBe` length chains

    it "the chain hashes are stable across calls" $ do
      let b = threeStepBuild
      chainHashes b `shouldBe` chainHashes b

    it "step hashes only change for the changed step" $ do
      let b1 = threeStepBuild
          b2 =
            withProvisioners $
              BS8.unlines
                [ "        - shell: \"echo one\""
                , "        - shell: \"echo TWO-CHANGED\""
                , "        - shell: \"echo three\""
                ]
          [(s1a, _), (s2a, _), (s3a, _)] = chainHashes b1
          [(s1b, _), (s2b, _), (s3b, _)] = chainHashes b2
      s1a `shouldBe` s1b
      s2a `shouldNotBe` s2b
      s3a `shouldBe` s3b

    it "a step change invalidates that chain hash AND all subsequent" $ do
      let b1 = threeStepBuild
          b2 =
            withProvisioners $
              BS8.unlines
                [ "        - shell: \"echo one\""
                , "        - shell: \"echo TWO-CHANGED\""
                , "        - shell: \"echo three\""
                ]
          [(_, c1a), (_, c2a), (_, c3a)] = chainHashes b1
          [(_, c1b), (_, c2b), (_, c3b)] = chainHashes b2
      c1a `shouldBe` c1b
      c2a `shouldNotBe` c2b
      c3a `shouldNotBe` c3b

    it "an envelope change invalidates EVERY chain hash" $ do
      let b1 = threeStepBuild
          b2 = b1 {buildTemplate = "other-template"}
      case (chainHashes b1, chainHashes b2) of
        ((_, c1a) : _, (_, c1b) : _) -> c1a `shouldNotBe` c1b
        _ -> expectationFailure "expected at least one chain hash from each build"

    it "is empty when there are no provisioners" $ do
      let b =
            parse $
              BS8.unlines
                [ "pipeline:"
                , "  - build:"
                , "      name: x"
                , "      template: tpl"
                , "      target: {}"
                ]
      chainHashes b `shouldBe` []

    it "filters out auto-injected CORVUS_* envs from the step hash" $ do
      let bWithoutInjected =
            withProvisioners $
              BS8.unlines
                [ "        - shell: \"echo one\""
                ]
          bWithInjected =
            withProvisioners $
              BS8.unlines
                [ "        - shell:"
                , "            inline: \"echo one\""
                , "            env:"
                , "              CORVUS_BAKEVM_ID: \"42\""
                , "              CORVUS_BUILD_TASK_ID: \"99\""
                ]
          [(s1a, _)] = chainHashes bWithoutInjected
          [(s1b, _)] = chainHashes bWithInjected
      s1a `shouldBe` s1b

    it "DOES include user-defined envs in the step hash" $ do
      let b1 =
            withProvisioners $
              BS8.unlines
                [ "        - shell: \"echo one\""
                ]
          b2 =
            withProvisioners $
              BS8.unlines
                [ "        - shell:"
                , "            inline: \"echo one\""
                , "            env:"
                , "              FOO: \"bar\""
                ]
          [(s1, _)] = chainHashes b1
          [(s2, _)] = chainHashes b2
      s1 `shouldNotBe` s2

    it "step hashes are independent of env-list ordering (sorted in canonical form)" $ do
      let b1 =
            withProvisioners $
              BS8.unlines
                [ "        - shell:"
                , "            inline: \"echo one\""
                , "            env:"
                , "              A: \"1\""
                , "              B: \"2\""
                ]
          b2 =
            withProvisioners $
              BS8.unlines
                [ "        - shell:"
                , "            inline: \"echo one\""
                , "            env:"
                , "              B: \"2\""
                , "              A: \"1\""
                ]
          [(s1, _)] = chainHashes b1
          [(s2, _)] = chainHashes b2
      s1 `shouldBe` s2

  describe "cacheSnapshotName" $ do
    it "is 'cache-' followed by the first 16 chain hash chars" $ do
      let h = "abcdef0123456789deadbeef1337c0de"
      cacheSnapshotName h `shouldBe` "cache-abcdef0123456789"

    it "tolerates short hashes (less than 16 chars)" $ do
      cacheSnapshotName "abc" `shouldBe` "cache-abc"

  describe "shortChainHash" $ do
    it "returns the first 16 characters" $ do
      shortChainHash "abcdef0123456789deadbeef" `shouldBe` "abcdef0123456789"

    it "leaves shorter hashes alone" $ do
      shortChainHash "abc" `shouldBe` "abc"

    it "every cache snapshot name is 'cache-' + shortChainHash" $ do
      let h = "0123456789abcdef0123456789abcdef"
      cacheSnapshotName h `shouldBe` "cache-" `T.append` shortChainHash h
