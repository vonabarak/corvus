{-# LANGUAGE OverloadedStrings #-}

-- | Pure-side coverage of the 'Corvus.Action' task-classification
-- helpers. The interesting case is 'RespBuildResult': a build's
-- per-build error strings are carried inside the 'BuildResult'
-- payload, and the task layer must surface them so @crv task
-- show@ reflects what actually happened. A previous version of
-- 'classifyResponse' treated every 'RespBuildResult' as
-- @TaskSuccess@ regardless of the payload, which made failed
-- builds invisible at the task layer (the streaming sink saw
-- 'PipelineEnd' with the error but @--wait@'s polling reads
-- @task.result@).
module Corvus.ActionSpec (spec) where

import Corvus.Action (classifyResponse)
import Corvus.Model (TaskResult (..))
import Corvus.Protocol (BuildOne (..), BuildResult (..), Response (..))
import Test.Hspec

spec :: Spec
spec = describe "classifyResponse" $ do
  describe "RespBuildResult" $ do
    it "empty BuildResult is success" $
      classifyResponse (RespBuildResult (BuildResult []))
        `shouldBe` (TaskSuccess, Nothing)

    it "all-success builds are success" $
      let ok name =
            BuildOne
              { boName = name
              , boArtifactDiskId = Just 1
              , boError = Nothing
              }
          br = BuildResult [ok "a", ok "b"]
       in classifyResponse (RespBuildResult br)
            `shouldBe` (TaskSuccess, Nothing)

    it "first failing build wins" $
      let mk name err =
            BuildOne
              { boName = name
              , boArtifactDiskId = Nothing
              , boError = err
              }
          br =
            BuildResult
              [ mk "a" Nothing
              , mk "b" (Just "step 3 failed: wipefs")
              , mk "c" (Just "step 5 failed: kernel")
              ]
       in classifyResponse (RespBuildResult br)
            `shouldBe` (TaskError, Just "step 3 failed: wipefs")

    it "single failed build surfaces its error" $
      let br =
            BuildResult
              [ BuildOne
                  { boName = "only"
                  , boArtifactDiskId = Nothing
                  , boError = Just "boom"
                  }
              ]
       in classifyResponse (RespBuildResult br)
            `shouldBe` (TaskError, Just "boom")

  describe "non-build responses pass through unchanged" $ do
    it "RespError is TaskError with its message" $
      classifyResponse (RespError "nope")
        `shouldBe` (TaskError, Just "nope")

    it "RespOk is TaskSuccess with no message" $
      classifyResponse RespOk `shouldBe` (TaskSuccess, Nothing)
