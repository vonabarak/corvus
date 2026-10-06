{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Corvus.ImageTagsSpec (spec) where

import Control.Exception (bracket)
import Corvus.DiskSelector
import Corvus.Handlers.Disk.Path (resolveDiskFilePath)
import Corvus.Images
import Corvus.Model
import Data.Either (isLeft)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Database.Persist
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import qualified Test.Database as Db
import Test.Hspec

spec :: Spec
spec = do
  describe "disk selectors" $ do
    it "defaults bare names to latest and accepts explicit tags and IDs" $ do
      parseDiskSelector "ubuntu" `shouldBe` Right (ImageTag "ubuntu" "latest")
      parseDiskSelector "ubuntu:24.04" `shouldBe` Right (ImageTag "ubuntu" "24.04")
      parseDiskSelector "123" `shouldBe` Right (ImageId 123)
    it "rejects malformed digit-leading selectors and overflowing IDs" $
      mapM_
        (\value -> parseDiskSelector value `shouldSatisfy` isLeft)
        ["0", "123abc", "123:latest", "9223372036854775808"]
    it "rejects invalid publication names and tags" $
      mapM_
        (\value -> publicationSelector value `shouldSatisfy` isLeft)
        ["123", "1ubuntu:latest", "ubuntu:", "ubuntu:a:b", "../ubuntu", "ubuntu:bad tag"]
  describe "image filenames" $ do
    it "prefixes generated filenames with the ID and preserves explicit files" $ do
      let key = toSqlKey 123 :: DiskImageId
      resolveDiskFilePath key "/images" Nothing "ubuntu:24.04.qcow2"
        `shouldReturn` "/images/123-ubuntu:24.04.qcow2"
      resolveDiskFilePath key "/images" (Just "project/") "ubuntu.qcow2"
        `shouldReturn` "/images/project/123-ubuntu.qcow2"
      resolveDiskFilePath key "/images" (Just "/srv/images/") "ubuntu.qcow2"
        `shouldReturn` "/srv/images/123-ubuntu.qcow2"
      resolveDiskFilePath key "/images" (Just "custom.qcow2") "ubuntu.qcow2"
        `shouldReturn` "/images/custom.qcow2"
  describe "transactional image tags" $ do
    it "reserves invisible, non-reusable IDs and publishes under the reserved ID" $
      bracket Db.setupTestDb Db.teardownTestDb $ \env -> do
        let run :: SqlPersistT IO a -> IO a
            run action = runSqlPool action (Db.tePool env)
            date = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
            image = DiskImage "ubuntu:v1" FormatRaw Nothing date Nothing False
        abandoned <- run reserveImageId
        reserved <- run reserveImageId
        reserved `shouldSatisfy` (> abandoned)
        run (get reserved) `shouldReturn` Nothing
        run (count ([] :: [Filter DiskImageTag])) `shouldReturn` 0
        automatic <- run (publishImage image)
        automatic `shouldSatisfy` (> reserved)
        run (publishImageWithId reserved image) `shouldReturn` reserved
        run (resolveImage (ImageTag "ubuntu" "latest")) `shouldReturn` Just reserved
        run (get reserved) `shouldReturn` Just image {diskImageName = "ubuntu"}
    it "moves tags, retains old versions and promotes latest on deletion" $
      bracket Db.setupTestDb Db.teardownTestDb $ \env -> do
        let run :: SqlPersistT IO a -> IO a
            run action = runSqlPool action (Db.tePool env)
            date = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
            image name = DiskImage name FormatRaw (Just 1024) date Nothing False
        old <- run $ publishImage (image "ubuntu:24.04")
        new <- run $ publishImage (image "ubuntu:26.04")
        run (resolveImage (ImageTag "ubuntu" "latest")) `shouldReturn` Just new
        run (resolveImage (ImageTag "ubuntu" "24.04")) `shouldReturn` Just old
        run (assignImageTag new "stable" >> assignImageTag old "stable")
        run (resolveImage (ImageTag "ubuntu" "stable")) `shouldReturn` Just old
        run (removeImageTag old "latest") `shouldThrow` anyException
        run (deleteImageTags new >> delete new)
        run (resolveImage (ImageTag "ubuntu" "latest")) `shouldReturn` Just old
        run (deleteImageTags old >> delete old)
        run (resolveImage (ImageTag "ubuntu" "latest")) `shouldReturn` Nothing
    it "promotes by creation date, breaking equal-date ties by ID" $
      bracket Db.setupTestDb Db.teardownTestDb $ \env -> do
        let run :: SqlPersistT IO a -> IO a
            run action = runSqlPool action (Db.tePool env)
            date = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
            image timestamp = DiskImage "ubuntu" FormatRaw Nothing timestamp Nothing False
        first <- run (publishImage (image date))
        second <- run (publishImage (image date))
        older <- run (publishImage (image (UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0))))
        run (resolveImage (ImageTag "ubuntu" "latest")) `shouldReturn` Just older
        run (deleteImageTags older >> delete older)
        run (resolveImage (ImageTag "ubuntu" "latest")) `shouldReturn` Just second
        run (assignImageTag first "latest")
        run (deleteImageTags first >> delete first)
        run (resolveImage (ImageTag "ubuntu" "latest")) `shouldReturn` Just second
    it "keeps tags case sensitive and isolated by image name" $
      bracket Db.setupTestDb Db.teardownTestDb $ \env -> do
        let run :: SqlPersistT IO a -> IO a
            run action = runSqlPool action (Db.tePool env)
            date = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
            image name = DiskImage name FormatRaw Nothing date Nothing False
        ubuntu <- run (publishImage (image "ubuntu:Stable"))
        debian <- run (publishImage (image "debian:Stable"))
        run (assignImageTag ubuntu "stable")
        run (removeImageTag ubuntu "Stable")
        run (resolveImage (ImageTag "ubuntu" "Stable")) `shouldReturn` Nothing
        run (resolveImage (ImageTag "ubuntu" "stable")) `shouldReturn` Just ubuntu
        run (resolveImage (ImageTag "debian" "Stable")) `shouldReturn` Just debian
    it "rolls back publication and tag changes together" $
      bracket Db.setupTestDb Db.teardownTestDb $ \env -> do
        let run :: SqlPersistT IO a -> IO a
            run action = runSqlPool action (Db.tePool env)
            date = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
            image = DiskImage "ubuntu" FormatRaw Nothing date Nothing False
        old <- run (publishImage image)
        run (publishImage image >> fail "injected placement failure") `shouldThrow` anyException
        run (resolveImage (ImageTag "ubuntu" "latest")) `shouldReturn` Just old
        run (count [DiskImageName ==. "ubuntu"]) `shouldReturn` 1
