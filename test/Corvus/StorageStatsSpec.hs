{-# LANGUAGE OverloadedStrings #-}

module Corvus.StorageStatsSpec (spec) where

import Control.Monad.Logger (LogLevel (..))
import Corvus.Node.StorageStats (existingStoragePath, newStorageStatsReader, parseStorageStats, readStorageStats)
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.Directory (doesPathExist, removeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "storage filesystem observations" $ do
  it "accepts zero available bytes on a known filesystem" $
    parseStorageStats "Size Avail\n123 0\n" `shouldBe` Just (123, 0)
  it "rejects overflow, partial integers, impossible capacity, and malformed output" $
    map parseStorageStats ["Size Avail\n9223372036854775808 0\n", "Size Avail\n123x 1\n", "Size Avail\n123 -1\n", "Size Avail\n123 124\n", "Size Avail\n0 0\n", "Size Avail\n123 1 extra\n", "garbage"] `shouldBe` replicate 7 Nothing
  it "uses an existing ancestor without creating the storage directory" $
    withSystemTempDirectory "corvus-storage" $ \root -> do
      let missing = root </> "missing" </> "VMs"
      existingStoragePath missing `shouldReturn` root
      reader <- newStorageStatsReader
      (total, free) <- readStorageStats reader (\_ _ -> pure ()) missing
      total `shouldSatisfy` (> 0)
      free `shouldSatisfy` (>= 0)
      doesPathExist missing `shouldReturn` False
  it "warns once per failure transition and reports recovery" $
    withSystemTempDirectory "corvus-storage" $ \root -> do
      let path = root </> "VMs"
      writeFile path "not a directory"
      reader <- newStorageStatsReader
      levels <- newIORef []
      let sample = readStorageStats reader (\level _ -> modifyIORef' levels (++ [level])) path
      sample `shouldReturn` (0, 0)
      sample `shouldReturn` (0, 0)
      removeFile path
      _ <- sample
      readIORef levels `shouldReturn` [LevelWarn, LevelDebug, LevelDebug, LevelInfo]
