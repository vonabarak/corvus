module Corvus.StatusFreshnessSpec (spec) where

import Control.Exception (throwIO)
import Corvus.Node.StatusFreshness
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Test.Hspec

spec :: Spec
spec = describe "status subscription freshness" $ do
  it "expires at 45 seconds without snapshots, including before the first snapshot" $ do
    clock <- newIORef 100
    freshness <- newStatusFreshness (readIORef clock)
    waitForStaleStatus freshness (pure False) (modifyIORef' clock (+ 5)) 45
      `shouldReturn` Just 45

  it "refreshes after successful processing, including an empty snapshot" $ do
    clock <- newIORef 0
    freshness <- newStatusFreshness (readIORef clock)
    writeIORef clock 40
    recordStatusSnapshot freshness (pure []) `shouldReturn` ([] :: [()])
    waitForStaleStatus freshness (pure False) (modifyIORef' clock (+ 5)) 45
      `shouldReturn` Just 45
    readIORef clock `shouldReturn` 85

  it "does not refresh when snapshot processing fails" $ do
    clock <- newIORef 0
    freshness <- newStatusFreshness (readIORef clock)
    writeIORef clock 40
    recordStatusSnapshot freshness (throwIO (userError "database failed")) `shouldThrow` anyIOException
    waitForStaleStatus freshness (pure False) (modifyIORef' clock (+ 5)) 45
      `shouldReturn` Just 45
    readIORef clock `shouldReturn` 45

  it "stays connected while snapshots keep arriving, then honors shutdown" $ do
    clock <- newIORef 0
    freshness <- newStatusFreshness (readIORef clock)
    let pause = do
          modifyIORef' clock (+ 5)
          recordStatusSnapshot freshness (pure ())
    waitForStaleStatus freshness ((>= 100) <$> readIORef clock) pause 45
      `shouldReturn` Nothing

  it "does not wait or reconnect on shutdown even if the subscription is stale" $ do
    clock <- newIORef 0
    freshness <- newStatusFreshness (readIORef clock)
    writeIORef clock 100
    waitForStaleStatus freshness (pure True) (expectationFailure "waited after shutdown") 45
      `shouldReturn` Nothing
