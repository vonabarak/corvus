{-# LANGUAGE RecordWildCards #-}

module Corvus.CoreSpec (spec) where

import Corvus.Protocol (Response (..), StatusInfo (..))
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "ping" $ do
    testCase "responds with pong" $ do
      resp <- whenPing
      liftIO $ resp `shouldBe` RespPong

  describe "status" $ do
    testCase "returns status info" $ do
      resp <- whenStatus
      liftIO $ case resp of
        RespStatus StatusInfo {..} -> do
          siUptime `shouldSatisfy` (>= 0)
          siConnections `shouldSatisfy` (>= 0)
          siDatabaseBackend `shouldBe` "unknown"
          siDatabaseVersion `shouldBe` "unknown"
        _ -> fail $ "Expected RespStatus, got: " ++ show resp

  describe "shutdown" $ do
    testCase "acknowledges shutdown" $ do
      resp <- whenShutdown
      liftIO $ resp `shouldBe` RespShutdownAck True
