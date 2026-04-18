{-# LANGUAGE RecordWildCards #-}

module Corvus.CoreSpec (spec) where

import Corvus.Protocol (Request (..), Response (..), StatusInfo (..))
import Test.DSL.When (executeRequest)
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "ping" $ do
    testCase "responds with pong" $ do
      resp <- executeRequest ReqPing
      liftIO $ resp `shouldBe` RespPong

  describe "status" $ do
    testCase "returns status info" $ do
      resp <- executeRequest ReqStatus
      liftIO $ case resp of
        RespStatus StatusInfo {..} -> do
          siUptime `shouldSatisfy` (>= 0)
          siConnections `shouldSatisfy` (>= 0)
        _ -> fail $ "Expected RespStatus, got: " ++ show resp

  describe "shutdown" $ do
    testCase "acknowledges shutdown" $ do
      resp <- executeRequest ReqShutdown
      liftIO $ resp `shouldBe` RespShutdownAck True
