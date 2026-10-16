{-# LANGUAGE OverloadedStrings #-}

module Corvus.BuildPlacementSpec (spec) where

import Control.Exception (bracket)
import Corvus.Handlers.Build.Placement (placeBuild)
import Corvus.Images (publishImage)
import Corvus.Model
import Corvus.Schema.Build
import Data.Either (isLeft)
import Data.Time (getCurrentTime)
import qualified Data.Yaml as Yaml
import Database.Persist (insert_, update, (=.))
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Test.DSL.When (createTestServerState)
import qualified Test.Database as Db
import Test.Hspec

spec :: Spec
spec = describe "offline build placement" $ do
  it "honors explicit nodes, requires the entire source chain, and rejects offline nodes" $
    bracket Db.setupTestDb Db.teardownTestDb $ \env -> do
      state <- createTestServerState (Db.tePool env) (Db.teTempDir env)
      now <- getCurrentTime
      let run :: SqlPersistT IO a -> IO a
          run action = runSqlPool action (Db.tePool env)
          fixture = Yaml.decodeEither' "name: output\ntemplate: helper\nstrategy: offline-overlay\nsourceImage: source\ntarget: {}"
      b <- either (error . show) pure fixture
      placeBuild state b `shouldReturnSatisfy` isLeft
      base <- run $ publishImage $ DiskImage "base" FormatQcow2 (Just 1024) now Nothing False
      source <- run $ publishImage $ DiskImage "source" FormatQcow2 (Just 1024) now (Just base) False
      let captured = b {buildResolvedSourceImage = Just (fromSqlKey source)}
          check input = fmap buildNode <$> placeBuild state input
      check captured `shouldReturnSatisfy` isLeft
      run $ insert_ $ DiskImageNode source (toSqlKey 1) "/source.qcow2"
      check captured `shouldReturnSatisfy` isLeft
      run $ insert_ $ DiskImageNode base (toSqlKey 1) "/base.qcow2"
      check captured `shouldReturn` Right "1"
      check (captured {buildNode = "test-node"}) `shouldReturn` Right "1"
      check (captured {buildNode = "missing-node"}) `shouldReturnSatisfy` isLeft
      run $ update (toSqlKey 1) [NodeAdminState =. NodeMaintenance]
      check captured `shouldReturnSatisfy` isLeft
      check (captured {buildNode = "test-node"}) `shouldReturnSatisfy` isLeft
      check (b {buildStrategy = BuildStrategyOverlay}) `shouldReturn` Right ""
  where
    shouldReturnSatisfy action predicate = action >>= (`shouldSatisfy` predicate)
