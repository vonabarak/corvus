{-# LANGUAGE OverloadedStrings #-}

module Corvus.BuildUpdateSpec (spec) where

import Control.Exception (bracket)
import Control.Monad.Logger (runLoggingT)
import Corvus.Handlers.Build.Artifact (checkBuildUpdate)
import Corvus.Images (publishImage)
import Corvus.Model
import Corvus.Schema.Build
import Data.Time (getCurrentTime)
import Database.Persist (insert_, update, updateWhere, (=.), (==.))
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Test.DSL.When (createTestServerState)
import qualified Test.Database as Db
import Test.Hspec

spec :: Spec
spec = describe "build update publication policy" $ do
  it "requires matching provenance and ephemeral flags while keeping publication lifetime separate from content" $
    bracket Db.setupTestDb Db.teardownTestDb $ \env -> do
      state <- createTestServerState (Db.tePool env) (Db.teTempDir env)
      now <- getCurrentTime
      let target = BuildTarget FormatQcow2 (Just 10737418240) True False Nothing BuildIfExistsUpdate
          identity = BuildIdentity "fingerprint" "{}"
          check requested = runLoggingT (checkBuildUpdate state (const (pure ())) "output" requested identity) (\_ _ _ _ -> pure ())
          run :: SqlPersistT IO a -> IO a
          run action = runSqlPool action (Db.tePool env)
      check target `shouldReturn` Nothing
      key <- run $ publishImage $ DiskImage "output" FormatQcow2 Nothing now Nothing False
      check target `shouldReturn` Nothing
      run $ insert_ $ DiskImageBuildIdentity key "different" "{}"
      check target `shouldReturn` Nothing
      -- Matching provenance permits reuse only with the same publication lifetime.
      run $ updateWhere [DiskImageBuildIdentityDiskImageId ==. key] [DiskImageBuildIdentityFingerprint =. "fingerprint"]
      check target `shouldReturn` Just (fromSqlKey key)
      check (target {btEphemeral = True}) `shouldReturn` Nothing
      run $ update key [DiskImageEphemeral =. True]
      check (target {btEphemeral = True}) `shouldReturn` Just (fromSqlKey key)
      check target `shouldReturn` Nothing
