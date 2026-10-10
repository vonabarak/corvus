{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Corvus.CloudInitHandlerSpec (spec) where

import Corvus.Action (runAction)
import Corvus.Handlers.CloudInit
import qualified Corvus.Model as M
import Corvus.Protocol (CloudInitInfo (..))
import qualified Data.Text as T
import Database.Persist (Filter, getBy, selectList)
import Test.DSL.Core (runDb)
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "cloud-init configuration actions" $ do
    testCase "get, set and delete reject a missing VM" $ do
      _ <- withState (`handleCloudInitGet` 999)
      responseIsVmNotFound
      _ <- withState (\st -> runAction st "alice" (CloudInitSet 999 Nothing Nothing False))
      responseIsVmNotFound
      _ <- withState (\st -> runAction st "alice" (CloudInitDelete 999))
      responseIsVmNotFound
    testCase "set and delete reject disabled cloud-init without creating a config" $ do
      ident <- insertVm "disabled" VmStopped
      _ <- withState (\st -> runAction st "alice" (CloudInitSet ident (Just "data") Nothing True))
      responseIs (== RespError "Cloud-init is not enabled on this VM")
      _ <- withState (\st -> runAction st "alice" (CloudInitDelete ident))
      responseIs (== RespError "Cloud-init is not enabled on this VM")
      assertConfig ident Nothing
    testCase "get returns absence for an enabled VM without overrides" $ do
      ident <- givenCloudInitVmExists "web"
      _ <- withState (`handleCloudInitGet` ident)
      responseIs (== RespCloudInitConfig Nothing)
    testCase "upserts and deletes overrides even when ISO regeneration is unavailable" $ do
      ident <- givenCloudInitVmExists "web"
      key <- insertSshKey "key" "ssh-ed25519 AAA"
      _ <- attachSshKeyToVm ident key
      _ <- withState (\st -> runAction st "alice" (CloudInitSet ident (Just "data") (Just "network") True))
      regenerationFailed
      assertConfig ident (Just (Just "data", Just "network", True))
      _ <- withState (`handleCloudInitGet` ident)
      responseIs (== RespCloudInitConfig (Just (CloudInitInfo (Just "data") (Just "network") True)))
      _ <- withState (\st -> runAction st "alice" (CloudInitSet ident Nothing (Just "") False))
      regenerationFailed
      assertConfig ident (Just (Nothing, Just "", False))
      _ <- withState (\st -> runAction st "alice" (CloudInitDelete ident))
      regenerationFailed
      assertConfig ident Nothing
      _ <- withState (\st -> runAction st "alice" (CloudInitDelete ident))
      regenerationFailed
      tasks <- runDb $ selectList ([] :: [Filter M.Task]) []
      liftIO $ length tasks `shouldBe` 8
      liftIO $ map (M.taskClientName . entityVal) tasks `shouldBe` replicate 8 "alice"
      liftIO $ map (M.taskResult . entityVal) tasks `shouldBe` replicate 8 TaskError

regenerationFailed :: TestM ()
regenerationFailed = responseIs $ \case
  RespError message -> "Cloud-init ISO regeneration failed: " `T.isPrefixOf` message
  _ -> False

assertConfig :: Int64 -> Maybe (Maybe Text, Maybe Text, Bool) -> TestM ()
assertConfig ident expected = do
  config <- runDb $ getBy (M.UniqueCloudInitVm (M.toSqlKey ident))
  let actual = (\row -> (M.cloudInitUserData row, M.cloudInitNetworkConfig row, M.cloudInitInjectSshKeys row)) . entityVal <$> config
  liftIO $ actual `shouldBe` expected
