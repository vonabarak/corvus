{-# LANGUAGE OverloadedStrings #-}

module Corvus.BakeVmCleanupSpec (spec) where

import Corvus.Action
import Corvus.Handlers.Build.CleanupBakeVm (cleanupBakeVm)
import qualified Corvus.Model as M
import Database.Persist (count, get, (==.))
import Test.DSL.Core (runDb)
import Test.Prelude

data Cleanup = Cleanup Int64 | CleanupWithSharedImage Int64 Int64

instance Action Cleanup where
  actionSubsystem _ = SubBuild
  actionCommand _ = "cleanup-test"
  actionResources state (CleanupWithSharedImage _ image) = imageResources state SharedAccess image
  actionResources _ (Cleanup _) = pure []
  actionExecute ctx (Cleanup ident) = cleanupBakeVm ctx ident >> pure RespVmDeleted
  actionExecute ctx (CleanupWithSharedImage ident _) = cleanupBakeVm ctx ident >> pure RespVmDeleted

spec :: Spec
spec = sequential $ withTestDb $ do
  testCase "deletes a stopped bake VM without recording a failed stop" $ do
    ident <- insertVm "bake" M.VmStopped
    _ <- withState (\state -> runAction state "test" (Cleanup ident))
    responseIs (== RespVmDeleted)
    remaining <- runDb $ get (M.toSqlKey ident :: M.VmId)
    stops <- runDb $ count [M.TaskCommand ==. "stop"]
    liftIO $ do
      remaining `shouldBe` Nothing
      stops `shouldBe` 0
  testCase "preserves shared read-only firmware while deleting the bake VM" $ do
    ident <- insertVm "bake" M.VmStopped
    firmware <- insertDiskImage "firmware" FormatRaw
    _ <- attachDriveFull ident firmware InterfacePflash Nothing True CacheWriteback False
    _ <- withState (\state -> runAction state "test" (CleanupWithSharedImage ident firmware))
    responseIs (== RespVmDeleted)
    remaining <- runDb $ get (M.toSqlKey ident :: M.VmId)
    image <- runDb $ get (M.toSqlKey firmware :: M.DiskImageId)
    drives <- runDb $ count [M.DriveVmId ==. M.toSqlKey ident]
    stops <- runDb $ count [M.TaskCommand ==. "stop"]
    liftIO $ do
      remaining `shouldBe` Nothing
      fmap M.diskImageName image `shouldBe` Just "firmware"
      drives `shouldBe` 0
      stops `shouldBe` 0
  mapM_
    ( \status -> testCase ("deletes a bake VM in state " ++ show status ++ " without stopping") $ do
        ident <- insertVm "bake" status
        _ <- withState (\state -> runAction state "test" (Cleanup ident))
        responseIs (== RespVmDeleted)
        remaining <- runDb $ get (M.toSqlKey ident :: M.VmId)
        stops <- runDb $ count [M.TaskCommand ==. "stop"]
        liftIO $ do
          remaining `shouldBe` Nothing
          stops `shouldBe` 0
    )
    [M.VmSaved, M.VmError]
  testCase "already missing bake VM cleanup succeeds without child tasks" $ do
    _ <- withState (\state -> runAction state "test" (Cleanup 999))
    responseIs (== RespVmDeleted)
    stops <- runDb $ count [M.TaskCommand ==. "stop"]
    liftIO $ stops `shouldBe` 0
