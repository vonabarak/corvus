{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Corvus.DiskProgressSpec (spec) where

import qualified Capnp as C
import qualified Capnp.Gen.Streams as S
import Capnp.Rpc.Server (SomeServer)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar')
import Corvus.Action (Action (..), ActionContext (..), runAction)
import Corvus.Handlers.Disk.Progress
import Corvus.Rpc.Common (handleParsed)
import Corvus.Types (ServerState (..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Database.Persist (getJust)
import Database.Persist.Sql (fromSqlKey, runSqlPool)
import Supervisors (withSupervisor)
import Test.Prelude

data ProgressAction = ProgressAction (IORef (Maybe DiskProgress)) (C.Client S.TaskProgressSink)

newtype Observer = Observer (IORef [C.Parsed S.TaskProgressEvent])
instance SomeServer Observer
instance S.TaskProgressSink'server_ Observer where
  taskProgressSink'push (Observer events) = handleParsed $ \S.TaskProgressSink'push'params {S.event = event} -> do
    modifyIORef' events (event :)
    pure S.TaskProgressSink'push'results

instance Action ProgressAction where
  actionSubsystem _ = SubDisk
  actionCommand _ = "progress-test"
  actionExecute ctx (ProgressAction saved observer) = do
    atomically $ modifyTVar' (ssTaskProgressSubs (acState ctx)) (Map.insert (fromSqlKey (acTaskId ctx)) [observer])
    progress <- newDiskProgress ctx
    writeIORef saved (Just progress)
    let message = taskMessage <$> runSqlPool (getJust (acTaskId ctx)) (ssDbPool (acState ctx))
    progress "Copying" 0 1024
    message `shouldReturn` Just "Copying: 0B / 1K (0%)"
    progress "Copying" 512 1024
    message `shouldReturn` Just "Copying: 0B / 1K (0%)"
    threadDelay 1100000
    progress "Copying" 512 1024
    message `shouldReturn` Just "Copying: 512B / 1K (50%)"
    progress "Copying" 1024 1024
    message `shouldReturn` Just "Copying: 1K / 1K (100%)"
    progress "Verifying" 0 0
    message `shouldReturn` Just "Verifying"
    progress "Copying" 512 0
    message `shouldReturn` Just "Copying: 512B"
    progress "Copying" 0 0
    message `shouldReturn` Just "Copying"
    progress "Copying" 768 0
    message `shouldReturn` Just "Copying"
    progress "Verifying" 0 0
    pure RespDiskOk

spec :: Spec
spec = do
  describe "disk progress messages" $ do
    it "formats phase-only and unknown-length progress" $ do
      formatDiskProgress "Hashing source" 0 0 `shouldBe` "Hashing source"
      formatDiskProgress "Downloading" 2048 0 `shouldBe` "Downloading: 2K"
    it "computes percentages without overflowing and clamps invalid counts" $ do
      formatDiskProgress "Copying" maxBound maxBound `shouldBe` "Copying: 9223372036854775807B / 9223372036854775807B (100%)"
      formatDiskProgress "Copying" (-1) 1024 `shouldBe` "Copying: 0B / 1K (0%)"
      formatDiskProgress "Copying" 2048 1024 `shouldBe` "Copying: 2K / 1K (100%)"
  sequential $ withTestDb $ testCase "publishes throttled progress, immediate phases, and preserves terminal results" $ do
    saved <- liftIO $ newIORef Nothing
    _ <- withState $ \state -> withSupervisor $ \sup -> do
      observed <- newIORef []
      observer <- C.export @S.TaskProgressSink sup (Observer observed)
      response <- runAction state "test" (ProgressAction saved observer)
      readIORef observed >>= (`shouldSatisfy` any isFinalUnknownCount)
      pure response
    final <- getLastTask
    liftIO $ fmap (taskMessage . entityVal) final `shouldBe` Just Nothing
    callback <- liftIO $ readIORef saved
    liftIO $ case callback of
      Nothing -> fail "missing progress callback"
      Just progress -> progress "Late observer update" 0 0
    afterLateUpdate <- getLastTask
    liftIO $ afterLateUpdate `shouldBe` final
  where
    isFinalUnknownCount :: C.Parsed S.TaskProgressEvent -> Bool
    isFinalUnknownCount S.TaskProgressEvent {S.union' = event} = case event of
      S.TaskProgressEvent'progress S.TaskProgressEvent'progress' {S.label = label, S.completed = completed, S.total = total} -> label == "Copying: 768B" && completed == 768 && total == 0
      _ -> False
