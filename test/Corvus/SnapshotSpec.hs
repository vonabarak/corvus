{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Corvus.SnapshotSpec (spec) where

import Corvus.Action (runAction)
import Corvus.Handlers.Disk.Snapshot (SnapshotCreate (..), SnapshotDelete (..))
import qualified Corvus.Model as M
import Corvus.NodeAgentClient (QuiesceMode (..))
import Corvus.Protocol (Ref (..))
import qualified Corvus.Protocol as P
import qualified Data.Text as T
import Database.Persist (Filter, count, get, selectList, update, (=.))
import Test.DSL.Core (runDb)
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "snapshot list" $ do
    testCase "returns empty list for disk with no snapshots" $ do
      given $ do
        _ <- insertDiskImage "test-disk" FormatQcow2
        pure ()
      _ <- when_ $ snapshotList 1
      then_ $ responseIs $ \case
        RespSnapshotList [] -> True
        _ -> False

    testCase "returns all snapshots for a disk" $ do
      given $ do
        diskId <- insertDiskImage "test-disk" FormatQcow2
        _ <- insertSnapshot diskId "snap1"
        _ <- insertSnapshot diskId "snap2"
        pure ()
      _ <- when_ $ snapshotList 1
      then_ $ responseIs $ \case
        RespSnapshotList snaps -> length snaps == 2
        _ -> False

    testCase "fails for non-existent disk" $ do
      _ <- when_ $ snapshotList 999
      then_ responseIsDiskNotFound

  describe "snapshot delete" $ do
    testCase "fails for non-existent disk" $ do
      _ <- when_ $ snapshotDelete 999 1
      then_ responseIsDiskNotFound

    testCase "fails for non-existent snapshot" $ do
      given $ do
        _ <- insertDiskImage "test-disk" FormatQcow2
        pure ()
      _ <- when_ $ snapshotDelete 1 999
      then_ responseIsSnapshotNotFound

    testCase "fails for raw format disk" $ do
      given $ do
        _ <- insertDiskImage "raw-disk" FormatRaw
        pure ()
      _ <- when_ $ snapshotDelete 1 1
      then_ $ responseIs $ \case
        RespFormatNotSupported _ -> True
        _ -> False

  describe "snapshot rollback" $ do
    testCase "fails for non-existent disk" $ do
      _ <- when_ $ snapshotRollback 999 1
      then_ responseIsDiskNotFound

    testCase "fails for non-existent snapshot" $ do
      given $ do
        _ <- insertDiskImage "test-disk" FormatQcow2
        pure ()
      _ <- when_ $ snapshotRollback 1 999
      then_ responseIsSnapshotNotFound

    testCase "fails for raw format disk" $ do
      given $ do
        _ <- insertDiskImage "raw-disk" FormatRaw
        pure ()
      _ <- when_ $ snapshotRollback 1 1
      then_ $ responseIs $ \case
        RespFormatNotSupported _ -> True
        _ -> False

  describe "snapshot merge" $ do
    testCase "fails for non-existent disk" $ do
      _ <- when_ $ snapshotMerge 999 1
      then_ responseIsDiskNotFound

    testCase "fails for non-existent snapshot" $ do
      given $ do
        _ <- insertDiskImage "test-disk" FormatQcow2
        pure ()
      _ <- when_ $ snapshotMerge 1 999
      then_ responseIsSnapshotNotFound

    testCase "fails for raw format disk" $ do
      given $ do
        _ <- insertDiskImage "raw-disk" FormatRaw
        pure ()
      _ <- when_ $ snapshotMerge 1 1
      then_ $ responseIs $ \case
        RespFormatNotSupported _ -> True
        _ -> False

  describe "snapshot references and failure persistence" $ do
    testCase "resolves names and IDs only within the selected disk" $ do
      disk <- insertDiskImageOnTestNode "disk" "/disk.qcow2" FormatQcow2
      other <- insertDiskImage "other" FormatQcow2
      snap <- insertSnapshot disk "saved"
      otherSnap <- insertSnapshot other "saved"
      _ <- withState (\st -> runAction st "alice" (SnapshotDelete disk (Ref (T.pack (show otherSnap)))))
      responseIsSnapshotNotFound
      _ <- withState (\st -> runAction st "alice" (SnapshotDelete disk (Ref "saved")))
      responseIs $ \case
        RespError _ -> True
        _ -> False
      _ <- snapshotRollback disk snap
      responseIs $ \case
        RespError _ -> True
        _ -> False
      rows <- runDb $ selectList ([] :: [Filter M.Snapshot]) []
      liftIO $ map (M.snapshotName . entityVal) rows `shouldBe` ["saved", "saved"]
    testCase "refuses block rollback while its VM is running" $ do
      disk <- insertDiskImageOnTestNode "disk" "/disk.qcow2" FormatQcow2
      ident <- insertVm "web" VmRunning
      _ <- attachDrive ident disk InterfaceVirtio
      snap <- insertSnapshot disk "saved"
      _ <- snapshotRollback disk snap
      responseIs (== RespVmMustBeStopped)
      vmHasStatus ident VmRunning
      rows <- runDb $ count ([] :: [Filter M.Snapshot])
      liftIO $ rows `shouldBe` 1
    testCase "reports full-machine guards before attempting agent calls" $ do
      disk <- insertDiskImageOnTestNode "disk" "/disk.qcow2" FormatQcow2
      _ <- withState (\st -> runAction st "alice" (SnapshotCreate disk "saved" QuiesceSkip True))
      responseIs $ \case
        RespError message -> "require the VM to be running" `T.isInfixOf` message
        _ -> False
      first <- insertVm "first" VmRunning
      second <- insertVm "second" VmRunning
      _ <- attachDrive first disk InterfaceVirtio
      _ <- attachDrive second disk InterfaceVirtio
      _ <- withState (\st -> runAction st "alice" (SnapshotCreate disk "saved" QuiesceSkip True))
      responseIs $ \case
        RespError message -> "require exactly one" `T.isInfixOf` message
        _ -> False
      rows <- runDb $ count ([] :: [Filter M.Snapshot])
      liftIO $ rows `shouldBe` 0
    testCase "does not create a snapshot when the nodeagent is unavailable" $ do
      disk <- insertDiskImageOnTestNode "disk" "/disk.qcow2" FormatQcow2
      _ <- withState (\st -> runAction st "alice" (SnapshotCreate disk "saved" QuiesceSkip False))
      responseIs $ \case
        RespError _ -> True
        _ -> False
      rows <- runDb $ count ([] :: [Filter M.Snapshot])
      liftIO $ rows `shouldBe` 0
    testCase "returns all persisted snapshot fields" $ do
      disk <- insertDiskImage "disk" FormatQcow2
      snap <- insertSnapshotWithVmstate disk "saved" True
      runDb $ update (M.toSqlKey snap :: M.SnapshotId) [M.SnapshotSize =. Just 4096, M.SnapshotQuiesced =. True]
      row <- runDb $ get (M.toSqlKey snap :: M.SnapshotId)
      _ <- snapshotList disk
      responseIs $ \case
        RespSnapshotList [info] -> (P.sniId info, P.sniName info, P.sniSize info, P.sniLive info, P.sniQuiesced info, P.sniHasVmstate info) == (snap, "saved", Just 4096, True, True, True) && Just (P.sniCreatedAt info) == (M.snapshotCreatedAt <$> row)
        _ -> False
    mapM_
      ( \name -> testCase ("rejects invalid snapshot name " <> show name) $ do
          _ <- withState (\st -> runAction st "alice" (SnapshotCreate 999 name QuiesceSkip False))
          responseIs $ \case
            RespError _ -> True
            _ -> False
      )
      ["", "123"]
