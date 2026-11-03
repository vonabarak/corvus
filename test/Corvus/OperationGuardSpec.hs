{-# LANGUAGE ScopedTypeVariables #-}

module Corvus.OperationGuardSpec (spec) where

import Control.Concurrent (forkFinally, forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (SomeException, throwIO, try)
import Corvus.OperationGuard
import Data.Either (isLeft)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "operation resource leases" $ do
  it "reports the original budget while enforcing the remaining wait" $ do
    guard <- newOperationGuard
    let resources = [(ImageFamily "output", ExclusiveAccess)]
    withOperationLeases guard 1000000 "task 42 (build)" [] resources $ \_ -> do
      outcome <- timeout 1000000 $ try $ withOperationLeasesWithBudget guard 60000000 10000 "publisher" [] resources (const (pure ()))
      case outcome of
        Just (Left (OperationLeaseTimeout message)) ->
          message `shouldBe` "Timed out after 60 seconds waiting for exclusive access to image family 'output'; held by task 42 (build)"
        _ -> expectationFailure "expected a timeout within the remaining wait, reported against the original budget"
  it "scopes object names by kind and node and atomically includes image dependencies" $ do
    guard <- newOperationGuard
    let vm node = ObjectName VmObject (Just node) "same"
    withOperationLeases guard 1000000 "owner" [] [(vm 1, ExclusiveAccess), (ImageFamily "output", ExclusiveAccess)] $ \_ -> do
      withOperationLeases guard 1000000 "other node" [] [(vm 2, ExclusiveAccess), (ObjectName NetworkObject (Just 1) "same", ExclusiveAccess)] (const (pure ()))
      result <- try $ withOperationLeases guard 10000 "collision" [] [(vm 1, ExclusiveAccess)] (const (pure ()))
      (result :: Either OperationLeaseTimeout ()) `shouldSatisfy` isLeft
  it "coordinates endpoints and child identities independently of object names" $ do
    guard <- newOperationGuard
    let endpoint = NodeEndpoint "host" 9000
        child node = ChildIdentity "disk path" node "image.qcow2"
    withOperationLeases guard 1000000 "owner" [] [(endpoint, ExclusiveAccess), (child 1, ExclusiveAccess)] $ \_ -> do
      withOperationLeases guard 1000000 "other node" [] [(child 2, ExclusiveAccess)] (const (pure ()))
      mapM_
        ( \key -> do
            result <- try $ withOperationLeases guard 10000 "collision" [] [(key, ExclusiveAccess)] (const (pure ()))
            (result :: Either OperationLeaseTimeout ()) `shouldSatisfy` isLeft
        )
        [endpoint, child 1]
  it "lets an inherited owner finish ahead of a waiter blocked by that owner" $ do
    guard <- newOperationGuard
    done <- newEmptyMVar
    withOperationLeases guard 1000000 "parent" [] [(ObjectName TemplateObject Nothing "template", ExclusiveAccess)] $ \held -> do
      _ <- forkFinally (withOperationLeases guard 1000000 "waiter" [] [(ObjectName TemplateObject Nothing "template", ExclusiveAccess), (ImageFamily "disk", ExclusiveAccess)] (const (pure ()))) (putMVar done)
      let queued = do
            probe <- try $ withOperationLeases guard 10000 "probe" [] [(ImageFamily "disk", SharedAccess)] (const (pure ()))
            case (probe :: Either OperationLeaseTimeout ()) of
              Left _ -> pure ()
              Right () -> queued
      timeout 500000 queued `shouldReturn` Just ()
      withOperationLeases guard 100000 "child" held [(ImageFamily "disk", ExclusiveAccess)] (const (pure ()))
      pure ()
    result <- timeout 1000000 (takeMVar done)
    case result of
      Just (Right ()) -> pure ()
      other -> expectationFailure (show other)
  it "allows shared readers, disjoint writers and explicit nested reuse" $ do
    guard <- newOperationGuard
    withOperationLeases guard 1000000 "build" [] [(ImageVersion 1, SharedAccess)] $ \held -> do
      withOperationLeases guard 1000000 "clone subtask" held [(ImageVersion 1, SharedAccess)] (const (pure ()))
      done <- newEmptyMVar
      _ <- forkIO $ withOperationLeases guard 1000000 "another build" [] [(ImageVersion 1, SharedAccess), (ImageFamily "other", ExclusiveAccess)] (const (putMVar done ()))
      timeout 1000000 (takeMVar done) `shouldReturn` Just ()
  it "times out conflicting image and family requests with holder diagnostics" $ do
    guard <- newOperationGuard
    withOperationLeases guard 1000000 "task 42 (build)" [] [(ImageVersion 1, SharedAccess), (ImageFamily "output", ExclusiveAccess)] $ \_ -> do
      outcome <- try $ withOperationLeases guard 10000 "resize" [] [(ImageVersion 1, ExclusiveAccess)] (const (pure ()))
      case outcome of
        Left (OperationLeaseTimeout message) -> show message `shouldContain` "task 42 (build)"
        Right () -> expectationFailure "conflicting writer acquired a reader's image"
      family <- try $ withOperationLeases guard 10000 "publisher" [] [(ImageFamily "output", ExclusiveAccess)] (const (pure ()))
      (family :: Either OperationLeaseTimeout ()) `shouldSatisfy` isLeft
  it "does not reserve a subset while waiting and removes timed out requests" $ do
    guard <- newOperationGuard
    withOperationLeases guard 1000000 "holder" [] [(ImageVersion 1, ExclusiveAccess)] $ \_ -> do
      done <- newEmptyMVar
      _ <- forkIO $ do
        result <- try $ withOperationLeases guard 10000 "atomic request" [] [(ImageVersion 1, ExclusiveAccess), (ImageVersion 2, ExclusiveAccess)] (const (pure ()))
        putMVar done (result :: Either OperationLeaseTimeout ())
      takeMVar done `shouldReturnSatisfy` isLeft
      withOperationLeases guard 1000000 "unrelated" [] [(ImageVersion 2, ExclusiveAccess)] (const (pure ()))
    withOperationLeases guard 1000000 "next writer" [] [(ImageVersion 1, ExclusiveAccess)] (const (pure ()))
  it "releases acquired leases on exception and cancels queued acquisitions" $ do
    guard <- newOperationGuard
    outcome <- try $ withOperationLeases guard 1000000 "failed operation" [] [(ImageVersion 1, ExclusiveAccess)] (const (throwIO (userError "failed")))
    (outcome :: Either SomeException ()) `shouldSatisfy` isLeft
    withOperationLeases guard 1000000 "holder" [] [(ImageVersion 1, ExclusiveAccess)] $ \_ -> do
      entered <- newEmptyMVar
      finished <- newEmptyMVar
      thread <-
        forkFinally
          ( do
              putMVar entered ()
              withOperationLeases guard 1000000 "cancelled waiter" [] [(ImageVersion 1, ExclusiveAccess)] (const (pure ()))
          )
          (putMVar finished)
      takeMVar entered
      killThread thread
      timeout 1000000 (takeMVar finished) >>= (`shouldSatisfy` maybe False isLeft)
    withOperationLeases guard 1000000 "after cancellation" [] [(ImageVersion 1, ExclusiveAccess)] (const (pure ()))

  it "does not let the same task name bypass an independent lease" $ do
    guard <- newOperationGuard
    withOperationLeases guard 1000000 "task 1" [] [(ImageVersion 1, ExclusiveAccess)] $ \_ -> do
      result <- try $ withOperationLeases guard 10000 "task 1" [] [(ImageVersion 1, SharedAccess)] (const (pure ()))
      (result :: Either OperationLeaseTimeout ()) `shouldSatisfy` isLeft
  it "rejects shared to exclusive upgrades without waiting" $ do
    guard <- newOperationGuard
    withOperationLeases guard 1000000 "reader" [] [(ImageVersion 1, SharedAccess)] $ \held -> do
      result <- timeout 100000 $ try $ withOperationLeases guard 1000000 "upgrade" held [(ImageVersion 1, ExclusiveAccess)] (const (pure ()))
      (result :: Maybe (Either OperationLeaseTimeout ())) `shouldSatisfy` maybe False isLeft
  it "acquires a waiting request when its holder releases before the deadline" $ do
    guard <- newOperationGuard
    started <- newEmptyMVar
    done <- newEmptyMVar
    withOperationLeases guard 1000000 "holder" [] [(ImageVersion 1, ExclusiveAccess)] $ \_ -> do
      _ <- forkFinally (putMVar started () >> withOperationLeases guard 1000000 "waiter" [] [(ImageVersion 1, ExclusiveAccess)] (const (pure ()))) (putMVar done)
      takeMVar started
    result <- timeout 2000000 (takeMVar done)
    case result of
      Just (Right ()) -> pure ()
      other -> expectationFailure (show other)
  it "gives a queued writer priority over later readers without blocking disjoint work" $ do
    guard <- newOperationGuard
    writerEntered <- newEmptyMVar
    writerRelease <- newEmptyMVar
    writerDone <- newEmptyMVar
    readerEntered <- newEmptyMVar
    readerDone <- newEmptyMVar
    withOperationLeases guard 2000000 "first reader" [] [(ImageVersion 1, SharedAccess)] $ \_ -> do
      _ <-
        forkFinally
          (withOperationLeases guard 2000000 "queued writer" [] [(ImageVersion 1, ExclusiveAccess)] (const (putMVar writerEntered () >> takeMVar writerRelease)))
          (putMVar writerDone)
      -- A shared probe can only time out once the writer is queued.
      let waitForWriter = do
            probe <- try $ withOperationLeases guard 10000 "probe" [] [(ImageVersion 1, SharedAccess)] (const (pure ()))
            case (probe :: Either OperationLeaseTimeout ()) of
              Left _ -> pure ()
              Right () -> waitForWriter
      timeout 1000000 waitForWriter `shouldReturn` Just ()
      withOperationLeases guard 1000000 "disjoint" [] [(ImageVersion 2, ExclusiveAccess)] (const (pure ()))
      _ <-
        forkFinally
          (withOperationLeases guard 2000000 "later reader" [] [(ImageVersion 1, SharedAccess)] (const (putMVar readerEntered ())))
          (putMVar readerDone)
      pure ()
    timeout 1000000 (takeMVar writerEntered) `shouldReturn` Just ()
    tryReadMVar readerEntered `shouldReturn` Nothing
    putMVar writerRelease ()
    takeMVar writerDone >>= either (expectationFailure . show) pure
    takeMVar readerDone >>= either (expectationFailure . show) pure
    takeMVar readerEntered
  it "does not reuse expired inherited handles" $ do
    guard <- newOperationGuard
    old <- withOperationLeases guard 1000000 "old" [] [(ImageVersion 1, SharedAccess)] pure
    withOperationLeases guard 1000000 "new holder" [] [(ImageVersion 1, ExclusiveAccess)] $ \_ -> do
      result <- try $ withOperationLeases guard 10000 "stale handle" old [(ImageVersion 1, SharedAccess)] (const (pure ()))
      (result :: Either OperationLeaseTimeout ()) `shouldSatisfy` isLeft

shouldReturnSatisfy :: (Show a) => IO a -> (a -> Bool) -> Expectation
shouldReturnSatisfy operation predicate = operation >>= (`shouldSatisfy` predicate)
