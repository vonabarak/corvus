{-# LANGUAGE ScopedTypeVariables #-}

module Corvus.ImageOperationGuardSpec (spec) where

import Control.Concurrent (forkFinally, forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (SomeException, throwIO, try)
import Corvus.ImageOperationGuard
import Data.Either (isLeft)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "image resource leases" $ do
  it "allows shared readers, disjoint writers and explicit nested reuse" $ do
    guard <- newImageOperationGuard
    withImageLeases guard 1000000 "build" [] [(ImageVersion 1, SharedImage)] $ \held -> do
      withImageLeases guard 1000000 "clone subtask" held [(ImageVersion 1, SharedImage)] (const (pure ()))
      done <- newEmptyMVar
      _ <- forkIO $ withImageLeases guard 1000000 "another build" [] [(ImageVersion 1, SharedImage), (ImageFamily "other", ExclusiveImage)] (const (putMVar done ()))
      timeout 1000000 (takeMVar done) `shouldReturn` Just ()
  it "times out conflicting image and family requests with holder diagnostics" $ do
    guard <- newImageOperationGuard
    withImageLeases guard 1000000 "task 42 (build)" [] [(ImageVersion 1, SharedImage), (ImageFamily "output", ExclusiveImage)] $ \_ -> do
      outcome <- try $ withImageLeases guard 10000 "resize" [] [(ImageVersion 1, ExclusiveImage)] (const (pure ()))
      case outcome of
        Left (ImageLeaseTimeout message) -> show message `shouldContain` "task 42 (build)"
        Right () -> expectationFailure "conflicting writer acquired a reader's image"
      family <- try $ withImageLeases guard 10000 "publisher" [] [(ImageFamily "output", ExclusiveImage)] (const (pure ()))
      (family :: Either ImageLeaseTimeout ()) `shouldSatisfy` isLeft
  it "does not reserve a subset while waiting and removes timed out requests" $ do
    guard <- newImageOperationGuard
    withImageLeases guard 1000000 "holder" [] [(ImageVersion 1, ExclusiveImage)] $ \_ -> do
      done <- newEmptyMVar
      _ <- forkIO $ do
        result <- try $ withImageLeases guard 10000 "atomic request" [] [(ImageVersion 1, ExclusiveImage), (ImageVersion 2, ExclusiveImage)] (const (pure ()))
        putMVar done (result :: Either ImageLeaseTimeout ())
      takeMVar done `shouldReturnSatisfy` isLeft
      withImageLeases guard 1000000 "unrelated" [] [(ImageVersion 2, ExclusiveImage)] (const (pure ()))
    withImageLeases guard 1000000 "next writer" [] [(ImageVersion 1, ExclusiveImage)] (const (pure ()))
  it "releases acquired leases on exception and cancels queued acquisitions" $ do
    guard <- newImageOperationGuard
    outcome <- try $ withImageLeases guard 1000000 "failed operation" [] [(ImageVersion 1, ExclusiveImage)] (const (throwIO (userError "failed")))
    (outcome :: Either SomeException ()) `shouldSatisfy` isLeft
    withImageLeases guard 1000000 "holder" [] [(ImageVersion 1, ExclusiveImage)] $ \_ -> do
      entered <- newEmptyMVar
      finished <- newEmptyMVar
      thread <-
        forkFinally
          ( do
              putMVar entered ()
              withImageLeases guard 1000000 "cancelled waiter" [] [(ImageVersion 1, ExclusiveImage)] (const (pure ()))
          )
          (putMVar finished)
      takeMVar entered
      killThread thread
      timeout 1000000 (takeMVar finished) >>= (`shouldSatisfy` maybe False isLeft)
    withImageLeases guard 1000000 "after cancellation" [] [(ImageVersion 1, ExclusiveImage)] (const (pure ()))

  it "does not let the same task name bypass an independent lease" $ do
    guard <- newImageOperationGuard
    withImageLeases guard 1000000 "task 1" [] [(ImageVersion 1, ExclusiveImage)] $ \_ -> do
      result <- try $ withImageLeases guard 10000 "task 1" [] [(ImageVersion 1, SharedImage)] (const (pure ()))
      (result :: Either ImageLeaseTimeout ()) `shouldSatisfy` isLeft
  it "rejects shared to exclusive upgrades without waiting" $ do
    guard <- newImageOperationGuard
    withImageLeases guard 1000000 "reader" [] [(ImageVersion 1, SharedImage)] $ \held -> do
      result <- timeout 100000 $ try $ withImageLeases guard 1000000 "upgrade" held [(ImageVersion 1, ExclusiveImage)] (const (pure ()))
      (result :: Maybe (Either ImageLeaseTimeout ())) `shouldSatisfy` maybe False isLeft
  it "acquires a waiting request when its holder releases before the deadline" $ do
    guard <- newImageOperationGuard
    started <- newEmptyMVar
    done <- newEmptyMVar
    withImageLeases guard 1000000 "holder" [] [(ImageVersion 1, ExclusiveImage)] $ \_ -> do
      _ <- forkFinally (putMVar started () >> withImageLeases guard 1000000 "waiter" [] [(ImageVersion 1, ExclusiveImage)] (const (pure ()))) (putMVar done)
      takeMVar started
    result <- timeout 2000000 (takeMVar done)
    case result of
      Just (Right ()) -> pure ()
      other -> expectationFailure (show other)
  it "gives a queued writer priority over later readers without blocking disjoint work" $ do
    guard <- newImageOperationGuard
    writerEntered <- newEmptyMVar
    writerRelease <- newEmptyMVar
    writerDone <- newEmptyMVar
    readerEntered <- newEmptyMVar
    readerDone <- newEmptyMVar
    withImageLeases guard 2000000 "first reader" [] [(ImageVersion 1, SharedImage)] $ \_ -> do
      _ <-
        forkFinally
          (withImageLeases guard 2000000 "queued writer" [] [(ImageVersion 1, ExclusiveImage)] (const (putMVar writerEntered () >> takeMVar writerRelease)))
          (putMVar writerDone)
      -- A shared probe can only time out once the writer is queued.
      let waitForWriter = do
            probe <- try $ withImageLeases guard 10000 "probe" [] [(ImageVersion 1, SharedImage)] (const (pure ()))
            case (probe :: Either ImageLeaseTimeout ()) of
              Left _ -> pure ()
              Right () -> waitForWriter
      timeout 1000000 waitForWriter `shouldReturn` Just ()
      withImageLeases guard 1000000 "disjoint" [] [(ImageVersion 2, ExclusiveImage)] (const (pure ()))
      _ <-
        forkFinally
          (withImageLeases guard 2000000 "later reader" [] [(ImageVersion 1, SharedImage)] (const (putMVar readerEntered ())))
          (putMVar readerDone)
      pure ()
    timeout 1000000 (takeMVar writerEntered) `shouldReturn` Just ()
    tryReadMVar readerEntered `shouldReturn` Nothing
    putMVar writerRelease ()
    takeMVar writerDone >>= either (expectationFailure . show) pure
    takeMVar readerDone >>= either (expectationFailure . show) pure
    takeMVar readerEntered
  it "does not reuse expired inherited handles" $ do
    guard <- newImageOperationGuard
    old <- withImageLeases guard 1000000 "old" [] [(ImageVersion 1, SharedImage)] pure
    withImageLeases guard 1000000 "new holder" [] [(ImageVersion 1, ExclusiveImage)] $ \_ -> do
      result <- try $ withImageLeases guard 10000 "stale handle" old [(ImageVersion 1, SharedImage)] (const (pure ()))
      (result :: Either ImageLeaseTimeout ()) `shouldSatisfy` isLeft

shouldReturnSatisfy :: (Show a) => IO a -> (a -> Bool) -> Expectation
shouldReturnSatisfy operation predicate = operation >>= (`shouldSatisfy` predicate)
