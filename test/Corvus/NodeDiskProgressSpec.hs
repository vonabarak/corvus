{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Corvus.NodeDiskProgressSpec (spec) where

import qualified Capnp as C
import qualified Capnp.Gen.Streams as S
import Capnp.Rpc.Server (SomeServer)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, withAsync)
import Corvus.Node.DiskProgress (newDiskProgressSink, withByteProgress)
import Corvus.Rpc.Common (handleParsed)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Supervisors (withSupervisor)
import System.Timeout (timeout)
import Test.Hspec

newtype Observer = Observer (IO ())
instance SomeServer Observer
instance S.DiskProgressSink'server_ Observer where
  diskProgressSink'progress (Observer action) = handleParsed $ \_ -> do
    action
    pure S.DiskProgressSink'progress'results

spec :: Spec
spec = describe "disk progress observers" $ do
  it "disables a disconnected observer without failing the operation" $ withSupervisor $ \sup -> do
    calls <- newIORef (0 :: Int)
    sink <- C.export @S.DiskProgressSink sup (Observer (modifyIORef' calls (+ 1) >> ioError (userError "observer disconnected")))
    report <- newDiskProgressSink sink
    report mempty 0 1
    report mempty 1 1
    readIORef calls `shouldReturn` 1
  it "bounds a stalled observer and disables subsequent calls" $ withSupervisor $ \sup -> do
    calls <- newIORef (0 :: Int)
    sink <- C.export @S.DiskProgressSink sup (Observer (modifyIORef' calls (+ 1) >> threadDelay 10000000))
    report <- newDiskProgressSink sink
    timeout 6000000 (report mempty 0 1) `shouldReturn` Just ()
    timeout 100000 (report mempty 1 1) `shouldReturn` Just ()
    readIORef calls `shouldReturn` 1
  it "allows cancellation while waiting for an observer" $ withSupervisor $ \sup -> do
    sink <- C.export @S.DiskProgressSink sup (Observer (threadDelay 10000000))
    report <- newDiskProgressSink sink
    withAsync (report mempty 0 1) $ \worker -> do
      threadDelay 10000
      timeout 1000000 (cancel worker) `shouldReturn` Just ()
  it "reports the final byte count after stopping the sampler" $ do
    bytes <- newIORef 0
    observed <- newIORef []
    withByteProgress (readIORef bytes) (\n -> modifyIORef' observed (n :)) (modifyIORef' bytes (+ 1024))
    counts <- readIORef observed
    take 1 counts `shouldBe` [1024]
