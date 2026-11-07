{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (AsyncCancelled (..))
import Control.Concurrent.STM
import Control.Exception (IOException, bracket_, throwIO, try)
import Control.Monad (replicateM_)
import Supervisors
import System.Timeout (timeout)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "owned worker teardown" $ do
    it "quietly cancels and joins children before returning" $ do
      started <- newEmptyTMVarIO
      finished <- newEmptyTMVarIO
      failures <- newTVarIO (0 :: Int)
      result <- timeout 1000000 $ withSupervisorReporting (\_ -> atomically $ modifyTVar' failures (+ 1)) $ \sup -> do
        supervise sup $ bracket_ (atomically $ putTMVar started ()) (atomically $ putTMVar finished ()) (threadDelay 10000000)
        atomically $ takeTMVar started
      result `shouldBe` Just ()
      atomically (tryReadTMVar finished) `shouldReturn` Just ()
      readTVarIO failures `shouldReturn` 0
    it "reports unexpected asynchronous exceptions" $ do
      reported <- newEmptyTMVarIO
      withSupervisorReporting (\_ -> atomically $ putTMVar reported ()) $ \sup -> do
        supervise sup (throwIO AsyncCancelled)
        timeout 1000000 (atomically $ takeTMVar reported) `shouldReturn` Just ()
    it "retains parent exceptions after joining children" $ do
      finished <- newEmptyTMVarIO
      started <- newEmptyTMVarIO
      result <- try $ withSupervisor $ \sup -> do
        supervise sup $ bracket_ (atomically $ putTMVar started ()) (atomically $ putTMVar finished ()) (threadDelay 10000000)
        atomically $ takeTMVar started
        ioError (userError "parent failed")
      case result of
        Left (_ :: IOException) -> pure ()
        Right () -> expectationFailure "parent exception disappeared"
      atomically (tryReadTMVar finished) `shouldReturn` Just ()
    it "supports queued workers and repeated immediate teardown" $ do
      replicateM_ 100 $ withSupervisor $ \sup -> do
        supervise sup (pure ())
        atomically $ superviseSTM sup (pure ())
      ready <- newEmptyTMVarIO
      withSupervisor $ \sup -> do
        atomically $ superviseSTM sup (atomically $ putTMVar ready ())
        timeout 1000000 (atomically $ takeTMVar ready) `shouldReturn` Just ()
    it "does not start work submitted after its scope closes" $ do
      sup <- withSupervisor pure
      started <- newTVarIO False
      supervise sup (atomically $ writeTVar started True)
      atomically $ superviseSTM sup (atomically $ writeTVar started True)
      threadDelay 1000
      readTVarIO started `shouldReturn` False
