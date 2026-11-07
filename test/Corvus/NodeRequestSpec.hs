{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Corvus.NodeRequestSpec (spec) where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as N
import qualified Capnp.Gen.Streams as S
import Capnp.Rpc.Server (SomeServer)
import Control.Concurrent.Async (asyncThreadId, cancel, poll, waitCatch, withAsync)
import Control.Concurrent.MVar (newMVar, readMVar)
import Control.Concurrent.STM
import Control.Monad (void)
import Corvus.Node.Caps.Session (SessionServer (..), newSessionCap)
import Corvus.Node.GuestAgent.Connection (newGuestAgentConns)
import qualified Corvus.Node.Ledger as L
import Corvus.Node.SocketBuffer (newSocketBuffer, writeBuffer)
import qualified Corvus.Node.StatusPoller as SP
import Corvus.Node.Transfer (FileReader (..), newFileReader, newTokenRegistry)
import Corvus.NodeAgentClient.Core (remote)
import Corvus.Rpc.Common (handleParsed)
import Corvus.Rpc.Streams (callSink)
import Corvus.Types (SocketBufferHandle (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Supervisors (withSupervisor)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

-- A real RPC callback that never acknowledges a write. Its dispatch thread
-- remains blocked until the test supervisor closes it.
newtype StalledSink = StalledSink (TMVar ())
instance SomeServer StalledSink
instance S.ByteSink'server_ StalledSink where
  byteSink'write (StalledSink entered) = handleParsed $ \_ -> do
    atomically $ putTMVar entered ()
    atomically retry
  byteSink'end _ = handleParsed $ \_ -> pure S.ByteSink'end'results
  byteSink'abort _ = handleParsed $ \_ -> pure S.ByteSink'abort'results

spec :: Spec
spec = sequential $ describe "node request isolation" $ do
  it "preserves a caller deadline across a stalled remote call" $ do
    result <- timeout 100000 (remote (atomically retry :: IO ()))
    result `shouldSatisfy` isNothing

  it "propagates cancellation through remote calls" $ do
    entered <- newEmptyTMVarIO
    withAsync (remote (atomically (putTMVar entered ()) >> (atomically retry :: IO ()))) $ \worker -> do
      atomically $ takeTMVar entered
      cancel worker
      result <- waitCatch worker
      result `shouldSatisfy` either (const True) (const False)

  it "joins a dispatch worker blocked inside the shared RPC wrapper" $ do
    result <- timeout 2000000 $ withSupervisor $ \sup -> do
      entered <- newEmptyTMVarIO
      sink <- C.export @S.ByteSink sup (StalledSink entered)
      withAsync (callSink #write S.ByteSink'write'params {S.chunk = "blocked"} sink) $ \_ ->
        atomically $ takeTMVar entered
    result `shouldBe` Just ()

  it "returns console input and serves ping while replay acknowledgement is stalled" $
    withSupervisor $ \sup -> do
      buf <- newSocketBuffer 32
      writeBuffer buf "boot output"
      qemu <- newTVarIO Nothing
      shutdown <- newTVarIO False
      let handle = SocketBufferHandle buf qemu shutdown
      buffers <- newTVarIO (Map.singleton 1 handle)
      ledger <- L.newVmLedger
      subs <- SP.newSubscribers
      conns <- newGuestAgentConns (\_ _ -> pure ())
      tokens <- newTokenRegistry
      launch <- newMVar ()
      session <- newSessionCap "test" sup ledger subs conns buffers buffers tokens Nothing launch
      client <- C.export @N.Session sup (SessionServer session)
      entered <- newEmptyTMVarIO
      sink <- C.export @S.ByteSink sup (StalledSink entered)
      opened <- timeout 1000000 $ C.callP #openSerialConsole N.Session'openSerialConsole'params {N.vmId = 1, N.sink = sink} client >>= C.waitPipeline
      case opened of
        Nothing -> expectationFailure "console attachment waited for its callback"
        Just raw -> do
          N.Session'openSerialConsole'results {N.input = input} <-
            C.evalLimitT C.defaultLimit (C.parse raw)
              :: IO (C.Parsed N.Session'openSerialConsole'results)
          timeout 1000000 (atomically $ takeTMVar entered) `shouldReturn` Just ()
          timeout 1000000 (C.callP #ping N.Session'ping'params client >>= C.waitPipeline >> pure ()) `shouldReturn` Just ()
          timeout 2000000 (callSink #end S.ByteSink'end'params input) `shouldReturn` Just ()

  it "does not kill a completed reader worker before its RPC reply" $
    withSystemTempDirectory "corvus-reader-completed" $ \dir ->
      withSupervisor $ \sup -> do
        let path = dir </> "source"
        writeFile path "payload"
        reader <- newFileReader path
        client <- C.export @N.DiskReader sup reader
        -- Model the boundary after closeReader signals completion but before
        -- pipeInto clears frWorker and sends its successful RPC reply.
        withAsync (atomically retry :: IO ()) $ \worker -> do
          atomically $ do
            writeTVar (frWorker reader) (Just (asyncThreadId worker))
            writeTVar (frFinished reader) True
          timeout 2000000 (C.callP #cancel N.DiskReader'cancel'params client >>= C.waitPipeline >> pure ()) `shouldReturn` Just ()
          poll worker >>= (`shouldSatisfy` isNothing)
          readMVar (frHandle reader) >>= (`shouldSatisfy` isNothing)

  it "cancels a disk reader while its callback is stalled and rejects reuse" $
    withSystemTempDirectory "corvus-reader-cancel" $ \dir ->
      withSupervisor $ \sup -> do
        let path = dir </> "source"
        writeFile path "payload"
        reader <- newFileReader path
        client <- C.export @N.DiskReader sup reader
        entered <- newEmptyTMVarIO
        sink <- C.export @S.ByteSink sup (StalledSink entered)
        withAsync (C.callP #pipeInto N.DiskReader'pipeInto'params {N.sink = sink} client >>= C.waitPipeline) $ \pipe -> do
          timeout 1000000 (atomically $ takeTMVar entered) `shouldReturn` Just ()
          timeout 2000000 (C.callP #cancel N.DiskReader'cancel'params client >>= C.waitPipeline >> pure ()) `shouldReturn` Just ()
          timeout 2000000 (void (waitCatch pipe)) `shouldReturn` Just ()
          readMVar (frHandle reader) >>= (`shouldSatisfy` isNothing)
          (C.callP #pipeInto N.DiskReader'pipeInto'params {N.sink = sink} client >>= C.waitPipeline) `shouldThrow` anyException
