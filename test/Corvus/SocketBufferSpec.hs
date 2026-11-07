{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the ring-buffer replay sanitiser. Covers the
-- DSR / DA / CPR / window-operation stripping that 'Corvus.Rpc.Streams.runByteSinkRelay'
-- applies to the initial buffered replay.
module Corvus.SocketBufferSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Control.Concurrent.Async (cancel, wait, withAsync)
import Control.Concurrent.STM (atomically, check, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (bracket, finally)
import Corvus.Node.SocketBuffer (newSocketBuffer, startSocketBufferThread, stripTerminalQueries, waitForData, writeBuffer)
import Corvus.Qemu.Config (defaultQemuConfig)
import Corvus.Types (SocketBufferHandle (..))
import qualified Data.Map.Strict as Map
import qualified Network.Socket as NS
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

esc :: BS.ByteString
esc = BS.singleton 0x1b

spec :: Spec
spec = describe "stripTerminalQueries" $ do
  it "strips a bare cursor-position query" $ do
    stripTerminalQueries (esc <> "[6n") `shouldBe` BS.empty

  it "strips a cursor-position report response" $ do
    stripTerminalQueries (esc <> "[1;124R") `shouldBe` BS.empty

  it "strips a device-attributes query" $ do
    stripTerminalQueries (esc <> "[c") `shouldBe` BS.empty

  it "strips a secondary device-attributes query" $ do
    stripTerminalQueries (esc <> "[>c") `shouldBe` BS.empty

  it "strips a device-status query" $ do
    stripTerminalQueries (esc <> "[5n") `shouldBe` BS.empty

  it "strips window queries and reports" $ do
    mapM_
      (\params -> stripTerminalQueries (esc <> "[" <> params <> "t") `shouldBe` BS.empty)
      ["11", "13;2", "14;2", "15", "16", "18", "19", "20", "21", "8;34;115"]

  it "strips repeated window exchanges while preserving surrounding text and colors" $ do
    let exchange = esc <> "[18t" <> esc <> "[8;34;115t"
        colored = esc <> "[31mlogin:" <> esc <> "[0m"
    stripTerminalQueries ("boot" <> exchange <> exchange <> colored <> exchange)
      `shouldBe` "boot" <> colored

  it "preserves private, intermediate and nonstandard parameter forms ending in t" $ do
    let seqs = esc <> "[>0t" <> esc <> "[1 t" <> esc <> "[1;1;2;2;7$t" <> esc <> "[18:1t"
    stripTerminalQueries seqs `shouldBe` seqs

  it "leaves plain text untouched" $ do
    stripTerminalQueries "hello world\n" `shouldBe` "hello world\n"

  it "leaves unrelated CSI sequences (cursor moves, color) untouched" $ do
    let seqs =
          BSC.concat
            [ esc <> "[2J" -- clear screen
            , esc <> "[H" -- cursor home
            , esc <> "[31m" -- red
            , "hello"
            , esc <> "[0m" -- reset
            , esc <> "[5;10H" -- cursor position set
            ]
    stripTerminalQueries seqs `shouldBe` seqs

  it "strips a query embedded in surrounding text" $ do
    stripTerminalQueries ("before " <> esc <> "[6n after") `shouldBe` "before  after"

  it "strips repeated queries/responses (the reported reconnect symptom)" $ do
    let junk = BSC.concat (replicate 5 (esc <> "[1;1R" <> esc <> "[1;124R"))
    stripTerminalQueries junk `shouldBe` BS.empty

  it "leaves a lone ESC (no CSI follow-up) untouched" $ do
    stripTerminalQueries (esc <> "not a CSI") `shouldBe` esc <> "not a CSI"

  it "handles an unterminated CSI at the end of the buffer" $ do
    -- CSI intro with params but no final byte — leave intact so the
    -- caller's terminal can continue parsing when the next chunk
    -- arrives (though for replay, the buffer has already wrapped).
    stripTerminalQueries (esc <> "[12;")
      `shouldBe` (esc <> "[12;")

  describe "subscriber wakeups" $ do
    it "broadcasts output to all waiting subscribers" $ do
      buf <- newSocketBuffer 16
      stopped <- newTVarIO False
      withAsync (waitForData buf 0 stopped) $ \first ->
        withAsync (waitForData buf 0 stopped) $ \second -> do
          writeBuffer buf "boot"
          timeout 1000000 (wait first) `shouldReturn` Just ("boot", 4)
          timeout 1000000 (wait second) `shouldReturn` Just ("boot", 4)

    it "observes output written before the subscriber starts waiting" $ do
      buf <- newSocketBuffer 4
      stopped <- newTVarIO False
      writeBuffer buf "abcdef"
      timeout 1000000 (waitForData buf 0 stopped) `shouldReturn` Just ("cdef", 6)

    it "wakes idle subscribers when QEMU shuts down" $ do
      buf <- newSocketBuffer 16
      stopped <- newTVarIO False
      withAsync (waitForData buf 0 stopped) $ \subscriber -> do
        atomically $ writeTVar stopped True
        timeout 1000000 (wait subscriber) `shouldReturn` Just ("", 0)

  describe "managed socket workers" $ do
    it "connects after delayed socket creation and wakes subscribers on cancellation" $
      within $
        withSystemTempDirectory "corvus-buffer" $ \dir -> do
          buffers <- newTVarIO Map.empty
          workers <- newTVarIO []
          eligible <- newTVarIO True
          let path = dir </> "serial.sock"
              cleanup = readTVarIO workers >>= mapM_ cancel
          flip finally cleanup $ do
            startSocketBufferThread defaultQemuConfig 1 path buffers 64 "serial" workers (readTVar eligible) (\_ _ -> pure ())
            bracket (NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol) NS.close $ \listener -> do
              NS.bind listener (NS.SockAddrUnix path)
              NS.listen listener 1
              bracket (fst <$> NS.accept listener) NS.close $ \_ -> do
                found <- timeout 2000000 $ atomically $ do
                  current <- readTVar buffers
                  check (Map.member 1 current)
                  pure (current Map.! 1)
                case found of
                  Nothing -> expectationFailure "worker failed to register after socket appeared"
                  Just handle -> withAsync (waitForData (sbhBuffer handle) 0 (sbhShutdown handle)) $ \subscriber -> do
                    cleanup
                    timeout 1000000 (wait subscriber) `shouldReturn` Just ("", 0)
                    Map.null <$> readTVarIO buffers `shouldReturn` True

    it "does not publish a socket for a retired runtime" $
      within $
        withSystemTempDirectory "corvus-retired-buffer" $ \dir -> do
          buffers <- newTVarIO Map.empty
          workers <- newTVarIO []
          startSocketBufferThread defaultQemuConfig 1 (dir </> "missing.sock") buffers 64 "serial" workers (pure False) (\_ _ -> pure ())
          owned <- readTVarIO workers
          timeout 1000000 (mapM_ wait owned) `shouldReturn` Just ()
          Map.null <$> readTVarIO buffers `shouldReturn` True

within :: IO () -> IO ()
within action = timeout 10000000 action `shouldReturn` Just ()
