{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the ring-buffer replay sanitiser. Covers the DSR
-- / DA / CPR stripping that 'relayClient' applies to the initial
-- buffered replay.
module Corvus.SocketBufferSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Corvus.Qemu.SocketBuffer (stripTerminalQueries)
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
