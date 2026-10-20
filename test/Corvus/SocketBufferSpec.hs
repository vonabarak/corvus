{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the ring-buffer replay sanitiser. Covers the
-- DSR / DA / CPR / window-operation stripping that 'Corvus.Rpc.Streams.runByteSinkRelay'
-- applies to the initial buffered replay.
module Corvus.SocketBufferSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Corvus.Node.SocketBuffer (stripTerminalQueries)
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
