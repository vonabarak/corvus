{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the @emit*@ dispatchers in "Corvus.Client.Output".
--
-- These replaced ~200 LOC of @if isStructured fmt then … else …@ blocks
-- across every CLI command handler (refactor P6, commit @6016d0d@). A
-- regression in one of them silently breaks output for one format
-- across the entire CLI, so direct tests earn their keep.
module Corvus.EmitSpec (spec) where

import Control.Exception (ErrorCall (..))
import Corvus.Client.Output
import Corvus.Client.Types (OutputFormat (..))
import Data.Aeson (toJSON, (.=))
import qualified Data.Aeson as Aeson
import Data.IORef
import Data.Text (Text)
import System.IO.Silently (capture_)
import Test.Hspec

-- | Run an IO action and capture stdout into a 'String'.
grab :: IO () -> IO String
grab = capture_

-- | A text-mode "body" that records whether it ran. Tests use this to
-- prove that structured formats /don't/ invoke the text callback.
mkSentinel :: IO (IORef Bool, IO ())
mkSentinel = do
  ref <- newIORef False
  pure (ref, writeIORef ref True)

-- | All cases in this spec capture stdout via 'capture_', which
-- redirects the process-wide file descriptor 1. Running them in
-- parallel with other specs (or with hspec's own progress printer)
-- would race; hence 'sequential'.
spec :: Spec
spec = sequential $ do
  describe "emitOk" $ do
    it "runs the text action under TextOutput" $ do
      (ref, action) <- mkSentinel
      emitOk TextOutput action
      readIORef ref `shouldReturn` True

    it "under JsonOutput emits {\"status\":\"ok\"} and skips the text action" $ do
      (ref, action) <- mkSentinel
      out <- grab (emitOk JsonOutput action)
      out `shouldContain` "\"status\":\"ok\""
      readIORef ref `shouldReturn` False

    it "under YamlOutput emits status: ok and skips the text action" $ do
      (ref, action) <- mkSentinel
      out <- grab (emitOk YamlOutput action)
      out `shouldContain` "status: ok"
      readIORef ref `shouldReturn` False

  describe "emitOkWith" $ do
    it "runs the text action under TextOutput" $ do
      (ref, action) <- mkSentinel
      emitOkWith TextOutput [("id", toJSON (42 :: Int))] action
      readIORef ref `shouldReturn` True

    it "includes extra fields in JSON output" $ do
      (_, action) <- mkSentinel
      out <- grab (emitOkWith JsonOutput [("id", toJSON (42 :: Int))] action)
      out `shouldContain` "\"id\":42"
      out `shouldContain` "\"status\":\"ok\""

    it "does not run the text action under JsonOutput" $ do
      (ref, action) <- mkSentinel
      _ <- grab (emitOkWith JsonOutput [("id", toJSON (42 :: Int))] action)
      readIORef ref `shouldReturn` False

  describe "emitError" $ do
    it "runs the text action under TextOutput" $ do
      (ref, action) <- mkSentinel
      emitError TextOutput "not_found" "VM missing" action
      readIORef ref `shouldReturn` True

    it "emits {status, error, message} under JsonOutput" $ do
      (_, action) <- mkSentinel
      out <- grab (emitError JsonOutput "not_found" "VM missing" action)
      out `shouldContain` "\"status\":\"error\""
      out `shouldContain` "\"error\":\"not_found\""
      out `shouldContain` "\"message\":\"VM missing\""

    it "does not run the text action under JsonOutput" $ do
      (ref, action) <- mkSentinel
      _ <- grab (emitError JsonOutput "err" "msg" action)
      readIORef ref `shouldReturn` False

  describe "emitRpcError" $ do
    it "prints 'Error: <show>' on text output" $ do
      out <- grab (emitRpcError TextOutput (ErrorCall "boom"))
      out `shouldContain` "Error:"
      out `shouldContain` "boom"

    it "emits error code rpc_error under JsonOutput" $ do
      out <- grab (emitRpcError JsonOutput (ErrorCall "boom"))
      out `shouldContain` "\"status\":\"error\""
      out `shouldContain` "\"error\":\"rpc_error\""
      out `shouldContain` "boom"

    it "works with any Show instance" $ do
      let _ = ErrorCall "unused" :: ErrorCall -- typeable constraint sanity
      out <- grab (emitRpcError JsonOutput (42 :: Int))
      out `shouldContain` "\"error\":\"rpc_error\""
      out `shouldContain` "42"

  describe "emitResult" $ do
    it "runs the text action under TextOutput" $ do
      (ref, action) <- mkSentinel
      emitResult TextOutput ("hello" :: Text) action
      readIORef ref `shouldReturn` True

    it "serialises the value directly (no wrapping object) under JsonOutput" $ do
      (_, action) <- mkSentinel
      let val = Aeson.object ["name" .= ("vm1" :: Text), "id" .= (7 :: Int)]
      out <- grab (emitResult JsonOutput val action)
      -- Unlike emitOk/emitError there's no {"status": ...} wrapper.
      out `shouldNotContain` "\"status\""
      out `shouldContain` "\"name\":\"vm1\""
      out `shouldContain` "\"id\":7"

    it "does not run the text action under JsonOutput" $ do
      (ref, action) <- mkSentinel
      _ <- grab (emitResult JsonOutput ("ok" :: Text) action)
      readIORef ref `shouldReturn` False
