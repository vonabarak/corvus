{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the @emit*@ dispatchers in "Corvus.Client.Output".
--
-- These replaced ~200 LOC of @if isStructured fmt then … else …@ blocks
-- across every CLI command handler (refactor P6, commit @6016d0d@). A
-- regression in one of them silently breaks output for one format
-- across the entire CLI, so direct tests earn their keep.
module Corvus.EmitSpec (spec) where

import Capnp.Rpc.Errors (eFailed)
import Control.Exception (ErrorCall (..), SomeException, toException)
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
    it "runs the text action for non-Cap'n-Proto exceptions under TextOutput" $ do
      (ref, action) <- mkSentinel
      emitRpcError TextOutput (ErrorCall "boom") action
      readIORef ref `shouldReturn` True

    it "does not run the text action for structured Cap'n Proto errors under TextOutput" $ do
      (ref, action) <- mkSentinel
      let exn = toException (eFailed "vm_not_found :: VM 'web-1' not found") :: SomeException
      emitRpcError TextOutput exn action
      -- Text mode with structured code prints its own message and skips textAction
      readIORef ref `shouldReturn` False

    it "runs the text action for code-less Cap'n Proto errors under TextOutput" $ do
      (ref, action) <- mkSentinel
      let exn = toException (eFailed "connection reset by peer") :: SomeException
      emitRpcError TextOutput exn action
      readIORef ref `shouldReturn` True

    it "emits a generic rpc_error for non-Cap'n-Proto exceptions" $ do
      (_, action) <- mkSentinel
      out <- grab (emitRpcError JsonOutput (ErrorCall "boom") action)
      out `shouldContain` "\"status\":\"error\""
      out `shouldContain` "\"error\":\"rpc_error\""
      out `shouldContain` "boom"
      out `shouldNotContain` "\"code\""

    it "extracts the structured wire code from a Cap'n Proto exception" $ do
      (_, action) <- mkSentinel
      let exn =
            toException (eFailed "vm_not_found :: VM 'web-1' not found") :: SomeException
      out <- grab (emitRpcError JsonOutput exn action)
      out `shouldContain` "\"error\":\"rpc_error\""
      out `shouldContain` "\"code\":\"vm_not_found\""
      out `shouldContain` "\"message\":\"VM 'web-1' not found\""

    it "keeps a code-less Cap'n Proto reason as a plain rpc_error" $ do
      (_, action) <- mkSentinel
      let exn = toException (eFailed "connection reset by peer") :: SomeException
      out <- grab (emitRpcError JsonOutput exn action)
      out `shouldContain` "\"error\":\"rpc_error\""
      out `shouldContain` "\"message\":\"connection reset by peer\""
      out `shouldNotContain` "\"code\""

    it "emits code in text mode for structured errors" $ do
      out <- grab (emitRpcError TextOutput (toException (eFailed "vm_not_found :: test message") :: SomeException) (pure ()))
      out `shouldContain` "Error [vm_not_found]: test message"

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
