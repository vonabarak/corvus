module Test.ClientCommand (cliOptions, runCli, checkCli) where

import Control.Exception (try)
import Corvus.Client.Commands (runCommand)
import Corvus.Client.Types
import System.Exit (ExitCode (..))
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import System.Timeout (timeout)
import Test.ClientRpc (Fixture, assertCalls, mockSocket)
import Test.Hspec

cliOptions :: Fixture -> OutputFormat -> Command -> Options
cliOptions f fmt = Options (Just (mockSocket f)) False "127.0.0.1" 9876 fmt BordersNoneOpt False [] False False Nothing

runCli :: Options -> IO (ExitCode, String, String)
runCli opts = do
  (err, (out, result)) <- hCapture [stderr] $ hCapture [stdout] $ timeout 5000000 (try (runCommand opts))
  case result of
    Just (Left code) -> pure (code, out, err)
    _ -> expectationFailure "CLI did not exit within five seconds" >> pure (ExitFailure 99, out, err)

checkCli :: Fixture -> OutputFormat -> Command -> ExitCode -> IO String
checkCli f fmt cmd expected = do
  (code, out, err) <- runCli (cliOptions f fmt cmd)
  (code, err) `shouldBe` (expected, "")
  assertCalls f
  pure out
