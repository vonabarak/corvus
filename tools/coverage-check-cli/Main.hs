{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless)
import Corvus.Coverage (Baseline (..), Coverage (..), loadCoverage, meetsBaseline, readBaseline)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess)
import Text.Printf (printf)
import Trace.Hpc.Tix (writeTix)

main :: IO ()
main = do
  result <- try @SomeException check
  case result of
    Left err -> hPutStrLn stderr (displayException err) >> exitFailure
    Right passed -> unless passed exitFailure

check :: IO Bool
check = do
  args <- getArgs
  case args of
    [baselinePath, mixDirectory, tracePath, reportDirectory] -> do
      baseline <- readBaseline baselinePath
      (actual, trace, clients, commands) <- loadCoverage mixDirectory tracePath
      createDirectoryIfMissing True reportDirectory
      let reportTrace = reportDirectory </> "authored.tix"
      writeTix reportTrace trace
      callProcess "hpc" ["markup", reportTrace, "--hpcdir=" ++ takeDirectory mixDirectory, "--destdir=" ++ reportDirectory]
      printf
        "Haskell expression coverage: %.6f%% (%d/%d); minimum %.6f%%\n"
        (percentage actual)
        (coveredExpressions actual)
        (totalExpressions actual)
        (percentage (overallMinimum baseline))
      cliPassed <- reportScope "CLI" clients (cliMinimumPercent baseline)
      commandResults <- mapM (\(name, c) -> reportScope name c (commandMinimumPercent baseline)) commands
      putStrLn ("Coverage report: " ++ (reportDirectory </> "hpc_index.html"))
      let passed = meetsBaseline actual (overallMinimum baseline) && cliPassed && and commandResults
      if passed then pure () else hPutStrLn stderr "Coverage is below an overall or CLI minimum"
      pure passed
    _ -> fail "Usage: corvus-coverage-check BASELINE MIX_DIRECTORY TIX_FILE REPORT_DIRECTORY"
  where
    reportScope :: String -> Coverage -> Integer -> IO Bool
    reportScope name actual minimumPercent = do
      let passed = meetsBaseline actual (Coverage minimumPercent 100)
      printf "%s expression coverage: %.6f%% (%d/%d); minimum %d%% %s\n" name (percentage actual) (coveredExpressions actual) (totalExpressions actual) minimumPercent (if passed then "PASS" else "FAIL" :: String)
      pure passed
    percentage c = 100 * fromIntegral (coveredExpressions c) / fromIntegral (totalExpressions c) :: Double
