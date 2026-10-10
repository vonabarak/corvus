{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless)
import Corvus.Coverage (Coverage (..), loadCoverage, meetsBaseline, readBaseline)
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
      (actual, trace) <- loadCoverage mixDirectory tracePath
      createDirectoryIfMissing True reportDirectory
      let reportTrace = reportDirectory </> "authored.tix"
      writeTix reportTrace trace
      callProcess "hpc" ["markup", reportTrace, "--hpcdir=" ++ takeDirectory mixDirectory, "--destdir=" ++ reportDirectory]
      printf
        "Haskell expression coverage: %.6f%% (%d/%d); minimum %.6f%%\n"
        (percentage actual)
        (coveredExpressions actual)
        (totalExpressions actual)
        (percentage baseline)
      putStrLn ("Coverage report: " ++ (reportDirectory </> "hpc_index.html"))
      let passed = meetsBaseline actual baseline
      if passed then pure () else hPutStrLn stderr "Coverage is below the committed baseline"
      pure passed
    _ -> fail "Usage: corvus-coverage-check BASELINE MIX_DIRECTORY TIX_FILE REPORT_DIRECTORY"
  where
    percentage c = 100 * fromIntegral (coveredExpressions c) / fromIntegral (totalExpressions c) :: Double
