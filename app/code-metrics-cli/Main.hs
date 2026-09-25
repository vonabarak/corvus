{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (forM_)
import Corvus.CodeMetrics (analyzeProject, limitViolations, renderReport)
import System.Directory (getCurrentDirectory)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  projectRoot <- getCurrentDirectory
  analyzeProject projectRoot >>= \case
    Left errors -> do
      forM_ errors (hPutStrLn stderr)
      exitFailure
    Right report -> do
      putStr (renderReport report)
      case limitViolations report of
        [] -> pure ()
        violations -> do
          hPutStrLn stderr "CodeMetrics limit violations:"
          forM_ violations (hPutStrLn stderr . ("  " <>))
          exitFailure
