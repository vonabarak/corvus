module Main (main) where

import qualified Spec
import System.Directory (doesFileExist, getCurrentDirectory, withCurrentDirectory)
import System.FilePath (takeDirectory, (</>))

main :: IO ()
main = do
  root <- getCurrentDirectory >>= projectRoot
  withCurrentDirectory root Spec.main

-- Stack runs tests from the tools package; these checks inspect the repository.
projectRoot :: FilePath -> IO FilePath
projectRoot directory = do
  found <- doesFileExist (directory </> "coverage-baseline.json")
  if found
    then pure directory
    else
      let parent = takeDirectory directory
       in if parent == directory
            then fail "Cannot find the Corvus project root"
            else projectRoot parent
