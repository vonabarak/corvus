module Main where

import Corvus.Client (optsInfo, readClientDefaults, runCommand)
import Options.Applicative (execParser)

main :: IO ()
main = do
  defs <- readClientDefaults
  opts <- execParser (optsInfo defs)
  runCommand opts
