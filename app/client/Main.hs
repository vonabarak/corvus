module Main where

import Corvus.Client (optsInfo, runCommand)
import Options.Applicative (execParser)

main :: IO ()
main = do
  opts <- execParser optsInfo
  runCommand opts
