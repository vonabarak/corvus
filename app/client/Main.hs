{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.Async (AsyncCancelled)
import Control.Exception (fromException)
import Corvus.Client (optsInfo, readClientDefaults, runCommand)
import qualified GHC.Conc as Conc
import Options.Applicative (execParser)
import System.IO (hPrint, stderr)

main :: IO ()
main = do
  -- Streaming RPCs (build, serial console, task / stats / guest-agent
  -- subscriptions) export a client-side sink cap into the connection's
  -- supervisor. When the command returns, the supervisor's teardown
  -- relays AsyncCancelled to every supervised child, including the
  -- sink's runServer loop. Those threads are forkIO'd by the
  -- 'supervisors' package without a catch, so AsyncCancelled escapes
  -- to GHC's default uncaught-exception handler, which prints
  -- "crv: AsyncCancelled" on stderr after an otherwise successful
  -- exit. Swallow that one specific exception here; pass everything
  -- else through unchanged.
  Conc.setUncaughtExceptionHandler $ \e -> case fromException e of
    Just (_ :: AsyncCancelled) -> pure ()
    Nothing -> hPrint stderr e
  defs <- readClientDefaults
  opts <- execParser (optsInfo defs)
  runCommand opts
