{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Drift watcher for `corvus-netd`.
--
-- Detects out-of-band kernel-state changes — typically an admin
-- running @ip link del corvus-br-7@ — by parsing the streaming
-- output of @ip monitor link@.
--
-- Phase 2 first cut: shell out to @ip monitor@ and grep its
-- output. Direct rtnetlink via the @netlink@ Hackage package
-- avoids the fork and gives finer-grained event types, but
-- @ip monitor@'s line-protocol is stable, easy to parse, and
-- adds no library deps. Worth a rewrite only if the watcher
-- becomes a hot path.
module Corvus.Netd.IpMonitor
  ( LinkEvent (..)
  , runIpMonitor
  )
where

import qualified Control.Exception as E
import Control.Monad (forever)
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (BufferMode (..), hSetBuffering)
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , terminateProcess
  )

-- | Events we extract from `ip monitor link`. Phase 2 ships only
-- the "Deleted" case — that's what triggers reconciliation in
-- the agent's ledger. Additions / state changes can be added
-- when a consumer needs them.
newtype LinkEvent
  = LinkDeleted T.Text
  deriving (Eq, Show)

-- | Run @ip monitor link@ in a long-lived child process, parse
-- each line into a 'LinkEvent', and call the consumer for each.
-- Blocks the calling thread; intended to be spawned via
-- 'Control.Concurrent.Async.async'.
--
-- On parse failure for a given line we silently skip it — the
-- monitor format is stable enough that an unrecognised line
-- almost always means a kind of event we don't handle, not a
-- broken parser. (e.g. @CHANGE@ events for link state
-- transitions, which Phase 2 ignores.)
runIpMonitor :: (LinkEvent -> IO ()) -> IO ()
runIpMonitor onEvent = do
  (_, mOut, _, hdl) <-
    createProcess
      (proc "ip" ["-t", "monitor", "link"])
        { std_in = NoStream
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  case mOut of
    Nothing -> pure () -- shouldn't happen with CreatePipe
    Just out -> do
      hSetBuffering out LineBuffering
      let loop = forever $ do
            line <- TIO.hGetLine out
            for_ (parseLine line) onEvent
      E.finally
        (E.catch loop (\(_ :: IOError) -> pure ()))
        (terminateProcess hdl)

-- ---------------------------------------------------------------------------
-- Parser

-- | Parse one line of @ip monitor link@. Format observed in
-- practice (iproute2 6.x):
--
-- > Deleted 5: corvus-br-7: <BROADCAST,MULTICAST> mtu 1500 ...
-- > 5: corvus-br-7: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 ...
--
-- We split on whitespace and pick the third token (the ifname
-- with a trailing colon) for any line starting with @Deleted@.
parseLine :: T.Text -> Maybe LinkEvent
parseLine line = case T.words line of
  ("Deleted" : _idx : ifname : _rest) ->
    Just (LinkDeleted (T.dropWhileEnd (== ':') ifname))
  _ -> Nothing
