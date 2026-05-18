{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | EventSink subscriber registry + dispatch.
--
-- The agent runs one 'Subscribers' instance per process. Sessions
-- register their EventSink caps via 'addSink'; the rtnetlink
-- watcher fires 'dispatchVanished' on every drift event. Each
-- call is best-effort: a dead sink (the daemon disconnected
-- without explicit removal) is logged and silently dropped from
-- the registry so it doesn't keep failing on every subsequent
-- event.
module Corvus.Netd.Events
  ( Subscribers
  , newSubscribers
  , addSink
  , dispatchVanished
  )
where

import qualified Capnp.Gen.Netagent as CGN
import Capnp.Rpc.Common (Client)
import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVarIO
  )
import qualified Control.Exception as E
import Control.Monad (foldM)
import Control.Monad.Logger (logWarnN, runStderrLoggingT)
import Corvus.Rpc.Streams (callSink)
import qualified Data.Text as T

-- | The list of currently-subscribed sinks. New sinks are
-- prepended; failed sinks are removed during dispatch.
newtype Subscribers = Subscribers
  { subsList :: TVar [Client CGN.EventSink]
  }

newSubscribers :: IO Subscribers
newSubscribers = Subscribers <$> newTVarIO []

-- | Add a sink to the registry. There's no automatic removal —
-- failed sinks are pruned only when a dispatch attempt fails
-- against them. That's fine for Phase 2: the registry is
-- per-process and gets fully reset on agent restart.
addSink :: Subscribers -> Client CGN.EventSink -> IO ()
addSink subs sink =
  atomically $ modifyTVar' (subsList subs) (sink :)

-- | Fire @onResourceVanished(kind, name)@ on every registered
-- sink. Sinks that throw are removed from the registry so
-- subsequent dispatches don't keep hitting them.
dispatchVanished :: Subscribers -> T.Text -> T.Text -> IO ()
dispatchVanished subs kind name = do
  sinks <- readTVarIO (subsList subs)
  remaining <-
    foldM
      ( \acc sink -> do
          result <- E.try @E.SomeException $ deliver sink
          case result of
            Right () -> pure (sink : acc)
            Left e -> do
              runStderrLoggingT $
                logWarnN
                  ("[netd] dropping event sink (" <> T.pack (show e) <> ")")
              pure acc
      )
      []
      sinks
  atomically $ modifyTVar' (subsList subs) (const (reverse remaining))
  where
    deliver =
      callSink
        #onResourceVanished
        CGN.EventSink'onResourceVanished'params
          { kind = kind
          , name = name
          }
