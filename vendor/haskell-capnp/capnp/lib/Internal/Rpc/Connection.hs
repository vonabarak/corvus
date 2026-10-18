{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Internal.Rpc.Connection (waitConn, handleConn, withConn, acquireConn, newConn, runConn, stopConn, callbacksLoop, sendLoop) where

import Capnp.Convert (parsedToMsg)
import qualified Capnp.Gen.Capnp.Rpc as R
import Capnp.Rpc.Errors (eDisconnected)
import Capnp.Rpc.Promise (breakPromise, fulfill)
import Capnp.Rpc.Transport (Transport (sendMsg))
import Control.Concurrent.Async (concurrently_, race)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Concurrent.STM
  ( atomically
  , flushTQueue
  , newEmptyTMVar
  , newTChan
  , newTQueue
  , newTVar
  , putTMVar
  , readTChan
  , readTMVar
  , retry
  , writeTVar
  )
import Control.Exception.Safe (bracket, finally, throwIO, try)
import Control.Monad (forever, void)
import Data.Foldable (for_)
import Internal.BuildPure (createPure)
import {-# SOURCE #-} Internal.Rpc.Client (dropConnExport)
import Internal.Rpc.Foundation (mapQueueSTM)
import Internal.Rpc.Protocol (recvLoop)
import Internal.Rpc.Types
  ( Conn (..)
  , Conn' (..)
  , ConnConfig (..)
  , EntryE (EntryE, client)
  , EntryQA (HaveFinish, NewQA)
  , LiveState (Dead, Live)
  , Return (Return, answerId, releaseParamCaps, union')
  , Return' (Return'exception)
  , RpcError (SentAbort)
  , newIdPool
  , unwrapClient
  )
import Lifetimes (Acquire, mkAcquire, withAcquire)
import Lifetimes.Async (acquireAsync)
import qualified ListT (traverse_)
import qualified StmContainers.Map as M (listT, new)
import System.Mem.StableName (makeStableName)
import System.Timeout (timeout)

-- | Wait until a connection has shut down, then re-throw the
-- exception that killed it, if any.
waitConn :: Conn -> IO ()
waitConn Conn {done} = atomically (readTMVar done) >>= either throwIO pure

-- | Handle a connection to another vat. Returns when the connection is closed.
handleConn
  :: Transport
  -> ConnConfig
  -> IO ()
handleConn transport cfg =
  withAcquire (acquireConn transport cfg) waitConn

-- | Run the function with access to a connection. Shut down the connection
-- when it returns.
withConn :: Transport -> ConnConfig -> (Conn -> IO a) -> IO a
withConn transport cfg f =
  withAcquire (acquireConn transport cfg) $ \conn -> do
    result <- f conn `race` waitConn conn
    case result of
      Left v -> pure v
      Right () ->
        error "BUG: waitConn returned normally"

acquireConn :: Transport -> ConnConfig -> Acquire Conn
acquireConn transport cfg = do
  (conn, conn') <- mkAcquire (newConn cfg) stopConn
  let Conn {done = completion} = conn
  _ <- acquireAsync $ do
    result <- try $ runConn transport (conn, conn')
    atomically $ putTMVar completion result
    either throwIO pure result
  pure conn

newConn :: ConnConfig -> IO (Conn, Conn')
newConn
  ConnConfig
    { maxQuestions
    , maxExports
    , maxCallWords
    , debugMode
    , bootstrap
    } = do
    stableName <- makeStableName =<< newEmptyMVar
    atomically $ do
      questionIdPool <- newIdPool maxQuestions
      exportIdPool <- newIdPool maxExports

      sendQ <- newTChan

      availableCallWords <- newTVar maxCallWords

      questions <- M.new
      answers <- M.new
      exports <- M.new
      imports <- M.new

      embargos <- M.new
      pendingCallbacks <- newTQueue

      let conn' =
            Conn'
              { questionIdPool
              , exportIdPool
              , sendQ
              , availableCallWords
              , questions
              , answers
              , exports
              , imports
              , embargos
              , pendingCallbacks
              , myBootstrap = bootstrap
              }
      done <- newEmptyTMVar
      liveState <- newTVar (Live conn')
      let conn =
            Conn
              { stableName
              , debugMode
              , liveState
              , done
              }
      pure (conn, conn')

runConn :: Transport -> (Conn, Conn') -> IO ()
runConn transport (conn, conn') = do
  result <-
    try $
      recvLoop transport conn
        `concurrently_` sendLoop transport conn'
        `concurrently_` callbacksLoop conn'
  case result of
    Left (SentAbort e) -> do
      -- We need to actually send it:
      rawMsg <- createPure maxBound $ parsedToMsg $ R.Message'abort e
      void $ timeout 1000000 $ sendMsg transport rawMsg
      throwIO $ SentAbort e
    Left e ->
      throwIO e
    Right _ ->
      pure ()

stopConn :: (Conn, Conn') -> IO ()
stopConn
  ( conn@Conn {liveState}
    , conn'@Conn' {questions, exports, embargos}
    ) = do
    atomically $ do
      let walk table = flip ListT.traverse_ (M.listT table)
      -- drop the bootstrap interface:
      for_
        (myBootstrap conn' >>= unwrapClient)
        (dropConnExport conn)
      -- Remove everything from the exports table:
      walk exports $ \(_, EntryE {client}) ->
        dropConnExport conn client
      -- Outstanding questions should all throw disconnected:
      walk questions $ \(qid, entry) ->
        let raiseDisconnected onReturn =
              mapQueueSTM conn' onReturn $
                Return
                  { answerId = qid
                  , releaseParamCaps = False
                  , union' = Return'exception eDisconnected
                  }
         in case entry of
              (NewQA _ onReturn) -> raiseDisconnected onReturn
              (HaveFinish _ onReturn) -> raiseDisconnected onReturn
              _ -> pure ()
      -- same thing with embargos:
      walk embargos $ \(_, fulfiller) ->
        breakPromise fulfiller eDisconnected
      -- mark the connection as dead, making the live state inaccessible:
      writeTVar liveState Dead

-- | See Note [callbacks]
callbacksLoop :: Conn' -> IO ()
callbacksLoop Conn' {pendingCallbacks} =
  loop `finally` cleanup
  where
    loop =
      forever $
        doCallbacks $
          atomically $
            flushTQueue pendingCallbacks >>= \case
              -- We need to make sure to block if there weren't any jobs, since
              -- otherwise we'll busy loop, pegging the CPU.
              [] -> retry
              cbs -> pure cbs
    cleanup =
      -- Make sure any pending callbacks get run. This is important, since
      -- some of these do things like raise disconnected exceptions.
      doCallbacks $ atomically $ flushTQueue pendingCallbacks
    doCallbacks getCbs =
      -- We need to be careful not to lose any callbacks in the event
      -- of an exception (even an async one):
      bracket
        getCbs
        (foldr finally (pure ()))
        (\_ -> pure ())

-- | 'sendLoop' shunts messages from the send queue into the transport.
sendLoop :: Transport -> Conn' -> IO ()
sendLoop transport Conn' {sendQ} =
  forever $ do
    (msg, f) <- atomically $ readTChan sendQ
    sendMsg transport msg
    atomically $ fulfill f ()
