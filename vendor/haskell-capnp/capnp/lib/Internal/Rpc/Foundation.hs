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

module Internal.Rpc.Foundation (unwrapServer, clientExportMap, addBumpExport, queueIO, queueSTM, mapQueueSTM, newQuestion, freeQuestion, newExport, freeExport, newEmbargo, freeEmbargo, marshalMsgTarget, marshalPromisedAnswer, unmarshalPromisedAnswer, unmarshalOps, ptrPathClient, transformClient, ptrClient, followPtrs, lookupAbort, insertNewAbort, sendPureMsg, finishQuestion, updateQAReturn, updateQAFinish, subscribeReturn, abortConn, getLive, whenLive) where

import Capnp.Convert (parsedToMsg)
import Capnp.Fields (Which)
import qualified Capnp.Gen.Capnp.Rpc as R
import Capnp.Mutability (Mutability (Const))
import Capnp.Rpc.Errors (eDisconnected, eFailed)
import Capnp.Rpc.Promise (Fulfiller, newPromise)
import Capnp.TraversalLimit (defaultLimit, evalLimitT)
import qualified Capnp.Untyped as UntypedRaw
  ( Ptr (PtrCap, PtrStruct)
  , ReadCtx
  , getClient
  , getPtr
  )
import Control.Concurrent.STM
  ( STM
  , atomically
  , catchSTM
  , readTVar
  , throwSTM
  , writeTChan
  , writeTQueue
  )
import Control.Exception.Safe (MonadThrow, throwM)
import Data.Foldable (Foldable (toList), traverse_)
import Data.Hashable (Hashable)
import Data.String (fromString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Word (Word16)
import qualified Focus (alter, alterM)
import Internal.BuildPure (createPure)
import Internal.Rpc.Breaker (Client, nullClient)
import Internal.Rpc.Types
  ( Client' (..)
  , Conn (Conn, liveState)
  , Conn'
    ( Conn'
    , exportIdPool
    , pendingCallbacks
    , questionIdPool
    , questions
    , sendQ
    )
  , EmbargoId (..)
  , EntryE (..)
  , EntryQA (..)
  , ExportMap (..)
  , IEId (..)
  , ImportRef (..)
  , IsClient (..)
  , LiveState (Dead, Live)
  , MsgTarget (..)
  , PromisedAnswer (..)
  , QAId (..)
  , RawMPtr
  , Return (Return, answerId)
  , RpcError (SentAbort)
  , embargoWord
  , freeId
  , ieWord
  , newId
  , qaWord
  , unwrapClient
  )
import Internal.SnocList (SnocList)
import qualified Internal.SnocList as SnocList (fromList, snoc)
import qualified Lifetimes.Gc as Fin (readCell)
import qualified StmContainers.Map as M
  ( Map
  , delete
  , focus
  , insert
  , lookup
  )

queueIO :: Conn' -> IO () -> STM ()
queueIO Conn' {pendingCallbacks} = writeTQueue pendingCallbacks

-- | Queue another transaction to be run some time after this transaction
-- commits, in a thread bound to the lifetime of the connection. If this is
-- called multiple times within the same transaction, each of the
-- transactions will be run separately, in the order they were queued.
--
-- See Note [callbacks]
queueSTM :: Conn' -> STM () -> STM ()
queueSTM conn = queueIO conn . atomically

-- | @'mapQueueSTM' conn fs val@ queues the list of transactions obtained
-- by applying each element of @fs@ to @val@.
mapQueueSTM :: Conn' -> SnocList (a -> STM ()) -> a -> STM ()
mapQueueSTM conn fs x = traverse_ (\f -> queueSTM conn (f x)) fs

-- Note [callbacks]
-- ================
--
-- There are many places where we want to register some code to run after
-- some later event has happened -- for exmaple:
--

-- * We send a Call to the remote vat, and when a corresponding Return message

--   is received, we want to fulfill (or break) the local promise for the
--   result.

-- * We send a Disembargo (with senderLoopback set), and want to actually lift

--   the embargo when the corresponding (receiverLoopback) message arrives.
--
-- Keeping the two parts of these patterns together tends to result in better
-- separation of concerns, and is easier to maintain.
--
-- To achieve this, the four tables and other connection state have fields in
-- which callbacks can be registered -- for example, an outstanding question has
-- fields containing transactions to run when the return and/or finish messages
-- arrive.
--
-- When it is time to actually run these, we want to make sure that each of them
-- runs as their own transaction. If, for example, when registering a callback to
-- run when a return message is received, we find that the return message is
-- already available, it might be tempting to just run the transaction immediately.
-- But this means that the synchronization semantics are totally different from the
-- case where the callback really does get run later!
--
-- In addition, we sometimes want to register a finalizer inside a transaction,
-- but this can only be done in IO.
--
-- To solve these issues, the connection maintains a queue of all callback actions
-- that are ready to run, and when the event a callback is waiting for occurs, we
-- simply move the callback to the queue, using 'queueIO' or 'queueSTM'. When the
-- connection starts up, it creates a thread running 'callbacksLoop', which just
-- continually flushes the queue, running the actions in the queue.

-- | Get a new question id. retries if we are out of available question ids.
newQuestion :: Conn' -> STM QAId
newQuestion = fmap QAId . newId . questionIdPool

-- | Return a question id to the pool of available ids.
freeQuestion :: Conn' -> QAId -> STM ()
freeQuestion conn = freeId (questionIdPool conn) . qaWord

-- | Get a new export id. retries if we are out of available export ids.
newExport :: Conn' -> STM IEId
newExport = fmap IEId . newId . exportIdPool

-- | Return a export id to the pool of available ids.
freeExport :: Conn' -> IEId -> STM ()
freeExport conn = freeId (exportIdPool conn) . ieWord

-- | Get a new embargo id. This shares the same pool as questions.
newEmbargo :: Conn' -> STM EmbargoId
newEmbargo = fmap EmbargoId . newId . questionIdPool

-- | Return an embargo id. to the available pool.
freeEmbargo :: Conn' -> EmbargoId -> STM ()
freeEmbargo conn = freeId (exportIdPool conn) . embargoWord

-- | Wait until a connection has shut down, then re-throw the
-- exception that killed it, if any.
marshalMsgTarget :: MsgTarget -> R.Parsed R.MessageTarget
marshalMsgTarget = \case
  ImportTgt importId ->
    R.MessageTarget $ R.MessageTarget'importedCap (ieWord importId)
  AnswerTgt tgt ->
    R.MessageTarget $ R.MessageTarget'promisedAnswer $ marshalPromisedAnswer tgt

marshalPromisedAnswer :: PromisedAnswer -> R.Parsed R.PromisedAnswer
marshalPromisedAnswer PromisedAnswer {answerId, transform} =
  R.PromisedAnswer
    { R.questionId = qaWord answerId
    , R.transform =
        map
          (R.PromisedAnswer'Op . R.PromisedAnswer'Op'getPointerField)
          (toList transform)
    }

unmarshalPromisedAnswer :: (MonadThrow m) => R.Parsed R.PromisedAnswer -> m PromisedAnswer
unmarshalPromisedAnswer R.PromisedAnswer {questionId, transform} = do
  idxes <- unmarshalOps (toList transform)
  pure
    PromisedAnswer
      { answerId = QAId questionId
      , transform = SnocList.fromList idxes
      }

unmarshalOps :: (MonadThrow m) => [R.Parsed R.PromisedAnswer'Op] -> m [Word16]
unmarshalOps [] = pure []
unmarshalOps (R.PromisedAnswer'Op {union' = R.PromisedAnswer'Op'noop} : ops) =
  unmarshalOps ops
unmarshalOps (R.PromisedAnswer'Op {union' = R.PromisedAnswer'Op'getPointerField i} : ops) =
  (i :) <$> unmarshalOps ops
unmarshalOps (R.PromisedAnswer'Op {union' = R.PromisedAnswer'Op'unknown' tag} : _) =
  throwM $ eFailed $ "Unknown PromisedAnswer.Op: " <> fromString (show tag)

-- | Create a new client based on a promise. The fulfiller can be used to
-- supply the final client.
ptrPathClient :: (MonadThrow m) => [Word16] -> RawMPtr -> m Client
ptrPathClient is ptr =
  evalLimitT defaultLimit $ followPtrs is ptr >>= ptrClient

transformClient :: [R.Parsed R.PromisedAnswer'Op] -> RawMPtr -> Conn' -> STM Client
transformClient transform ptr conn =
  (unmarshalOps transform >>= flip ptrPathClient ptr)
    `catchSTM` abortConn conn

ptrClient :: (UntypedRaw.ReadCtx m 'Const) => RawMPtr -> m Client
ptrClient Nothing = pure nullClient
ptrClient (Just (UntypedRaw.PtrCap cap)) = UntypedRaw.getClient cap
ptrClient (Just _) = throwM $ eFailed "Tried to call method on non-capability."

-- | Follow a series of pointer indicies, returning the final value, or 'Left'
-- with an error if any of the pointers in the chain (except the last one) is
-- a non-null non struct.
followPtrs :: (UntypedRaw.ReadCtx m 'Const) => [Word16] -> RawMPtr -> m RawMPtr
followPtrs [] ptr =
  pure ptr
followPtrs (_ : _) Nothing =
  pure Nothing
followPtrs (i : is) (Just (UntypedRaw.PtrStruct struct)) =
  UntypedRaw.getPtr (fromIntegral i) struct >>= followPtrs is
followPtrs (_ : _) (Just _) =
  throwM $ eFailed "Tried to access pointer field of non-struct."

lookupAbort
  :: (Hashable k, Show k)
  => Text
  -> Conn'
  -> M.Map k v
  -> k
  -> (v -> STM a)
  -> STM a
lookupAbort keyTypeName conn m key f = do
  result <- M.lookup key m
  case result of
    Just val ->
      f val
    Nothing ->
      abortConn conn $
        eFailed $
          mconcat
            [ "No such "
            , keyTypeName
            , ": "
            , fromString (show key)
            ]

-- | @'insertNewAbort' keyTypeName conn key value stmMap@ inserts a key into a
-- map, aborting the connection if it is already present. @keyTypeName@ will be
-- used in the error message sent to the remote vat.
insertNewAbort :: (Hashable k) => Text -> Conn' -> k -> v -> M.Map k v -> STM ()
insertNewAbort keyTypeName conn key value =
  M.focus
    ( Focus.alterM $ \case
        Just _ ->
          abortConn conn $
            eFailed $
              "duplicate entry in " <> keyTypeName <> " table."
        Nothing ->
          pure (Just value)
    )
    key

-- | Generate a cap table describing the capabilities reachable from the given
-- pointer. The capability table will be correct for any message where all of
-- the capabilities are within the subtree under the pointer.
sendPureMsg :: Conn' -> R.Parsed (Which R.Message) -> Fulfiller () -> STM ()
sendPureMsg Conn' {sendQ} msg onSent = do
  rawMsg <- createPure maxBound (parsedToMsg (R.Message msg))
  writeTChan sendQ (rawMsg, onSent)

-- | Send a finish message, updating connection state and triggering
-- callbacks as necessary.
finishQuestion :: Conn' -> R.Parsed R.Finish -> STM ()
finishQuestion conn@Conn' {questions} finish@R.Finish {questionId} = do
  -- arrange for the question ID to be returned to the pool once
  -- the return has also been received:
  subscribeReturn "question" conn questions (QAId questionId) $ \_ ->
    freeQuestion conn (QAId questionId)
  (_, onSent) <- newPromise
  sendPureMsg conn (R.Message'finish finish) onSent
  updateQAFinish conn questions "question" finish

-- TODO(cleanup): updateQAReturn/Finish have a lot in common; can we refactor?

updateQAReturn :: Conn' -> M.Map QAId EntryQA -> Text -> Return -> STM ()
updateQAReturn conn table tableName ret@Return {answerId} =
  lookupAbort tableName conn table answerId $ \case
    (NewQA onFinish onReturn) -> do
      M.insert
        (HaveReturn ret onFinish)
        answerId
        table
      traverse_ ($ ret) onReturn
    (HaveFinish _ onReturn) -> do
      M.delete answerId table
      traverse_ ($ ret) onReturn
    (HaveReturn _ _) ->
      abortConn conn $
        eFailed $
          "Duplicate return message for "
            <> tableName
            <> " #"
            <> fromString (show answerId)

updateQAFinish :: Conn' -> M.Map QAId EntryQA -> Text -> R.Parsed R.Finish -> STM ()
updateQAFinish conn table tableName finish@R.Finish {questionId} =
  lookupAbort tableName conn table (QAId questionId) $ \case
    (NewQA onFinish onReturn) -> do
      traverse_ ($ finish) onFinish
      M.insert
        (HaveFinish finish onReturn)
        (QAId questionId)
        table
    (HaveReturn _ onFinish) -> do
      traverse_ ($ finish) onFinish
      M.delete (QAId questionId) table
    (HaveFinish _ _) ->
      abortConn conn $
        eFailed $
          "Duplicate finish message for "
            <> tableName
            <> " #"
            <> fromString (show questionId)

-- | Update an entry in the questions or answers table to queue the given
-- callback when the return message for that answer comes in. If the return
-- has already arrived, the callback is queued immediately.
--
-- If the entry already has other callbacks registered, this callback is
-- run *after* the others (see Note [callbacks]). Note that this is an
-- important property, as it is necessary to preserve E-order if the
-- callbacks are successive method calls on the returned object.
subscribeReturn :: Text -> Conn' -> M.Map QAId EntryQA -> QAId -> (Return -> STM ()) -> STM ()
subscribeReturn tableName conn table qaId onRet =
  lookupAbort tableName conn table qaId $ \qa -> do
    updated <- go qa
    M.insert updated qaId table
  where
    go = \case
      (NewQA onFinish onReturn) ->
        pure
          (NewQA onFinish (SnocList.snoc onReturn onRet))
      (HaveFinish finishMsg onReturn) ->
        pure
          (HaveFinish finishMsg (SnocList.snoc onReturn onRet))
      val@(HaveReturn returnMsg _) -> do
        onRet returnMsg
        pure val

-- | Abort the connection, sending an abort message. This is only safe to call
-- from within either the thread running the receieve loop or the callback loop.
abortConn :: Conn' -> R.Parsed R.Exception -> STM a
abortConn _ e = throwSTM (SentAbort e)

-- | Gets the live connection state, or throws disconnected if it is not live.
getLive :: Conn -> STM Conn'
getLive Conn {liveState} =
  readTVar liveState >>= \case
    Live conn' -> pure conn'
    Dead -> throwSTM eDisconnected

-- | Performs an action with the live connection state. Does nothing if the
-- connection is dead.
whenLive :: Conn -> (Conn' -> STM ()) -> STM ()
whenLive Conn {liveState} f =
  readTVar liveState >>= \case
    Live conn' -> f conn'
    Dead -> pure ()

-- | Attempt to unwrap a client, to get at an underlying value from the
-- server. Returns 'Nothing' on failure.
--
-- This shells out to the underlying server's implementation of
-- 'Server.unwrap'. It will fail with 'Nothing' if any of these are true:
--
-- * The client is a promise.
-- * The client points to an object in a remote vat.
-- * The underlying Server's 'unwrap' method returns 'Nothing' for type 'a'.
unwrapServer :: (IsClient c, Typeable a) => c -> Maybe a
unwrapServer c = case unwrapClient (toClient c) of
  Just (LocalClient _ _ _ unwrapper) -> unwrapper
  _ -> Nothing

-- | Wait for the client to be fully resolved, and then return a client
-- pointing directly to the destination.
--
-- If the argument is null, a local client, or a (permanent) remote client,
-- this returns the argument immediately. If the argument is a promise client,
-- then this waits for the promise to resolve and returns the result of
-- the resolution. If the promise resolves to *another* promise, then this waits
-- for that promise to also resolve.
--
-- If the promise is rejected, then this throws the corresponding exception.
clientExportMap :: Client' -> STM ExportMap
clientExportMap (LocalClient exportMap _ _ _) = pure exportMap
clientExportMap (PromiseClient _ exportMap _) = pure exportMap
clientExportMap (ImportClient cell) = do
  ImportRef {proxies} <- Fin.readCell cell
  pure proxies

-- | insert the client into the exports table, bumping the refcount if it is
-- already there. If a different client is already in the table at the same
-- id, call 'error'.
addBumpExport :: IEId -> Client' -> M.Map IEId EntryE -> STM ()
addBumpExport exportId client =
  M.focus (Focus.alter go) exportId
  where
    go Nothing = Just EntryE {client, refCount = 1}
    go (Just EntryE {client = oldClient, refCount})
      | client /= oldClient =
          error $
            "BUG: addExportRef called with a client that is different "
              ++ "from what is already in our exports table."
      | otherwise =
          Just EntryE {client, refCount = refCount + 1}
