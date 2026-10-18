{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Internal.Rpc.Client (walkPipelinePtr, pipelineClient, waitPipeline, promisedAnswerClient, call, makeLocalPipeline, callRemote, cbCallReturn, newPromiseClient, unwrapServer, export, genSendableCapTableRaw, makeOutgoingPayload, requestBootstrap, resolveClientExn, resolveClientPtr, resolveClientClient, disembargo, resolveClientReturn, getConnExport, dropConnExport, clientExportMap, addBumpExport, emitCap, acceptPayload, acceptCap, newImport, releaseImport, newLocalAnswerClient, module Internal.Rpc.Types, Client, Pipeline, nullClient, waitConn, handleConn, withConn, acquireConn, newConn, runConn, stopConn, callbacksLoop, sendLoop) where

import Capnp.Accessors (parseField, readField)
import Capnp.Fields (Which)
import qualified Capnp.Gen.Capnp.Rpc as R
import qualified Capnp.Message as Message
  ( getCapTable
  , withCapTable
  )
import Capnp.Mutability (Mutability (Const))
import Capnp.Repr (Raw (..))
import Capnp.Rpc.Errors
  ( eDisconnected
  , eFailed
  , eMethodUnimplemented
  , eUnimplemented
  , wrapException
  )
import Capnp.Rpc.Promise
  ( Fulfiller
  , Promise
  , breakOrFulfill
  , breakPromise
  , fulfill
  , newCallback
  , newPromise
  , newReadyPromise
  )
import qualified Capnp.Rpc.Server as Server
  ( CallInfo (..)
  , ServerOps (handleCast, handleStop)
  , runServer
  )
import Capnp.TraversalLimit (LimitT, defaultLimit, evalLimitT)
import qualified Capnp.Untyped as UntypedRaw
  ( HasMessage (message)
  , Ptr (PtrCap)
  , TraverseMsg (tMsg)
  , getClient
  )
import Control.Concurrent.STM
  ( STM
  , TVar
  , atomically
  , catchSTM
  , flushTQueue
  , newTQueue
  , newTVar
  , readTVar
  , retry
  , writeTQueue
  , writeTVar
  )
import Control.Exception.Safe (finally, throwM, try)
import Control.Monad (when)
import Control.Monad.STM.Class (MonadSTM (..))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Default (Default (def))
import Data.Foldable (Foldable (toList), traverse_)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import qualified Data.Vector as V (fromList, toList)
import Data.Word (Word16)
import qualified Internal.Rc as Rc
  ( decr
  , get
  , incr
  , new
  , release
  )
import Internal.Rpc.Breaker (Client, Pipeline, nullClient)
import Internal.Rpc.Connection
  ( acquireConn
  , callbacksLoop
  , handleConn
  , newConn
  , runConn
  , sendLoop
  , stopConn
  , waitConn
  , withConn
  )
import Internal.Rpc.Foundation
  ( abortConn
  , addBumpExport
  , clientExportMap
  , finishQuestion
  , followPtrs
  , freeExport
  , getLive
  , lookupAbort
  , marshalMsgTarget
  , marshalPromisedAnswer
  , newEmbargo
  , newExport
  , newQuestion
  , ptrPathClient
  , queueIO
  , queueSTM
  , sendPureMsg
  , subscribeReturn
  , unmarshalPromisedAnswer
  , unwrapServer
  , whenLive
  )
import Internal.Rpc.Protocol (releaseExport, sendCall)
import Internal.Rpc.Types
  ( Call (..)
  , Client' (..)
  , Conn (..)
  , Conn' (..)
  , ConnConfig (..)
  , EmbargoId (..)
  , EntryE (..)
  , EntryI (..)
  , EntryQA (..)
  , ExportMap (..)
  , IEId (..)
  , IdPool (..)
  , ImportRef (..)
  , IsClient (..)
  , LiveState (..)
  , LocalDest (..)
  , MsgTarget (..)
  , Payload (..)
  , Pipeline' (..)
  , PipelineState (..)
  , PromiseState (..)
  , PromisedAnswer (..)
  , QAId (..)
  , RawMPtr
  , RemoteDest (..)
  , Return (..)
  , Return' (..)
  , RpcError (..)
  , TmpDest (..)
  , embargoWord
  , freeId
  , ieWord
  , makeAbortExn
  , newId
  , newIdPool
  , qaWord
  , unwrapClient
  , unwrapPipeline
  , wrapClient
  , wrapPipeline
  )
import qualified Internal.SnocList as SnocList
  ( empty
  , fromList
  , singleton
  , snoc
  )
import qualified Internal.TCloseQ as TCloseQ (close, new, write)
import qualified Lifetimes.Gc as Fin
  ( Cell
  , addFinalizer
  , newCell
  , readCell
  )
import qualified StmContainers.Map as M
  ( delete
  , insert
  , lookup
  , new
  )
import Supervisors (Supervisor, superviseSTM)

walkPipelinePtr :: Pipeline -> Word16 -> Pipeline
walkPipelinePtr (unwrapPipeline -> p@Pipeline' {steps}) step =
  wrapPipeline $ p {steps = SnocList.snoc steps step}

-- | Convert a 'Pipeline' into a 'Client', which can be used to send messages to the
-- referant of the 'Pipeline', using promise pipelining.
pipelineClient :: (MonadSTM m) => Pipeline -> m Client
pipelineClient (unwrapPipeline -> Pipeline' {state, steps}) = liftSTM $ do
  readTVar state >>= \case
    (PendingRemotePipeline answerId clientMap conn) -> do
      maybeClient <- M.lookup steps clientMap
      case maybeClient of
        Nothing -> do
          client <-
            promisedAnswerClient
              conn
              PromisedAnswer {answerId, transform = steps}
          M.insert client steps clientMap
          pure client
        Just client ->
          pure client
    PendingLocalPipeline subscribers -> do
      (ret, retFulfiller) <- newPromiseClient
      ptrFulfiller <- newCallback $ \r -> do
        writeTVar state (ReadyPipeline r)
        case r of
          Left e ->
            breakPromise retFulfiller e
          Right v ->
            (ptrPathClient (toList steps) v >>= fulfill retFulfiller)
              `catchSTM` (breakPromise retFulfiller . wrapException False)
      writeTVar state $ PendingLocalPipeline $ SnocList.snoc subscribers ptrFulfiller
      pure ret
    ReadyPipeline r -> do
      -- TODO(cleanup): factor out the commonalities between this and the above case.
      (p, f) <- newPromiseClient
      case r of
        Left e -> breakPromise f e >> pure p
        Right v ->
          ptrPathClient (toList steps) v
            `catchSTM` ( \e -> do
                          breakPromise f (wrapException False e)
                          pure p
                       )

-- | Wait for the pipeline's target to resolve, and return the corresponding
-- pointer.
waitPipeline :: (MonadSTM m) => Pipeline -> m RawMPtr
waitPipeline (unwrapPipeline -> Pipeline' {state, steps}) = liftSTM $ do
  s <- readTVar state
  case s of
    ReadyPipeline (Left e) ->
      throwM e
    ReadyPipeline (Right v) ->
      evalLimitT defaultLimit $ followPtrs (toList steps) v
    _ ->
      retry

promisedAnswerClient :: Conn -> PromisedAnswer -> STM Client
promisedAnswerClient conn answer@PromisedAnswer {answerId, transform} = do
  let tmpDest = RemoteDest (AnswerDest conn answer)
  pState <- newTVar (Pending tmpDest)
  exportMap <- ExportMap <$> M.new
  let client =
        wrapClient $
          Just
            (PromiseClient pState exportMap tmpDest)
  readTVar (liveState conn) >>= \case
    Dead ->
      resolveClientExn tmpDest (writeTVar pState) eDisconnected
    Live conn'@Conn' {questions} ->
      subscribeReturn "questions" conn' questions answerId $
        resolveClientReturn tmpDest (writeTVar pState) conn' (toList transform)
  pure client

call :: (MonadSTM m) => Server.CallInfo -> Client -> m (Promise Pipeline)
call Server.CallInfo {response} (unwrapClient -> Nothing) = liftSTM $ do
  breakPromise response eMethodUnimplemented
  state <- newTVar $ ReadyPipeline (Left eMethodUnimplemented)
  newReadyPromise $ wrapPipeline Pipeline' {state, steps = mempty}
call info@Server.CallInfo {response} (unwrapClient -> Just client') = liftSTM $ do
  (localPipeline, response') <- makeLocalPipeline response
  let info' = info {Server.response = response'}
  case client' of
    (LocalClient _ qCall _ _) -> do
      Rc.get qCall >>= \case
        Just q -> do
          q info'
        Nothing ->
          breakPromise response' eDisconnected
      newReadyPromise localPipeline
    (PromiseClient pState _ _) ->
      readTVar pState >>= \case
        (Ready target) ->
          call info target
        (Embargo callBuffer) -> do
          writeTQueue callBuffer info'
          newReadyPromise localPipeline
        (Pending tmpDest) -> case tmpDest of
          LocalDest LocalBuffer {callBuffer} -> do
            writeTQueue callBuffer info'
            newReadyPromise localPipeline
          RemoteDest (AnswerDest conn answer) ->
            callRemote conn info $ AnswerTgt answer
          RemoteDest (ImportDest cell) -> do
            ImportRef {conn, importId} <- Fin.readCell cell
            callRemote conn info $ ImportTgt importId
        Error exn -> do
          breakPromise response' exn
          newReadyPromise localPipeline
    ImportClient cell -> do
      ImportRef {conn, importId} <- Fin.readCell cell
      callRemote conn info (ImportTgt importId)

makeLocalPipeline :: Fulfiller RawMPtr -> STM (Pipeline, Fulfiller RawMPtr)
makeLocalPipeline f = do
  state <- newTVar $ PendingLocalPipeline mempty
  f' <- newCallback $ \r -> do
    s <- readTVar state
    case s of
      PendingLocalPipeline fs -> do
        writeTVar state (ReadyPipeline r)
        breakOrFulfill f r
        traverse_ (`breakOrFulfill` r) fs
      _ ->
        -- TODO(cleanup): refactor so we don't need this case.
        error "impossible"
  pure (wrapPipeline Pipeline' {state, steps = mempty}, f')

-- | Send a call to a remote capability.
callRemote :: Conn -> Server.CallInfo -> MsgTarget -> STM (Promise Pipeline)
callRemote
  conn
  Server.CallInfo {interfaceId, methodId, arguments, response}
  target = do
    conn'@Conn' {questions} <- getLive conn
    qid <- newQuestion conn'
    payload@Payload {capTable} <- makeOutgoingPayload conn arguments
    -- save these in case the callee sends back releaseParamCaps = True in the return
    -- message:
    let paramCaps = catMaybes $ flip map capTable $ \R.CapDescriptor {union'} -> case union' of
          R.CapDescriptor'senderHosted eid -> Just (IEId eid)
          R.CapDescriptor'senderPromise eid -> Just (IEId eid)
          _ -> Nothing

    clientMap <- M.new
    rp <-
      newTVar
        (PendingRemotePipeline qid clientMap conn)

    response' <- newCallback $ \r -> do
      breakOrFulfill response r
      case r of
        Left e -> writeTVar rp $ ReadyPipeline (Left e)
        Right v ->
          writeTVar rp $ ReadyPipeline (Right v)

    M.insert
      (NewQA SnocList.empty (SnocList.singleton $ cbCallReturn paramCaps conn response'))
      qid
      questions
    (p, sentFulfiller) <- newPromise
    f <- newCallback $ \r ->
      breakOrFulfill sentFulfiller (wrapPipeline Pipeline' {state = rp, steps = mempty} <$ r)
    sendCall
      conn'
      Call
        { questionId = qid
        , target = target
        , params = payload
        , interfaceId
        , methodId
        }
      f
    pure p

-- | Callback to run when a return comes in that corresponds to a call
-- we sent. Registered in callRemote. The first argument is a list of
-- export IDs to release if the return message has releaseParamCaps = true.
cbCallReturn :: [IEId] -> Conn -> Fulfiller RawMPtr -> Return -> STM ()
cbCallReturn
  paramCaps
  conn
  response
  Return {answerId, union', releaseParamCaps} = do
    conn'@Conn' {answers} <- getLive conn
    when releaseParamCaps $
      traverse_ (releaseExport conn 1) paramCaps
    case union' of
      Return'exception exn ->
        breakPromise response exn
      Return'results Payload {content} ->
        fulfill response content
      Return'canceled ->
        breakPromise response $ eFailed "Canceled"
      Return'resultsSentElsewhere ->
        -- This should never happen, since we always set
        -- sendResultsTo = caller
        abortConn conn' $
          eFailed $
            mconcat
              [ "Received Return.resultsSentElswhere for a call "
              , "with sendResultsTo = caller."
              ]
      Return'takeFromOtherQuestion qid ->
        -- TODO(cleanup): we should be a little stricter; the protocol
        -- requires that (1) each answer is only used this way once, and
        -- (2) The question was sent with sendResultsTo set to 'yourself',
        -- but we don't enforce either of these requirements.
        subscribeReturn "answer" conn' answers qid $
          cbCallReturn [] conn response
      Return'acceptFromThirdParty _ ->
        -- Note [Level 3]
        abortConn conn' $
          eUnimplemented
            "This vat does not support level 3."
    -- Defer this until after any other callbacks run, in case disembargos
    -- need to be sent due to promise resolutions that we triggered:
    queueSTM conn' $
      finishQuestion
        conn'
        def
          { R.questionId = qaWord answerId
          , R.releaseResultCaps = False
          }

newPromiseClient :: (MonadSTM m, IsClient c) => m (c, Fulfiller c)
newPromiseClient = liftSTM $ do
  callBuffer <- newTQueue
  let tmpDest = LocalDest LocalBuffer {callBuffer}
  pState <- newTVar (Pending tmpDest)
  exportMap <- ExportMap <$> M.new
  f <- newCallback $ \case
    Left e -> resolveClientExn tmpDest (writeTVar pState) e
    Right v -> resolveClientClient tmpDest (writeTVar pState) (toClient v)
  let p =
        wrapClient $
          Just (PromiseClient pState exportMap tmpDest)
  pure (fromClient p, f)

-- | Spawn a local server with its lifetime bound to the supervisor,
-- and return a client for it. When the client is garbage collected,
-- the server will be stopped (if it is still running).
export :: (MonadSTM m) => Supervisor -> Server.ServerOps -> m Client
export sup ops = liftSTM $ do
  q <- TCloseQ.new
  qCall <- Rc.new (TCloseQ.write q) (TCloseQ.close q)
  exportMap <- ExportMap <$> M.new
  finalizerKey <- Fin.newCell ()
  let client' =
        LocalClient exportMap qCall finalizerKey (Server.handleCast ops)
  superviseSTM
    sup
    ( ( do
          Fin.addFinalizer finalizerKey $ atomically $ Rc.release qCall
          Server.runServer q ops
      )
        `finally` Server.handleStop ops
    )
  pure $ wrapClient (Just client')

-- | Generate a cap table describing the capabilities reachable from the given
-- pointer. The capability table will be correct for any message where all of
-- the capabilities are within the subtree under the pointer.
genSendableCapTableRaw
  :: Conn
  -> Maybe (UntypedRaw.Ptr 'Const)
  -> STM [R.Parsed R.CapDescriptor]
genSendableCapTableRaw _ Nothing = pure []
genSendableCapTableRaw conn (Just ptr) =
  traverse
    ( \c -> do
        union' <- emitCap conn c
        pure (R.CapDescriptor 0 union')
    )
    (V.toList $ Message.getCapTable (UntypedRaw.message @UntypedRaw.Ptr ptr))

-- | Convert the pointer into a Payload, including a capability table for
-- the clients in the pointer's cap table.
makeOutgoingPayload :: Conn -> RawMPtr -> STM Payload
makeOutgoingPayload conn content = do
  capTable <- genSendableCapTableRaw conn content
  pure Payload {content, capTable}

-- | Request the remote vat's bootstrap interface.
requestBootstrap :: (MonadSTM m) => Conn -> m Client
requestBootstrap conn@Conn {liveState} =
  liftSTM $
    readTVar liveState >>= \case
      Dead ->
        pure nullClient
      Live conn'@Conn' {questions} -> do
        qid <- newQuestion conn'
        let tmpDest =
              RemoteDest
                ( AnswerDest
                    conn
                    ( PromisedAnswer
                        { answerId = qid
                        , transform = SnocList.empty
                        }
                    )
                )
        pState <- newTVar (Pending tmpDest)

        -- Arguably, we should wait for this promise, since it's analagous
        -- to a call in terms of operation, but we only send one of these
        -- per connection, so whatever.
        (_, onSent) <- newPromise
        sendPureMsg
          conn'
          (R.Message'bootstrap (R.Bootstrap (qaWord qid) Nothing))
          onSent

        M.insert
          ( NewQA
              SnocList.empty
              ( SnocList.fromList
                  [ resolveClientReturn tmpDest (writeTVar pState) conn' []
                  , \_ ->
                      finishQuestion
                        conn'
                        R.Finish
                          { questionId = qaWord qid
                          , releaseResultCaps = False
                          }
                  ]
              )
          )
          qid
          questions
        exportMap <- ExportMap <$> M.new
        pure $
          wrapClient $
            Just
              (PromiseClient pState exportMap tmpDest)

-- Note [resolveClient]
-- ====================
--
-- There are several functions resolveClient*, each of which resolves a
-- 'PromiseClient', which will previously have been in the 'Pending' state.
-- Each function accepts three parameters: the 'TmpDest' that the
-- pending promise had been targeting, a function for setting the new state,
-- and a thing to resolve the promise to. The type of the latter is specific
-- to each function.

-- | Resolve a promised client to an exception. See Note [resolveClient]
resolveClientExn :: TmpDest -> (PromiseState -> STM ()) -> R.Parsed R.Exception -> STM ()
resolveClientExn tmpDest resolve exn = do
  case tmpDest of
    LocalDest LocalBuffer {callBuffer} -> do
      calls <- flushTQueue callBuffer
      traverse_
        ( \Server.CallInfo {response} ->
            breakPromise response exn
        )
        calls
    RemoteDest (AnswerDest _ _) ->
      pure ()
    RemoteDest (ImportDest _) ->
      pure ()
  resolve $ Error exn

-- Resolve a promised client to a pointer. If it is a non-null non-capability
-- pointer, it resolves to an exception. See Note [resolveClient]
resolveClientPtr :: TmpDest -> (PromiseState -> STM ()) -> RawMPtr -> STM ()
resolveClientPtr tmpDest resolve ptr = case ptr of
  Nothing ->
    resolveClientClient tmpDest resolve nullClient
  Just (UntypedRaw.PtrCap c) -> do
    c' <- evalLimitT defaultLimit $ UntypedRaw.getClient c
    resolveClientClient tmpDest resolve c'
  Just _ ->
    resolveClientExn tmpDest resolve $
      eFailed "Promise resolved to non-capability pointer"

-- | Resolve a promised client to another client. See Note [resolveClient]
resolveClientClient :: TmpDest -> (PromiseState -> STM ()) -> Client -> STM ()
resolveClientClient tmpDest resolve (unwrapClient -> client) =
  case (client, tmpDest) of
    -- Remote resolved to local; we need to embargo:
    (Just (LocalClient {}), RemoteDest dest) ->
      disembargoAndResolve dest
    (Just (PromiseClient _ _ (LocalDest _)), RemoteDest dest) ->
      disembargoAndResolve dest
    (Nothing, RemoteDest _) ->
      -- If it resolves to a null client, then we can't send a disembargo.
      -- Note that this may result in futrther calls throwing exceptions
      -- /before/ the outstanding calls, which is a bit weird. But all
      -- calls will throw at some point, so it's probably fine.
      resolveNow
    -- Local promises never need embargos; we can just forward:
    (_, LocalDest LocalBuffer {callBuffer}) ->
      flushAndResolve callBuffer
    -- These cases are slightly subtle; despite resolving to a
    -- client that points at a "remote" target, if it points into a
    -- _different_ connection, we must be proxying it, so we treat
    -- it as local and do a disembargo like above. We may need to
    -- change this when we implement level 3, since third-party
    -- handoff is a possibility; see Note [Level 3].
    --
    -- If it's pointing into the same connection, we don't need to
    -- do a disembargo.
    (Just (PromiseClient _ _ (RemoteDest newDest)), RemoteDest oldDest) -> do
      destinationConn <- destConn newDest
      oldConn <- destConn oldDest
      if destinationConn == oldConn
        then resolveNow
        else disembargoAndResolve oldDest
    (Just (ImportClient cell), RemoteDest oldDest) -> do
      ImportRef {conn = destinationConn} <- Fin.readCell cell
      oldConn <- destConn oldDest
      if destinationConn == oldConn
        then resolveNow
        else disembargoAndResolve oldDest
  where
    destConn (AnswerDest conn _) = pure conn
    destConn (ImportDest cell) = do
      ImportRef {conn} <- Fin.readCell cell
      pure conn
    destTarget (AnswerDest _ answer) = pure $ AnswerTgt answer
    destTarget (ImportDest cell) = do
      ImportRef {importId} <- Fin.readCell cell
      pure $ ImportTgt importId

    resolveNow = do
      resolve $ Ready (wrapClient client)

    -- Flush the call buffer into the client's queue, and then pass the client
    -- to resolve.
    flushAndResolve callBuffer = do
      flushTQueue callBuffer >>= traverse_ (`call` wrapClient client)
      resolve $ Ready (wrapClient client)
    flushAndRaise callBuffer e =
      flushTQueue callBuffer
        >>= traverse_ (\Server.CallInfo {response} -> breakPromise response e)
    disembargoAndResolve dest = do
      Conn {liveState} <- destConn dest
      readTVar liveState >>= \case
        Live conn' -> do
          callBuffer <- newTQueue
          target <- destTarget dest
          disembargo conn' target $ \case
            Right () ->
              flushAndResolve callBuffer
            Left e ->
              flushAndRaise callBuffer e
          resolve (Embargo callBuffer)
        Dead ->
          resolveClientExn tmpDest resolve eDisconnected

-- | Send a (senderLoopback) disembargo to the given message target, and
-- register the transaction to run when the corresponding receiverLoopback
-- message is received.
--
-- The callback may be handed a 'Left' with a disconnected exception if
-- the connection is dropped before the disembargo is echoed.
disembargo :: Conn' -> MsgTarget -> (Either (R.Parsed R.Exception) () -> STM ()) -> STM ()
disembargo conn@Conn' {embargos} tgt onEcho = do
  callback <- newCallback onEcho
  eid <- newEmbargo conn
  M.insert callback eid embargos
  (_, onSent) <- newPromise
  sendPureMsg
    conn
    ( R.Message'disembargo
        R.Disembargo
          { target = marshalMsgTarget tgt
          , context =
              R.Disembargo'context' $
                R.Disembargo'context'senderLoopback (embargoWord eid)
          }
    )
    onSent

-- | Resolve a promised client to the result of a return. See Note [resolveClient]
--
-- The [Word16] is a list of pointer indexes to follow from the result.
resolveClientReturn :: TmpDest -> (PromiseState -> STM ()) -> Conn' -> [Word16] -> Return -> STM ()
resolveClientReturn tmpDest resolve conn@Conn' {answers} transform Return {union'} = case union' of
  -- TODO(cleanup) there is a lot of redundency betwen this and cbCallReturn; can
  -- we refactor?
  Return'exception exn ->
    resolveClientExn tmpDest resolve exn
  Return'results Payload {content} -> do
    res <- try $ evalLimitT defaultLimit $ followPtrs transform content
    case res of
      Right v ->
        resolveClientPtr tmpDest resolve v
      Left e ->
        resolveClientExn tmpDest resolve e
  Return'canceled ->
    resolveClientExn tmpDest resolve $ eFailed "Canceled"
  Return'resultsSentElsewhere ->
    -- Should never happen; we don't set sendResultsTo to anything other than
    -- caller.
    abortConn conn $
      eFailed $
        mconcat
          [ "Received Return.resultsSentElsewhere for a call "
          , "with sendResultsTo = caller."
          ]
  Return'takeFromOtherQuestion qid ->
    subscribeReturn "answer" conn answers qid $
      resolveClientReturn tmpDest resolve conn transform
  Return'acceptFromThirdParty _ ->
    -- Note [Level 3]
    abortConn conn $
      eUnimplemented
        "This vat does not support level 3."

-- | Get the client's export ID for this connection, or allocate a new one if needed.
-- If this is the first time this client has been exported on this connection,
-- bump the refcount.
getConnExport :: Conn -> Client' -> STM IEId
getConnExport conn client =
  getLive conn >>= \conn'@Conn' {exports} -> do
    ExportMap m <- clientExportMap client
    val <- M.lookup conn m
    case val of
      Just eid -> do
        addBumpExport eid client exports
        pure eid
      Nothing -> do
        eid <- newExport conn'
        addBumpExport eid client exports
        M.insert eid conn m
        pure eid

-- | Remove export of the client on the connection. This entails removing it
-- from the export id, removing the connection from the client's ExportMap,
-- freeing the export id, and dropping the client's refcount.
dropConnExport :: Conn -> Client' -> STM ()
dropConnExport conn client' = do
  ExportMap eMap <- clientExportMap client'
  val <- M.lookup conn eMap
  case val of
    Just eid -> do
      M.delete conn eMap
      whenLive conn $ \conn'@Conn' {exports} -> do
        M.delete eid exports
        freeExport conn' eid
    Nothing ->
      -- The export is already gone (e.g. the peer already released
      -- it via a Finish with releaseResultCaps=True, or stopConn is
      -- dropping a bootstrap that the peer never imported). The
      -- post-condition "this client is not exported on this conn"
      -- already holds, so this is a no-op rather than a bug.
      pure ()

-- | Generate a CapDescriptor', which we can send to the connection's remote
-- vat to identify client. In the process, this may allocate export ids, update
-- reference counts, and so forth.
emitCap :: Conn -> Client -> STM (R.Parsed (Which R.CapDescriptor))
emitCap _targetConn (unwrapClient -> Nothing) =
  pure R.CapDescriptor'none
emitCap targetConn (unwrapClient -> Just client') = case client' of
  (LocalClient {}) ->
    R.CapDescriptor'senderHosted . ieWord <$> getConnExport targetConn client'
  (PromiseClient pState _ _) ->
    readTVar pState >>= \case
      (Pending (RemoteDest (AnswerDest conn answer)))
        | conn == targetConn ->
            pure $ R.CapDescriptor'receiverAnswer (marshalPromisedAnswer answer)
      (Pending (RemoteDest (ImportDest cell))) -> do
        ImportRef {conn, importId = IEId iid} <- Fin.readCell cell
        if conn == targetConn
          then pure (R.CapDescriptor'receiverHosted iid)
          else newSenderPromise
      _ ->
        newSenderPromise
  ImportClient cell -> do
    ImportRef {conn = hostConn, importId} <- Fin.readCell cell
    if hostConn == targetConn
      then pure (R.CapDescriptor'receiverHosted (ieWord importId))
      else R.CapDescriptor'senderHosted . ieWord <$> getConnExport targetConn client'
  where
    newSenderPromise = R.CapDescriptor'senderPromise . ieWord <$> getConnExport targetConn client'

acceptPayload :: Conn -> Raw R.Payload 'Const -> LimitT STM Payload
acceptPayload conn payload = do
  capTable <- parseField #capTable payload
  clients <- lift $ V.fromList <$> traverse (\R.CapDescriptor {union'} -> acceptCap conn union') capTable
  Raw rawContent <- readField #content payload
  content <- traverse (UntypedRaw.tMsg (pure . Message.withCapTable clients)) rawContent
  pure Payload {content, capTable}

-- | 'acceptCap' is a dual of 'emitCap'; it derives a Client from a CapDescriptor'
-- received via the connection. May update connection state as necessary.
acceptCap :: Conn -> R.Parsed (Which R.CapDescriptor) -> STM Client
acceptCap conn cap = getLive conn >>= \conn' -> go conn' cap
  where
    go _ R.CapDescriptor'none = pure (wrapClient Nothing)
    go conn'@Conn' {imports} (R.CapDescriptor'senderHosted (IEId -> importId)) = do
      entry <- M.lookup importId imports
      case entry of
        Just EntryI {promiseState = Just _} ->
          let imp = fromString (show importId)
           in abortConn conn' $
                eFailed $
                  "received senderHosted capability #"
                    <> imp
                    <> ", but the imports table says #"
                    <> imp
                    <> " is senderPromise."
        Just ent@EntryI {localRc, remoteRc, proxies} -> do
          Rc.incr localRc
          M.insert ent {localRc, remoteRc = remoteRc + 1} importId imports
          cell <-
            Fin.newCell
              ImportRef
                { conn
                , importId
                , proxies
                }
          queueIO conn' $ Fin.addFinalizer cell $ atomically $ Rc.decr localRc
          pure $ wrapClient $ Just $ ImportClient cell
        Nothing ->
          wrapClient . Just . ImportClient <$> newImport importId conn Nothing
    go conn'@Conn' {imports} (R.CapDescriptor'senderPromise (IEId -> importId)) = do
      entry <- M.lookup importId imports
      case entry of
        Just EntryI {promiseState = Nothing} ->
          let imp = fromString (show importId)
           in abortConn conn' $
                eFailed $
                  "received senderPromise capability #"
                    <> imp
                    <> ", but the imports table says #"
                    <> imp
                    <> " is senderHosted."
        Just ent@EntryI {remoteRc, proxies, promiseState = Just (pState, origTarget)} -> do
          M.insert ent {remoteRc = remoteRc + 1} importId imports
          pure $
            wrapClient $
              Just
                (PromiseClient pState proxies origTarget)
        Nothing -> do
          rec imp <- newImport importId conn (Just (pState, tmpDest))
              ImportRef {proxies} <- Fin.readCell imp
              let tmpDest = RemoteDest (ImportDest imp)
              pState <- newTVar (Pending tmpDest)
          pure $
            wrapClient $
              Just
                (PromiseClient pState proxies tmpDest)
    go conn'@Conn' {exports} (R.CapDescriptor'receiverHosted exportId) =
      lookupAbort "export" conn' exports (IEId exportId) $
        \EntryE {client} ->
          pure $ wrapClient $ Just client
    go conn' (R.CapDescriptor'receiverAnswer encodedAnswer) = do
      pa <- unmarshalPromisedAnswer encodedAnswer `catchSTM` abortConn conn'
      newLocalAnswerClient conn' pa
    go conn' (R.CapDescriptor'thirdPartyHosted _) =
      -- Note [Level 3]
      abortConn conn' $
        eUnimplemented
          "thirdPartyHosted unimplemented; level 3 is not supported."
    go conn' (R.CapDescriptor'unknown' tag) =
      abortConn conn' $
        eUnimplemented $
          "Unimplemented CapDescriptor variant #" <> fromString (show tag)

-- | Create a new entry in the imports table, with the given import id and
-- 'promiseState', and return a corresponding ImportRef. When the ImportRef is
-- garbage collected, the refcount in the table will be decremented.
newImport :: IEId -> Conn -> Maybe (TVar PromiseState, TmpDest) -> STM (Fin.Cell ImportRef)
newImport importId conn promiseState =
  getLive conn >>= \conn'@Conn' {imports} -> do
    localRc <- Rc.new () $ releaseImport importId conn'
    proxies <- ExportMap <$> M.new
    let importRef =
          ImportRef
            { conn
            , importId
            , proxies
            }
    M.insert
      EntryI
        { localRc
        , remoteRc = 1
        , proxies
        , promiseState
        }
      importId
      imports
    cell <- Fin.newCell importRef
    queueIO conn' $ Fin.addFinalizer cell $ atomically $ Rc.decr localRc
    pure cell

-- | Release the identified import. Removes it from the table and sends a release
-- message with the correct count.
releaseImport :: IEId -> Conn' -> STM ()
releaseImport importId conn'@Conn' {imports} = do
  (_, onSent) <- newPromise
  lookupAbort "imports" conn' imports importId $ \EntryI {remoteRc} ->
    sendPureMsg
      conn'
      ( R.Message'release
          R.Release
            { id = ieWord importId
            , referenceCount = remoteRc
            }
      )
      onSent
  M.delete importId imports

-- | Create a new client targeting an object in our answers table.
-- Important: in this case the 'PromisedAnswer' refers to a question we
-- have recevied, not sent.
newLocalAnswerClient :: Conn' -> PromisedAnswer -> STM Client
newLocalAnswerClient conn@Conn' {answers} PromisedAnswer {answerId, transform} = do
  callBuffer <- newTQueue
  let tmpDest = LocalDest $ LocalBuffer {callBuffer}
  pState <- newTVar (Pending tmpDest)
  subscribeReturn "answer" conn answers answerId $
    resolveClientReturn
      tmpDest
      (writeTVar pState)
      conn
      (toList transform)
  exportMap <- ExportMap <$> M.new
  pure $
    wrapClient $
      Just
        (PromiseClient pState exportMap tmpDest)

-- Note [Limiting resource usage]
-- ==============================
--
-- N.B. much of this Note is future tense; the library is not yet robust against
-- resource useage attacks.
--
-- We employ various strategies to prevent remote vats from causing excessive
-- resource usage. In particular:
--

-- * We set a maximum size for incoming messages; this is in keeping with how

--   we mitigate these concerns when dealing with plain capnp data (i.e. not
--   rpc).

-- * We set a limit on the total *size* of all messages from the remote vat that

--   are currently being serviced. For example, if a Call message comes in,
--   we note its size, and deduct it from the quota. Once we have sent a return
--   and received a finish for this call, and thus can safely forget about it,
--   we remove it from our answers table, and add its size back to the available
--   quota.
--
-- Still TBD:
--

-- * We should come up with some way of guarding against too many intra-vat calls;

--   depending on the object graph, it may be possible for an attacker to get us
--   to "eat our own tail" so to speak.
--
--   Ideas:
--     * Per-object bounded queues for messages
--     * Global limit on intra-vat calls.
--
--   Right now I(zenhack) am more fond of the former.
--

-- * What should we actually do when limits are exceeded?

--
--   Possible strategies:
--     * Block
--     * Throw an 'overloaded' exception
--     * Some combination of the two; block with a timeout, then throw.
--
--   If we just block, we need to make sure this doesn't hang the vat;
--   we probably need a timeout at some level.
