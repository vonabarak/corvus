{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Internal.Rpc.Protocol (recvLoop, handleAbortMsg, handleUnimplementedMsg, handleBootstrapMsg, handleCallMsg, sendRawMsg, sendCall, sendReturn, acceptReturn, handleReturnMsg, handleFinishMsg, handleResolveMsg, handleReleaseMsg, releaseExport, handleDisembargoMsg, returnAnswer) where

import Capnp.Accessors
  ( encodeField
  , getField
  , parseField
  , readField
  , setField
  , setVariant
  , structWhich
  )
import qualified Capnp.Basics as B (AnyPointer)
import Capnp.Classes (new, newRoot, parse)
import Capnp.Convert (msgToRaw)
import qualified Capnp.Gen.Capnp.Rpc as R
import Capnp.Message (Message)
import qualified Capnp.Message as Message
  ( newMessage
  , totalNumWords
  )
import Capnp.Mutability (Mutability (..), thaw)
import Capnp.Repr (Raw (Raw))
import Capnp.Rpc.Errors (eFailed, eUnimplemented)
import Capnp.Rpc.Promise
  ( Fulfiller
  , fulfill
  , newCallback
  , newPromise
  )
import qualified Capnp.Rpc.Server as Server
  ( CallInfo (CallInfo, arguments, interfaceId, methodId, response)
  )
import Capnp.Rpc.Transport (Transport (recvMsg))
import Capnp.TraversalLimit (LimitT, defaultLimit, evalLimitT)
import qualified Capnp.Untyped as UntypedRaw
  ( HasMessage (message)
  , Ptr (PtrCap)
  , appendCap
  )
import Control.Concurrent.STM
  ( STM
  , atomically
  , catchSTM
  , modifyTVar'
  , readTVar
  , retry
  , throwSTM
  , writeTChan
  , writeTVar
  )
import Control.Monad (forever, void, when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Default (Default (def))
import Data.Foldable (for_)
import Data.Function ((&))
import Data.String (fromString)
import Data.Word (Word32)
import qualified Focus (alterM)
import GHC.Prim (coerce)
import Internal.BuildPure (createPure)
import {-# SOURCE #-} Internal.Rpc.Client
  ( acceptCap
  , acceptPayload
  , call
  , dropConnExport
  , emitCap
  , genSendableCapTableRaw
  , resolveClientClient
  , resolveClientExn
  )
import Internal.Rpc.Foundation
  ( abortConn
  , freeEmbargo
  , getLive
  , insertNewAbort
  , lookupAbort
  , marshalMsgTarget
  , queueSTM
  , sendPureMsg
  , subscribeReturn
  , transformClient
  , updateQAFinish
  , updateQAReturn
  )
import Internal.Rpc.Types
  ( Call (..)
  , Client' (ImportClient, PromiseClient)
  , Conn (Conn, debugMode)
  , Conn'
    ( Conn'
    , answers
    , availableCallWords
    , embargos
    , exports
    , imports
    , myBootstrap
    , questions
    , sendQ
    )
  , EmbargoId (EmbargoId)
  , EntryE (EntryE, client, refCount)
  , EntryI (EntryI, promiseState)
  , EntryQA (HaveReturn, NewQA)
  , IEId (..)
  , ImportRef (ImportRef, conn, importId)
  , Payload (Payload, capTable, content)
  , PromiseState (Ready)
  , QAId (QAId)
  , Return (..)
  , Return'
    ( Return'acceptFromThirdParty
    , Return'canceled
    , Return'exception
    , Return'results
    , Return'resultsSentElsewhere
    , Return'takeFromOtherQuestion
    )
  , RpcError (ReceivedAbort)
  , embargoWord
  , ieWord
  , makeAbortExn
  , qaWord
  , unwrapClient
  , wrapClient
  )
import qualified Internal.SnocList as SnocList (fromList)
import qualified Lifetimes.Gc as Fin (readCell)
import qualified StmContainers.Map as M
  ( delete
  , focus
  , insert
  , lookup
  )

-- | 'recvLoop' processes incoming messages.
recvLoop :: Transport -> Conn -> IO ()
-- The logic here mostly routes messages to other parts of the code that know
-- more about the objects in question; See Note [Organization] for more info.
recvLoop transport conn@Conn {debugMode} = forever $ do
  capnpMsg <- recvMsg transport
  atomically $ do
    flip catchSTM (throwSTM . makeAbortExn debugMode) $ do
      evalLimitT defaultLimit $ do
        rpcMsg <- msgToRaw capnpMsg
        which <- structWhich rpcMsg
        case which of
          R.RW_Message'abort exn ->
            parse exn >>= lift . handleAbortMsg conn
          R.RW_Message'unimplemented oldMsg ->
            parse oldMsg >>= lift . handleUnimplementedMsg conn
          R.RW_Message'bootstrap bs ->
            parse bs >>= lift . handleBootstrapMsg conn
          R.RW_Message'call incomingCall -> do
            handleCallMsg conn incomingCall
          R.RW_Message'return ret -> do
            ret' <- acceptReturn conn ret
            lift $ handleReturnMsg conn ret'
          R.RW_Message'finish finish ->
            parse finish >>= lift . handleFinishMsg conn
          R.RW_Message'resolve res ->
            parse res >>= lift . handleResolveMsg conn
          R.RW_Message'release release ->
            parse release >>= lift . handleReleaseMsg conn
          R.RW_Message'disembargo incomingDisembargo ->
            parse incomingDisembargo >>= lift . handleDisembargoMsg conn
          _ -> do
            msg <- parse rpcMsg
            lift $ do
              (_, onSent) <- newPromise
              conn' <- getLive conn
              sendPureMsg conn' (R.Message'unimplemented msg) onSent

-- Each function handle*Msg handles a message of a particular type;
-- 'recvLoop' dispatches to these.

handleAbortMsg :: Conn -> R.Parsed R.Exception -> STM ()
handleAbortMsg _ exn =
  throwSTM (ReceivedAbort exn)

handleUnimplementedMsg :: Conn -> R.Parsed R.Message -> STM ()
handleUnimplementedMsg conn (R.Message msg) =
  getLive conn >>= \conn' -> case msg of
    R.Message'unimplemented _ ->
      -- If the client itself doesn't handle unimplemented messages, that's
      -- weird, but ultimately their problem.
      pure ()
    R.Message'abort _ ->
      abortConn conn' $
        eFailed $
          "Your vat sent an 'unimplemented' message for an abort message "
            <> "that its remote peer never sent. This is likely a bug in your "
            <> "capnproto library."
    _ ->
      abortConn conn' $
        eFailed "Received unimplemented response for required message."

handleBootstrapMsg :: Conn -> R.Parsed R.Bootstrap -> STM ()
handleBootstrapMsg conn R.Bootstrap {questionId} =
  getLive conn >>= \conn' -> do
    ret <- case myBootstrap conn' of
      Nothing ->
        pure
          Return
            { answerId = QAId questionId
            , releaseParamCaps = True -- Not really meaningful for bootstrap, but...
            , union' =
                Return'exception $
                  eFailed "No bootstrap interface for this connection."
            }
      Just client -> do
        capDesc <- emitCap conn client
        content <- fmap Just $ createPure defaultLimit $ do
          msg <- Message.newMessage Nothing
          UntypedRaw.PtrCap <$> UntypedRaw.appendCap msg client
        pure
          Return
            { answerId = QAId questionId
            , releaseParamCaps = True -- Not really meaningful for bootstrap, but...
            , union' =
                Return'results
                  Payload
                    { content
                    , capTable =
                        [R.CapDescriptor 0 capDesc]
                    }
            }
    M.focus
      (Focus.alterM $ insertBootstrap conn' ret)
      (QAId questionId)
      (answers conn')
    sendReturn conn' ret
  where
    insertBootstrap _ ret Nothing =
      pure $
        Just
          ( HaveReturn
              ret
              ( SnocList.fromList
                  [ \R.Finish {releaseResultCaps} ->
                      case ret of
                        Return
                          { union' =
                            Return'results
                              Payload
                                { capTable = [R.CapDescriptor {union' = R.CapDescriptor'receiverHosted (IEId -> eid)}]
                                }
                          } ->
                            when releaseResultCaps $
                              releaseExport conn 1 eid
                        _ ->
                          pure ()
                  ]
              )
          )
    insertBootstrap conn' _ (Just _) =
      abortConn conn' $ eFailed "Duplicate question ID"

handleCallMsg :: Conn -> Raw R.Call 'Const -> LimitT STM ()
handleCallMsg conn callMsg = do
  conn'@Conn' {exports, answers, availableCallWords} <- lift $ getLive conn
  let capnpMsg = UntypedRaw.message @(Raw R.Call) callMsg

  -- Apply backpressure, by limiting the memory usage of outstanding call
  -- messages.
  msgWords <- Message.totalNumWords capnpMsg
  lift $ do
    available <- readTVar availableCallWords
    when
      (msgWords > available)
      retry
    writeTVar availableCallWords $! available - msgWords

  questionId <- parseField #questionId callMsg
  R.MessageTarget target <- parseField #target callMsg
  interfaceId <- parseField #interfaceId callMsg
  methodId <- parseField #methodId callMsg
  payload <- readField #params callMsg

  Payload {content = callParams, capTable} <- acceptPayload conn payload

  lift $ do
    -- First, add an entry in our answers table:
    insertNewAbort
      "answer"
      conn'
      (QAId questionId)
      ( NewQA
          ( SnocList.fromList
              [ \R.Finish {releaseResultCaps} ->
                  when releaseResultCaps $
                    for_ capTable $ \R.CapDescriptor {union'} -> case union' of
                      R.CapDescriptor'receiverHosted (IEId -> importId) ->
                        releaseExport conn 1 importId
                      _ ->
                        pure ()
              ]
          )
          ( SnocList.fromList
              [ \_ ->
                  modifyTVar' availableCallWords (msgWords +)
              ]
          )
      )
      answers

    -- Set up a callback for when the call is finished, to
    -- send the return message:
    fulfiller <- newCallback $ \case
      Left e ->
        returnAnswer
          conn'
          Return
            { answerId = QAId questionId
            , releaseParamCaps = False
            , union' = Return'exception e
            }
      Right content -> do
        resultCaps <- genSendableCapTableRaw conn content
        returnAnswer
          conn'
          Return
            { answerId = QAId questionId
            , releaseParamCaps = False
            , union' =
                Return'results
                  Payload
                    { content = content
                    , capTable = resultCaps
                    }
            }
    -- Package up the info for the call:
    let callInfo =
          Server.CallInfo
            { interfaceId
            , methodId
            , arguments = callParams
            , response = coerce fulfiller
            }
    -- Finally, figure out where to send it:
    case target of
      R.MessageTarget'importedCap exportId ->
        lookupAbort "export" conn' exports (IEId exportId) $
          \EntryE {client} -> void $ call callInfo $ wrapClient $ Just client
      R.MessageTarget'promisedAnswer R.PromisedAnswer {questionId = targetQid, transform} ->
        let onReturn Return {union', releaseParamCaps} =
              case union' of
                Return'exception _ ->
                  returnAnswer conn' (Return (QAId questionId) releaseParamCaps union')
                Return'canceled ->
                  returnAnswer conn' (Return (QAId questionId) releaseParamCaps union')
                Return'results Payload {content} ->
                  void $ transformClient transform content conn' >>= call callInfo
                Return'resultsSentElsewhere ->
                  -- our implementation should never actually do this, but
                  -- this way we don't have to change this if/when we
                  -- support the feature:
                  abortConn conn' $
                    eFailed $
                      "Tried to call a method on a promised answer that "
                        <> "returned resultsSentElsewhere"
                Return'takeFromOtherQuestion otherQid ->
                  subscribeReturn "answer" conn' answers otherQid onReturn
                Return'acceptFromThirdParty _ ->
                  -- Note [Level 3]
                  error "BUG: our implementation unexpectedly used a level 3 feature"
         in subscribeReturn "answer" conn' answers (QAId targetQid) onReturn
      R.MessageTarget'unknown' ordinal ->
        abortConn conn' $
          eUnimplemented $
            "Unknown MessageTarget ordinal #" <> fromString (show ordinal)

sendRawMsg :: Conn' -> Message 'Const -> Fulfiller () -> STM ()
sendRawMsg conn' msg onSent = writeTChan (sendQ conn') (msg, onSent)

sendCall :: Conn' -> Call -> Fulfiller () -> STM ()
sendCall
  conn'
  Call {questionId, target, interfaceId, methodId, params = Payload {content, capTable}}
  onSent = do
    msg <- createPure defaultLimit $ do
      mcontent <- traverse thaw content
      msg <- case mcontent of
        Just v -> pure $ UntypedRaw.message @UntypedRaw.Ptr v
        Nothing -> Message.newMessage Nothing
      payload <- new @R.Payload () msg
      payload & setField #content (Raw mcontent)
      payload & encodeField #capTable capTable
      outgoingCall <- new @R.Call () msg
      setField #params payload outgoingCall
      outgoingCall & encodeField #questionId (qaWord questionId)
      outgoingCall & encodeField #interfaceId interfaceId
      outgoingCall & encodeField #methodId methodId
      outgoingCall & encodeField #target (marshalMsgTarget target)
      rpcMsg <- newRoot @R.Message () msg
      setVariant #call rpcMsg outgoingCall
      pure msg
    sendRawMsg conn' msg onSent

sendReturn :: Conn' -> Return -> STM ()
sendReturn conn' Return {answerId, releaseParamCaps, union'} = do
  (_, onSent) <- newPromise
  case union' of
    Return'results Payload {content, capTable} -> do
      msg <- createPure defaultLimit $ do
        mcontent <- traverse thaw content
        msg <- case mcontent of
          Just v -> pure $ UntypedRaw.message @UntypedRaw.Ptr v
          Nothing -> Message.newMessage Nothing
        payload <- new @R.Payload () msg
        payload & setField #content (Raw mcontent)
        payload & encodeField #capTable capTable
        ret <- new @R.Return () msg
        setVariant #results ret payload
        ret & encodeField #answerId (qaWord answerId)
        ret & encodeField #releaseParamCaps releaseParamCaps
        rpcMsg <- newRoot @R.Message () msg
        setVariant #return rpcMsg ret
        pure msg
      sendRawMsg conn' msg onSent
    Return'exception exn ->
      sendPureMsg
        conn'
        ( R.Message'return
            R.Return
              { answerId = qaWord answerId
              , releaseParamCaps
              , union' = R.Return'exception exn
              }
        )
        onSent
    Return'canceled ->
      sendPureMsg
        conn'
        ( R.Message'return
            R.Return
              { answerId = qaWord answerId
              , releaseParamCaps
              , union' = R.Return'canceled
              }
        )
        onSent
    Return'resultsSentElsewhere ->
      sendPureMsg
        conn'
        ( R.Message'return
            R.Return
              { answerId = qaWord answerId
              , releaseParamCaps
              , union' = R.Return'resultsSentElsewhere
              }
        )
        onSent
    Return'takeFromOtherQuestion (QAId qid) ->
      sendPureMsg
        conn'
        ( R.Message'return
            R.Return
              { answerId = qaWord answerId
              , releaseParamCaps
              , union' = R.Return'takeFromOtherQuestion qid
              }
        )
        onSent
    Return'acceptFromThirdParty ptr -> do
      msg <- createPure defaultLimit $ do
        mptr <- traverse thaw ptr
        msg <- case mptr of
          Just v -> pure $ UntypedRaw.message @UntypedRaw.Ptr v
          Nothing -> Message.newMessage Nothing
        ret <- new @R.Return () msg
        ret & encodeField #answerId (qaWord answerId)
        ret & encodeField #releaseParamCaps releaseParamCaps
        setVariant #acceptFromThirdParty ret (Raw @(Maybe B.AnyPointer) mptr)
        rpcMsg <- newRoot @R.Message () msg
        setVariant #return rpcMsg ret
        pure msg
      sendRawMsg conn' msg onSent

acceptReturn :: Conn -> Raw R.Return 'Const -> LimitT STM Return
acceptReturn conn ret = do
  let answerId = QAId (getField #answerId ret)
      releaseParamCaps = getField #releaseParamCaps ret
  which <- structWhich ret
  union' <- case which of
    R.RW_Return'results payload ->
      Return'results <$> acceptPayload conn payload
    R.RW_Return'exception exn ->
      Return'exception <$> parse exn
    R.RW_Return'canceled _ ->
      pure Return'canceled
    R.RW_Return'resultsSentElsewhere _ ->
      pure Return'resultsSentElsewhere
    R.RW_Return'takeFromOtherQuestion otherId ->
      Return'takeFromOtherQuestion . QAId <$> parse otherId
    R.RW_Return'acceptFromThirdParty (Raw ptr) ->
      pure $ Return'acceptFromThirdParty ptr
    R.RW_Return'unknown' ordinal ->
      lift $ throwSTM $ eFailed $ "Unknown return variant #" <> fromString (show ordinal)
  pure Return {answerId, releaseParamCaps, union'}

handleReturnMsg :: Conn -> Return -> STM ()
handleReturnMsg conn ret =
  getLive conn >>= \conn'@Conn' {questions} ->
    updateQAReturn conn' questions "question" ret

handleFinishMsg :: Conn -> R.Parsed R.Finish -> STM ()
handleFinishMsg conn finish =
  getLive conn >>= \conn'@Conn' {answers} ->
    updateQAFinish conn' answers "answer" finish

handleResolveMsg :: Conn -> R.Parsed R.Resolve -> STM ()
handleResolveMsg conn R.Resolve {promiseId, union'} =
  getLive conn >>= \conn'@Conn' {imports} -> do
    entry <- M.lookup (IEId promiseId) imports
    case entry of
      Nothing ->
        -- This can happen if we dropped the promise, but the release
        -- message is still in flight when the resolve message is sent.
        case union' of
          R.Resolve'cap R.CapDescriptor {union' = R.CapDescriptor'receiverHosted importId} -> do
            (_, onSent) <- newPromise
            -- Send a release message for the resolved cap, since
            -- we're not going to use it:
            sendPureMsg
              conn'
              ( R.Message'release
                  def
                    { R.id = importId
                    , R.referenceCount = 1
                    }
              )
              onSent
          -- Note [Level 3]: do we need to do something with
          -- thirdPartyHosted here?
          _ -> pure ()
      Just EntryI {promiseState = Nothing} ->
        -- This wasn't a promise! The remote vat has done something wrong;
        -- abort the connection.
        abortConn conn' $
          eFailed $
            mconcat
              [ "Received a resolve message for export id #"
              , fromString (show promiseId)
              , ", but that capability is not a promise!"
              ]
      Just EntryI {promiseState = Just (tvar, tmpDest)} ->
        case union' of
          R.Resolve'cap R.CapDescriptor {union' = cap} -> do
            client <- acceptCap conn cap
            resolveClientClient tmpDest (writeTVar tvar) client
          R.Resolve'exception exn ->
            resolveClientExn tmpDest (writeTVar tvar) exn
          R.Resolve'unknown' tag ->
            abortConn conn' $
              eUnimplemented $
                mconcat
                  [ "Resolve variant #"
                  , fromString (show tag)
                  , " not understood"
                  ]

handleReleaseMsg :: Conn -> R.Parsed R.Release -> STM ()
handleReleaseMsg
  conn
  R.Release
    { id = (IEId -> eid)
    , referenceCount = refCountDiff
    } =
    releaseExport conn refCountDiff eid

releaseExport :: Conn -> Word32 -> IEId -> STM ()
releaseExport conn refCountDiff eid =
  getLive conn >>= \conn'@Conn' {exports} ->
    lookupAbort "export" conn' exports eid $
      \EntryE {client, refCount = oldRefCount} ->
        case compare oldRefCount refCountDiff of
          LT ->
            abortConn conn' $
              eFailed $
                "Received release for export with referenceCount "
                  <> "greater than our recorded total ref count."
          EQ ->
            dropConnExport conn client
          GT ->
            M.insert
              EntryE
                { client
                , refCount = oldRefCount - refCountDiff
                }
              eid
              exports

handleDisembargoMsg :: Conn -> R.Parsed R.Disembargo -> STM ()
handleDisembargoMsg conn d = getLive conn >>= go d
  where
    go
      R.Disembargo
        { context =
          R.Disembargo'context'
            (R.Disembargo'context'receiverLoopback (EmbargoId -> eid))
        }
      conn'@Conn' {embargos} =
        do
          result <- M.lookup eid embargos
          case result of
            Nothing ->
              abortConn conn' $
                eFailed $
                  "No such embargo: " <> fromString (show $ embargoWord eid)
            Just fulfiller -> do
              queueSTM conn' (fulfill fulfiller ())
              M.delete eid embargos
              freeEmbargo conn' eid
    go
      R.Disembargo
        { target = R.MessageTarget target
        , context = R.Disembargo'context' (R.Disembargo'context'senderLoopback embargoId)
        }
      conn'@Conn' {exports, answers} =
        case target of
          R.MessageTarget'importedCap exportId ->
            lookupAbort "export" conn' exports (IEId exportId) $ \EntryE {client} ->
              disembargoPromise client
          R.MessageTarget'promisedAnswer R.PromisedAnswer {questionId, transform} ->
            lookupAbort "answer" conn' answers (QAId questionId) $ \case
              (HaveReturn (Return {union' = Return'results Payload {content}}) _) ->
                transformClient transform content conn' >>= \case
                  (unwrapClient -> Just client') -> disembargoClient client'
                  (unwrapClient -> Nothing) -> abortDisembargo "targets a null capability"
              _ ->
                abortDisembargo $
                  "does not target an answer which has resolved to a value hosted by"
                    <> " the sender."
          R.MessageTarget'unknown' ordinal ->
            abortConn conn' $
              eUnimplemented $
                "Unknown MessageTarget ordinal #" <> fromString (show ordinal)
        where
          disembargoPromise (PromiseClient pState _ _) =
            readTVar pState >>= \case
              Ready (unwrapClient -> Just client) ->
                disembargoClient client
              Ready (unwrapClient -> Nothing) ->
                abortDisembargo "targets a promise which resolved to null."
              _ ->
                abortDisembargo "targets a promise which has not resolved."
          disembargoPromise _ =
            abortDisembargo "targets something that is not a promise."

          disembargoClient (ImportClient cell) = do
            client <- Fin.readCell cell
            case client of
              ImportRef {conn = targetConn, importId}
                | conn == targetConn -> do
                    (_, onSent) <- newPromise
                    sendPureMsg
                      conn'
                      ( R.Message'disembargo
                          R.Disembargo
                            { context =
                                R.Disembargo'context' $
                                  R.Disembargo'context'receiverLoopback embargoId
                            , target =
                                R.MessageTarget $
                                  R.MessageTarget'importedCap (ieWord importId)
                            }
                      )
                      onSent
              _ ->
                abortDisembargoClient
          disembargoClient _ = abortDisembargoClient

          abortDisembargoClient =
            abortDisembargo $
              "targets a promise which has not resolved to a capability"
                <> " hosted by the sender."

          abortDisembargo info =
            abortConn conn' $
              eFailed $
                mconcat
                  [ "Disembargo #"
                  , fromString (show embargoId)
                  , " with context = senderLoopback "
                  , info
                  ]
    -- Note [Level 3]
    go other conn' = do
      (_, onSent) <- newPromise
      sendPureMsg
        conn'
        (R.Message'unimplemented $ R.Message $ R.Message'disembargo other)
        onSent

-- | Send a return message, update the corresponding entry in our
-- answers table, and queue any registered callbacks. Calls 'error'
-- if the answerId is not in the table, or if we've already sent a
-- return for this answer.
returnAnswer :: Conn' -> Return -> STM ()
returnAnswer conn@Conn' {answers} ret = do
  sendReturn conn ret
  updateQAReturn conn answers "answer" ret
