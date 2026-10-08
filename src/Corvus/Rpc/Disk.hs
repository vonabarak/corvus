{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | DiskManager + Disk + Snapshot cap implementations.
--
-- Disk-manager capability implementation.
module Corvus.Rpc.Disk
  ( DiskManagerCap (..)
  , DiskCap (..)
  , SnapshotCap (..)
  , newDiskManagerCap
  )
where

import Capnp (export)
import qualified Capnp as C
import qualified Capnp.Gen.Disk as CGDisk
import qualified Capnp.Gen.Enums as CGE
import qualified Capnp.Gen.Streams as CGS
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer, methodUnimplemented)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception (SomeException, onException, throwIO, try)
import Control.Monad (when)
import Corvus.Action (runAction, runActionAsyncWithId)
import Corvus.Handlers.Disk.Create (DiskCreate (..), DiskRegister (..))
import Corvus.Handlers.Disk.Derive (DiskClone (..), DiskCreateOverlay (..))
import Corvus.Handlers.Disk.Import (DiskImportAction (..))
import Corvus.Handlers.Disk.Maintenance (DiskDelete (..), DiskRefresh (..), DiskResize (..))
import Corvus.Handlers.Disk.Media (MediaChange (..), MediaEject (..))
import Corvus.Handlers.Disk.Placement (DiskCopy (..), DiskMove (..))
import Corvus.Handlers.Disk.Query (handleDiskList, handleDiskShow)
import Corvus.Handlers.Disk.Rebase (DiskRebase (..))
import Corvus.Handlers.Disk.RegisterPlacement (DiskRegisterPlacement (..))
import Corvus.Handlers.Disk.Snapshot
  ( SnapshotCreate (..)
  , SnapshotDelete (..)
  , SnapshotMerge (..)
  , SnapshotRollback (..)
  , handleSnapshotList
  )
import Corvus.Handlers.Disk.SnapshotAutoStop (SnapshotRollbackAutoStop (..))
import Corvus.Handlers.Disk.Tags (DiskTag (..), DiskUntag (..))
import Corvus.Handlers.Disk.Upload (DiskUploadFinalize (..), DiskUploadPlan (..), checkDiskUpload, prepareDiskUpload)
import Corvus.Handlers.Resolve (resolveDisk, resolveNode, resolveSnapshot)
import Corvus.Model (EnumText (enumToText))
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Protocol (Response (..))
import qualified Corvus.Protocol as P
import Corvus.Protocol.Disk (UploadIfExists (..))
import Corvus.Rpc.Common (capnpDiskRefToRef, capnpRefToRef, handleParsed, resolveOrThrow, throwError, throwWireError)
import Corvus.Rpc.Streams (callSink)
import Corvus.Types (ServerState (..), lookupNodeAgent)
import Corvus.Wire.Disk (toCapnpDiskImageInfo, toCapnpSnapshotInfo)
import Corvus.Wire.Enums (fromCapnpDriveFormat, fromCapnpUploadIfExists)
import Corvus.Wire.Error (ErrorCode (..))
import Crypto.Hash (Context, Digest, SHA256, hashFinalize, hashInit, hashUpdate)
import Data.Char (isHexDigit)
import Data.Int (Int64)
import qualified Data.Text as T
import Database.Persist.Sql (fromSqlKey)
import Supervisors (Supervisor)

-- ---------------------------------------------------------------------
-- Manager
-- ---------------------------------------------------------------------

data DiskManagerCap = DiskManagerCap
  { dmState :: !ServerState
  , dmSup :: !Supervisor
  , dmClientName :: !T.Text
  }

newDiskManagerCap :: ServerState -> Supervisor -> T.Text -> IO DiskManagerCap
newDiskManagerCap st sup cn = pure (DiskManagerCap st sup cn)

instance SomeServer DiskManagerCap

instance CGDisk.DiskManager'server_ DiskManagerCap where
  diskManager'list (DiskManagerCap st _ cn) = handleParsed $ \_ -> do
    resp <- handleDiskList st
    case resp of
      RespDiskList disks ->
        pure CGDisk.DiskManager'list'results {CGDisk.disks = map toCapnpDiskImageInfo disks}
      _ -> throwError resp

  diskManager'get (DiskManagerCap st sup cn) =
    handleParsed $ \CGDisk.DiskManager'get'params {..} -> do
      ref' <- capnpDiskRefToRef ref
      eid <- resolveOrThrow =<< resolveDisk ref' (ssDbPool st)
      client <- export @CGDisk.Disk sup (DiskCap st sup eid cn)
      pure CGDisk.DiskManager'get'results {CGDisk.disk = client}

  diskManager'create (DiskManagerCap st sup cn) =
    handleParsed $ \CGDisk.DiskManager'create'params {params = CGDisk.DiskCreateParams {..}} -> do
      fmt <- enumOrThrow (fromCapnpDriveFormat format)
      nodeRef' <- capnpRefToRef node
      let act =
            DiskCreate
              { dcrName = name
              , dcrFormat = fmt
              , dcrSize = size
              , dcrPath = if T.null path then Nothing else Just path
              , dcrEphemeral = ephemeral
              , dcrNodeRef = P.unRef nodeRef'
              }
      resp <- runAction st cn act
      case resp of
        RespDiskCreated newId -> do
          client <- export @CGDisk.Disk sup (DiskCap st sup newId cn)
          pure CGDisk.DiskManager'create'results {CGDisk.disk = client}
        _ -> throwError resp

  diskManager'register (DiskManagerCap st sup cn) =
    handleParsed $ \CGDisk.DiskManager'register'params {params = CGDisk.DiskRegisterParams {..}} -> do
      nodeRef' <- capnpRefToRef node
      mFormat <-
        if formatProvided
          then Just <$> enumOrThrow (fromCapnpDriveFormat format)
          else pure Nothing
      mBackingId <-
        if backingProvided
          then do
            backingRef' <- capnpDiskRefToRef backingDiskRef
            Just <$> (resolveOrThrow =<< resolveDisk backingRef' (ssDbPool st))
          else pure Nothing
      let act =
            DiskRegister
              { drgName = name
              , drgPath = filePath
              , drgFormat = mFormat
              , drgBackingDiskId = mBackingId
              , drgEphemeral = ephemeral
              , drgNodeRef = P.unRef nodeRef'
              }
      resp <- runAction st cn act
      case resp of
        RespDiskCreated newId -> do
          client <- export @CGDisk.Disk sup (DiskCap st sup newId cn)
          pure CGDisk.DiskManager'register'results {CGDisk.disk = client}
        _ -> throwError resp

  diskManager'createOverlay (DiskManagerCap st sup cn) =
    handleParsed $ \CGDisk.DiskManager'createOverlay'params {params = CGDisk.DiskCreateOverlayParams {..}} -> do
      baseRef' <- capnpDiskRefToRef backingDiskRef
      baseId <- resolveOrThrow =<< resolveDisk baseRef' (ssDbPool st)
      let act =
            DiskCreateOverlay
              { dcoName = name
              , dcoBaseDiskId = baseId
              , dcoResize = Nothing
              , dcoPath = if T.null path then Nothing else Just path
              , dcoEphemeral = ephemeral
              }
      resp <- runAction st cn act
      case resp of
        RespDiskCreated newId -> do
          client <- export @CGDisk.Disk sup (DiskCap st sup newId cn)
          pure CGDisk.DiskManager'createOverlay'results {CGDisk.disk = client}
        _ -> throwError resp

  diskManager'clone (DiskManagerCap st sup cn) =
    handleParsed $ \CGDisk.DiskManager'clone'params {params = CGDisk.DiskCloneParams {..}} -> do
      srcRef' <- capnpDiskRefToRef sourceRef
      srcId <- resolveOrThrow =<< resolveDisk srcRef' (ssDbPool st)
      -- Empty `path` means "let the daemon pick the default
      -- location"; non-empty is forwarded verbatim (the handler
      -- accepts both relative-to-basePath and absolute paths).
      let mPath = if T.null path then Nothing else Just path
          act =
            DiskClone
              { dclName = newName
              , dclBaseDiskId = srcId
              , dclResize = Nothing
              , dclPath = mPath
              , dclEphemeral = ephemeral
              }
      resp <- runAction st cn act
      case resp of
        RespDiskCreated newId -> do
          client <- export @CGDisk.Disk sup (DiskCap st sup newId cn)
          pure CGDisk.DiskManager'clone'results {CGDisk.disk = client}
        _ -> throwError resp

  diskManager'rebase (DiskManagerCap st _ cn) =
    handleParsed $ \CGDisk.DiskManager'rebase'params {params = CGDisk.DiskRebaseParams {..}} -> do
      diskRef' <- capnpDiskRefToRef diskRef
      diskId' <- resolveOrThrow =<< resolveDisk diskRef' (ssDbPool st)
      mBackingId <-
        if newBackingProvided
          then do
            backingRef' <- capnpDiskRefToRef newBackingDiskRef
            Just <$> (resolveOrThrow =<< resolveDisk backingRef' (ssDbPool st))
          else pure Nothing
      let act =
            DiskRebase
              { drbDiskId = diskId'
              , drbNewBackingId = mBackingId
              , drbUnsafe = unsafe
              }
      resp <- runAction st cn act
      case resp of
        RespDiskOk -> pure CGDisk.DiskManager'rebase'results
        _ -> throwError resp

  diskManager'flatten (DiskManagerCap st _ cn) =
    handleParsed $ \CGDisk.DiskManager'flatten'params {..} -> do
      diskRef' <- capnpDiskRefToRef diskRef
      diskId' <- resolveOrThrow =<< resolveDisk diskRef' (ssDbPool st)
      -- @drbNewBackingId = Nothing@ is the flatten signal in the
      -- daemon's @DiskRebase@ action (`Handlers/Disk/Rebase.hs`).
      let act =
            DiskRebase
              { drbDiskId = diskId'
              , drbNewBackingId = Nothing
              , drbUnsafe = False
              }
      resp <- runAction st cn act
      case resp of
        RespDiskOk -> pure CGDisk.DiskManager'flatten'results
        _ -> throwError resp

  diskManager'import_ (DiskManagerCap st _ cn) =
    handleParsed $ \CGDisk.DiskManager'import'params {params = CGDisk.DiskImportParams {..}} -> do
      nodeRef' <- capnpRefToRef node
      mFormat <-
        if formatProvided
          then Just <$> enumOrThrow (fromCapnpDriveFormat format)
          else pure Nothing
      let act =
            DiskImportAction
              { diaName = name
              , diaSource = srcPath
              , diaDestPath = if T.null destPath then Nothing else Just destPath
              , diaFormat = enumToText <$> mFormat
              , diaChecksum = Nothing
              , diaEphemeral = ephemeral
              , diaNodeRef = P.unRef nodeRef'
              }
      resp <- runActionAsyncWithId st cn act RespDiskImportStarted
      case resp of
        RespDiskImportStarted tid ->
          pure CGDisk.DiskManager'import'results {CGDisk.taskId = tid}
        _ -> throwError resp

  diskManager'copy (DiskManagerCap st _ cn) =
    handleParsed $ \CGDisk.DiskManager'copy'params {params = CGDisk.DiskCopyParams {..}} -> do
      dr <- capnpDiskRefToRef diskRef
      diskId <- resolveOrThrow =<< resolveDisk dr (ssDbPool st)
      nr <- capnpRefToRef toNodeRef
      nodeId <- resolveOrThrow =<< resolveNode nr (ssDbPool st)
      let act =
            DiskCopy
              { dcpDiskId = diskId
              , dcpDestNodeId = nodeId
              , dcpToPath = emptyToNothing toPath
              , dcpWithBackingChain = withBackingChain
              }
      resp <- runActionAsyncWithId st cn act RespDiskTransferStarted
      case resp of
        RespDiskTransferStarted tid ->
          pure CGDisk.DiskManager'copy'results {CGDisk.taskId = tid}
        _ -> throwError resp

  diskManager'move (DiskManagerCap st _ cn) =
    handleParsed $ \CGDisk.DiskManager'move'params {params = CGDisk.DiskMoveParams {..}} -> do
      dr <- capnpDiskRefToRef diskRef
      diskId <- resolveOrThrow =<< resolveDisk dr (ssDbPool st)
      nr <- capnpRefToRef toNodeRef
      nodeId <- resolveOrThrow =<< resolveNode nr (ssDbPool st)
      let act =
            DiskMove
              { dmvDiskId = diskId
              , dmvDestNodeId = nodeId
              , dmvToPath = emptyToNothing toPath
              , dmvWithBackingChain = withBackingChain
              }
      resp <- runActionAsyncWithId st cn act RespDiskTransferStarted
      case resp of
        RespDiskTransferStarted tid ->
          pure CGDisk.DiskManager'move'results {CGDisk.taskId = tid}
        _ -> throwError resp

  diskManager'beginUpload (DiskManagerCap st sup cn) =
    handleParsed $ \CGDisk.DiskManager'beginUpload'params {params = CGDisk.DiskUploadParams {..}} -> do
      fmt <- enumOrThrow (fromCapnpDriveFormat format)
      policy <- enumOrThrow (fromCapnpUploadIfExists ifExists)
      let expected = T.toLower expectedSha256
      when (policy == UploadUpdate && T.null expected) (throwFailed "update requires expectedSha256")
      when (not (T.null expected) && (T.length expected /= 64 || not (T.all isHexDigit expected))) $
        throwFailed "expectedSha256 must be a SHA-256 hex digest"
      existing <- either throwFailed pure =<< checkDiskUpload st name fmt policy (emptyToNothing expected)
      result <- case existing of
        Just key -> do
          disk <- export @CGDisk.Disk sup (DiskCap st sup (fromSqlKey key) cn)
          pure (CGDisk.DiskUploadResult'existing disk)
        Nothing -> do
          nodeRef' <- capnpRefToRef node
          plan <- either throwFailed pure =<< prepareDiskUpload st cn name fmt (emptyToNothing path) ephemeral (P.unRef nodeRef') (emptyToNothing sourcePath)
          agent <- either throwFailed pure =<< lookupNodeAgent st (dupNodeId plan)
          sink <- either (throwFailed . T.pack . show) pure =<< NOA.diskOpenWrite agent (T.pack (dupFilePath plan))
          context <- newMVar (Just hashInit)
          upload <- export @CGDisk.DiskUpload sup (DiskUploadCap st sup cn plan sink context (emptyToNothing expected))
          pure (CGDisk.DiskUploadResult'upload upload)
      pure CGDisk.DiskManager'beginUpload'results {CGDisk.result = CGDisk.DiskUploadResult result}

  diskManager'mediaEject (DiskManagerCap st _ cn) =
    handleParsed $ \CGDisk.DiskManager'mediaEject'params {..} -> do
      resp <- runAction st cn (MediaEject {meDriveId = driveId})
      case resp of
        RespDiskOk -> pure CGDisk.DiskManager'mediaEject'results
        _ -> throwError resp

  diskManager'mediaChange (DiskManagerCap st _ cn) =
    handleParsed $ \CGDisk.DiskManager'mediaChange'params {..} -> do
      newDiskRef' <- capnpDiskRefToRef newDiskRef
      newDiskId <- resolveOrThrow =<< resolveDisk newDiskRef' (ssDbPool st)
      resp <- runAction st cn (MediaChange {mcDriveId = driveId, mcDiskId = newDiskId})
      case resp of
        RespDiskOk -> pure CGDisk.DiskManager'mediaChange'results
        _ -> throwError resp

-- | Treat the wire's empty-string default as 'Nothing'. Cap'n
-- Proto can't represent @Maybe Text@ natively without adding a
-- group; using @""@ as the unset sentinel is the convention used
-- elsewhere in the daemon (see e.g. @VmCreateParams@ handling).
emptyToNothing :: T.Text -> Maybe T.Text
emptyToNothing t = if T.null t then Nothing else Just t

-- | Daemon-owned relay for a nodeagent atomic writer. The client only sees
-- this capability, so uploads retain the normal daemon/node mTLS boundary.
data DiskUploadCap = DiskUploadCap
  { ducState :: !ServerState
  , ducSup :: !Supervisor
  , ducClientName :: !T.Text
  , ducPlan :: !DiskUploadPlan
  , ducSink :: !(C.Client CGS.ByteSink)
  , ducContext :: !(MVar (Maybe (Context SHA256)))
  , ducExpected :: !(Maybe T.Text)
  }

instance SomeServer DiskUploadCap

instance CGDisk.DiskUpload'server_ DiskUploadCap where
  diskUpload'write cap =
    handleParsed $ \CGDisk.DiskUpload'write'params {CGDisk.chunk = chunk} -> do
      outcome <- modifyMVar (ducContext cap) $ \case
        Nothing -> throwFailed "Upload session is closed"
        Just current -> do
          written <- try @SomeException $ callSink #write CGS.ByteSink'write'params {CGS.chunk = chunk} (ducSink cap)
          case written of
            Right () -> do
              -- Evaluate each update now so the context cannot retain all chunks.
              let !updated = hashUpdate current chunk
              pure (Just updated, Right ())
            Left err -> do
              _ <- try @SomeException $ callSink #abort CGS.ByteSink'abort'params (ducSink cap)
              pure (Nothing, Left err)
      either throwIO pure outcome
      pure CGDisk.DiskUpload'write'results

  diskUpload'finish cap =
    handleParsed $ \_ -> do
      -- Close under the same lock as writes: no chunk can race finalization.
      digest <- modifyMVar (ducContext cap) $ \case
        Nothing -> throwFailed "Upload session is closed"
        Just current -> pure (Nothing, T.pack (show (hashFinalize current :: Digest SHA256)))
      when (maybe False (/= digest) (ducExpected cap)) $ do
        callSink #abort CGS.ByteSink'abort'params (ducSink cap)
        throwFailed "Uploaded bytes do not match expectedSha256"
      callSink #end CGS.ByteSink'end'params (ducSink cap)
        `onException` callSink #abort CGS.ByteSink'abort'params (ducSink cap)
      resp <- runAction (ducState cap) (ducClientName cap) (DiskUploadFinalize (ducPlan cap) digest)
      case resp of
        RespDiskCreated did -> do
          disk <- export @CGDisk.Disk (ducSup cap) (DiskCap (ducState cap) (ducSup cap) did (ducClientName cap))
          pure CGDisk.DiskUpload'finish'results {CGDisk.disk = disk}
        _ -> throwError resp

  diskUpload'abort cap =
    handleParsed $ \_ -> do
      modifyMVar_ (ducContext cap) $ \context -> do
        case context of
          Nothing -> pure ()
          Just _ -> callSink #abort CGS.ByteSink'abort'params (ducSink cap)
        pure Nothing
      pure CGDisk.DiskUpload'abort'results

-- ---------------------------------------------------------------------
-- Disk resource cap
-- ---------------------------------------------------------------------

data DiskCap = DiskCap
  { dskState :: !ServerState
  , dskSup :: !Supervisor
  , dskId :: !Int64
  , dskClientName :: !T.Text
  }

instance SomeServer DiskCap

instance CGDisk.Disk'server_ DiskCap where
  disk'show (DiskCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- handleDiskShow st eid
    case resp of
      RespDiskInfo info ->
        pure CGDisk.Disk'show'results {CGDisk.info = toCapnpDiskImageInfo info}
      _ -> throwError resp

  disk'delete (DiskCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- runAction st cn (DiskDelete eid)
    case resp of
      RespDiskOk -> pure CGDisk.Disk'delete'results
      _ -> throwError resp

  disk'resize (DiskCap st _ eid cn) = handleParsed $ \CGDisk.Disk'resize'params {..} -> do
    resp <- runAction st cn (DiskResize {drzDiskId = eid, drzNewSize = newSize})
    case resp of
      RespDiskOk -> pure CGDisk.Disk'resize'results
      _ -> throwError resp

  disk'snapshotCreate (DiskCap st sup eid cn) =
    handleParsed $ \CGDisk.Disk'snapshotCreate'params {..} -> do
      resp <-
        runAction
          st
          cn
          SnapshotCreate
            { scrDiskId = eid
            , scrName = name
            , scrQuiesce = decodeQuiesceMode quiesce
            , scrFullMachine = fullMachine
            }
      case resp of
        RespSnapshotCreated sid -> do
          client <- export @CGDisk.Snapshot sup (SnapshotCap st eid sid cn)
          pure CGDisk.Disk'snapshotCreate'results {CGDisk.snapshot = client, CGDisk.snapshotId = sid}
        _ -> throwError resp

  disk'snapshotList (DiskCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- handleSnapshotList st eid
    case resp of
      RespSnapshotList snaps ->
        pure CGDisk.Disk'snapshotList'results {CGDisk.snapshots = map toCapnpSnapshotInfo snaps}
      _ -> throwError resp

  disk'snapshotGet (DiskCap st sup eid cn) =
    handleParsed $ \CGDisk.Disk'snapshotGet'params {..} -> do
      ref' <- capnpRefToRef ref
      sid <- resolveOrThrow =<< resolveSnapshot ref' eid (ssDbPool st)
      client <- export @CGDisk.Snapshot sup (SnapshotCap st eid sid cn)
      pure CGDisk.Disk'snapshotGet'results {CGDisk.snapshot = client}

  disk'tag (DiskCap st _ eid cn) = handleParsed $ \CGDisk.Disk'tag'params {..} -> do
    resp <- runAction st cn (DiskTag eid tag)
    case resp of
      RespDiskOk -> pure CGDisk.Disk'tag'results
      _ -> throwError resp

  disk'untag (DiskCap st _ eid cn) = handleParsed $ \CGDisk.Disk'untag'params {..} -> do
    resp <- runAction st cn (DiskUntag eid tag)
    case resp of
      RespDiskOk -> pure CGDisk.Disk'untag'results
      _ -> throwError resp

  disk'registerPlacement (DiskCap st _ eid cn) = handleParsed $ \CGDisk.Disk'registerPlacement'params {..} -> do
    nodeRef <- capnpRefToRef node
    nid <- resolveOrThrow =<< resolveNode nodeRef (ssDbPool st)
    resp <- runAction st cn (DiskRegisterPlacement eid nid path)
    case resp of
      RespDiskOk -> pure CGDisk.Disk'registerPlacement'results
      _ -> throwError resp

  disk'refresh (DiskCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- runAction st cn (DiskRefresh eid)
    case resp of
      RespDiskInfo info ->
        pure CGDisk.Disk'refresh'results {CGDisk.info = toCapnpDiskImageInfo info}
      RespDiskOk -> do
        -- some refresh paths return RespDiskOk; re-fetch
        info <- handleDiskShow st eid
        case info of
          RespDiskInfo i ->
            pure CGDisk.Disk'refresh'results {CGDisk.info = toCapnpDiskImageInfo i}
          _ -> throwError info
      _ -> throwError resp

-- ---------------------------------------------------------------------
-- Snapshot resource cap
-- ---------------------------------------------------------------------

data SnapshotCap = SnapshotCap
  { _snState :: !ServerState
  , _snDiskId :: !Int64
  , _snId :: !Int64
  , _snClientName :: !T.Text
  }

instance SomeServer SnapshotCap

instance CGDisk.Snapshot'server_ SnapshotCap where
  snapshot'show (SnapshotCap st diskId sid cn) = handleParsed $ \_ -> do
    resp <- handleSnapshotList st diskId
    case resp of
      RespSnapshotList snaps ->
        case filter ((== sid) . P.sniId) snaps of
          (s : _) ->
            pure CGDisk.Snapshot'show'results {CGDisk.info = toCapnpSnapshotInfo s}
          [] -> throwError RespSnapshotNotFound
      _ -> throwError resp
  snapshot'delete (SnapshotCap st diskId sid cn) = handleParsed $ \_ -> do
    resp <- runAction st cn (SnapshotDelete {sdelDiskId = diskId, sdelSnapRef = P.Ref (T.pack (show sid))})
    case resp of
      RespSnapshotOk -> pure CGDisk.Snapshot'delete'results
      _ -> throwError resp
  snapshot'rollback (SnapshotCap st diskId sid cn) =
    handleParsed $ \CGDisk.Snapshot'rollback'params {..} -> do
      let snapRefT = P.Ref (T.pack (show sid))
      resp <-
        if autoStop
          then
            runAction
              st
              cn
              SnapshotRollbackAutoStop {srasDiskId = diskId, srasSnapRef = snapRefT}
          else
            runAction
              st
              cn
              SnapshotRollback {srlDiskId = diskId, srlSnapRef = snapRefT}
      case resp of
        RespSnapshotOk -> pure CGDisk.Snapshot'rollback'results
        _ -> throwError resp
  snapshot'merge (SnapshotCap st diskId sid cn) = handleParsed $ \_ -> do
    resp <- runAction st cn (SnapshotMerge {smrDiskId = diskId, smrSnapRef = P.Ref (T.pack (show sid))})
    case resp of
      RespSnapshotOk -> pure CGDisk.Snapshot'merge'results
      _ -> throwError resp

-- ---------------------------------------------------------------------
-- Helper
-- ---------------------------------------------------------------------

enumOrThrow :: Either e a -> IO a
enumOrThrow (Right a) = pure a
enumOrThrow (Left _) = throwWireError ProtocolError "unknown enum tag in request"

-- | Translate the wire 'QuiesceMode' enum to the daemon-internal
-- one used by 'Corvus.NodeAgentClient'. Unknown future variants
-- fall back to 'NOA.QuiesceAuto' so forward-compatible clients
-- that introduce a new mode never get rejected outright.
decodeQuiesceMode :: CGE.QuiesceMode -> NOA.QuiesceMode
decodeQuiesceMode CGE.QuiesceMode'auto = NOA.QuiesceAuto
decodeQuiesceMode CGE.QuiesceMode'require = NOA.QuiesceRequire
decodeQuiesceMode CGE.QuiesceMode'skip = NOA.QuiesceSkip
decodeQuiesceMode (CGE.QuiesceMode'unknown' _) = NOA.QuiesceAuto
