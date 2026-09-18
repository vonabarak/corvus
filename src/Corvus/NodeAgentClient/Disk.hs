{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Disk image operations and inter-agent disk transfer RPCs.
module Corvus.NodeAgentClient.Disk
  ( DiskOpResult (..)
  , DiskOpKind (..)
  , DiskInspectInfo (..)
  , DiskSnapshotInfo (..)
  , decodeDiskOpResult
  , diskCreate
  , diskCreateOverlay
  , diskDelete
  , diskResize
  , diskRebase
  , diskClone
  , diskInspect
  , diskDownload
  , diskDecompressXz
  , diskHash
  , diskMd5
  , DiskOpenReadResult (..)
  , diskOpenRead
  , attachReader
  , diskReaderPipeInto
  , diskImportFromPeer
  , diskOpenWrite
  )
where

import Capnp (export)
import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
import qualified Capnp.Gen.Streams as CGS
import Capnp.Rpc.Server (SomeServer, handleParsed)
import qualified Control.Exception as E
import Corvus.NodeAgentClient.Core
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word (Word32)

-- ---------------------------------------------------------------------------
-- Result types — mirror DiskOpResult / DiskInspectInfo on the wire so the
-- daemon can pattern-match without parsing error strings.

-- | Result kind matching the agent-side @ImageResult@ ADT.
data DiskOpKind
  = DiskOpSuccess
  | DiskOpError !T.Text
  | DiskOpNotFound
  | DiskOpFormatUnsupported !T.Text
  deriving (Eq, Show)

newtype DiskOpResult = DiskOpResult
  { dorKind :: DiskOpKind
  }
  deriving (Eq, Show)

data DiskSnapshotInfo = DiskSnapshotInfo
  { dsiId :: !T.Text
  , dsiName :: !T.Text
  , dsiSizeMb :: !(Maybe Int64)
  }
  deriving (Eq, Show)

data DiskInspectInfo = DiskInspectInfo
  { diiFormat :: !T.Text
  , diiVirtualSizeMb :: !Int64
  , diiActualSizeMb :: !(Maybe Int64)
  , diiSnapshots :: ![DiskSnapshotInfo]
  }
  deriving (Eq, Show)

decodeDiskOpResult :: C.Parsed CGNA.DiskOpResult -> DiskOpResult
decodeDiskOpResult CGNA.DiskOpResult {CGNA.kind = k, CGNA.message = m} =
  DiskOpResult $ case k of
    CGNA.DiskOpKind'success -> DiskOpSuccess
    CGNA.DiskOpKind'errorGeneric -> DiskOpError m
    CGNA.DiskOpKind'errorNotFound -> DiskOpNotFound
    CGNA.DiskOpKind'errorFormatUnsupported -> DiskOpFormatUnsupported m
    CGNA.DiskOpKind'unknown' _ ->
      DiskOpError ("unknown DiskOpKind: " <> m)

decodeDiskSnapshotInfo :: C.Parsed CGNA.DiskSnapshotInfo -> DiskSnapshotInfo
decodeDiskSnapshotInfo
  CGNA.DiskSnapshotInfo
    { CGNA.id = i
    , CGNA.name = n
    , CGNA.sizeMb = sz
    , CGNA.hasSize = hs
    } =
    DiskSnapshotInfo
      { dsiId = i
      , dsiName = n
      , dsiSizeMb = if hs then Just sz else Nothing
      }

decodeDiskInspectInfo :: C.Parsed CGNA.DiskInspectInfo -> DiskInspectInfo
decodeDiskInspectInfo
  CGNA.DiskInspectInfo
    { CGNA.format = fmt
    , CGNA.virtualSizeMb = vs
    , CGNA.actualSizeMb = ac
    , CGNA.hasActualSize = ha
    , CGNA.snapshots = ss
    } =
    DiskInspectInfo
      { diiFormat = fmt
      , diiVirtualSizeMb = vs
      , diiActualSizeMb = if ha then Just ac else Nothing
      , diiSnapshots = map decodeDiskSnapshotInfo ss
      }

-- ---------------------------------------------------------------------------
-- Disk image operations

diskCreate
  :: NodeAgentClient
  -> T.Text
  -> T.Text
  -> Int64
  -> IO (Either NodeAgentError DiskOpResult)
diskCreate nac path format sizeMb = remote $ do
  CGNA.Session'diskCreate'results {CGNA.result = r} <-
    callOn
      #diskCreate
      CGNA.Session'diskCreate'params
        { CGNA.path = path
        , CGNA.format = format
        , CGNA.sizeMb = sizeMb
        }
      (nacSession nac)
  pure (decodeDiskOpResult r)

diskCreateOverlay
  :: NodeAgentClient
  -> T.Text
  -> T.Text
  -> T.Text
  -> IO (Either NodeAgentError DiskOpResult)
diskCreateOverlay nac overlayPath backingPath backingFormat = remote $ do
  CGNA.Session'diskCreateOverlay'results {CGNA.result = r} <-
    callOn
      #diskCreateOverlay
      CGNA.Session'diskCreateOverlay'params
        { CGNA.overlayPath = overlayPath
        , CGNA.backingPath = backingPath
        , CGNA.backingFormat = backingFormat
        }
      (nacSession nac)
  pure (decodeDiskOpResult r)

diskDelete :: NodeAgentClient -> T.Text -> IO (Either NodeAgentError DiskOpResult)
diskDelete nac path = remote $ do
  CGNA.Session'diskDelete'results {CGNA.result = r} <-
    callOn
      #diskDelete
      CGNA.Session'diskDelete'params {CGNA.path = path}
      (nacSession nac)
  pure (decodeDiskOpResult r)

diskResize
  :: NodeAgentClient
  -> T.Text
  -> Int64
  -> IO (Either NodeAgentError DiskOpResult)
diskResize nac path newSizeMb = remote $ do
  CGNA.Session'diskResize'results {CGNA.result = r} <-
    callOn
      #diskResize
      CGNA.Session'diskResize'params
        { CGNA.path = path
        , CGNA.newSizeMb = newSizeMb
        }
      (nacSession nac)
  pure (decodeDiskOpResult r)

-- | Rebase an overlay onto a new backing image (or flatten if
-- 'Nothing'). Mirrors 'Corvus.Node.Image.rebaseImage'.
diskRebase
  :: NodeAgentClient
  -> T.Text
  -> Maybe (T.Text, T.Text)
  -- ^ @Just (newBackingPath, newBackingFormat)@ to rebase;
  -- @Nothing@ to flatten.
  -> Bool
  -- ^ Unsafe (@-u@) mode.
  -> IO (Either NodeAgentError DiskOpResult)
diskRebase nac overlayPath mNewBacking unsafeUpdate = remote $ do
  let (newBacking, newBackingFormat, hasNewBacking) = case mNewBacking of
        Just (p, f) -> (p, f, True)
        Nothing -> ("", "", False)
  CGNA.Session'diskRebase'results {CGNA.result = r} <-
    callOn
      #diskRebase
      CGNA.Session'diskRebase'params
        { CGNA.overlayPath = overlayPath
        , CGNA.newBacking = newBacking
        , CGNA.newBackingFormat = newBackingFormat
        , CGNA.hasNewBacking = hasNewBacking
        , CGNA.unsafeUpdate = unsafeUpdate
        }
      (nacSession nac)
  pure (decodeDiskOpResult r)

diskClone
  :: NodeAgentClient
  -> T.Text
  -> T.Text
  -> T.Text
  -- ^ destination on-disk format (\"qcow2\" / \"raw\"); passed
  -- to @qemu-img convert -O <destFormat>@.
  -> IO (Either NodeAgentError DiskOpResult)
diskClone nac src dest destFormat = remote $ do
  CGNA.Session'diskClone'results {CGNA.result = r} <-
    callOn
      #diskClone
      CGNA.Session'diskClone'params
        { CGNA.sourcePath = src
        , CGNA.destPath = dest
        , CGNA.destFormat = destFormat
        }
      (nacSession nac)
  pure (decodeDiskOpResult r)

diskInspect
  :: NodeAgentClient
  -> T.Text
  -> IO (Either NodeAgentError DiskInspectInfo)
diskInspect nac path = remote $ do
  CGNA.Session'diskInspect'results {CGNA.info = i} <-
    callOn
      #diskInspect
      CGNA.Session'diskInspect'params {CGNA.path = path}
      (nacSession nac)
  pure (decodeDiskInspectInfo i)

-- ---------------------------------------------------------------------------
-- Download / decompress / hash

-- | Daemon-side 'CGS.DiskDownloadSink' implementation. The agent
-- calls 'progress' once per byte-count update; the callback we
-- close over forwards each event to whatever in-process consumer
-- the caller of 'diskDownload' supplied. A no-op callback (used
-- when no progress reporting is wanted) makes the exported server
-- a cheap one-shot — cancelled when the supervisor exits.
newtype DaemonDiskDownloadSink = DaemonDiskDownloadSink
  { ddsOnProgress :: Int64 -> Int64 -> IO ()
  }

instance SomeServer DaemonDiskDownloadSink

instance CGS.DiskDownloadSink'server_ DaemonDiskDownloadSink where
  diskDownloadSink'progress (DaemonDiskDownloadSink cb) =
    handleParsed $ \CGS.DiskDownloadSink'progress'params {CGS.downloaded = d, CGS.total = t} -> do
      _ <- E.try (cb d t) :: IO (Either E.SomeException ())
      pure CGS.DiskDownloadSink'progress'results

-- | Download an image to the node. The optional @onProgress@
-- callback receives @(downloaded, total)@ updates every ~250 ms
-- during the transfer; pass 'Nothing' to opt out (a no-op sink is
-- still exported because Cap'n Proto requires the param to be
-- present).
diskDownload
  :: NodeAgentClient
  -> T.Text
  -> T.Text
  -> Maybe (Int64 -> Int64 -> IO ())
  -> IO (Either NodeAgentError DiskOpResult)
diskDownload nac destPath url mProgress = remote $ do
  let cb = fromMaybe (\_ _ -> pure ()) mProgress
  sinkClient <- export @CGS.DiskDownloadSink (nacSupervisor nac) (DaemonDiskDownloadSink cb)
  CGNA.Session'diskDownload'results {CGNA.result = r} <-
    callOn
      #diskDownload
      CGNA.Session'diskDownload'params
        { CGNA.destPath = destPath
        , CGNA.url = url
        , CGNA.sink = sinkClient
        }
      (nacSession nac)
  pure (decodeDiskOpResult r)

diskDecompressXz
  :: NodeAgentClient
  -> T.Text
  -> IO (Either NodeAgentError T.Text)
diskDecompressXz nac xzPath = remote $ do
  CGNA.Session'diskDecompressXz'results {CGNA.finalPath = f} <-
    callOn
      #diskDecompressXz
      CGNA.Session'diskDecompressXz'params {CGNA.xzPath = xzPath}
      (nacSession nac)
  pure f

diskHash :: NodeAgentClient -> T.Text -> T.Text -> IO (Either NodeAgentError T.Text)
diskHash nac algorithm path = remote $ do
  CGNA.Session'diskHash'results {CGNA.hex = h} <-
    callOn
      #diskHash
      CGNA.Session'diskHash'params {CGNA.path = path, CGNA.algorithm = algorithm}
      (nacSession nac)
  pure h

diskMd5 :: NodeAgentClient -> T.Text -> IO (Either NodeAgentError T.Text)
diskMd5 nac = diskHash nac "md5"

-- ---------------------------------------------------------------------------
-- Inter-agent disk transfer

-- | Result of 'diskOpenRead': a 'DiskReader' cap, the token the
-- destination presents to @attachReader@ on its own session, and
-- the source-side size + md5 the destination cross-checks the
-- received bytes against.
data DiskOpenReadResult = DiskOpenReadResult
  { dorReader :: !(C.Client CGNA.DiskReader)
  , dorToken :: !T.Text
  , dorSizeBytes :: !Int64
  , dorMd5 :: !T.Text
  }

-- | Open a file on the source agent for inter-agent transfer.
diskOpenRead
  :: NodeAgentClient
  -> T.Text
  -- ^ source-side absolute path
  -> Word32
  -- ^ token TTL in seconds; the source agent evicts the entry
  -- after this many seconds if @attachReader@ never claims it.
  -> IO (Either NodeAgentError DiskOpenReadResult)
diskOpenRead nac path ttlSec = remote $ do
  CGNA.Session'diskOpenRead'results
    { CGNA.reader = reader
    , CGNA.token = token
    , CGNA.sizeBytes = sz
    , CGNA.md5 = md5
    } <-
    callOn
      #diskOpenRead
      CGNA.Session'diskOpenRead'params
        { CGNA.path = path
        , CGNA.ttlSec = ttlSec
        }
      (nacSession nac)
  pure $
    DiskOpenReadResult
      { dorReader = reader
      , dorToken = token
      , dorSizeBytes = sz
      , dorMd5 = md5
      }

-- | Single-use re-resolve. Typically called by the destination
-- agent on its own session to claim the same 'DiskReader' the
-- daemon obtained from 'diskOpenRead'.
attachReader
  :: NodeAgentClient
  -> T.Text
  -> IO (Either NodeAgentError (C.Client CGNA.DiskReader))
attachReader nac token = remote $ do
  CGNA.Session'attachReader'results {CGNA.reader = reader} <-
    callOn
      #attachReader
      CGNA.Session'attachReader'params {CGNA.token = token}
      (nacSession nac)
  pure reader

-- | Drive a 'DiskReader' to stream into a caller-supplied
-- 'ByteSink'. Blocks until @sink.end@ is delivered (or until the
-- source raises, in which case the RPC returns with that error).
diskReaderPipeInto
  :: C.Client CGNA.DiskReader
  -> C.Client CGS.ByteSink
  -> IO (Either NodeAgentError ())
diskReaderPipeInto reader sink = remote $ do
  _ :: C.Parsed CGNA.DiskReader'pipeInto'results <-
    callOn
      #pipeInto
      CGNA.DiskReader'pipeInto'params {CGNA.sink = sink}
      reader
  pure ()

-- | Ask a destination agent to pull a file from a peer source
-- agent. Resolves @peerHost:peerPort@, opens a fresh session,
-- claims the reader via @attachReader(token)@, then runs
-- @reader.pipeInto(localSink)@ where @localSink@ writes to
-- @destPath.part@ on the destination. On clean completion the
-- destination renames @.part@ to @destPath@ after verifying
-- @expectedBytes@ + @expectedMd5@.
diskImportFromPeer
  :: NodeAgentClient
  -> T.Text
  -- ^ destination path
  -> T.Text
  -- ^ peer host (source's @nodeHost@)
  -> Int32
  -- ^ peer port (source's @nodeAgentPort@)
  -> T.Text
  -- ^ token issued by 'diskOpenRead'
  -> Int64
  -- ^ expected size in bytes
  -> T.Text
  -- ^ expected md5 hex hash
  -> IO (Either NodeAgentError ())
diskImportFromPeer nac destPath peerHost peerPort token expectedBytes expectedMd5 = remote $ do
  _ :: C.Parsed CGNA.Session'diskImportFromPeer'results <-
    callOn
      #diskImportFromPeer
      CGNA.Session'diskImportFromPeer'params
        { CGNA.destPath = destPath
        , CGNA.peerHost = peerHost
        , CGNA.peerPort = peerPort
        , CGNA.token = token
        , CGNA.expectedBytes = expectedBytes
        , CGNA.expectedMd5 = expectedMd5
        }
      (nacSession nac)
  pure ()

-- | Open a node-local atomic writer used by daemon-mediated client uploads.
-- The returned capability remains routed through the daemon connection when
-- it is handed back to an API client.
diskOpenWrite
  :: NodeAgentClient
  -> T.Text
  -> IO (Either NodeAgentError (C.Client CGS.ByteSink))
diskOpenWrite nac destPath = remote $ do
  CGNA.Session'diskOpenWrite'results {CGNA.sink = sink} <-
    callOn
      #diskOpenWrite
      CGNA.Session'diskOpenWrite'params {CGNA.destPath = destPath}
      (nacSession nac)
  pure sink
