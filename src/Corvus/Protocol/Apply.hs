{-# LANGUAGE DeriveGeneric #-}

-- | @crv apply@ response data + streaming events for the
-- 'Daemon.apply' RPC's @ApplyEventSink@.
module Corvus.Protocol.Apply
  ( ApplyCreated (..)
  , ApplyResult (..)
  , ApplyEvent (..)
  , ApplySink
  , silentApplySink
  )
where

import Corvus.Model (TaskResult)
import Corvus.Protocol.JsonOptions (innerOptions)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Generics (Generic)

-- | A resource created during an apply operation
data ApplyCreated = ApplyCreated
  { acName :: !Text
  , acId :: !Int64
  }
  deriving (Eq, Show, Generic)

-- | Summary of resources created by an apply operation
data ApplyResult = ApplyResult
  { arSshKeys :: ![ApplyCreated]
  , arDisks :: ![ApplyCreated]
  , arNetworks :: ![ApplyCreated]
  , arVms :: ![ApplyCreated]
  , arTemplates :: ![ApplyCreated]
  }
  deriving (Eq, Show, Generic)

instance ToJSON ApplyCreated where
  toJSON = genericToJSON innerOptions

instance ToJSON ApplyResult where
  toJSON = genericToJSON innerOptions

-- | Streaming events emitted by @Daemon.apply@ when the client
-- supplies an @ApplyEventSink@ cap.
--
-- A streaming apply emits, in order: one 'PhaseStart' per non-empty
-- phase (sshKeys → disks → networks → vms → templates), bracketed by
-- one 'EntityStart' / 'EntityEnd' pair per entity. Disk imports that
-- download from a URL additionally emit 'DownloadStart',
-- 'DownloadProgress' (one or more), and 'DownloadEnd' between the
-- entity bracket. The stream terminates with exactly one 'ApplyEnd'
-- followed by a single @end()@ call on the sink cap.
data ApplyEvent
  = -- | Free-form log line (validator notes, daemon-level messages).
    ApplyLogLine !Text
  | -- | A new phase is about to run. The second arg is the entity count.
    PhaseStart !Text !Word32
  | -- | One entity creation is about to start. Args are
    -- @(phase, entityName, kindTag)@; @kindTag@ is e.g.
    -- @"disk-import"@, @"vm-create"@, @"skip"@.
    EntityStart !Text !Text !Text
  | -- | One entity creation finished. Args are
    -- @(phase, entityName, result, message, entityId)@. @entityId == 0@
    -- when the entity was skipped or failed before insert.
    EntityEnd !Text !Text !TaskResult !Text !Int64
  | -- | A disk's HTTP download is starting. Args are @(diskName, url)@.
    DownloadStart !Text !Text
  | -- | In-flight download progress. Args are
    -- @(diskName, downloadedBytes, totalBytes)@; @totalBytes == 0@
    -- means Content-Length wasn't known.
    DownloadProgress !Text !Int64 !Int64
  | -- | A disk's download finished. Args are @(diskName, success, message)@.
    DownloadEnd !Text !Bool !Text
  | -- | Terminal event: @(result, message, parentTaskId)@. Always
    -- the last event before @end()@.
    ApplyEnd !TaskResult !Text !Int64
  deriving (Eq, Show, Generic)

instance ToJSON ApplyEvent where
  toJSON = genericToJSON innerOptions

-- | In-process callback the daemon pipeline pushes events through.
-- The streaming RPC wraps the wire sink as one of these; non-streaming
-- callers pass 'silentApplySink'.
type ApplySink = ApplyEvent -> IO ()

-- | No-op sink, used when the caller didn't supply a streaming cap.
silentApplySink :: ApplySink
silentApplySink _ = pure ()
