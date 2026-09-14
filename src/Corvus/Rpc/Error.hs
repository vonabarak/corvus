{-# LANGUAGE LambdaCase #-}

-- | The single mapping from the daemon's internal 'Response' sum to a
-- structured wire error (code + human-readable message).
--
-- Every RPC method that fails renders its failure through
-- 'responseError' and 'Corvus.Rpc.Common.throwError', so this is the
-- only place where a 'Response' error constructor chooses its code
-- and wire text. The messages preserve the historical free-form
-- strings clients already display; the codes are new.
module Corvus.Rpc.Error (responseError, resolveErrorWire) where

import Corvus.Handlers.Resolve (ResolveError (..), resolveErrorMessage)
import Corvus.Model (VmStatus)
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Wire.Error
import Data.Text (Text)

-- | Map every error 'Response' to its wire code and message.
--
-- Total by design: a method that throws on a success constructor is a
-- daemon bug, and the catch-all reports it as 'InternalError' rather
-- than panicking.
responseError :: Response -> (ErrorCode, Text)
responseError = \case
  RespAmbiguousRef message -> (AmbiguousRef, message)
  RespVmNotFound -> (VmNotFound, "VM not found")
  RespDiskNotFound -> (DiskNotFound, "Disk not found")
  RespSnapshotNotFound -> (SnapshotNotFound, "Snapshot not found")
  RespDriveNotFound -> (DriveNotFound, "Drive not found")
  RespNetIfNotFound -> (NetIfNotFound, "Net-if not found")
  RespSshKeyNotFound -> (SshKeyNotFound, "SSH key not found")
  RespSshKeyInUse _ -> (SshKeyInUse, "SSH key in use")
  RespSharedDirNotFound -> (SharedDirNotFound, "Shared directory not found")
  RespTemplateNotFound -> (TemplateNotFound, "Template not found")
  RespTaskNotFound -> (TaskNotFound, "Task not found")
  RespNodeNotFound -> (NodeNotFound, "Node not found")
  RespNodeInUse message -> (NodeInUse, message)
  RespDiskInUse _ -> (DiskInUse, "Disk in use")
  RespDiskHasOverlays _ -> (DiskHasOverlays, "Disk has overlays")
  RespVmMustBeStopped -> (VmMustBeStopped, "VM must be stopped")
  RespVmNotRunning -> (VmNotRunning, "VM not running")
  RespVmHeadless -> (VmHeadless, "VM has no SPICE display")
  RespNetworkNotFound -> (NetworkNotFound, "Network not found")
  RespNetworkInUse -> (NetworkInUse, "Network in use")
  RespNetworkAlreadyRunning -> (NetworkAlreadyRunning, "Network already running")
  RespNetworkNotRunning -> (NetworkNotRunning, "Network not running")
  RespInvalidTransition status reason ->
    ( InvalidTransition
    , "invalid transition from " <> M.enumToText (status :: VmStatus) <> ": " <> reason
    )
  RespFormatNotSupported message -> (FormatNotSupported, message)
  RespGuestAgentNotEnabled -> (GuestAgentNotEnabled, "Guest agent not enabled")
  RespGuestAgentError message -> (GuestAgentError, message)
  RespError message -> (InternalError, message)
  RespNetworkError message -> (InternalError, message)
  -- Success constructors never reach here; a method that throws on
  -- one is a daemon bug.
  _ -> (InternalError, "unexpected response")

-- | Map a 'ResolveError' to its wire code and message. A not-found
-- failure takes the code for the entity kind; an ambiguous match
-- always surfaces as 'AmbiguousRef'.
resolveErrorWire :: ResolveError -> (ErrorCode, Text)
resolveErrorWire re = (code, resolveErrorMessage re)
  where
    code = case re of
      RefNotFound entity _ -> entityCode entity
      RefAmbiguous {} -> AmbiguousRef

-- | Pick the not-found wire code from the entity label used by the
-- 'Corvus.Handlers.Resolve' helpers.
entityCode :: Text -> ErrorCode
entityCode "VM" = VmNotFound
entityCode "Disk image" = DiskNotFound
entityCode "Snapshot" = SnapshotNotFound
entityCode "Network" = NetworkNotFound
entityCode "SSH key" = SshKeyNotFound
entityCode "Template" = TemplateNotFound
entityCode "Node" = NodeNotFound
entityCode _ = InternalError
