{-# LANGUAGE LambdaCase #-}

-- | Structured wire error codes.
--
-- Cap'n Proto exceptions carry only a free-form description string, so
-- the daemon renders every RPC failure as
--
--     <code> :: <message>
--
-- where <code> is one of the stable snake_case tokens below and
-- <message> is the existing human-readable text. The code tokens are
-- the snake_case form of the 'ErrorCode' enum in @schema/enums.capnp@
-- (the source of truth for the wire vocabulary).
--
-- Clients parse the prefix back out of the exception description. A
-- code-less or unknown-code message (e.g. a peer from an older daemon
-- release) degrades to a generic server error, so version skew in
-- either direction is safe.
module Corvus.Wire.Error
  ( -- * Codes
    ErrorCode (..)
  , allErrorCodes
  , errorCodeText

    -- * Wire format
  , WireErrorInfo (..)
  , renderWireError
  , parseWireError
  )
where

import Data.Text (Text)
import qualified Data.Text as T

-- | Stable machine-readable error codes. Constructor order mirrors
-- the @ErrorCode@ enum in @schema/enums.capnp@; the snake_case wire
-- tokens are produced by 'errorCodeText'.
data ErrorCode
  = VmNotFound
  | DiskNotFound
  | SnapshotNotFound
  | DriveNotFound
  | NetworkNotFound
  | NetIfNotFound
  | SshKeyNotFound
  | SharedDirNotFound
  | TemplateNotFound
  | TaskNotFound
  | NodeNotFound
  | DiskInUse
  | DiskHasOverlays
  | VmMustBeStopped
  | VmNotRunning
  | VmHeadless
  | NetworkInUse
  | NetworkAlreadyRunning
  | NetworkNotRunning
  | SshKeyInUse
  | NodeInUse
  | InvalidTransition
  | FormatNotSupported
  | GuestAgentNotEnabled
  | GuestAgentError
  | AmbiguousRef
  | InternalError
  | ProtocolError
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Every code in declaration order.
allErrorCodes :: [ErrorCode]
allErrorCodes = [minBound ..]

-- | The snake_case wire token for a code.
errorCodeText :: ErrorCode -> Text
errorCodeText = \case
  VmNotFound -> "vm_not_found"
  DiskNotFound -> "disk_not_found"
  SnapshotNotFound -> "snapshot_not_found"
  DriveNotFound -> "drive_not_found"
  NetworkNotFound -> "network_not_found"
  NetIfNotFound -> "netif_not_found"
  SshKeyNotFound -> "ssh_key_not_found"
  SharedDirNotFound -> "shared_dir_not_found"
  TemplateNotFound -> "template_not_found"
  TaskNotFound -> "task_not_found"
  NodeNotFound -> "node_not_found"
  DiskInUse -> "disk_in_use"
  DiskHasOverlays -> "disk_has_overlays"
  VmMustBeStopped -> "vm_must_be_stopped"
  VmNotRunning -> "vm_not_running"
  VmHeadless -> "vm_headless"
  NetworkInUse -> "network_in_use"
  NetworkAlreadyRunning -> "network_already_running"
  NetworkNotRunning -> "network_not_running"
  SshKeyInUse -> "ssh_key_in_use"
  NodeInUse -> "node_in_use"
  InvalidTransition -> "invalid_transition"
  FormatNotSupported -> "format_not_supported"
  GuestAgentNotEnabled -> "guest_agent_not_enabled"
  GuestAgentError -> "guest_agent_error"
  AmbiguousRef -> "ambiguous_ref"
  InternalError -> "internal_error"
  ProtocolError -> "protocol_error"

-- | A wire error: one code plus the human-readable message.
data WireErrorInfo
  = -- | stable machine-readable code
    WireErrorInfo
    { wiCode :: !ErrorCode
    -- ^ human-readable message; must not contain the
    -- @ :: @ delimiter
    , wiMessage :: !Text
    }
  deriving (Eq, Show)

-- | Render the fixed @<code> :: <message>@ form.
renderWireError :: WireErrorInfo -> Text
renderWireError (WireErrorInfo code message) =
  errorCodeText code <> " :: " <> message

-- | Parse the @<code> :: <message>@ form.
--
-- Tokens are matched in declaration order. That is sound only because
-- no token is a prefix of another (pinned down by a unit test); keep
-- the code set prefix-free when extending it. Returns 'Nothing' for
-- code-less (legacy) or unknown-code messages; callers should treat
-- those as a generic server error.
parseWireError :: Text -> Maybe (ErrorCode, Text)
parseWireError text = go allErrorCodes
  where
    go [] = Nothing
    go (code : rest) =
      let prefix = errorCodeText code <> " :: "
       in if T.isPrefixOf prefix text
            then Just (code, T.drop (T.length prefix) text)
            else go rest
