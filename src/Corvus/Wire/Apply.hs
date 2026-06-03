{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Cap'n Proto conversion for the @crv apply@ result payload and
-- the streaming @ApplyEvent@ union used by @ApplyEventSink@.
module Corvus.Wire.Apply
  ( toCapnpApplyCreated
  , fromCapnpApplyCreated
  , toCapnpApplyResult
  , fromCapnpApplyResult
  , toCapnpApplyEvent
  , fromCapnpApplyEvent
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Corvus as CGCorvus
import qualified Capnp.Gen.Streams as CGS
import qualified Corvus.Protocol.Apply as P
import Corvus.Wire.Enums (fromCapnpTaskResult, toCapnpTaskResult)
import Corvus.Wire.Errors (WireError (..))

toCapnpApplyCreated :: P.ApplyCreated -> C.Parsed CGCorvus.ApplyCreated
toCapnpApplyCreated P.ApplyCreated {..} =
  CGCorvus.ApplyCreated {CGCorvus.name = acName, CGCorvus.id = acId}

fromCapnpApplyCreated :: C.Parsed CGCorvus.ApplyCreated -> P.ApplyCreated
fromCapnpApplyCreated CGCorvus.ApplyCreated {..} =
  P.ApplyCreated {P.acName = name, P.acId = id}

toCapnpApplyResult :: P.ApplyResult -> C.Parsed CGCorvus.ApplyResult
toCapnpApplyResult P.ApplyResult {..} =
  CGCorvus.ApplyResult
    { CGCorvus.sshKeys = map toCapnpApplyCreated arSshKeys
    , CGCorvus.disks = map toCapnpApplyCreated arDisks
    , CGCorvus.networks = map toCapnpApplyCreated arNetworks
    , CGCorvus.vms = map toCapnpApplyCreated arVms
    , CGCorvus.templates = map toCapnpApplyCreated arTemplates
    }

fromCapnpApplyResult :: C.Parsed CGCorvus.ApplyResult -> P.ApplyResult
fromCapnpApplyResult CGCorvus.ApplyResult {..} =
  P.ApplyResult
    { P.arSshKeys = map fromCapnpApplyCreated sshKeys
    , P.arDisks = map fromCapnpApplyCreated disks
    , P.arNetworks = map fromCapnpApplyCreated networks
    , P.arVms = map fromCapnpApplyCreated vms
    , P.arTemplates = map fromCapnpApplyCreated templates
    }

-- | Encode an in-process 'P.ApplyEvent' as its Cap'n Proto wire form
-- (an 'CGS.ApplyEvent' union).
toCapnpApplyEvent :: P.ApplyEvent -> C.Parsed CGS.ApplyEvent
toCapnpApplyEvent ev =
  CGS.ApplyEvent {CGS.union' = encodeUnion ev}
  where
    encodeUnion = \case
      P.ApplyLogLine t ->
        CGS.ApplyEvent'logLine t
      P.PhaseStart phase total ->
        CGS.ApplyEvent'phaseStart
          CGS.ApplyEvent'phaseStart'
            { CGS.phase = phase
            , CGS.total = total
            }
      P.EntityStart phase name kind ->
        CGS.ApplyEvent'entityStart
          CGS.ApplyEvent'entityStart'
            { CGS.phase = phase
            , CGS.name = name
            , CGS.kind = kind
            }
      P.EntityEnd phase name result message entityId ->
        CGS.ApplyEvent'entityEnd
          CGS.ApplyEvent'entityEnd'
            { CGS.phase = phase
            , CGS.name = name
            , CGS.result = toCapnpTaskResult result
            , CGS.message = message
            , CGS.entityId = entityId
            }
      P.DownloadStart name url ->
        CGS.ApplyEvent'downloadStart
          CGS.ApplyEvent'downloadStart'
            { CGS.name = name
            , CGS.url = url
            }
      P.DownloadProgress name downloaded total ->
        CGS.ApplyEvent'downloadProgress
          CGS.ApplyEvent'downloadProgress'
            { CGS.name = name
            , CGS.downloaded = downloaded
            , CGS.total = total
            }
      P.DownloadEnd name success message ->
        CGS.ApplyEvent'downloadEnd
          CGS.ApplyEvent'downloadEnd'
            { CGS.name = name
            , CGS.success = success
            , CGS.message = message
            }
      P.ApplyEnd result message taskId ->
        CGS.ApplyEvent'applyEnd
          CGS.ApplyEvent'applyEnd'
            { CGS.result = toCapnpTaskResult result
            , CGS.message = message
            , CGS.taskId = taskId
            }

-- | Decode a wire 'CGS.ApplyEvent' back into the Haskell ADT used by
-- the streaming sink renderer.
fromCapnpApplyEvent :: C.Parsed CGS.ApplyEvent -> Either WireError P.ApplyEvent
fromCapnpApplyEvent CGS.ApplyEvent {CGS.union' = u} = case u of
  CGS.ApplyEvent'logLine t ->
    Right (P.ApplyLogLine t)
  CGS.ApplyEvent'phaseStart CGS.ApplyEvent'phaseStart' {CGS.phase, CGS.total} ->
    Right (P.PhaseStart phase total)
  CGS.ApplyEvent'entityStart CGS.ApplyEvent'entityStart' {CGS.phase, CGS.name, CGS.kind} ->
    Right (P.EntityStart phase name kind)
  CGS.ApplyEvent'entityEnd CGS.ApplyEvent'entityEnd' {CGS.phase, CGS.name, CGS.result, CGS.message, CGS.entityId} -> do
    tr <- fromCapnpTaskResult result
    Right (P.EntityEnd phase name tr message entityId)
  CGS.ApplyEvent'downloadStart CGS.ApplyEvent'downloadStart' {CGS.name, CGS.url} ->
    Right (P.DownloadStart name url)
  CGS.ApplyEvent'downloadProgress CGS.ApplyEvent'downloadProgress' {CGS.name, CGS.downloaded, CGS.total} ->
    Right (P.DownloadProgress name downloaded total)
  CGS.ApplyEvent'downloadEnd CGS.ApplyEvent'downloadEnd' {CGS.name, CGS.success, CGS.message} ->
    Right (P.DownloadEnd name success message)
  CGS.ApplyEvent'applyEnd CGS.ApplyEvent'applyEnd' {CGS.result, CGS.message, CGS.taskId} -> do
    tr <- fromCapnpTaskResult result
    Right (P.ApplyEnd tr message taskId)
  CGS.ApplyEvent'unknown' _ ->
    Left (WireMissingUnionVariant "ApplyEvent")
