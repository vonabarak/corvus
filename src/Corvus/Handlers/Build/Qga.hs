{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Agent-routed QGA helpers for the build pipeline.
--
-- All guest-exec / guest-ping calls during a bake-VM build go
-- through @nodeagent.vmGuestExec@ (the agent owns the QGA
-- socket). These helpers wrap the underlying RPC and preserve
-- the daemon-side 'GuestExecResult' shape so the bake-pipeline
-- code reads the same.
--
-- Trade-off (Phase 4): @agentGuestExecWithTail@ no longer
-- streams stdout line-by-line as QGA chunks arrive — the agent's
-- vmGuestExec aggregates and returns the full output on exit.
-- Build provisioner output therefore appears at end-of-step
-- instead of live. A streaming RPC variant
-- (@vmGuestExecStream(req, sink)@) is the natural follow-up.
module Corvus.Handlers.Build.Qga
  ( agentGuestExec
  , agentGuestExecWithStdin
  , agentGuestPing
  , agentGuestExecWithTail
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Streams as CGS
import Control.Exception (SomeException, try)
import Corvus.Node.GuestAgent (GuestExecResult (..))
import qualified Corvus.Node.VmSpec as VS
import qualified Corvus.NodeAgentClient as NOA
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Types (ServerState)
import Corvus.Rpc.Streams (newLineBufferSink)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word32)
import qualified Data.ByteString as BS

-- | One-shot guest exec via @nodeagent.vmGuestExec@.
agentGuestExec :: ServerState -> Int64 -> Text -> Word32 -> IO GuestExecResult
agentGuestExec state vmId cmd =
  agentGuestExecCore state vmId cmd BS.empty

-- | Guest exec with a stdin payload.
agentGuestExecWithStdin
  :: ServerState
  -> Int64
  -> Text
  -> BS.ByteString
  -> Word32
  -> IO GuestExecResult
agentGuestExecWithStdin = agentGuestExecCore

-- | Trivial liveness probe — run @/bin/true@; success iff exit 0.
-- Drop-in replacement for the old QGA @guest-ping@.
agentGuestPing :: ServerState -> Int64 -> IO Bool
agentGuestPing state vmId = do
  r <- agentGuestExecCore state vmId "true" BS.empty 5
  pure $ case r of
    GuestExecSuccess 0 _ _ -> True
    _ -> False

-- | Guest exec whose stdout / stderr stream line-by-line through
-- @onLine@ as the guest emits them. Drives the agent's
-- @vmGuestExecStream@ RPC: we export two 'LineBufferSink' caps,
-- the agent pushes raw QGA chunks into them as @guest-exec-status@
-- returns, and our line buffer fires @onLine@ for each complete
-- line on the way in. The agent calls @sink.end()@ on completion
-- (success or failure) so any trailing partial line gets flushed
-- before this function returns.
--
-- Stdout and stderr each get their own line buffer (lines from
-- the two streams never split mid-line), but the SAME @onLine@
-- callback — so the build sees a single merged event stream,
-- matching the user-visible UX of 0.10's @exec 2>&1@ log file.
agentGuestExecWithTail
  :: ServerState
  -> Int64
  -> Text
  -> Word32
  -> (Text -> IO ())
  -> IO GuestExecResult
agentGuestExecWithTail state vmId cmd timeoutSec onLine = do
  let req =
        VS.VmGuestExecReq
          { VS.vgeVmId = vmId
          , VS.vgePath = cmd
          , VS.vgeArgs = []
          , VS.vgeCaptureOutput = True
          , VS.vgeInputData = BS.empty
          , VS.vgeTimeoutSec = timeoutSec
          }
  outer <- withVmNodeAgent state vmId $ \nac -> do
    let sup = NOA.nacSupervisor nac
    stdoutSink <- newLineBufferSink onLine
    stderrSink <- newLineBufferSink onLine
    stdoutCap <- C.export @CGS.ByteSink sup stdoutSink
    stderrCap <- C.export @CGS.ByteSink sup stderrSink
    NOA.vmGuestExecStream nac req stdoutCap stderrCap
  case outer of
    Left err -> pure (GuestExecConnectionFailed err)
    Right r -> case r of
      Left e ->
        pure (GuestExecError ("vmGuestExecStream: " <> T.pack (show e)))
      Right info
        | VS.vgiHasExit info ->
            -- Bytes already flowed through 'onLine'. Stdout /
            -- stderr fields are empty by design (the agent's
            -- streaming path doesn't echo them back), so don't
            -- forward them to the caller — pass 'T.empty'.
            pure $
              GuestExecSuccess
                (fromIntegral (VS.vgiExitCode info))
                T.empty
                T.empty
        | otherwise ->
            let stderrText =
                  TE.decodeUtf8With lenientDecode (VS.vgiStderr info)
                msg
                  | T.null stderrText =
                      "guest-exec did not return an exit code"
                  | otherwise = stderrText
             in pure (GuestExecError msg)

-- | Common body for the four wrappers above.
agentGuestExecCore
  :: ServerState
  -> Int64
  -> Text
  -> BS.ByteString
  -> Word32
  -> IO GuestExecResult
agentGuestExecCore state vmId cmd stdinPayload timeoutSec = do
  -- Send the bare command. The agent's 'guestExec' detects
  -- the guest OS and wraps with @/bin/sh -c@ (Linux/BSD) or
  -- @cmd.exe /c@ (Windows). Pre-wrapping here would double-
  -- wrap on Windows.
  let req =
        VS.VmGuestExecReq
          { VS.vgeVmId = vmId
          , VS.vgePath = cmd
          , VS.vgeArgs = []
          , VS.vgeCaptureOutput = True
          , VS.vgeInputData = stdinPayload
          , VS.vgeTimeoutSec = timeoutSec
          }
  outer <- withVmNodeAgent state vmId $ \nac -> NOA.vmGuestExec nac req
  case outer of
    Left err -> pure (GuestExecConnectionFailed err)
    Right r -> case r of
      Left e ->
        pure (GuestExecError ("vmGuestExec: " <> T.pack (show e)))
      Right info
        | VS.vgiHasExit info ->
            pure $
              GuestExecSuccess
                (fromIntegral (VS.vgiExitCode info))
                (TE.decodeUtf8With lenientDecode (VS.vgiStdout info))
                (TE.decodeUtf8With lenientDecode (VS.vgiStderr info))
        | otherwise ->
            -- hasExit=False: forward the agent's stderr (QGA
            -- timeout, connection error, …) so the failure is
            -- actually diagnosable in build output.
            let stderrText =
                  TE.decodeUtf8With lenientDecode (VS.vgiStderr info)
                msg
                  | T.null stderrText =
                      "guest-exec did not return an exit code"
                  | otherwise = stderrText
             in pure (GuestExecError msg)
