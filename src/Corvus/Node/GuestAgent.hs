{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | QEMU Guest Agent (QGA) interaction.
-- Provides functions to execute commands inside VMs via the guest agent.
--
-- == Connection model
--
-- QEMU's chardev listen backlog is hardcoded to 1, so concurrent @connect()@
-- calls get EAGAIN.  To avoid this, we keep a persistent socket per VM in an
-- @MVar (Maybe Socket)@.  The MVar serializes access — only one thread talks
-- to the guest agent at a time — and the socket is reused across operations.
--
-- On any error the socket is closed and the MVar set to @Nothing@; the next
-- operation will reconnect automatically.  @SO_RCVTIMEO@ is set on the socket
-- so @recv@ never blocks indefinitely.
module Corvus.Node.GuestAgent
  ( -- * Types
    GuestExecResult (..)
  , GuestIpAddress (..)
  , GuestNetIf (..)

    -- * Persistent connection management
  , GuestAgentConns
  , releaseConn

    -- * Commands
  , guestExec
  , guestExecWithTimeout
  , guestExecWithStdin
  , guestExecStream
  , ChunkSink
  , guestPing
  , guestShutdown
  , guestNetworkGetInterfaces
  , guestFsFreeze
  , guestFsThaw
  , guestSetTime

    -- * Internal (exposed for tests)
  , parseGuestInterfaces
  , splitLines
  , pollStatus
  , pollRecvTimeoutMicros
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Exception (SomeException, catch, mask, mask_, onException, try)
import Control.Monad (unless)
import Corvus.Node.Runtime (getGuestAgentSocket)
import Corvus.Qemu.Config (QemuConfig)
import Data.Aeson (Value (..), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word8)
import GHC.IO.Exception (IOErrorType (..))
import Network.Socket
  ( Family (..)
  , SockAddr (..)
  , Socket
  , SocketType (..)
  , close
  , connect
  , defaultProtocol
  , socket
  )
import Network.Socket.ByteString (recv, sendAll)
import System.IO.Error (ioeGetErrorType)
import System.Random (randomRIO)
import System.Timeout (timeout)

-- | Per-VM persistent guest agent connections.
-- Each entry is an MVar holding the socket state:
--   * @Nothing@ — not connected (will connect on next operation)
--   * @Just sock@ — persistent connection ready for commands
type GuestAgentConns = TVar (Map.Map Int64 (MVar (Maybe Socket)))

-- | Per-recv timeout used inside the @guest-exec-status@ /
-- @guest-file-read@ poll loops and other short single-round-trip
-- commands. QEMU keeps the host-side chardev socket open across a
-- guest reboot or qemu-ga crash, so a plain @recv@ blocks until the
-- outer 'withPersistentConn' timeout (90 s for the default
-- 60 s-budget exec) fires — multiplied by 5 retries that's ~7.5 min
-- per @vm.cap.guest_exec(...)@ call on a vanished agent. A 5 s
-- per-recv bound shortens that window without ever tripping on
-- healthy guests (the slowest QGAs we've measured — FreeBSD on
-- nested KVM — respond to status queries inside a few hundred ms).
pollRecvTimeoutMicros :: Int
pollRecvTimeoutMicros = 5000000

-- | Result of a guest-exec command
data GuestExecResult
  = -- | Command completed (exitcode, stdout, stderr)
    GuestExecSuccess !Int !Text !Text
  | -- | Error communicating with guest agent
    GuestExecError !Text
  | -- | Could not connect to guest agent socket
    GuestExecConnectionFailed !Text
  deriving (Eq, Show)

-- | A single IP address reported by the guest agent
data GuestIpAddress = GuestIpAddress
  { giaType :: !Text
  -- ^ "ipv4" or "ipv6"
  , giaAddress :: !Text
  -- ^ e.g. "10.0.0.5"
  , giaPrefix :: !Int
  -- ^ e.g. 24
  }
  deriving (Eq, Show)

-- | A network interface reported by the guest agent
data GuestNetIf = GuestNetIf
  { gniHardwareAddress :: !Text
  -- ^ MAC address, e.g. "52:54:00:xx:xx:xx"
  , gniIpAddresses :: ![GuestIpAddress]
  -- ^ All IP addresses on this interface
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Persistent connection core
--------------------------------------------------------------------------------

-- | Get or create the MVar for a VM's connection.
getOrCreateConn :: GuestAgentConns -> Int64 -> IO (MVar (Maybe Socket))
getOrCreateConn connsVar vmId = do
  mExisting <- atomically $ Map.lookup vmId <$> readTVar connsVar
  case mExisting of
    Just c -> pure c
    Nothing -> do
      newConn <- newMVar Nothing
      atomically $ do
        conns <- readTVar connsVar
        case Map.lookup vmId conns of
          Just existing -> pure existing -- another thread beat us
          Nothing -> do
            writeTVar connsVar (Map.insert vmId newConn conns)
            pure newConn

-- | Run an action on a VM's persistent guest agent connection.
-- Connects and syncs on first use; reuses the socket for subsequent calls.
-- On any failure the socket is closed (next call will reconnect).
-- Retries up to @retries@ times with 1-second backoff. Each attempt is
-- bounded by @timeoutMicros@ covering connect + sync + action.
withPersistentConn
  :: GuestAgentConns
  -> QemuConfig
  -> Int64
  -> Int
  -- ^ Retry count
  -> Int
  -- ^ Per-attempt timeout in microseconds
  -> (Socket -> IO a)
  -> IO (Either Text a)
withPersistentConn connsVar config vmId retries timeoutMicros action = do
  connVar <- getOrCreateConn connsVar vmId
  path <- getGuestAgentSocket config vmId
  go connVar path retries
  where
    go _ _ 0 = pure $ Left "Guest agent unavailable after retries"
    go connVar path n = do
      result <- runOnce connVar path
      case result of
        Right a -> pure $ Right a
        Left err
          | n > 1 -> do
              threadDelay 1000000
              go connVar path (n - 1)
          | otherwise -> pure $ Left err

    runOnce connVar path = mask $ \restore -> do
      mSock <- takeMVar connVar
      -- Track the socket currently in use so we can close it from the
      -- exception/timeout path. Starts as @mSock@ (the cached socket, if any);
      -- updated to the fresh socket right after @connectWithRetry@ succeeds,
      -- so a timeout firing mid-@syncGuest@ still closes the new fd instead
      -- of leaking it and blocking QEMU's single-slot chardev backlog.
      sockRef <- newIORef mSock
      eResult <-
        restore (doWork mSock path sockRef) `onException` do
          cur <- readIORef sockRef
          closeMaybe cur
          putMVar connVar Nothing
      case eResult of
        Right (sock', a) -> do
          putMVar connVar (Just sock')
          pure $ Right a
        Left err -> do
          cur <- readIORef sockRef
          closeMaybe cur
          putMVar connVar Nothing
          pure $ Left err

    doWork mSock path sockRef = do
      -- Timeout covers the entire operation: connect + sync + action.
      -- GHC uses non-blocking sockets internally, so SO_RCVTIMEO is
      -- ineffective — async exceptions from timeout are the only way
      -- to interrupt a blocked recv().
      mResult <- timeout timeoutMicros $ try $ do
        sock <- case mSock of
          Just s -> pure s
          Nothing -> do
            -- Connect and publish the fd to sockRef atomically, so if an
            -- async exception fires (most importantly the timeout's) the
            -- handler in runOnce can still find and close this socket.
            s <- mask_ $ do
              s <- connectWithRetry 10 path
              writeIORef sockRef (Just s)
              pure s
            syncGuest s
            pure s
        result <- action sock
        pure (sock, result)
      case mResult of
        Just (Right (sock, a)) -> pure $ Right (sock, a)
        Just (Left (e :: SomeException)) ->
          pure $ Left $ T.pack (show e)
        Nothing ->
          pure $ Left "Timed out waiting for guest agent"

-- | Connect to a Unix domain socket with retries on EAGAIN.
connectWithRetry :: Int -> FilePath -> IO Socket
connectWithRetry 0 path = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock (SockAddrUnix path)
  pure sock
connectWithRetry n path = do
  sock <- socket AF_UNIX Stream defaultProtocol
  (connect sock (SockAddrUnix path) >> pure sock)
    `catch` \(e :: IOError) ->
      if ioeGetErrorType e == ResourceExhausted
        then do
          close sock
          threadDelay 300000 -- 300ms
          connectWithRetry (n - 1) path
        else do
          close sock
          ioError e

-- | Close a socket, ignoring errors (socket may already be closed).
closeSafe :: Socket -> IO ()
closeSafe sock = close sock `catch` \(_ :: SomeException) -> pure ()

-- | Close a Maybe Socket.
closeMaybe :: Maybe Socket -> IO ()
closeMaybe Nothing = pure ()
closeMaybe (Just s) = closeSafe s

-- | Close and forget the persistent QGA connection for a VM.
--
-- Idempotent: no-op if no entry exists. Drops the map entry FIRST
-- so a concurrent caller that arrives mid-stop can't @putMVar@ a
-- new socket back into a map slot we're about to throw away — it
-- instead enters @getOrCreateConn@'s slow path and synthesises a
-- fresh entry, which is what we want anyway because the VM is
-- going down.
--
-- Must be called from every code path that retires a VM
-- (graceful stop, hard stop, post-mortem cleanup) — otherwise the
-- cached socket fd lives forever inside the MVar that no later
-- caller will ever take, and the agent's fd table grows by one
-- per VM lifetime until @EMFILE@ kills the process.
releaseConn :: GuestAgentConns -> Int64 -> IO ()
releaseConn connsVar vmId = do
  mConn <- atomically $ do
    conns <- readTVar connsVar
    case Map.lookup vmId conns of
      Nothing -> pure Nothing
      Just c -> do
        writeTVar connsVar (Map.delete vmId conns)
        pure (Just c)
  case mConn of
    Nothing -> pure ()
    Just connVar -> do
      -- Drain the MVar. By the time the stop handlers call this,
      -- any in-flight QGA caller has either finished (and put
      -- @Just sock@ back) or hit an exception against the dying
      -- socket (and put @Nothing@ back). 'takeMVar' is the
      -- minimum waiting we can do without leaving the socket
      -- behind on a tight race.
      mSock <- takeMVar connVar
      closeMaybe mSock

--------------------------------------------------------------------------------
-- Public commands
--------------------------------------------------------------------------------

-- | Execute a command inside the guest via the QEMU Guest Agent.
-- Detects the guest OS and uses the appropriate shell:
-- Linux/BSD: /bin/sh -c, Windows: cmd.exe /c.
-- Default 60 s poll budget (600 ticks × 100 ms); use
-- 'guestExecWithTimeout' for longer-running commands.
guestExec :: GuestAgentConns -> QemuConfig -> Int64 -> Text -> IO GuestExecResult
guestExec conns config vmId command = guestExecImpl conns config vmId command Nothing 600

-- | Like 'guestExec' but with a caller-supplied poll budget (in
-- 100 ms ticks). Used by 'Corvus.Node.Caps.Session.handleVmGuestExec'
-- so the daemon-supplied @vmGuestExecReq.timeoutSec@ is honoured.
guestExecWithTimeout
  :: GuestAgentConns
  -> QemuConfig
  -> Int64
  -> Text
  -> Int
  -- ^ poll timeout, in 100 ms ticks
  -> IO GuestExecResult
guestExecWithTimeout conns config vmId command =
  guestExecImpl conns config vmId command Nothing

-- | Like 'guestExec' but also pipes raw bytes onto the guest process's stdin
-- and accepts a custom poll-timeout (in 100 ms ticks; 600 = ~60 s).
-- Suited for file uploads (@printf %s | base64 -d > /path@) and for builds
-- where individual provisioners may run for many minutes.
guestExecWithStdin
  :: GuestAgentConns
  -> QemuConfig
  -> Int64
  -> Text
  -- ^ command body (passed as the shell's @-c@ argument)
  -> BS.ByteString
  -- ^ raw stdin bytes (will be base64-encoded for QGA transport)
  -> Int
  -- ^ poll timeout, in 100 ms ticks
  -> IO GuestExecResult
guestExecWithStdin conns config vmId command stdinBs =
  guestExecImpl conns config vmId command (Just stdinBs)

-- | Split a buffer on @\\n@ boundaries: returns the list of complete
-- lines (without trailing @\\n@) and the trailing partial line (which
-- may be empty if the buffer ends in @\\n@). Exposed for tests.
splitLines :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitLines bs =
  let nl = fromIntegral (fromEnum '\n') :: Word8
      pieces = BS.split nl bs
   in case reverse pieces of
        [] -> ([], BS.empty)
        (lastPiece : rest) -> (reverse rest, lastPiece)

-- | Callback fed each non-empty stdout / stderr chunk that QGA's
-- @guest-exec-status@ returns. The chunk is the raw bytes QGA
-- buffered since the previous status call. Returning quickly is
-- important: the poll loop runs synchronously on the QGA socket,
-- so a slow callback delays the next status fetch.
type ChunkSink = BS.ByteString -> IO ()

-- | Drop-on-the-floor sink. Used by the non-streaming wrappers
-- when they accumulate the full output into 'GuestExecSuccess'
-- via 'IORef's instead.
discardChunk :: ChunkSink
discardChunk _ = pure ()

guestExecImpl
  :: GuestAgentConns
  -> QemuConfig
  -> Int64
  -> Text
  -> Maybe BS.ByteString
  -> Int
  -> IO GuestExecResult
guestExecImpl conns config vmId command mStdin maxPolls = do
  -- Aggregating path: accumulate every chunk pollStatus emits into
  -- per-stream IORefs so the returned 'GuestExecSuccess' carries
  -- the complete output, matching the pre-streaming contract.
  outRef <- newIORef BS.empty
  errRef <- newIORef BS.empty
  let onOut bs = modifyIORef' outRef (<> bs)
      onErr bs = modifyIORef' errRef (<> bs)
  result <-
    runGuestExecWithSinks conns config vmId command mStdin maxPolls onOut onErr
  case result of
    GuestExecSuccess code _ _ -> do
      outBs <- readIORef outRef
      errBs <- readIORef errRef
      pure $
        GuestExecSuccess
          code
          (decodeUtf8With lenientDecode outBs)
          (decodeUtf8With lenientDecode errBs)
    other -> pure other

-- | Streaming variant: stdout AND stderr (merged via shell
-- @2>&1@ redirection) flow through @onOut@ chunk-by-chunk as
-- the guest emits them. @onErr@ is unused by this path — the
-- shell wrapper merges both streams so the consumer sees a
-- single ordered stream, matching the pre-Phase-4 behaviour
-- ('crv build' output was always combined).
--
-- Implementation: redirect the user's command into a guest-side
-- log file (@/tmp/.corvus-build-step.log@), tail it via QGA's
-- @guest-file-read@ on the same polling cadence as the exec
-- status. QGA's @guest-exec-status@ buffers all captured output
-- and only returns it on @exited=true@ in modern qemu-ga
-- builds, so polling it for live data is futile; the log file
-- gives an authoritative byte-stream the agent can drain
-- incrementally.
--
-- Linux/BSD only — the shell-redirection trick uses POSIX
-- @exec >>file 2>&1@, which has no @cmd.exe@ analogue. On
-- Windows guests this falls back to the non-streaming
-- aggregator (output appears all at once at step end), same
-- degraded UX as 'guestExec'.
guestExecStream
  :: GuestAgentConns
  -> QemuConfig
  -> Int64
  -> Text
  -> Maybe BS.ByteString
  -> Int
  -> ChunkSink
  -- ^ stdout + stderr (merged) chunks
  -> ChunkSink
  -- ^ stderr chunks (unused on POSIX; populated only on Windows fallback)
  -> IO GuestExecResult
guestExecStream conns config vmId command mStdin maxPolls onOut onErr = do
  let pollConnTimeoutMicros = max 15000000 (maxPolls * 100000 + 30000000)
  -- Phase 1 (dispatch, retries=5): detect the guest shell and send
  -- the @guest-exec@ command. On POSIX guests we also open a
  -- per-step log file for the streaming tail; on Windows we fall
  -- back to the aggregating @pollStatus@ path. The result carries
  -- the dispatched pid plus (POSIX) the log read handle.
  eDispatch <- withPersistentConn conns config vmId 5 30000000 $ \sock -> do
    (shellPath, shellArgs) <- detectGuestShell sock
    if shellPath == "cmd.exe"
      then dispatchWindowsExec sock shellPath shellArgs command mStdin
      else dispatchLogTailExec sock shellPath shellArgs command mStdin
  case eDispatch of
    Left err -> pure $ GuestExecConnectionFailed err
    Right (Left e) -> pure $ GuestExecError e
    Right (Right disp) -> do
      -- Phase 2 (poll, retries=1): wait for the dispatched process
      -- to exit, streaming output along the way. Per-recv timeouts
      -- inside 'pollStatus' / 'pollWithLogTail' / 'guestFileRead'
      -- guard against a vanished QGA; on a timeout the action
      -- throws and the retries=1 budget surfaces it to the caller
      -- without re-dispatching the (already-running) command.
      ePoll <- withPersistentConn conns config vmId 1 pollConnTimeoutMicros $ \sock ->
        case disp of
          DispatchedWindows pid ->
            pollStatus sock pid 0 maxPolls onOut onErr
          DispatchedPosix pid logHandle -> do
            r <- pollWithLogTail sock pid logHandle 0 maxPolls onOut
            _ <- guestFileClose sock logHandle
            pure r
      case ePoll of
        Left err -> pure $ GuestExecConnectionFailed err
        Right r -> pure r

-- | Outcome of the dispatch phase of 'guestExecStream'.
data DispatchedExec
  = -- | POSIX: pid of the wrapped command + open log read handle.
    DispatchedPosix !Int !Int
  | -- | Windows: pid of the wrapped command (no log tail).
    DispatchedWindows !Int

-- | Dispatch the @guest-exec@ on a POSIX guest with an output log
-- file. Returns @Left@ on QGA-side parse failure or file-open error
-- (no retry-worthy condition); @Right (DispatchedPosix pid handle)@
-- on success. The handle is owned by the caller and must be closed
-- once the poll loop finishes.
dispatchLogTailExec
  :: Socket
  -> Text
  -> [Text]
  -> Text
  -> Maybe BS.ByteString
  -> IO (Either Text DispatchedExec)
dispatchLogTailExec sock shellPath shellArgs command mStdin = do
  let logPath = "/tmp/.corvus-build-step.log" :: Text
  -- Open the log handle for reading. The shell wrapper truncates
  -- and writes to the SAME path via fs (not via this handle), so
  -- "r" is enough — the handle just needs to point at the file
  -- the writer is appending to. Open BEFORE exec so the file
  -- exists from the start; the @: >'<path>'@ truncation in the
  -- shell wrapper handles the existing-file case.
  --
  -- Truncate-and-open dance: we issue a write open first (mode
  -- @w+@) to truncate any leftover from a previous step, then
  -- close that and re-open for read. Doing the truncate via the
  -- shell wrapper would race the agent's first read.
  truncResult <- guestFileOpenForTruncate sock logPath
  case truncResult of
    Left e -> pure $ Left ("guest-file-open(truncate): " <> e)
    Right truncHandle -> do
      _ <- guestFileClose sock truncHandle
      readResult <- guestFileOpenForRead sock logPath
      case readResult of
        Left e -> pure $ Left ("guest-file-open(read): " <> e)
        Right readHandle -> do
          let wrappedBody =
                "exec >>" <> shellQuote logPath <> " 2>&1\n" <> command
              stdinField = case mStdin of
                Nothing -> []
                Just bs -> ["input-data" .= decodeUtf8 (B64.encode bs)]
              execCmd =
                Aeson.object
                  [ "execute" .= ("guest-exec" :: Text)
                  , "arguments"
                      .= Aeson.object
                        ( [ "path" .= shellPath
                          , "arg" .= (shellArgs ++ [wrappedBody])
                          , "capture-output" .= True
                          ]
                            ++ stdinField
                        )
                  ]
          sendJson sock execCmd
          execResp <- recvJson sock
          case parsePid execResp of
            Nothing -> do
              _ <- guestFileClose sock readHandle
              pure $
                Left
                  ("Failed to parse guest-exec response: " <> T.pack (show execResp))
            Just pid -> pure $ Right (DispatchedPosix pid readHandle)

-- | Dispatch the @guest-exec@ on a Windows guest (no log file —
-- @pollStatus@ aggregates output and emits it at process exit).
dispatchWindowsExec
  :: Socket
  -> Text
  -> [Text]
  -> Text
  -> Maybe BS.ByteString
  -> IO (Either Text DispatchedExec)
dispatchWindowsExec sock shellPath shellArgs command mStdin = do
  let stdinField = case mStdin of
        Nothing -> []
        Just bs -> ["input-data" .= decodeUtf8 (B64.encode bs)]
      execCmd =
        Aeson.object
          [ "execute" .= ("guest-exec" :: Text)
          , "arguments"
              .= Aeson.object
                ( [ "path" .= shellPath
                  , "arg" .= (shellArgs ++ [command])
                  , "capture-output" .= True
                  ]
                    ++ stdinField
                )
          ]
  sendJson sock execCmd
  execResp <- recvJson sock
  case parsePid execResp of
    Nothing ->
      pure $
        Left ("Failed to parse guest-exec response: " <> T.pack (show execResp))
    Just pid -> pure $ Right (DispatchedWindows pid)

-- | One poll cycle: drain any new bytes from the log file, then
-- check whether the guest process exited. On exit, do a final
-- drain and return the exit code.
pollWithLogTail
  :: Socket
  -> Int
  -- ^ pid of the wrapped guest process
  -> Int
  -- ^ guest file handle for the log
  -> Int
  -- ^ current attempt
  -> Int
  -- ^ max attempts
  -> ChunkSink
  -> IO GuestExecResult
pollWithLogTail sock pid logHandle attempts maxAttempts onOut
  | attempts > maxAttempts =
      pure $
        GuestExecError "guest-exec timed out waiting for process to exit"
  | otherwise = do
      drainLog sock logHandle onOut
      let statusCmd =
            Aeson.object
              [ "execute" .= ("guest-exec-status" :: Text)
              , "arguments" .= Aeson.object ["pid" .= pid]
              ]
      sendJson sock statusCmd
      -- Per-recv timeout: see 'pollStatus' for the rationale. Throws
      -- via 'userError' so the outer 'withPersistentConn' (called
      -- with retries=1 for the poll phase) closes the socket and
      -- surfaces a clear "agent vanished" error to the caller.
      mStatusResp <- timeout pollRecvTimeoutMicros (recvJson sock)
      statusResp <- case mStatusResp of
        Nothing -> ioError (userError "guest agent stopped responding mid-exec")
        Just r -> pure r
      case parseExecStatus statusResp of
        Just (True, exitcode, _, _) -> do
          -- Drain anything written between our drainLog above
          -- and the process actually exiting (guest scheduler
          -- gap, final newline, etc.).
          drainLog sock logHandle onOut
          pure $ GuestExecSuccess exitcode T.empty T.empty
        Just (False, _, _, _) -> do
          threadDelay 100000 -- 100 ms
          pollWithLogTail sock pid logHandle (attempts + 1) maxAttempts onOut
        Nothing ->
          pure $ GuestExecError "Failed to parse guest-exec-status response"

-- | Read until EOF from a guest file handle and emit ONE
-- consolidated chunk to @onOut@. EOF here means "no more data
-- currently buffered" — calling 'drainLog' again later picks
-- up new bytes the guest wrote in between.
--
-- Why consolidate: each @onOut@ call is a separate Cap'n Proto
-- RPC from the agent back to the daemon. Pushing each 64 KiB
-- read individually multiplies the RPC count and starves the
-- daemon's @availableCallWords@ budget under bursty output
-- (emerge can spew MB/s during package install). One push per
-- drain cycle keeps the wire chatty enough to feel live but
-- bounded enough to never back up.
drainLog :: Socket -> Int -> ChunkSink -> IO ()
drainLog sock logHandle onOut = do
  bytes <- collect BS.empty
  unless (BS.null bytes) (onOut bytes)
  where
    collect acc = do
      r <- guestFileRead sock logHandle 65536
      case r of
        Left _ -> pure acc -- swallow transient read errors
        Right (bs, eof) -> do
          let acc' = acc <> bs
          if eof then pure acc' else collect acc'

-- | Open a guest file via QGA with mode @w+@, which truncates
-- existing files and grants read+write access. The handle we
-- get back can be discarded (we use it just to clear the file);
-- the shell wrapper then appends from scratch via the path.
guestFileOpenForTruncate :: Socket -> Text -> IO (Either Text Int)
guestFileOpenForTruncate = guestFileOpenMode "w+"

-- | Open a guest file via QGA for reading only.
guestFileOpenForRead :: Socket -> Text -> IO (Either Text Int)
guestFileOpenForRead = guestFileOpenMode "r"

guestFileOpenMode :: Text -> Socket -> Text -> IO (Either Text Int)
guestFileOpenMode mode sock path = do
  sendJson sock $
    Aeson.object
      [ "execute" .= ("guest-file-open" :: Text)
      , "arguments"
          .= Aeson.object
            [ "path" .= path
            , "mode" .= mode
            ]
      ]
  resp <- recvJson sock
  case parseHandle resp of
    Just h -> pure (Right h)
    Nothing ->
      pure $ Left ("guest-file-open: " <> T.pack (show resp))
  where
    parseHandle mVal = do
      val <- mVal
      AT.parseMaybe
        (AT.withObject "resp" $ \obj -> obj .: "return")
        val

-- | Read up to @count@ bytes from a guest file handle. Returns
-- @(bytes, eof)@ where eof signals "no more data currently".
guestFileRead :: Socket -> Int -> Int -> IO (Either Text (BS.ByteString, Bool))
guestFileRead sock handle count = do
  sendJson sock $
    Aeson.object
      [ "execute" .= ("guest-file-read" :: Text)
      , "arguments"
          .= Aeson.object
            [ "handle" .= handle
            , "count" .= count
            ]
      ]
  -- Per-recv timeout: 'drainLog' calls this inside the streaming
  -- poll loop, and we don't want a vanished QGA to leave the
  -- drain blocked. A timeout fires 'userError' so the enclosing
  -- 'withPersistentConn' (called with retries=1 in the poll phase
  -- of 'guestExecStream') closes the socket and returns to the
  -- caller; matches the behaviour in 'pollWithLogTail'.
  mResp <- timeout pollRecvTimeoutMicros (recvJson sock)
  resp <- case mResp of
    Nothing -> ioError (userError "guest agent stopped responding mid-exec")
    Just r -> pure r
  case parseRead resp of
    Just (bs, eof) -> pure (Right (bs, eof))
    Nothing -> pure $ Left ("guest-file-read: " <> T.pack (show resp))
  where
    parseRead mVal = do
      val <- mVal
      AT.parseMaybe
        ( AT.withObject "resp" $ \obj -> do
            ret <- obj .: "return"
            bufB64 <- ret .:? "buf-b64" AT..!= ""
            eof <- ret .:? "eof" AT..!= False
            pure (decodeBase64Bytes bufB64, eof)
        )
        val

-- | Close a guest file handle. Best-effort; ignores errors.
guestFileClose :: Socket -> Int -> IO (Either Text ())
guestFileClose sock handle = do
  sendJson sock $
    Aeson.object
      [ "execute" .= ("guest-file-close" :: Text)
      , "arguments" .= Aeson.object ["handle" .= handle]
      ]
  _ <- recvJson sock
  pure (Right ())

-- | POSIX-shell single-quoting. Wraps in @'…'@ and replaces
-- embedded @'@ with @'\\''@. Used to interpolate the log path
-- into the @exec >>… 2>&1@ wrapper.
shellQuote :: Text -> Text
shellQuote t = "'" <> T.replace "'" "'\\''" t <> "'"

-- | Internal: open the persistent connection, dispatch
-- @guest-exec@, hand off to 'pollStatus' with the supplied
-- callbacks. The aggregating and streaming wrappers share this
-- body to keep the @guest-exec@ + OS-detection plumbing in one
-- place; only the callback shape differs.
runGuestExecWithSinks
  :: GuestAgentConns
  -> QemuConfig
  -> Int64
  -> Text
  -> Maybe BS.ByteString
  -> Int
  -> ChunkSink
  -> ChunkSink
  -> IO GuestExecResult
runGuestExecWithSinks conns config vmId command mStdin maxPolls onOut onErr = do
  -- Split into two 'withPersistentConn' phases:
  --
  --   * Phase 1 (dispatch) — retries=5, 30 s per attempt.  Covers
  --     the connect + sync handshake, OS detection, and the initial
  --     @guest-exec@ round-trip.  Retries here are safe (re-dispatch
  --     is harmless before the command has been spawned).
  --   * Phase 2 (poll) — retries=1, with a per-recv timeout inside
  --     'pollStatus'.  Retries here would *re-execute* the command
  --     (think @reboot -f@), so the poll loop instead reports
  --     "agent stopped responding" and surfaces it to the caller
  --     unchanged.
  --
  -- The socket is shared between phases via 'withPersistentConn''s
  -- 'connVar' cache: Phase 1 leaves a healthy socket; Phase 2 picks
  -- it up without reconnecting.
  let pollConnTimeoutMicros = max 15000000 (maxPolls * 100000 + 30000000)
  eDispatch <- withPersistentConn conns config vmId 5 30000000 $ \sock -> do
    (shellPath, shellArgs) <- detectGuestShell sock
    let stdinField = case mStdin of
          Nothing -> []
          Just bs -> ["input-data" .= decodeUtf8 (B64.encode bs)]
        execCmd =
          Aeson.object
            [ "execute" .= ("guest-exec" :: Text)
            , "arguments"
                .= Aeson.object
                  ( [ "path" .= shellPath
                    , "arg" .= (shellArgs ++ [command])
                    , "capture-output" .= True
                    ]
                      ++ stdinField
                  )
            ]
    sendJson sock execCmd
    execResp <- recvJson sock
    case parsePid execResp of
      Nothing -> pure (Left ("Failed to parse guest-exec response: " <> T.pack (show execResp)))
      Just pid -> pure (Right pid)
  case eDispatch of
    Left err -> pure $ GuestExecConnectionFailed err
    Right (Left parseErr) -> pure $ GuestExecError parseErr
    Right (Right pid) -> do
      ePoll <- withPersistentConn conns config vmId 1 pollConnTimeoutMicros $ \sock ->
        pollStatus sock pid 0 maxPolls onOut onErr
      case ePoll of
        Left err -> pure $ GuestExecConnectionFailed err
        Right r -> pure r

-- | Ping the guest agent to check if it's available.
-- Single attempt: the caller (usually a polling loop) retries on its own
-- cadence, so there's no point multiplying the per-call cost with inner
-- retries. The 15 s per-attempt budget matches the other commands and is
-- enough to accommodate the first @guest-sync@ handshake on slow guests
-- (FreeBSD/Gentoo qemu-ga startup can take several seconds) — a shorter
-- budget risks timing out mid-sync, closing the socket, and losing the
-- reply that was about to arrive.
guestPing :: GuestAgentConns -> QemuConfig -> Int64 -> IO Bool
guestPing conns config vmId = do
  mResult <- withPersistentConn conns config vmId 1 15000000 $ \sock -> do
    sendJson sock $ Aeson.object ["execute" .= ("guest-ping" :: Text)]
    -- Per-recv timeout: ping is the healthcheck poller's signal of
    -- "agent reachable", so we want a snappy "no" when QGA has
    -- vanished — bounded by 'pollRecvTimeoutMicros' instead of the
    -- outer 15 s connection timeout. 'recvJsonWithin' THROWS on
    -- timeout so 'withPersistentConn' closes the socket; the
    -- alternative ("return Nothing, put socket back") would leak
    -- whatever response eventually arrives into the NEXT QGA call
    -- and produce a shape mismatch there.
    mResp <- recvJsonWithin pollRecvTimeoutMicros sock "guest-ping"
    case mResp of
      Just (Object obj) -> pure $ KM.member "return" obj
      _ -> pure False
  case mResult of
    Left _ -> pure False
    Right r -> pure r

-- | Request a graceful shutdown via the guest agent.
-- This triggers a clean shutdown from inside the guest (like running "poweroff").
-- Returns True if the command was accepted, False on error.
guestShutdown :: GuestAgentConns -> QemuConfig -> Int64 -> IO Bool
guestShutdown conns config vmId = do
  mResult <- withPersistentConn conns config vmId 3 15000000 $ \sock -> do
    -- guest-shutdown with mode "powerdown" triggers a clean OS shutdown
    sendJson sock $
      Aeson.object
        [ "execute" .= ("guest-shutdown" :: Text)
        , "arguments" .= Aeson.object ["mode" .= ("powerdown" :: Text)]
        ]
    -- guest-shutdown doesn't return a response on success (the agent shuts down),
    -- so a timeout here is expected and means success.
    -- If it does respond, it's an error.
    resp <- timeout 3000000 $ recvJson sock
    case resp of
      Just (Just (Object obj)) -> pure $ not $ KM.member "error" obj
      _ -> pure True
  case mResult of
    Left _ -> pure True
    Right r -> pure r

-- | Freeze all writable guest filesystems that support fsfreeze
-- (ext4, xfs, btrfs, ntfs on modern Windows…). QEMU's QGA flushes
-- pending writes through to the disk and then blocks further
-- writes until 'guestFsThaw' is called.
--
-- Returns the number of filesystems successfully frozen on
-- success, or @Left@ on transport / QGA error. A return of @0@
-- in a guest that has writable filesystems means none of them
-- support fsfreeze — the caller decides whether that's an error
-- (per 'QuiesceMode').
--
-- This RPC takes a one-shot 10s connection budget. Freeze on a
-- guest with many filesystems can take several seconds to flush
-- caches; the existing 'guestExec' helpers use the same window.
--
-- CRITICAL: every call site MUST guarantee 'guestFsThaw' runs
-- afterward (use 'Control.Exception.bracket'-style finalisation)
-- — leaving the guest frozen indefinitely on the error path
-- wedges in-guest I/O.
guestFsFreeze :: GuestAgentConns -> QemuConfig -> Int64 -> IO (Either Text Int)
guestFsFreeze conns config vmId = do
  mResult <- withPersistentConn conns config vmId 1 10000000 $ \sock -> do
    sendJson sock $ Aeson.object ["execute" .= ("guest-fsfreeze-freeze" :: Text)]
    mResp <- recvJsonWithin 10000000 sock "guest-fsfreeze-freeze"
    pure $ case mResp of
      Just (Object obj) ->
        case KM.lookup "return" obj of
          Just (Number n) -> Right (truncate n :: Int)
          _ -> Left (qgaErrText obj)
      _ -> Left "guest-fsfreeze-freeze: malformed reply"
  pure $ case mResult of
    Left e -> Left (T.pack (show e))
    Right r -> r

-- | Thaw guest filesystems previously frozen by 'guestFsFreeze'.
-- QGA's @guest-fsfreeze-thaw@ is idempotent — thawing a guest
-- that wasn't frozen returns 0 without error, so this is safe to
-- call unconditionally on the error path of a snapshot attempt.
--
-- Returns the number of thawed filesystems on success, @Left@ on
-- transport / QGA error.
guestFsThaw :: GuestAgentConns -> QemuConfig -> Int64 -> IO (Either Text Int)
guestFsThaw conns config vmId = do
  mResult <- withPersistentConn conns config vmId 1 10000000 $ \sock -> do
    sendJson sock $ Aeson.object ["execute" .= ("guest-fsfreeze-thaw" :: Text)]
    mResp <- recvJsonWithin 10000000 sock "guest-fsfreeze-thaw"
    pure $ case mResp of
      Just (Object obj) ->
        case KM.lookup "return" obj of
          Just (Number n) -> Right (truncate n :: Int)
          _ -> Left (qgaErrText obj)
      _ -> Left "guest-fsfreeze-thaw: malformed reply"
  pure $ case mResult of
    Left e -> Left (T.pack (show e))
    Right r -> r

-- | Tell QGA to resync the guest's wall clock from the host's
-- hardware clock. @guest-set-time@ with no @time@ argument asks
-- the agent to read the RTC and call @settimeofday()@; subsequent
-- calls to @gettimeofday()@ in the guest reflect host wall-clock
-- time.
--
-- Used after a vmstate restore: the restored guest thinks it's
-- still snapshot-time, which breaks anything time-sensitive
-- (cert validation, build mtime comparisons, NTP). Best-effort —
-- @Left@ when QGA isn't reachable (the caller logs at WARN and
-- continues; the snapshot restore itself isn't undone for a
-- clock-resync miss).
guestSetTime :: GuestAgentConns -> QemuConfig -> Int64 -> IO (Either Text ())
guestSetTime conns config vmId = do
  mResult <- withPersistentConn conns config vmId 1 10000000 $ \sock -> do
    sendJson sock $ Aeson.object ["execute" .= ("guest-set-time" :: Text)]
    mResp <- recvJsonWithin 10000000 sock "guest-set-time"
    pure $ case mResp of
      Just (Object obj) ->
        case KM.lookup "return" obj of
          Just _ -> Right ()
          Nothing -> Left (qgaErrText obj)
      _ -> Left "guest-set-time: malformed reply"
  pure $ case mResult of
    Left e -> Left (T.pack (show e))
    Right r -> r

-- | Pull a human-readable error string out of a QGA error reply.
-- QGA shape: @{"error": {"class": "...", "desc": "..."}}@.
qgaErrText :: AT.Object -> Text
qgaErrText obj = case KM.lookup "error" obj of
  Just (Object err) ->
    let desc = KM.lookup "desc" err
        cls = KM.lookup "class" err
        textOf (Just (String s)) = s
        textOf _ = ""
        d = textOf desc
        c = textOf cls
     in if not (T.null c) && not (T.null d)
          then c <> ": " <> d
          else
            if not (T.null d)
              then d
              else "QGA error (no description)"
  _ -> "QGA reply lacks both 'return' and 'error' fields"

-- | Query network interfaces from the guest via the QEMU Guest Agent.
-- Returns Nothing on failure, Just [] if no interfaces reported.
guestNetworkGetInterfaces :: GuestAgentConns -> QemuConfig -> Int64 -> IO (Maybe [GuestNetIf])
guestNetworkGetInterfaces conns config vmId = do
  mResult <- withPersistentConn conns config vmId 5 15000000 $ \sock -> do
    sendJson sock $ Aeson.object ["execute" .= ("guest-network-get-interfaces" :: Text)]
    resp <- recvJson sock
    pure $ parseGuestInterfaces resp
  case mResult of
    Left _ -> pure Nothing
    Right r -> pure r

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

-- | Synchronize with the guest agent.
-- Each new connection requires a guest-sync handshake before commands will work.
-- Drains any stale responses until the sync response (matching ID) arrives.
-- Uses bounded random IDs (0 to 2^30) to avoid floating-point precision issues
-- in JSON parsers that use double internally.
syncGuest :: Socket -> IO ()
syncGuest sock = do
  -- Use bounded range to avoid precision loss in JSON number handling
  syncId <- randomRIO (1, 1073741823 :: Int) -- 2^30 - 1
  let syncCmd =
        Aeson.object
          [ "execute" .= ("guest-sync" :: Text)
          , "arguments" .= Aeson.object ["id" .= syncId]
          ]
  sendJson sock syncCmd
  waitForSync syncId 10
  where
    waitForSync _ 0 = pure ()
    waitForSync expected n = do
      resp <- recvJson sock
      case resp of
        Just (Object obj)
          | KM.lookup "return" obj == Just (Number (fromIntegral expected)) -> pure ()
        _ -> waitForSync expected (n - 1 :: Int)

-- | Send a JSON value over the socket
sendJson :: Socket -> Value -> IO ()
sendJson sock val = sendAll sock (BL.toStrict (Aeson.encode val) <> "\n")

-- | Read a JSON response from the socket.
-- QGA sends newline-delimited JSON. Keep @recv@-ing and accumulating bytes
-- until at least one @\\n@ is seen (i.e. at least one complete message has
-- arrived), then parse the first parseable line. Strips @\\xff@ bytes (QGA
-- framing delimiter used by @guest-sync-delimited@ responses that may be
-- left in the buffer).
--
-- Looping is essential: QEMU's chardev backend forwards data from
-- virtio-serial as it arrives and is not guaranteed to deliver a whole
-- response in one socket write. Larger responses (e.g.
-- @guest-network-get-interfaces@, which runs several hundred bytes) can
-- arrive across multiple @recv@ calls. Reading only the first chunk and
-- trying to parse it fails the JSON decode, drops the in-flight reply, and
-- leaves the tail in the socket buffer where it desynchronises every
-- subsequent command-response cycle.
--
-- Cancellation is handled by the outer @timeout@ wrapper in
-- 'withPersistentConn'; the async exception interrupts the blocking @recv@.
-- | 'recvJson' bounded by @micros@. On timeout, THROWS rather
-- than returning 'Nothing', so 'withPersistentConn's onException
-- closes the socket instead of putting it back into the MVar with
-- the unread response still in the kernel buffer. Without this,
-- the NEXT caller's 'recvJson' would read our stale response,
-- parse it as its own reply, and report a shape mismatch (the
-- "guest-file-open got a guest-get-osinfo reply" cross-talk bug).
--
-- @callLabel@ is woven into the exception text so the eventual
-- @Left "user error (…): no reply within Xs"@ at the
-- 'withPersistentConn' boundary names the call that timed out.
recvJsonWithin :: Int -> Socket -> Text -> IO (Maybe Value)
recvJsonWithin micros sock callLabel = do
  mResp <- timeout micros (recvJson sock)
  case mResp of
    Just v -> pure v
    Nothing ->
      ioError $
        userError $
          T.unpack callLabel
            <> ": no reply within "
            <> show (micros `div` 1000000)
            <> "s"

recvJson :: Socket -> IO (Maybe Value)
recvJson sock = go BS.empty
  where
    nlByte = fromIntegral (fromEnum '\n') :: Word8

    go acc = do
      chunk <- recv sock 65536
      if BS.null chunk
        then
          if BS.null acc
            then pure Nothing
            else pure $ parseAcc acc
        else
          let acc' = acc <> chunk
           in if BS.elem nlByte acc'
                then pure $ parseAcc acc'
                else go acc'

    parseAcc bs =
      let cleaned = BS.filter (/= 0xFF) bs
          lines' = filter (not . BS.null) $ BS.split nlByte cleaned
       in firstParse lines'

    firstParse [] = Nothing
    firstParse (l : ls) = case Aeson.decodeStrict l of
      Just v -> Just v
      Nothing -> firstParse ls

-- | Detect the guest OS shell by querying guest-get-osinfo.
-- Returns (shell path, shell args) for the detected OS.
-- Defaults to /bin/sh -c if detection fails.
detectGuestShell :: Socket -> IO (Text, [Text])
detectGuestShell sock = do
  sendJson sock $ Aeson.object ["execute" .= ("guest-get-osinfo" :: Text)]
  resp <- recvJson sock
  case parseOsId resp of
    Just osId
      | "mswindows" `T.isPrefixOf` osId -> pure ("cmd.exe", ["/c"])
    _ -> pure ("/bin/sh", ["-c"])
  where
    parseOsId mVal = do
      val <- mVal
      AT.parseMaybe
        ( AT.withObject "response" $ \obj -> do
            ret <- obj .: "return"
            ret .:? "id" AT..!= ("" :: Text)
        )
        val

-- | Extract PID from guest-exec response: {"return": {"pid": N}}
parsePid :: Maybe Value -> Maybe Int
parsePid mVal = do
  val <- mVal
  AT.parseMaybe pidParser val
  where
    pidParser = AT.withObject "response" $ \obj -> do
      ret <- obj .: "return"
      ret .: "pid"

-- | Poll guest-exec-status until the process exits.
--
-- Each non-empty @out-data@ / @err-data@ field the QGA returns —
-- whether on an intermediate (exited=False) or final
-- (exited=True) response — is forwarded to @onOut@ / @onErr@ as
-- soon as it's read off the wire. QGA semantics: each
-- @guest-exec-status@ call drains the agent-side buffer, so
-- successive calls return successive chunks of the guest
-- process's output. Callers that need to stream output
-- (e.g. the build pipeline) wire the callbacks to a sink;
-- callers that just want the aggregate accumulate to IORefs.
--
-- @maxAttempts@ is the budget in 100 ms ticks; the previous
-- default of 600 (~60 s) is preserved by 'guestExec'. Long-
-- running provisioner steps in builds raise this to several
-- minutes.
pollStatus
  :: Socket
  -> Int
  -- ^ pid
  -> Int
  -- ^ current attempt
  -> Int
  -- ^ max attempts
  -> ChunkSink
  -- ^ stdout chunks (raw bytes from QGA)
  -> ChunkSink
  -- ^ stderr chunks (raw bytes from QGA)
  -> IO GuestExecResult
pollStatus sock pid attempts maxAttempts onOut onErr
  | attempts > maxAttempts = pure $ GuestExecError "guest-exec timed out waiting for process to exit"
  | otherwise = do
      let statusCmd =
            Aeson.object
              [ "execute" .= ("guest-exec-status" :: Text)
              , "arguments" .= Aeson.object ["pid" .= pid]
              ]
      sendJson sock statusCmd
      -- Per-recv timeout. QEMU keeps the chardev open across a guest
      -- reboot / qemu-ga crash, so recvJson would otherwise block
      -- until the outer 'withPersistentConn' timeout fires (~90 s).
      -- A timeout here throws via 'userError'; 'withPersistentConn'
      -- treats that as a connection failure, closes the socket, and
      -- — since the caller passes retries=1 for the poll phase —
      -- propagates a Left to the caller without re-dispatching the
      -- (already-executed) guest-exec command.
      mStatusResp <- timeout pollRecvTimeoutMicros (recvJson sock)
      case mStatusResp of
        Nothing -> ioError (userError "guest agent stopped responding mid-exec")
        Just statusResp -> case parseExecStatus statusResp of
          Just (exited, exitcode, outBs, errBs) -> do
            unless (BS.null outBs) (onOut outBs)
            unless (BS.null errBs) (onErr errBs)
            if exited
              then pure $ GuestExecSuccess exitcode T.empty T.empty
              else do
                threadDelay 100000 -- 100ms
                pollStatus sock pid (attempts + 1) maxAttempts onOut onErr
          Nothing -> pure $ GuestExecError "Failed to parse guest-exec-status response"

-- | Parse a @guest-exec-status@ response.
--
-- Returns @(exited, exitcode, stdout, stderr)@ where the two byte
-- strings hold whatever QGA buffered since the previous status
-- call. On intermediate (exited=False) responses, the @exitcode@
-- field is undefined (we return 0 as a placeholder; callers MUST
-- check @exited@ first). On the final (exited=True) response,
-- @exitcode@ is the guest process's exit status.
parseExecStatus :: Maybe Value -> Maybe (Bool, Int, BS.ByteString, BS.ByteString)
parseExecStatus mVal = do
  val <- mVal
  AT.parseMaybe statusParser val
  where
    statusParser = AT.withObject "response" $ \obj -> do
      ret <- obj .: "return"
      exited <- ret .: "exited"
      exitcode <- ret .:? "exitcode" AT..!= 1
      outData <- ret .:? "out-data" AT..!= ""
      errData <- ret .:? "err-data" AT..!= ""
      pure (exited, exitcode, decodeBase64Bytes outData, decodeBase64Bytes errData)

-- | Parse the guest-network-get-interfaces response.
-- Expected format: {"return": [{"name": "eth0", "hardware-address": "...", "ip-addresses": [...]}]}
--
-- Tolerance policy: a malformed IP entry is dropped but other addresses on the
-- same interface are kept; an interface that fails to parse is dropped but
-- other interfaces are kept. Losing one weird interface (e.g. a loopback with
-- an unexpected field shape) must not blank the whole address list — some
-- qemu-ga builds (notably FreeBSD's) emit one such entry and would otherwise
-- cause every interface to be discarded. Interfaces without a hardware
-- address are skipped since they can't be matched to host rows.
parseGuestInterfaces :: Maybe Value -> Maybe [GuestNetIf]
parseGuestInterfaces mVal = do
  val <- mVal
  AT.parseMaybe interfacesParser val
  where
    interfacesParser = AT.withObject "response" $ \obj -> do
      ret <- obj .: "return" :: AT.Parser [Value]
      pure $ mapMaybe (AT.parseMaybe parseIface) ret

    parseIface = AT.withObject "interface" $ \obj -> do
      mHwAddr <- obj .:? "hardware-address"
      case mHwAddr of
        Nothing -> fail "no hardware-address"
        Just hwAddr -> do
          rawIps <- obj .:? "ip-addresses" AT..!= ([] :: [Value])
          let parsedIps = mapMaybe (AT.parseMaybe parseIpAddr) rawIps
          pure GuestNetIf {gniHardwareAddress = hwAddr, gniIpAddresses = parsedIps}

    parseIpAddr = AT.withObject "ip-address" $ \obj -> do
      ipType <- obj .: "ip-address-type"
      ipAddr <- obj .: "ip-address"
      prefix <- obj .:? "prefix" AT..!= 0
      pure GuestIpAddress {giaType = ipType, giaAddress = ipAddr, giaPrefix = prefix}

-- | Decode a base64-encoded text field from QGA response
decodeBase64 :: Text -> Text
decodeBase64 t
  | T.null t = ""
  | otherwise = case B64.decode (encodeUtf8 t) of
      Right decoded -> decodeUtf8With lenientDecode decoded
      Left _ -> t

-- | Like 'decodeBase64' but returns raw bytes. Used by the
-- streaming poll path so callers can choose their own decoding
-- strategy (binary-safe line buffers, UTF-8 with replacement,
-- raw forwarding to a 'ByteSink', …).
decodeBase64Bytes :: Text -> BS.ByteString
decodeBase64Bytes t
  | T.null t = BS.empty
  | otherwise = case B64.decode (encodeUtf8 t) of
      Right decoded -> decoded
      Left _ -> encodeUtf8 t
