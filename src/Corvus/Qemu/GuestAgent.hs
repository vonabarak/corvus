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
module Corvus.Qemu.GuestAgent
  ( -- * Types
    GuestExecResult (..)
  , GuestIpAddress (..)
  , GuestNetIf (..)
  , GuestOsInfo (..)

    -- * Persistent connection management
  , GuestAgentConns
  , closeGuestAgentConn

    -- * Commands
  , guestExec
  , guestExecWithStdin
  , guestExecWithTail
  , guestPing
  , guestShutdown
  , guestNetworkGetInterfaces
  , guestGetOsInfo

    -- * Internal (exposed for tests)
  , parseGuestInterfaces
  , splitLines
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Exception (SomeException, catch, mask, mask_, onException, try)
import Control.Monad (unless)
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Qemu.Runtime (getGuestAgentSocket)
import Data.Aeson (Value (..), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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

-- | Result of a guest-exec command
data GuestExecResult
  = -- | Command completed (exitcode, stdout, stderr)
    GuestExecSuccess !Int !Text !Text
  | -- | Error communicating with guest agent
    GuestExecError !Text
  | -- | Could not connect to guest agent socket
    GuestExecConnectionFailed !Text
  deriving (Eq, Show)

-- | Guest OS information returned by guest-get-osinfo
data GuestOsInfo = GuestOsInfo
  { goiKernelRelease :: !Text
  -- ^ e.g. "10.0.19041" or "6.1.0-20-amd64"
  , goiKernelVersion :: !Text
  -- ^ e.g. "#1 SMP" or "10.0"
  , goiId :: !Text
  -- ^ OS ID: "mswindows", "linux", "freebsd", etc.
  , goiMachine :: !Text
  -- ^ Machine type, e.g. "x86_64"
  }
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

-- | Close a VM's persistent guest agent connection and remove it from the map.
-- Called when a VM is stopped to clean up the socket.
closeGuestAgentConn :: GuestAgentConns -> Int64 -> IO ()
closeGuestAgentConn connsVar vmId = do
  mConn <- atomically $ do
    conns <- readTVar connsVar
    let mC = Map.lookup vmId conns
    writeTVar connsVar (Map.delete vmId conns)
    pure mC
  case mConn of
    Nothing -> pure ()
    Just connVar -> do
      mSock <- takeMVar connVar
      closeMaybe mSock
      putMVar connVar Nothing

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

--------------------------------------------------------------------------------
-- Public commands
--------------------------------------------------------------------------------

-- | Execute a command inside the guest via the QEMU Guest Agent.
-- Detects the guest OS and uses the appropriate shell:
-- Linux/BSD: /bin/sh -c, Windows: cmd.exe /c
guestExec :: GuestAgentConns -> QemuConfig -> Int64 -> Text -> IO GuestExecResult
guestExec conns config vmId command = guestExecImpl conns config vmId command Nothing 600

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

-- | Path inside the guest where build-pipeline shell provisioners
-- redirect stdout+stderr so the daemon can tail them. Single path:
-- @guestExecWithTail@ truncates at start of every step (file mode @w+@)
-- and steps run sequentially per build, so the file never serves two
-- writers at once.
buildStepLogPath :: Text
buildStepLogPath = "/tmp/.corvus-build-step.log"

-- | Like 'guestExecWithStdin' but tails the guest's stdout+stderr live
-- and feeds each line to the supplied callback as it's written.
--
-- QGA's @guest-exec-status@ only returns captured output once the
-- process exits, so we redirect inside the guest to a log file and
-- @guest-file-read@ the file in parallel with the status poll. The
-- callback is invoked once per complete line (no trailing newline).
-- Any tail without a terminating newline is flushed as a final line
-- after the process exits.
--
-- The returned 'GuestExecResult' carries the exit code from
-- @guest-exec-status@; its @stdout@ and @stderr@ fields are always the
-- empty string because the wrapper redirects them into the log file.
guestExecWithTail
  :: GuestAgentConns
  -> QemuConfig
  -> Int64
  -> Text
  -- ^ command body (passed as the shell's @-c@ argument)
  -> Int
  -- ^ poll timeout, in 100 ms ticks
  -> (Text -> IO ())
  -- ^ called once per complete line of merged stdout+stderr
  -> IO GuestExecResult
guestExecWithTail conns config vmId command maxPolls onLine = do
  let connTimeoutMicros = max 15000000 (maxPolls * 100000 + 30000000)
  mResult <- withPersistentConn conns config vmId 5 connTimeoutMicros $ \sock -> do
    -- Open the log file in mode "w+" first: this both truncates any
    -- leftover content from a previous step and gives us a read handle
    -- whose offset starts at 0. The wrapper script uses ">>" so each
    -- write extends the file past our read position.
    mHandle <- openLogFile sock buildStepLogPath
    case mHandle of
      Left err -> pure $ GuestExecError err
      Right handle -> do
        (shellPath, shellArgs) <- detectGuestShell sock
        let logPathQuoted = "'" <> T.replace "'" "'\\''" buildStepLogPath <> "'"
            wrapped =
              "exec >>"
                <> logPathQuoted
                <> " 2>&1\n"
                <> command
            execCmd =
              Aeson.object
                [ "execute" .= ("guest-exec" :: Text)
                , "arguments"
                    .= Aeson.object
                      [ "path" .= shellPath
                      , "arg" .= (shellArgs ++ [wrapped])
                      , "capture-output" .= True
                      ]
                ]
        sendJson sock execCmd
        execResp <- recvJson sock
        case parsePid execResp of
          Nothing -> do
            _ <- closeLogFile sock handle
            pure $
              GuestExecError $
                "Failed to parse guest-exec response: " <> T.pack (show execResp)
          Just pid -> do
            tailRef <- newIORef BS.empty
            let drain = drainLog sock handle tailRef onLine
            r <- pollStatusWithTail sock pid 0 maxPolls drain
            -- Final drain to capture any output written between the
            -- last poll cycle and the process exit.
            _ <- drain
            -- Flush trailing partial line (output without final \n).
            partial <- readIORef tailRef
            unless (BS.null partial) $
              onLine (decodeUtf8With lenientDecode partial)
            _ <- closeLogFile sock handle
            pure r
  case mResult of
    Left err -> pure $ GuestExecConnectionFailed err
    Right r -> pure r

-- | Open a guest file with @fopen@ mode @"w+"@ (truncate, read+write).
-- Returns the QGA file handle on success.
openLogFile :: Socket -> Text -> IO (Either Text Int)
openLogFile sock path = do
  sendJson sock $
    Aeson.object
      [ "execute" .= ("guest-file-open" :: Text)
      , "arguments" .= Aeson.object ["path" .= path, "mode" .= ("w+" :: Text)]
      ]
  resp <- recvJson sock
  case resp of
    Just (Object o) -> case KM.lookup "return" o of
      Just (Number n) -> pure $ Right (truncate (realToFrac n :: Double))
      _ -> pure $ Left $ "guest-file-open: " <> T.pack (show resp)
    _ -> pure $ Left $ "guest-file-open: " <> T.pack (show resp)

-- | Close a guest file handle. Errors are ignored — the bake VM is torn
-- down at the end of the build either way.
closeLogFile :: Socket -> Int -> IO ()
closeLogFile sock handle = do
  sendJson sock $
    Aeson.object
      [ "execute" .= ("guest-file-close" :: Text)
      , "arguments" .= Aeson.object ["handle" .= handle]
      ]
  _ <- recvJson sock
  pure ()

-- | Read up to 65536 bytes from a guest file handle. Returns the raw
-- (base64-decoded) payload. The handle's read offset advances on each
-- call; QGA returns count=0 when nothing new is available, which is
-- normal for a slow-producing process.
readLogChunk :: Socket -> Int -> IO BS.ByteString
readLogChunk sock handle = do
  sendJson sock $
    Aeson.object
      [ "execute" .= ("guest-file-read" :: Text)
      , "arguments" .= Aeson.object ["handle" .= handle, "count" .= (65536 :: Int)]
      ]
  resp <- recvJson sock
  pure $ case resp of
    Just (Object o)
      | Just (Object ret) <- KM.lookup "return" o
      , Just (String b64) <- KM.lookup "buf-b64" ret ->
          case B64.decode (encodeUtf8 b64) of
            Right bs -> bs
            Left _ -> BS.empty
    _ -> BS.empty

-- | One cycle of: read a chunk from the guest log file, append to the
-- partial-line buffer, emit any complete lines via @onLine@, retain the
-- last incomplete line in the buffer.
drainLog
  :: Socket
  -> Int
  -> IORef BS.ByteString
  -> (Text -> IO ())
  -> IO ()
drainLog sock handle tailRef onLine = do
  chunk <- readLogChunk sock handle
  unless (BS.null chunk) $ do
    buf <- readIORef tailRef
    let combined = buf <> chunk
        (complete, leftover) = splitLines combined
    writeIORef tailRef leftover
    mapM_ (onLine . decodeUtf8With lenientDecode) complete

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

-- | Like 'pollStatus' but runs an extra IO action between status polls
-- so the caller can drain the log file in lockstep.
pollStatusWithTail
  :: Socket
  -> Int
  -> Int
  -> Int
  -> IO ()
  -> IO GuestExecResult
pollStatusWithTail sock pid attempts maxAttempts drain
  | attempts > maxAttempts = pure $ GuestExecError "guest-exec timed out waiting for process to exit"
  | otherwise = do
      let statusCmd =
            Aeson.object
              [ "execute" .= ("guest-exec-status" :: Text)
              , "arguments" .= Aeson.object ["pid" .= pid]
              ]
      sendJson sock statusCmd
      statusResp <- recvJson sock
      case parseExecStatus statusResp of
        Just (True, exitcode, _, _) ->
          -- Output went via the log file, not via QGA's out-data/err-data.
          pure $ GuestExecSuccess exitcode "" ""
        Just (False, _, _, _) -> do
          drain
          threadDelay 100000 -- 100ms
          pollStatusWithTail sock pid (attempts + 1) maxAttempts drain
        Nothing -> pure $ GuestExecError "Failed to parse guest-exec-status response"

guestExecImpl
  :: GuestAgentConns
  -> QemuConfig
  -> Int64
  -> Text
  -> Maybe BS.ByteString
  -> Int
  -> IO GuestExecResult
guestExecImpl conns config vmId command mStdin maxPolls = do
  -- The persistent-connection timeout has to cover the entire polling
  -- loop (pollStatus sleeps 100 ms between guest-exec-status calls).
  -- A 15 s default kills long shell provisioners (apt-get install nginx
  -- can comfortably take a minute on a cold cache); scale with the
  -- caller's @maxPolls@ budget plus a 30 s headroom for the initial
  -- exec dispatch and OS-detection round-trip.
  let connTimeoutMicros = max 15000000 (maxPolls * 100000 + 30000000)
  mResult <- withPersistentConn conns config vmId 5 connTimeoutMicros $ \sock -> do
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
      Nothing -> pure $ GuestExecError $ "Failed to parse guest-exec response: " <> T.pack (show execResp)
      Just pid -> pollStatus sock pid 0 maxPolls
  case mResult of
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
    resp <- recvJson sock
    case resp of
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

-- | Query guest OS info via the QEMU Guest Agent.
-- Returns Nothing on failure.
guestGetOsInfo :: GuestAgentConns -> QemuConfig -> Int64 -> IO (Maybe GuestOsInfo)
guestGetOsInfo conns config vmId = do
  mResult <- withPersistentConn conns config vmId 5 15000000 $ \sock -> do
    sendJson sock $ Aeson.object ["execute" .= ("guest-get-osinfo" :: Text)]
    resp <- recvJson sock
    pure $ parseOsInfo resp
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

-- | Parse guest-get-osinfo response.
parseOsInfo :: Maybe Value -> Maybe GuestOsInfo
parseOsInfo mVal = do
  val <- mVal
  AT.parseMaybe osInfoParser val
  where
    osInfoParser = AT.withObject "response" $ \obj -> do
      ret <- obj .: "return"
      kernelRelease <- ret .:? "kernel-release" AT..!= ""
      kernelVersion <- ret .:? "kernel-version" AT..!= ""
      osId <- ret .:? "id" AT..!= ""
      machine <- ret .:? "machine" AT..!= ""
      pure
        GuestOsInfo
          { goiKernelRelease = kernelRelease
          , goiKernelVersion = kernelVersion
          , goiId = osId
          , goiMachine = machine
          }

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
-- @maxAttempts@ is the budget in 100 ms ticks; the previous default of 600
-- (~60 s) is preserved by 'guestExec'. Long-running provisioner steps in
-- builds raise this to several minutes.
pollStatus :: Socket -> Int -> Int -> Int -> IO GuestExecResult
pollStatus sock pid attempts maxAttempts
  | attempts > maxAttempts = pure $ GuestExecError "guest-exec timed out waiting for process to exit"
  | otherwise = do
      let statusCmd =
            Aeson.object
              [ "execute" .= ("guest-exec-status" :: Text)
              , "arguments" .= Aeson.object ["pid" .= pid]
              ]
      sendJson sock statusCmd
      statusResp <- recvJson sock

      case parseExecStatus statusResp of
        Just (True, exitcode, stdout, stderr) ->
          pure $ GuestExecSuccess exitcode stdout stderr
        Just (False, _, _, _) -> do
          threadDelay 100000 -- 100ms
          pollStatus sock pid (attempts + 1) maxAttempts
        Nothing -> pure $ GuestExecError "Failed to parse guest-exec-status response"

-- | Parse guest-exec-status response.
-- Returns (exited, exitcode, stdout, stderr)
parseExecStatus :: Maybe Value -> Maybe (Bool, Int, Text, Text)
parseExecStatus mVal = do
  val <- mVal
  AT.parseMaybe statusParser val
  where
    statusParser = AT.withObject "response" $ \obj -> do
      ret <- obj .: "return"
      exited <- ret .: "exited"
      if exited
        then do
          exitcode <- ret .:? "exitcode" AT..!= 1
          outData <- ret .:? "out-data" AT..!= ""
          errData <- ret .:? "err-data" AT..!= ""
          pure (True, exitcode, decodeBase64 outData, decodeBase64 errData)
        else pure (False, 0, "", "")

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
