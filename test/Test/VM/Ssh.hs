{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SSH key management and command execution for VM integration tests.
module Test.VM.Ssh
  ( -- * Types
    SshKeyPair (..)
  , SshConfig (..)

    -- * Key generation
  , generateSshKeyPair
  , cleanupSshKeyPair

    -- * SSH operations
  , runSshCommand
  , waitForSsh
  , scpToVm
  , scpFromVm

    -- * TestVm SSH operations
  , runInTestVm
  , runInTestVm_
  , runInTestVmWith
  , waitForTestVmSsh
  , waitForTestVmSshWithKey

    -- * Vsock proxy selection
  , describeSshLocator
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.VM.Types (SshLocator (..), TestVm (..))
import Test.VM.VsockProxy (VsockProxy (..), getVsockProxy)

-- | SSH key pair paths
data SshKeyPair = SshKeyPair
  { skpPrivateKey :: !FilePath
  -- ^ Path to private key
  , skpPublicKey :: !FilePath
  -- ^ Path to public key
  }
  deriving (Show, Eq)

-- | SSH connection configuration
data SshConfig = SshConfig
  { sshLocator :: !SshLocator
  -- ^ Transport: TCP host/port or vsock CID
  , sshUser :: !Text
  -- ^ Username to connect as
  , sshKeyFile :: !FilePath
  -- ^ Path to private key file
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Locator description
--------------------------------------------------------------------------------

-- | Render a locator as a short string for log/error messages.
describeSshLocator :: SshLocator -> String
describeSshLocator (SshTcp host port) = host ++ ":" ++ show port
describeSshLocator (SshVsock cid) = "vsock/" ++ show cid
describeSshLocator SshDisabled = "<ssh disabled>"

--------------------------------------------------------------------------------
-- Argument builders
--------------------------------------------------------------------------------

-- | Common ssh -o options used everywhere in the test suite.
baseSshOpts :: [String]
baseSshOpts =
  [ "-o"
  , "StrictHostKeyChecking=no"
  , "-o"
  , "UserKnownHostsFile=/dev/null"
  , "-o"
  , "LogLevel=ERROR"
  , "-o"
  , "ConnectTimeout=10"
  , "-o"
  , "BatchMode=yes"
  ]

-- | Build the ssh argv tail for a given locator: transport-specific
-- options plus the @user\@host@ argument. Caller appends any
-- positional command. For 'SshDisabled', this is a programming
-- error — call sites should never hand a disabled locator to ssh.
sshArgsForIO :: SshLocator -> Text -> FilePath -> IO [String]
sshArgsForIO locator user keyFile = case locator of
  SshTcp host port ->
    pure ["-i", keyFile, "-p", show port, T.unpack user ++ "@" ++ host]
  SshVsock cid -> do
    proxy <- getVsockProxy
    let extraOpts = concatMap (\(k, v) -> ["-o", k ++ "=" ++ v]) (vpExtraOpts proxy)
        proxyOpt = ["-o", "ProxyCommand=" ++ vpProxyCmd proxy cid]
    pure $
      proxyOpt
        ++ extraOpts
        ++ ["-i", keyFile, T.unpack user ++ "@" ++ vpHost proxy cid]
  SshDisabled ->
    fail "sshArgsForIO: SshDisabled locator (test path has no SSH access)"

-- | Same as 'sshArgsForIO' but for scp, which uses @-P@ (capital P)
-- for the TCP port. Returns the opts portion only — the caller
-- appends source/destination paths and the @user\@host:path@ token.
scpOptsForIO :: SshLocator -> FilePath -> IO ([String], String)
-- ^ Returns (opts, hostToken) where hostToken is the bare
-- @user-less@ host used when constructing @user\@host:remote@.
scpOptsForIO locator keyFile = case locator of
  SshTcp host port ->
    pure
      (
        [ "-o"
        , "StrictHostKeyChecking=no"
        , "-o"
        , "UserKnownHostsFile=/dev/null"
        , "-o"
        , "LogLevel=ERROR"
        , "-i"
        , keyFile
        , "-P"
        , show port
        ]
      , host
      )
  SshVsock cid -> do
    proxy <- getVsockProxy
    let extraOpts = concatMap (\(k, v) -> ["-o", k ++ "=" ++ v]) (vpExtraOpts proxy)
    pure
      ( [ "-o"
        , "StrictHostKeyChecking=no"
        , "-o"
        , "UserKnownHostsFile=/dev/null"
        , "-o"
        , "LogLevel=ERROR"
        , "-o"
        , "ProxyCommand=" ++ vpProxyCmd proxy cid
        ]
          ++ extraOpts
          ++ ["-i", keyFile]
      , vpHost proxy cid
      )
  SshDisabled ->
    fail "scpOptsForIO: SshDisabled locator (test path has no SSH access)"

--------------------------------------------------------------------------------
-- Key generation
--------------------------------------------------------------------------------

-- | Generate a new SSH key pair for VM access.
-- Creates ed25519 keys in the specified directory.
generateSshKeyPair :: FilePath -> IO (Either Text SshKeyPair)
generateSshKeyPair tmpDir = do
  let privateKey = tmpDir </> "test_vm_key"
      publicKey = privateKey ++ ".pub"

  -- Remove existing keys if present
  privExists <- doesFileExist privateKey
  when privExists $ removeFile privateKey
  pubExists <- doesFileExist publicKey
  when pubExists $ removeFile publicKey

  -- Generate new key pair
  (code, _, stderr) <-
    readProcessWithExitCode
      "ssh-keygen"
      [ "-t"
      , "ed25519"
      , "-f"
      , privateKey
      , "-N"
      , "" -- Empty passphrase
      , "-C"
      , "corvus-test@localhost"
      ]
      ""

  case code of
    ExitSuccess ->
      pure $
        Right
          SshKeyPair
            { skpPrivateKey = privateKey
            , skpPublicKey = publicKey
            }
    ExitFailure n ->
      pure $
        Left $
          "Failed to generate SSH key pair (exit " <> T.pack (show n) <> "): " <> T.pack stderr

-- | Clean up SSH key pair files
cleanupSshKeyPair :: SshKeyPair -> IO ()
cleanupSshKeyPair keyPair = do
  privExists <- doesFileExist (skpPrivateKey keyPair)
  when privExists $ removeFile (skpPrivateKey keyPair)
  pubExists <- doesFileExist (skpPublicKey keyPair)
  when pubExists $ removeFile (skpPublicKey keyPair)

--------------------------------------------------------------------------------
-- ssh / scp commands
--------------------------------------------------------------------------------

-- | Run a command via SSH and return the result.
-- Returns (exit code, stdout, stderr).
runSshCommand :: SshConfig -> String -> IO (ExitCode, Text, Text)
runSshCommand config cmd = do
  tail' <- sshArgsForIO (sshLocator config) (sshUser config) (sshKeyFile config)
  let args = baseSshOpts ++ tail' ++ [cmd]
  (code, stdout, stderr) <- readProcessWithExitCode "ssh" args ""
  pure (code, T.pack stdout, T.pack stderr)

-- | Wait for SSH to become available on the VM.
-- Polls every second until SSH responds or timeout is reached.
-- Returns True if SSH is available, False if timeout.
waitForSsh :: SshConfig -> Int -> IO Bool
waitForSsh config = go
  where
    go 0 = pure False
    go remaining = do
      result <- try $ runSshCommand config "echo ok"
      case result of
        Right (ExitSuccess, _, _) -> pure True
        Right _ -> retry
        Left (_ :: SomeException) -> retry
      where
        retry = do
          threadDelay 1000000 -- 1 second
          go (remaining - 1)

-- | Copy a file to the VM using scp
scpToVm :: SshConfig -> FilePath -> FilePath -> IO (Either Text ())
scpToVm config localPath remotePath = do
  (opts, hostToken) <- scpOptsForIO (sshLocator config) (sshKeyFile config)
  let dest = T.unpack (sshUser config) ++ "@" ++ hostToken ++ ":" ++ remotePath
      args = opts ++ [localPath, dest]
  (code, _, stderr) <- readProcessWithExitCode "scp" args ""
  case code of
    ExitSuccess -> pure $ Right ()
    ExitFailure n ->
      pure $
        Left $
          "scp to VM failed (exit " <> T.pack (show n) <> "): " <> T.pack stderr

-- | Copy a file from the VM using scp
scpFromVm :: SshConfig -> FilePath -> FilePath -> IO (Either Text ())
scpFromVm config remotePath localPath = do
  (opts, hostToken) <- scpOptsForIO (sshLocator config) (sshKeyFile config)
  let src = T.unpack (sshUser config) ++ "@" ++ hostToken ++ ":" ++ remotePath
      args = opts ++ [src, localPath]
  (code, _, stderr) <- readProcessWithExitCode "scp" args ""
  case code of
    ExitSuccess -> pure $ Right ()
    ExitFailure n ->
      pure $
        Left $
          "scp from VM failed (exit " <> T.pack (show n) <> "): " <> T.pack stderr

--------------------------------------------------------------------------------
-- TestVm SSH operations
--------------------------------------------------------------------------------

-- | Run a command in the test VM via SSH using the configured key
runInTestVm :: TestVm -> Text -> IO (ExitCode, Text, Text)
runInTestVm vm cmd = go (3 :: Int)
  where
    go 0 = runOnce
    go n = do
      result@(code, _, _) <- runOnce
      case code of
        -- SSH transport error (e.g. cloud-init restarted sshd) — retry
        ExitFailure 255 -> do
          putStrLn "[ssh-run] SSH transport error (255), retrying..."
          threadDelay 2000000
          go (n - 1)
        _ -> pure result
    runOnce = do
      tail' <- sshArgsForIO (tvmSsh vm) (tvmSshUser vm) (tvmSshPrivateKey vm)
      let args = baseSshOpts ++ tail' ++ [T.unpack cmd]
      putStrLn $ "[ssh-run] Executing: " <> T.unpack cmd
      (code, stdout, stderr) <- readProcessWithExitCode "ssh" args ""
      putStrLn $ "[ssh-run] Exit code: " <> show code
      pure (code, T.pack stdout, T.pack stderr)

-- | Run a command in the test VM, failing on non-zero exit
runInTestVm_ :: TestVm -> Text -> IO ()
runInTestVm_ vm cmd = do
  (code, stdout, stderr) <- runInTestVm vm cmd
  case code of
    ExitSuccess -> pure ()
    ExitFailure n ->
      fail $
        "Command failed with exit code "
          <> show n
          <> "\nCommand: "
          <> T.unpack cmd
          <> "\nStdout: "
          <> T.unpack stdout
          <> "\nStderr: "
          <> T.unpack stderr

-- | Run a command via SSH with explicit connection parameters.
-- Useful when the VM was not created via the test helpers (e.g., via apply).
runInTestVmWith :: SshLocator -> FilePath -> Text -> Text -> IO (ExitCode, Text, Text)
runInTestVmWith locator privateKey user cmd = do
  tail' <- sshArgsForIO locator user privateKey
  let args = baseSshOpts ++ tail' ++ [T.unpack cmd]
  putStrLn $ "[ssh-run] Executing: " <> T.unpack cmd
  (code, stdout, stderr) <- readProcessWithExitCode "ssh" args ""
  putStrLn $ "[ssh-run] Exit code: " <> show code
  pure (code, T.pack stdout, T.pack stderr)

-- | Wait for SSH to be available on the VM using a default empty key
-- and the @corvus@ user — only useful when the VM has password
-- authentication enabled (which the test images do not). Retained
-- for completeness; new tests should pass an explicit key.
waitForTestVmSsh :: SshLocator -> Int -> IO ()
waitForTestVmSsh locator = waitForTestVmSshWithKey locator "" "corvus"

-- | Wait for SSH to be available on the VM using a specific key.
-- Uses wall-clock time for accurate timeout tracking.
-- Fails fast if SSH is up but key authentication is rejected (after a
-- grace period for cloud-init to finish deploying keys).
waitForTestVmSshWithKey :: SshLocator -> FilePath -> Text -> Int -> IO ()
waitForTestVmSshWithKey locator privateKey user timeoutSec = do
  startTime <- getCurrentTime
  go startTime Nothing
  where
    authGracePeriod :: Int
    authGracePeriod = 60

    elapsedSec :: UTCTime -> IO Int
    elapsedSec start = do
      now <- getCurrentTime
      pure $ round (diffUTCTime now start)

    target = describeSshLocator locator

    go startTime mAuthStart = do
      elapsed <- elapsedSec startTime
      if elapsed >= timeoutSec
        then
          fail $
            "Timeout waiting for SSH on "
              <> target
              <> " (after "
              <> show elapsed
              <> "s)"
        else do
          result <- trySshConnection
          case result of
            SshOk -> pure ()
            SshAuthRejected stderr -> do
              now <- getCurrentTime
              let authStart = fromMaybe now mAuthStart
                  authElapsed = round (diffUTCTime now authStart) :: Int
              putStrLn $
                "[ssh] Auth rejected, grace period: "
                  <> show (authGracePeriod - authElapsed)
                  <> "s remaining"
                  <> " (total elapsed: "
                  <> show elapsed
                  <> "s)"
              if authElapsed >= authGracePeriod
                then
                  fail $
                    "SSH key authentication failed on "
                      <> target
                      <> " (server is up but key was rejected after "
                      <> show authGracePeriod
                      <> "s grace period).\n"
                      <> "This likely means cloud-init did not deploy the SSH key.\n"
                      <> "SSH stderr: "
                      <> stderr
                else do
                  threadDelay 2000000
                  go startTime (Just authStart)
            SshNotReady -> do
              putStrLn $ "[ssh] Not ready, waiting... (" <> show elapsed <> "s elapsed)"
              threadDelay 2000000
              go startTime Nothing

    trySshConnection :: IO SshProbeResult
    trySshConnection = do
      let keyArgs = if null privateKey then [] else ["-i", privateKey]
      tail' <- sshTailWithoutKey locator user
      let args =
            [ "-o"
            , "StrictHostKeyChecking=no"
            , "-o"
            , "UserKnownHostsFile=/dev/null"
            , "-o"
            , "BatchMode=yes"
            , "-o"
            , "ConnectTimeout=3"
            ]
              ++ keyArgs
              ++ tail'
              ++ ["true"]
      result <- try $ readProcessWithExitCode "ssh" args ""
      case result of
        Left (_ :: SomeException) -> pure SshNotReady
        Right (ExitSuccess, _, _) -> pure SshOk
        Right (ExitFailure _, _, stderr)
          | "Permission denied" `isInfixOf` stderr -> pure $ SshAuthRejected stderr
          | otherwise -> pure SshNotReady

-- | Build the trailing transport-specific opts and @user\@host@
-- token, omitting @-i@ so the caller can decide whether to add a
-- key. Used by the SSH probe loop in 'waitForTestVmSshWithKey'.
sshTailWithoutKey :: SshLocator -> Text -> IO [String]
sshTailWithoutKey locator user = case locator of
  SshTcp host port ->
    pure ["-p", show port, T.unpack user ++ "@" ++ host]
  SshVsock cid -> do
    proxy <- getVsockProxy
    let extraOpts = concatMap (\(k, v) -> ["-o", k ++ "=" ++ v]) (vpExtraOpts proxy)
        proxyOpt = ["-o", "ProxyCommand=" ++ vpProxyCmd proxy cid]
    pure $ proxyOpt ++ extraOpts ++ [T.unpack user ++ "@" ++ vpHost proxy cid]
  SshDisabled ->
    fail "sshTailWithoutKey: SshDisabled locator"

-- | SSH connection attempt result (internal)
data SshProbeResult
  = SshOk
  | SshAuthRejected !String
  | SshNotReady
