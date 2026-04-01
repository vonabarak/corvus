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
import Test.VM.Types (TestVm (..))

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
  { sshHost :: !Text
  -- ^ Host to connect to (usually localhost for port-forwarded VMs)
  , sshPort :: !Int
  -- ^ SSH port on host
  , sshUser :: !Text
  -- ^ Username to connect as
  , sshKeyFile :: !FilePath
  -- ^ Path to private key file
  }
  deriving (Show, Eq)

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

-- | Run a command via SSH and return the result.
-- Returns (exit code, stdout, stderr).
runSshCommand :: SshConfig -> String -> IO (ExitCode, Text, Text)
runSshCommand config cmd = do
  let args =
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
        , "-i"
        , sshKeyFile config
        , "-p"
        , show (sshPort config)
        , T.unpack (sshUser config) ++ "@" ++ T.unpack (sshHost config)
        , cmd
        ]

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
  let dest =
        T.unpack (sshUser config)
          ++ "@"
          ++ T.unpack (sshHost config)
          ++ ":"
          ++ remotePath
      args =
        [ "-o"
        , "StrictHostKeyChecking=no"
        , "-o"
        , "UserKnownHostsFile=/dev/null"
        , "-o"
        , "LogLevel=ERROR"
        , "-i"
        , sshKeyFile config
        , "-P"
        , show (sshPort config)
        , localPath
        , dest
        ]

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
  let src =
        T.unpack (sshUser config)
          ++ "@"
          ++ T.unpack (sshHost config)
          ++ ":"
          ++ remotePath
      args =
        [ "-o"
        , "StrictHostKeyChecking=no"
        , "-o"
        , "UserKnownHostsFile=/dev/null"
        , "-o"
        , "LogLevel=ERROR"
        , "-i"
        , sshKeyFile config
        , "-P"
        , show (sshPort config)
        , src
        , localPath
        ]

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
      let args =
            [ "-o"
            , "StrictHostKeyChecking=no"
            , "-o"
            , "UserKnownHostsFile=/dev/null"
            , "-o"
            , "BatchMode=yes"
            , "-o"
            , "ConnectTimeout=10"
            , "-i"
            , tvmSshPrivateKey vm
            , "-p"
            , show (tvmSshPort vm)
            , T.unpack (tvmSshUser vm) ++ "@" ++ tvmSshHost vm
            , T.unpack cmd
            ]
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
runInTestVmWith :: String -> Int -> FilePath -> Text -> Text -> IO (ExitCode, Text, Text)
runInTestVmWith host port privateKey user cmd = do
  let args =
        [ "-o"
        , "StrictHostKeyChecking=no"
        , "-o"
        , "UserKnownHostsFile=/dev/null"
        , "-o"
        , "BatchMode=yes"
        , "-o"
        , "ConnectTimeout=10"
        , "-i"
        , privateKey
        , "-p"
        , show port
        , T.unpack user ++ "@" ++ host
        , T.unpack cmd
        ]
  putStrLn $ "[ssh-run] Executing: " <> T.unpack cmd
  (code, stdout, stderr) <- readProcessWithExitCode "ssh" args ""
  putStrLn $ "[ssh-run] Exit code: " <> show code
  pure (code, T.pack stdout, T.pack stderr)

-- | Wait for SSH to be available on the VM (without key)
waitForTestVmSsh :: String -> Int -> Int -> IO ()
waitForTestVmSsh host port = waitForTestVmSshWithKey host port "" "corvus"

-- | Wait for SSH to be available on the VM using a specific key.
-- Uses wall-clock time for accurate timeout tracking.
-- Fails fast if SSH is up but key authentication is rejected (after a
-- grace period for cloud-init to finish deploying keys).
waitForTestVmSshWithKey :: String -> Int -> FilePath -> Text -> Int -> IO ()
waitForTestVmSshWithKey host port privateKey user timeoutSec = do
  startTime <- getCurrentTime
  go startTime Nothing
  where
    authGracePeriod :: Int
    authGracePeriod = 60

    elapsedSec :: UTCTime -> IO Int
    elapsedSec start = do
      now <- getCurrentTime
      pure $ round (diffUTCTime now start)

    go startTime mAuthStart = do
      elapsed <- elapsedSec startTime
      if elapsed >= timeoutSec
        then
          fail $
            "Timeout waiting for SSH on "
              <> host
              <> ":"
              <> show port
              <> " (after "
              <> show elapsed
              <> "s)"
        else do
          result <- trySshConnection host port privateKey
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
                      <> host
                      <> ":"
                      <> show port
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

    trySshConnection :: String -> Int -> FilePath -> IO SshProbeResult
    trySshConnection sshHost sshPort keyFile = do
      let keyArgs = if null keyFile then [] else ["-i", keyFile]
          args =
            [ "-o"
            , "StrictHostKeyChecking=no"
            , "-o"
            , "UserKnownHostsFile=/dev/null"
            , "-o"
            , "BatchMode=yes"
            , "-o"
            , "ConnectTimeout=3"
            , "-p"
            , show sshPort
            ]
              ++ keyArgs
              ++ [T.unpack user ++ "@" ++ sshHost, "true"]
      result <- try $ readProcessWithExitCode "ssh" args ""
      case result of
        Left (_ :: SomeException) -> pure SshNotReady
        Right (ExitSuccess, _, _) -> pure SshOk
        Right (ExitFailure _, _, stderr)
          | "Permission denied" `isInfixOf` stderr -> pure $ SshAuthRejected stderr
          | otherwise -> pure SshNotReady

-- | SSH connection attempt result (internal)
data SshProbeResult
  = SshOk
  | SshAuthRejected !String
  | SshNotReady
