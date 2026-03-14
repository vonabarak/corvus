{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SSH key management and command execution for VM integration tests.
module Test.VM.Ssh
  ( -- * Types
    SshKeyPair (..),
    SshConfig (..),

    -- * Key generation
    generateSshKeyPair,
    cleanupSshKeyPair,

    -- * SSH operations
    runSshCommand,
    waitForSsh,
    scpToVm,
    scpFromVm,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

-- | SSH key pair paths
data SshKeyPair = SshKeyPair
  { -- | Path to private key
    skpPrivateKey :: !FilePath,
    -- | Path to public key
    skpPublicKey :: !FilePath
  }
  deriving (Show, Eq)

-- | SSH connection configuration
data SshConfig = SshConfig
  { -- | Host to connect to (usually localhost for port-forwarded VMs)
    sshHost :: !Text,
    -- | SSH port on host
    sshPort :: !Int,
    -- | Username to connect as
    sshUser :: !Text,
    -- | Path to private key file
    sshKeyFile :: !FilePath
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
      [ "-t",
        "ed25519",
        "-f",
        privateKey,
        "-N",
        "", -- Empty passphrase
        "-C",
        "corvus-test@localhost"
      ]
      ""

  case code of
    ExitSuccess ->
      pure $
        Right
          SshKeyPair
            { skpPrivateKey = privateKey,
              skpPublicKey = publicKey
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
        [ "-o",
          "StrictHostKeyChecking=no",
          "-o",
          "UserKnownHostsFile=/dev/null",
          "-o",
          "LogLevel=ERROR",
          "-o",
          "ConnectTimeout=10",
          "-o",
          "BatchMode=yes",
          "-i",
          sshKeyFile config,
          "-p",
          show (sshPort config),
          T.unpack (sshUser config) ++ "@" ++ T.unpack (sshHost config),
          cmd
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
        [ "-o",
          "StrictHostKeyChecking=no",
          "-o",
          "UserKnownHostsFile=/dev/null",
          "-o",
          "LogLevel=ERROR",
          "-i",
          sshKeyFile config,
          "-P",
          show (sshPort config),
          localPath,
          dest
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
        [ "-o",
          "StrictHostKeyChecking=no",
          "-o",
          "UserKnownHostsFile=/dev/null",
          "-o",
          "LogLevel=ERROR",
          "-i",
          sshKeyFile config,
          "-P",
          show (sshPort config),
          src,
          localPath
        ]

  (code, _, stderr) <- readProcessWithExitCode "scp" args ""
  case code of
    ExitSuccess -> pure $ Right ()
    ExitFailure n ->
      pure $
        Left $
          "scp from VM failed (exit " <> T.pack (show n) <> "): " <> T.pack stderr
