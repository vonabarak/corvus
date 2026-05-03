{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | YAML input schema for @crv build@ image-bake configurations.
--
-- A 'BuildConfig' is a list of 'Build's. Each build instantiates a named
-- template, runs an ordered list of provisioners inside the resulting VM,
-- then captures one of its disks as a registered Corvus disk image.
--
-- The client preprocesses the YAML before sending: any provisioner that
-- references a host file (@shell.script@, @file.from@) is read off the
-- operator's filesystem and inlined as @shell.inline@ / @file.content@ so
-- the daemon never needs access to the client's working directory.
module Corvus.Schema.Build
  ( BuildConfig (..)
  , Build (..)
  , BuildTarget (..)
  , BuildStrategy (..)
  , BuildVm (..)
  , Provisioner (..)
  , Shell (..)
  , FileProv (..)
  , WaitFor (..)
  , Reboot (..)
  , CleanupMode (..)
  , BootKey (..)
  )
where

import Corvus.Model (DriveFormat (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (FromJSON (..), Value (..), withObject, withText, (.!=), (.:), (.:?))
import qualified Data.Yaml as Yaml

newtype BuildConfig = BuildConfig
  { bcBuilds :: [Build]
  }
  deriving (Show)

instance FromJSON BuildConfig where
  parseJSON = withObject "BuildConfig" $ \o ->
    BuildConfig <$> o .:? "builds" .!= []

data Build = Build
  { buildName :: Text
  , buildDescription :: Maybe Text
  , buildTemplate :: Text
  , buildTarget :: BuildTarget
  , buildStrategy :: BuildStrategy
  , buildVm :: BuildVm
  , buildProvisioners :: [Provisioner]
  , buildCleanup :: CleanupMode
  , buildBootKeys :: [BootKey]
  , buildWaitForShutdownSec :: Int
  }
  deriving (Show)

instance FromJSON Build where
  parseJSON = withObject "Build" $ \o ->
    Build
      <$> o .: "name"
      <*> o .:? "description"
      <*> o .: "template"
      <*> o .: "target"
      <*> o .:? "strategy" .!= BuildStrategyOverlay
      <*> o .:? "vm" .!= defaultBuildVm
      <*> o .:? "provisioners" .!= []
      <*> o .:? "cleanup" .!= CleanupAlways
      <*> o .:? "bootKeys" .!= []
      <*> o .:? "waitForShutdownSec" .!= 3600

data BuildTarget = BuildTarget
  { btName :: Text
  , btFormat :: DriveFormat
  , btSizeGb :: Int
  , btCompact :: Bool
  }
  deriving (Show)

instance FromJSON BuildTarget where
  parseJSON = withObject "BuildTarget" $ \o ->
    BuildTarget
      <$> o .: "name"
      <*> o .:? "format" .!= FormatQcow2
      <*> o .:? "sizeGb" .!= 10
      <*> o .:? "compact" .!= True

data BuildStrategy
  = BuildStrategyOverlay
  | BuildStrategyFromScratch
  | BuildStrategyInstaller
  deriving (Eq, Show)

instance FromJSON BuildStrategy where
  parseJSON = withText "BuildStrategy" $ \case
    "overlay" -> pure BuildStrategyOverlay
    "from-scratch" -> pure BuildStrategyFromScratch
    "installer" -> pure BuildStrategyInstaller
    other -> fail $ "unknown strategy '" <> T.unpack other <> "' (expected: overlay, from-scratch, installer)"

data BuildVm = BuildVm
  { bvmCpuCount :: Int
  , bvmRamMb :: Int
  }
  deriving (Show)

defaultBuildVm :: BuildVm
defaultBuildVm = BuildVm 4 4096

instance FromJSON BuildVm where
  parseJSON = withObject "BuildVm" $ \o ->
    BuildVm
      <$> o .:? "cpuCount" .!= 4
      <*> o .:? "ramMb" .!= 4096

-- | A single step inside a build.
data Provisioner
  = ProvShell Shell
  | ProvFile FileProv
  | ProvWaitFor WaitFor
  | ProvReboot Reboot
  deriving (Show)

instance FromJSON Provisioner where
  parseJSON v = case v of
    Object o -> do
      mShell <- o .:? "shell"
      mFile <- o .:? "file"
      mWait <- o .:? "wait-for"
      mReboot <- o .:? "reboot"
      case (mShell, mFile, mWait, mReboot) of
        (Just s, Nothing, Nothing, Nothing) -> pure (ProvShell s)
        (Nothing, Just f, Nothing, Nothing) -> pure (ProvFile f)
        (Nothing, Nothing, Just w, Nothing) -> pure (ProvWaitFor w)
        (Nothing, Nothing, Nothing, Just r) -> pure (ProvReboot r)
        (Nothing, Nothing, Nothing, Nothing) ->
          fail "provisioner must have one of: shell, file, wait-for, reboot"
        _ -> fail "provisioner must have exactly one of: shell, file, wait-for, reboot"
    _ -> typeMismatch "Provisioner" v

-- | Shell command provisioner.
--
-- Either @inline@ (a script body as text) or @script@ (a path the client
-- reads and embeds as @inline@) must be present. After client preprocessing
-- the daemon only sees @inline@ — a non-Nothing @script@ at parse time on
-- the daemon is a bug in the client.
data Shell = Shell
  { shellInline :: Maybe Text
  , shellScript :: Maybe FilePath
  , shellWorkdir :: Maybe Text
  , shellEnv :: [(Text, Text)]
  , shellTimeoutSec :: Maybe Int
  }
  deriving (Show)

instance FromJSON Shell where
  parseJSON v = case v of
    String t ->
      pure $
        Shell
          { shellInline = Just t
          , shellScript = Nothing
          , shellWorkdir = Nothing
          , shellEnv = []
          , shellTimeoutSec = Nothing
          }
    Object o -> do
      mInline <- o .:? "inline"
      mScript <- o .:? "script"
      mWorkdir <- o .:? "workdir"
      mEnvVal <- o .:? "env" :: Yaml.Parser (Maybe Value)
      mTimeout <- o .:? "timeoutSec"
      envList <- case mEnvVal of
        Nothing -> pure []
        Just (Object km) -> traverse asTextPair (KM.toList km)
        Just other -> typeMismatch "shell.env (object of strings)" other
      pure
        Shell
          { shellInline = mInline
          , shellScript = mScript
          , shellWorkdir = mWorkdir
          , shellEnv = envList
          , shellTimeoutSec = mTimeout
          }
    _ -> typeMismatch "Shell" v
    where
      asTextPair (k, String t) = pure (Key.toText k, t)
      asTextPair (k, other) =
        typeMismatch ("shell.env." <> T.unpack (Key.toText k) <> " (string)") other

-- | File-upload provisioner.
--
-- Either @from@ (a host path the client reads and embeds as @content@) or
-- @content@ (base64-encoded bytes) must be present. After client
-- preprocessing the daemon only sees @content@.
data FileProv = FileProv
  { fileFrom :: Maybe FilePath
  , fileContentBase64 :: Maybe Text
  , fileTo :: Text
  , fileMode :: Maybe Text
  }
  deriving (Show)

instance FromJSON FileProv where
  parseJSON = withObject "FileProv" $ \o ->
    FileProv
      <$> o .:? "from"
      <*> o .:? "content"
      <*> o .: "to"
      <*> o .:? "mode"

data WaitFor
  = WaitForPing {wfTimeoutSec :: Int}
  | WaitForFile {wfPath :: Text, wfTimeoutSec :: Int}
  | WaitForPort {wfPort :: Int, wfTimeoutSec :: Int}
  deriving (Show)

instance FromJSON WaitFor where
  parseJSON = withObject "WaitFor" $ \o -> do
    mPing <- o .:? "ping" :: Yaml.Parser (Maybe Bool)
    mFile <- o .:? "file"
    mPort <- o .:? "port"
    timeoutSec <- o .:? "timeoutSec" .!= 300
    case (mPing, mFile, mPort) of
      (Just True, Nothing, Nothing) -> pure $ WaitForPing timeoutSec
      (Nothing, Just p, Nothing) -> pure $ WaitForFile p timeoutSec
      (Nothing, Nothing, Just p) -> pure $ WaitForPort p timeoutSec
      (Nothing, Nothing, Nothing) -> fail "wait-for: specify one of ping, file, port"
      _ -> fail "wait-for: specify exactly one of ping, file, port"

newtype Reboot = Reboot {rebootTimeoutSec :: Int}
  deriving (Show)

instance FromJSON Reboot where
  parseJSON = withObject "Reboot" $ \o ->
    Reboot <$> o .:? "timeoutSec" .!= 300

data CleanupMode = CleanupAlways | CleanupOnSuccess | CleanupNever
  deriving (Eq, Show)

instance FromJSON CleanupMode where
  parseJSON = withText "CleanupMode" $ \case
    "always" -> pure CleanupAlways
    "onSuccess" -> pure CleanupOnSuccess
    "never" -> pure CleanupNever
    other -> fail $ "unknown cleanup mode '" <> T.unpack other <> "' (expected: always, onSuccess, never)"

-- | A scripted key press at bake-VM start, used by the @installer@
-- strategy to dismiss firmware prompts (e.g. UEFI's "Press any key to
-- boot from CD"). Sent via QMP @send-key@ shortly after the bake VM
-- starts, before any cloud-init or guest agent activity.
--
-- @keys@ is a QEMU @qcode@ string (@ret@, @esc@, @spc@, @tab@, @up@,
-- @down@, an alphanumeric, etc.). The press is repeated @repeat@
-- times with @intervalSec@ seconds between presses, after waiting
-- @delaySec@ seconds from VM start.
data BootKey = BootKey
  { bkKeys :: Text
  , bkDelaySec :: Int
  , bkRepeat :: Int
  , bkIntervalSec :: Int
  }
  deriving (Eq, Show)

instance FromJSON BootKey where
  parseJSON = withObject "BootKey" $ \o ->
    BootKey
      <$> o .: "keys"
      <*> o .:? "delaySec" .!= 0
      <*> o .:? "repeat" .!= 1
      <*> o .:? "intervalSec" .!= 1
