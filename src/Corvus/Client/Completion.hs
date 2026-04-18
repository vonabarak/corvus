{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Shell tab-completion support for the Corvus CLI.
-- Provides dynamic completers that query the daemon for entity names,
-- and falls back silently when the daemon is unavailable.
module Corvus.Client.Completion
  ( -- * Entity completers
    vmCompleter
  , diskCompleter
  , networkCompleter
  , sshKeyCompleter
  , templateCompleter
  )
where

import Control.Exception (SomeException, try)
import Corvus.Client.Connection (Connection, ConnectionError, withConnection)
import Corvus.Client.Rpc
import Corvus.Protocol (DiskImageInfo (..), NetworkInfo (..), SshKeyInfo (..), TemplateVmInfo (..), VmInfo (..))
import Corvus.Types (ListenAddress (..), getDefaultSocketPath)
import qualified Data.Text as T
import Options.Applicative.Builder.Completer (listIOCompleter)
import Options.Applicative.Types (Completer)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Determine daemon address for completion context.
-- Checks CORVUS_SOCKET, then CORVUS_HOST+CORVUS_PORT, then default socket path.
getCompletionAddress :: IO ListenAddress
getCompletionAddress = do
  mSocket <- lookupEnv "CORVUS_SOCKET"
  case mSocket of
    Just path -> pure (UnixAddress path)
    Nothing -> do
      mHost <- lookupEnv "CORVUS_HOST"
      mPort <- lookupEnv "CORVUS_PORT"
      case (mHost, mPort >>= readMaybe) of
        (Just host, Just port) -> pure (TcpAddress host port)
        _ -> UnixAddress <$> getDefaultSocketPath

-- | Query daemon for entity names, returning [] on any failure.
fetchNames :: (Connection -> IO (Either ConnectionError a)) -> (a -> [String]) -> IO [String]
fetchNames query extract = do
  addr <- getCompletionAddress
  result <- try $ withConnection addr $ \conn -> do
    resp <- query conn
    pure $ case resp of
      Left _ -> []
      Right a -> extract a
  case result of
    Right (Right names) -> pure names
    Right (Left _) -> pure []
    Left (_ :: SomeException) -> pure []

-- | Completer for VM names.
vmCompleter :: Completer
vmCompleter =
  listIOCompleter $
    fetchNames vmList (map (T.unpack . viName))

-- | Completer for disk image names.
diskCompleter :: Completer
diskCompleter = listIOCompleter $
  fetchNames diskList $ \case
    DiskListResult disks -> map (T.unpack . diiName) disks
    _ -> []

-- | Completer for virtual network names.
networkCompleter :: Completer
networkCompleter = listIOCompleter $
  fetchNames networkList $ \case
    NetworkListResult networks -> map (T.unpack . nwiName) networks
    _ -> []

-- | Completer for SSH key names.
sshKeyCompleter :: Completer
sshKeyCompleter = listIOCompleter $
  fetchNames sshKeyList $ \case
    SshKeyListResult keys -> map (T.unpack . skiName) keys
    _ -> []

-- | Completer for template names.
templateCompleter :: Completer
templateCompleter = listIOCompleter $
  fetchNames templateList $ \case
    TemplateListResult templates -> map (T.unpack . tviName) templates
    _ -> []
