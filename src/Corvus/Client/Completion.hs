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
import qualified Corvus.Client.Capnp.Connection as CC
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Protocol (DiskImageInfo (..), NetworkInfo (..), SshKeyInfo (..), TemplateVmInfo (..), VmInfo (..))
import Corvus.Types (ListenAddress (..), getDefaultSocketPath)
import qualified Data.Text as T
import Options.Applicative.Builder.Completer (listIOCompleter)
import Options.Applicative.Types (Completer)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Determine daemon address for completion context.
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
fetchNames :: forall a. (CC.CapnpConnection -> IO [a]) -> (a -> String) -> IO [String]
fetchNames query extract = do
  addr <- getCompletionAddress
  -- Completion never uses TLS — the shell calls @crv … --bash-completion-…@
  -- before users ever pass connection flags, and TLS over Unix is a no-op
  -- anyway. Pass 'Nothing' for the cert config.
  result <- try $ CC.withCapnpConnection addr Nothing $ \conn -> do
    r <- try (query conn) :: IO (Either SomeException [a])
    pure $ case r of
      Right xs -> map extract xs
      Left _ -> []
  case result of
    Right (Right names) -> pure names
    Right (Left _) -> pure []
    Left (_ :: SomeException) -> pure []

vmCompleter :: Completer
vmCompleter =
  listIOCompleter $
    fetchNames CR.rpcVmList (T.unpack . viName)

diskCompleter :: Completer
diskCompleter =
  listIOCompleter $
    fetchNames CR.rpcDiskList (T.unpack . diiName)

networkCompleter :: Completer
networkCompleter =
  listIOCompleter $
    fetchNames CR.rpcNetworkList (T.unpack . nwiName)

sshKeyCompleter :: Completer
sshKeyCompleter =
  listIOCompleter $
    fetchNames CR.rpcSshKeyList (T.unpack . skiName)

templateCompleter :: Completer
templateCompleter =
  listIOCompleter $
    fetchNames CR.rpcTemplateList (T.unpack . tviName)
