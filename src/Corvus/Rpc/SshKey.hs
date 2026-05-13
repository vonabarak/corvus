{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | SshKeyManager + SshKey cap implementations.
module Corvus.Rpc.SshKey
  ( SshKeyManagerCap (..)
  , SshKeyCap (..)
  , newSshKeyManagerCap
  )
where

import Capnp (export)
import qualified Capnp.Gen.Sshkey as CGSsh
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer, handleParsed, methodUnimplemented)
import Corvus.Action (runAction)
import Corvus.Handlers.Resolve (resolveSshKey)
import Corvus.Handlers.SshKey (SshKeyCreate (..), SshKeyDelete (..), handleSshKeyList)
import Corvus.Protocol (Response (..))
import Corvus.Rpc.Common (capnpRefToRef, failOnLeft)
import Corvus.Types (ServerState (..))
import Corvus.Wire.SshKey (toCapnpSshKeyInfo)
import Data.Int (Int64)
import qualified Data.Text as T
import Supervisors (Supervisor)

data SshKeyManagerCap = SshKeyManagerCap
  { skmState :: !ServerState
  , skmSup :: !Supervisor
  }

newSshKeyManagerCap :: ServerState -> Supervisor -> IO SshKeyManagerCap
newSshKeyManagerCap st sup = pure (SshKeyManagerCap st sup)

instance SomeServer SshKeyManagerCap

instance CGSsh.SshKeyManager'server_ SshKeyManagerCap where
  sshKeyManager'list (SshKeyManagerCap st _) = handleParsed $ \_ -> do
    resp <- handleSshKeyList st
    case resp of
      RespSshKeyList keys ->
        pure CGSsh.SshKeyManager'list'results {CGSsh.keys = map toCapnpSshKeyInfo keys}
      RespError msg -> throwFailed msg
      _ -> throwFailed "sshKeyManager'list: unexpected response"

  sshKeyManager'get (SshKeyManagerCap st sup) =
    handleParsed $ \CGSsh.SshKeyManager'get'params {..} -> do
      ref' <- capnpRefToRef ref
      eid <- failOnLeft =<< resolveSshKey ref' (ssDbPool st)
      client <- export @CGSsh.SshKey sup (SshKeyCap st eid)
      pure CGSsh.SshKeyManager'get'results {CGSsh.key = client}

  sshKeyManager'create (SshKeyManagerCap st sup) =
    handleParsed $ \CGSsh.SshKeyManager'create'params {params = CGSsh.SshKeyCreateParams {..}} -> do
      resp <- runAction st (SshKeyCreate {skcName = name, skcPublicKey = publicKey})
      case resp of
        RespSshKeyCreated kid -> do
          client <- export @CGSsh.SshKey sup (SshKeyCap st kid)
          pure CGSsh.SshKeyManager'create'results {CGSsh.key = client}
        RespError msg -> throwFailed msg
        _ -> throwFailed (T.pack ("sshKeyManager'create: unexpected response: " <> show resp))

data SshKeyCap = SshKeyCap
  { _skState :: !ServerState
  , _skId :: !Int64
  }

instance SomeServer SshKeyCap

instance CGSsh.SshKey'server_ SshKeyCap where
  sshKey'show _ = methodUnimplemented
  sshKey'delete (SshKeyCap st eid) = handleParsed $ \_ -> do
    resp <- runAction st (SshKeyDelete eid)
    case resp of
      RespSshKeyOk -> pure CGSsh.SshKey'delete'results
      RespSshKeyNotFound -> throwFailed "SSH key not found"
      RespSshKeyInUse _ -> throwFailed "SSH key in use"
      RespError msg -> throwFailed msg
      _ -> throwFailed "sshKey'delete: unexpected response"
