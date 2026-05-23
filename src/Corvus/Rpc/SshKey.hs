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
import Capnp.Rpc.Server (SomeServer, methodUnimplemented)
import Corvus.Action (runAction)
import Corvus.Handlers.Resolve (resolveSshKey)
import Corvus.Handlers.SshKey (SshKeyCreate (..), SshKeyDelete (..), handleSshKeyList)
import Corvus.Protocol (Response (..))
import qualified Corvus.Protocol.SshKey as P
import Corvus.Rpc.Common (capnpRefToRef, failOnLeft, handleParsed)
import Corvus.Types (ServerState (..))
import Corvus.Wire.SshKey (toCapnpSshKeyInfo)
import Data.Int (Int64)
import qualified Data.Text as T
import Supervisors (Supervisor)

data SshKeyManagerCap = SshKeyManagerCap
  { skmState :: !ServerState
  , skmSup :: !Supervisor
  , skmClientName :: !T.Text
  }

newSshKeyManagerCap :: ServerState -> Supervisor -> T.Text -> IO SshKeyManagerCap
newSshKeyManagerCap st sup cn = pure (SshKeyManagerCap st sup cn)

instance SomeServer SshKeyManagerCap

instance CGSsh.SshKeyManager'server_ SshKeyManagerCap where
  sshKeyManager'list (SshKeyManagerCap st _ _) = handleParsed $ \_ -> do
    resp <- handleSshKeyList st
    case resp of
      RespSshKeyList keys ->
        pure CGSsh.SshKeyManager'list'results {CGSsh.keys = map toCapnpSshKeyInfo keys}
      RespError msg -> throwFailed msg
      _ -> throwFailed "sshKeyManager'list: unexpected response"

  sshKeyManager'get (SshKeyManagerCap st sup cn) =
    handleParsed $ \CGSsh.SshKeyManager'get'params {..} -> do
      ref' <- capnpRefToRef ref
      eid <- failOnLeft =<< resolveSshKey ref' (ssDbPool st)
      client <- export @CGSsh.SshKey sup (SshKeyCap st eid cn)
      pure CGSsh.SshKeyManager'get'results {CGSsh.key = client}

  sshKeyManager'create (SshKeyManagerCap st sup cn) =
    handleParsed $ \CGSsh.SshKeyManager'create'params {params = CGSsh.SshKeyCreateParams {..}} -> do
      resp <- runAction st cn (SshKeyCreate {skcName = name, skcPublicKey = publicKey})
      case resp of
        RespSshKeyCreated kid -> do
          client <- export @CGSsh.SshKey sup (SshKeyCap st kid cn)
          pure CGSsh.SshKeyManager'create'results {CGSsh.key = client}
        RespError msg -> throwFailed msg
        _ -> throwFailed (T.pack ("sshKeyManager'create: unexpected response: " <> show resp))

data SshKeyCap = SshKeyCap
  { _skState :: !ServerState
  , _skId :: !Int64
  , _skClientName :: !T.Text
  }

instance SomeServer SshKeyCap

instance CGSsh.SshKey'server_ SshKeyCap where
  sshKey'show (SshKeyCap st kid _cn) = handleParsed $ \_ -> do
    -- The legacy protocol has no `ssh-key show` request, so look
    -- the key up by walking the list. SSH-key inventories are
    -- small enough in practice that the cost is negligible.
    resp <- handleSshKeyList st
    case resp of
      RespSshKeyList keys ->
        case [k | k <- keys, P.skiId k == kid] of
          [k] -> pure CGSsh.SshKey'show'results {CGSsh.info = toCapnpSshKeyInfo k}
          _ -> throwFailed "SSH key not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "sshKey'show: unexpected response"

  sshKey'delete (SshKeyCap st eid cn) = handleParsed $ \_ -> do
    resp <- runAction st cn (SshKeyDelete eid)
    case resp of
      RespSshKeyOk -> pure CGSsh.SshKey'delete'results
      RespSshKeyNotFound -> throwFailed "SSH key not found"
      RespSshKeyInUse _ -> throwFailed "SSH key in use"
      RespError msg -> throwFailed msg
      _ -> throwFailed "sshKey'delete: unexpected response"
