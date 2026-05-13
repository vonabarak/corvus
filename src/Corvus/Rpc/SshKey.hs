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
import Corvus.Handlers.Resolve (resolveSshKey)
import Corvus.Handlers.SshKey (handleSshKeyList)
import Corvus.Protocol (Response (..))
import Corvus.Rpc.Common (capnpRefToRef, failOnLeft)
import Corvus.Types (ServerState (..))
import Corvus.Wire.SshKey (toCapnpSshKeyInfo)
import Data.Int (Int64)
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

  sshKeyManager'get (SshKeyManagerCap st sup) = handleParsed $ \CGSsh.SshKeyManager'get'params {..} -> do
    ref' <- capnpRefToRef ref
    eid <- failOnLeft =<< resolveSshKey ref' (ssDbPool st)
    client <- export @CGSsh.SshKey sup (SshKeyCap st eid)
    pure CGSsh.SshKeyManager'get'results {CGSsh.key = client}

  sshKeyManager'create _ = methodUnimplemented

data SshKeyCap = SshKeyCap
  { _skState :: !ServerState
  , _skId :: !Int64
  }

instance SomeServer SshKeyCap

instance CGSsh.SshKey'server_ SshKeyCap where
  sshKey'show _ = methodUnimplemented
  sshKey'delete _ = methodUnimplemented
