{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cap'n Proto-backed client RPC wrappers.
--
-- Each function below dials the Daemon cap held in
-- 'CapnpConnection', walks to the appropriate manager / resource
-- cap, invokes a method, and returns a Haskell-side type
-- (typically a 'Corvus.Protocol.*' info record, so CLI
-- "Corvus.Client.Commands.*" code keeps the same shape it has
-- today).
--
-- This is the Phase 4 / Phase 5 replacement for the legacy
-- "Corvus.Client.Rpc". The two coexist during the staged
-- migration; the legacy module is deleted in Phase 5.
--
-- Phase 4e (this commit) lands the read-only surface plus the
-- daemon-level operations (ping / status / shutdown). The
-- mutating wrappers ship in follow-up commits as their
-- corresponding cap methods get exercised by the test DSL and
-- CLI rewrites.
--
-- The module is split into sub-modules by resource domain:
--
-- * "Corvus.Client.Capnp.Rpc.Vm" — VM lifecycle, snapshots, streaming
-- * "Corvus.Client.Capnp.Rpc.Disk" — disk lifecycle, snapshots
-- * "Corvus.Client.Capnp.Rpc.Network" — network lifecycle
-- * "Corvus.Client.Capnp.Rpc.Node" — node lifecycle
-- * "Corvus.Client.Capnp.Rpc.Task" — task queries & subscriptions
-- * "Corvus.Client.Capnp.Rpc.Stream" — apply, build, cloud-init
module Corvus.Client.Capnp.Rpc
  ( module Corvus.Client.Capnp.Rpc.Disk
  , module Corvus.Client.Capnp.Rpc.Node
  , module Corvus.Client.Capnp.Rpc.Network
  , module Corvus.Client.Capnp.Rpc.Stream
  , module Corvus.Client.Capnp.Rpc.Task
  , module Corvus.Client.Capnp.Rpc.Vm
  , rpcSshKeyList
  , rpcSshKeyCreate
  , rpcSshKeyDelete
  , rpcTemplateCreate
  , rpcTemplateUpdate
  , rpcTemplateDelete
  , rpcTemplateInstantiate
  , rpcTemplateList
  , rpcTemplateShow
  , rpcPing
  , rpcStatus
  , rpcShutdown
  )
where

import Capnp (IsCap, IsStruct, Method, Parse, Parsed, Pipeline, callP, defaultLimit, evalLimitT, parse)
import qualified Capnp as C
import qualified Capnp.Gen.Corvus as CGCorvus
import qualified Capnp.Gen.Sshkey as CGSsh
import qualified Capnp.Gen.Template as CGTmpl
import qualified Capnp.Gen.Vm as CGVm
import Corvus.Client.Capnp.Connection (CapnpConnection (..))
import Corvus.Client.Capnp.Rpc.Disk
import Corvus.Client.Capnp.Rpc.Network
import Corvus.Client.Capnp.Rpc.Node
import Corvus.Client.Capnp.Rpc.Stream
import Corvus.Client.Capnp.Rpc.Task
import Corvus.Client.Capnp.Rpc.Vm
import qualified Corvus.Protocol as P
import qualified Corvus.Protocol.SshKey as PSk
import qualified Corvus.Protocol.Template as PTm
import Corvus.Wire.Common (EntityRef, entityRefFromText, fromCapnpStatusInfo, toCapnpEntityRef)
import Corvus.Wire.Errors (WireError, showWireError)
import qualified Corvus.Wire.SshKey as WSsh
import qualified Corvus.Wire.Template as WTmpl
import Data.Function ((&))
import Data.Int (Int64)
import Data.Text (Text)

-- | Call a method on a cap and return its parsed results struct.
callOn
  :: ( C.IsCap iface
     , C.IsStruct params
     , C.IsStruct results
     , C.Parse params (C.Parsed params)
     , C.Parse results (C.Parsed results)
     )
  => C.Method iface params results
  -> C.Parsed params
  -> C.Client iface
  -> IO (C.Parsed results)
callOn method p client = do
  raw <- (client & C.callP method p) >>= C.waitPipeline
  C.evalLimitT C.defaultLimit (C.parse raw)

failOnWire :: Either WireError a -> IO a
failOnWire (Right a) = pure a
failOnWire (Left e) = fail ("wire decode error: " <> show (showWireError e))

-- | SSH key list for the daemon.
rpcSshKeyList :: CapnpConnection -> IO [PSk.SshKeyInfo]
rpcSshKeyList conn = do
  CGCorvus.Daemon'sshKeys'results {CGCorvus.mgr = mgr} <-
    callOn #sshKeys CGCorvus.Daemon'sshKeys'params (ccDaemon conn)
  CGSsh.SshKeyManager'list'results {CGSsh.keys = ks} <-
    callOn #list CGSsh.SshKeyManager'list'params mgr
  pure (map WSsh.fromCapnpSshKeyInfo ks)

rpcSshKeyCreate :: CapnpConnection -> Text -> Text -> IO Int64
rpcSshKeyCreate conn name publicKey = do
  CGCorvus.Daemon'sshKeys'results {CGCorvus.mgr = mgr} <-
    callOn #sshKeys CGCorvus.Daemon'sshKeys'params (ccDaemon conn)
  let inner = CGSsh.SshKeyCreateParams {CGSsh.name = name, CGSsh.publicKey = publicKey}
  CGSsh.SshKeyManager'create'results {CGSsh.key = kClient} <-
    callOn #create CGSsh.SshKeyManager'create'params {CGSsh.params = inner} mgr
  CGSsh.SshKey'show'results {CGSsh.info = info} <-
    callOn #show CGSsh.SshKey'show'params kClient
  case info of CGSsh.SshKeyInfo {CGSsh.id = kid} -> pure kid

rpcTemplateCreate :: CapnpConnection -> Text -> IO Int64
rpcTemplateCreate conn yaml = do
  CGCorvus.Daemon'templates'results {CGCorvus.mgr = mgr} <-
    callOn #templates CGCorvus.Daemon'templates'params (ccDaemon conn)
  CGTmpl.TemplateManager'create'results {CGTmpl.template = tClient} <-
    callOn #create CGTmpl.TemplateManager'create'params {CGTmpl.yaml = yaml} mgr
  CGTmpl.Template'show'results {CGTmpl.details = det} <-
    callOn #show CGTmpl.Template'show'params tClient
  case det of CGTmpl.TemplateDetails {CGTmpl.id = tid} -> pure tid

rpcSshKeyDelete :: CapnpConnection -> EntityRef -> IO ()
rpcSshKeyDelete conn ref = do
  CGCorvus.Daemon'sshKeys'results {CGCorvus.mgr = mgr} <-
    callOn #sshKeys CGCorvus.Daemon'sshKeys'params (ccDaemon conn)
  CGSsh.SshKeyManager'get'results {CGSsh.key = kClient} <-
    callOn #get CGSsh.SshKeyManager'get'params {CGSsh.ref = toCapnpEntityRef ref} mgr
  _ <- callOn #delete CGSsh.SshKey'delete'params kClient
  pure ()

rpcTemplateUpdate :: CapnpConnection -> EntityRef -> Text -> IO ()
rpcTemplateUpdate conn ref yaml = do
  CGCorvus.Daemon'templates'results {CGCorvus.mgr = mgr} <-
    callOn #templates CGCorvus.Daemon'templates'params (ccDaemon conn)
  CGTmpl.TemplateManager'get'results {CGTmpl.template = tClient} <-
    callOn #get CGTmpl.TemplateManager'get'params {CGTmpl.ref = toCapnpEntityRef ref} mgr
  _ <- callOn #update CGTmpl.Template'update'params {CGTmpl.yaml = yaml} tClient
  pure ()

rpcTemplateDelete :: CapnpConnection -> EntityRef -> IO ()
rpcTemplateDelete conn ref = do
  CGCorvus.Daemon'templates'results {CGCorvus.mgr = mgr} <-
    callOn #templates CGCorvus.Daemon'templates'params (ccDaemon conn)
  CGTmpl.TemplateManager'get'results {CGTmpl.template = tClient} <-
    callOn #get CGTmpl.TemplateManager'get'params {CGTmpl.ref = toCapnpEntityRef ref} mgr
  _ <- callOn #delete CGTmpl.Template'delete'params tClient
  pure ()

rpcTemplateInstantiate :: CapnpConnection -> EntityRef -> Text -> Text -> IO Int64
rpcTemplateInstantiate conn ref vmName nodeRef = do
  CGCorvus.Daemon'templates'results {CGCorvus.mgr = mgr} <-
    callOn #templates CGCorvus.Daemon'templates'params (ccDaemon conn)
  CGTmpl.TemplateManager'get'results {CGTmpl.template = tClient} <-
    callOn #get CGTmpl.TemplateManager'get'params {CGTmpl.ref = toCapnpEntityRef ref} mgr
  CGTmpl.Template'instantiate'results {CGTmpl.vm = vmClient} <-
    callOn
      #instantiate
      CGTmpl.Template'instantiate'params
        { CGTmpl.name = vmName
        , CGTmpl.node = toCapnpEntityRef (entityRefFromText nodeRef)
        }
      tClient
  CGVm.Vm'show'results {CGVm.details = det} <-
    callOn #show CGVm.Vm'show'params vmClient
  case det of CGVm.VmDetails {CGVm.id = vid} -> pure vid

rpcTemplateList :: CapnpConnection -> IO [PTm.TemplateVmInfo]
rpcTemplateList conn = do
  CGCorvus.Daemon'templates'results {CGCorvus.mgr = mgr} <-
    callOn #templates CGCorvus.Daemon'templates'params (ccDaemon conn)
  CGTmpl.TemplateManager'list'results {CGTmpl.templates = ts} <-
    callOn #list CGTmpl.TemplateManager'list'params mgr
  pure (map WTmpl.fromCapnpTemplateVmInfo ts)

rpcTemplateShow :: CapnpConnection -> EntityRef -> IO PTm.TemplateDetails
rpcTemplateShow conn refIn = do
  CGCorvus.Daemon'templates'results {CGCorvus.mgr = mgr} <-
    callOn #templates CGCorvus.Daemon'templates'params (ccDaemon conn)
  CGTmpl.TemplateManager'get'results {CGTmpl.template = tClient} <-
    callOn #get CGTmpl.TemplateManager'get'params {CGTmpl.ref = toCapnpEntityRef refIn} mgr
  CGTmpl.Template'show'results {CGTmpl.details = det} <-
    callOn #show CGTmpl.Template'show'params tClient
  failOnWire (WTmpl.fromCapnpTemplateDetails det)

rpcPing :: CapnpConnection -> IO ()
rpcPing conn = do
  _ :: C.Parsed CGCorvus.Daemon'ping'results <-
    callOn #ping CGCorvus.Daemon'ping'params (ccDaemon conn)
  pure ()

rpcStatus :: CapnpConnection -> IO P.StatusInfo
rpcStatus conn = do
  CGCorvus.Daemon'status'results {CGCorvus.info = info} <-
    callOn #status CGCorvus.Daemon'status'params (ccDaemon conn)
  pure (fromCapnpStatusInfo info)

rpcShutdown :: CapnpConnection -> IO ()
rpcShutdown conn = do
  _ :: C.Parsed CGCorvus.Daemon'shutdown'results <-
    callOn #shutdown CGCorvus.Daemon'shutdown'params (ccDaemon conn)
  pure ()
