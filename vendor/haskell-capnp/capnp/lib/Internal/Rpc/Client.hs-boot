{-# LANGUAGE DataKinds #-}

module Internal.Rpc.Client (acceptCap, acceptPayload, call, dropConnExport, emitCap, export, genSendableCapTableRaw, resolveClientClient, resolveClientExn) where

import Capnp.Fields (Which)
import qualified Capnp.Gen.Capnp.Rpc as R
import Capnp.Mutability (Mutability (..))
import Capnp.Repr (Raw)
import Capnp.Rpc.Promise (Promise)
import qualified Capnp.Rpc.Server as Server
import Capnp.TraversalLimit (LimitT)
import qualified Capnp.Untyped as UntypedRaw
import Control.Concurrent.STM (STM)
import Control.Monad.STM.Class (MonadSTM)
import Internal.Rpc.Breaker (Client, Pipeline)
import Internal.Rpc.Types
import Supervisors (Supervisor)

acceptCap :: Conn -> R.Parsed (Which R.CapDescriptor) -> STM Client
acceptPayload :: Conn -> Raw R.Payload 'Const -> LimitT STM Payload
call :: (MonadSTM m) => Server.CallInfo -> Client -> m (Promise Pipeline)
dropConnExport :: Conn -> Client' -> STM ()
emitCap :: Conn -> Client -> STM (R.Parsed (Which R.CapDescriptor))
export :: (MonadSTM m) => Supervisor -> Server.ServerOps -> m Client
genSendableCapTableRaw
  :: Conn
  -> Maybe (UntypedRaw.Ptr 'Const)
  -> STM [R.Parsed R.CapDescriptor]
resolveClientClient :: TmpDest -> (PromiseState -> STM ()) -> Client -> STM ()
resolveClientExn :: TmpDest -> (PromiseState -> STM ()) -> R.Parsed R.Exception -> STM ()
