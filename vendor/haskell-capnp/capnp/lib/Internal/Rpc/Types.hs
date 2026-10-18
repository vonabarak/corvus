{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Internal.Rpc.Types (RawMPtr, RpcError (..), EmbargoId (..), QAId (..), IEId (..), Conn (..), LiveState (..), Conn' (..), ConnConfig (..), IdPool (..), EntryQA (..), EntryI (..), EntryE (..), IsClient (..), Client' (..), Pipeline' (..), PipelineState (..), PromiseState (..), TmpDest (..), LocalDest (..), RemoteDest (..), ImportRef (..), ExportMap (..), MsgTarget (..), PromisedAnswer (..), Call (..), Return (..), Return' (..), Payload (..), makeAbortExn, newIdPool, newId, freeId, wrapClient, unwrapClient, wrapPipeline, unwrapPipeline, qaWord, ieWord, embargoWord) where

import Capnp.Bits (WordCount, bytesToWordsFloor)
import qualified Capnp.Gen.Capnp.Rpc as R
import Capnp.Message (Message)
import Capnp.Mutability (Mutability (Const))
import Capnp.Rpc.Errors (wrapException)
import Capnp.Rpc.Promise (Fulfiller)
import qualified Capnp.Rpc.Server as Server (CallInfo)
import qualified Capnp.Untyped as UntypedRaw (Ptr)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM
  ( STM
  , TChan
  , TMVar
  , TQueue
  , TVar
  , modifyTVar'
  , newTVar
  , readTVar
  , retry
  , writeTVar
  )
import Control.Exception.Safe
  ( Exception (fromException)
  , SomeException
  )
import Control.Monad (join)
import Data.Default (Default (def))
import Data.Dynamic (fromDynamic)
import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64)
import GHC.Generics (Generic)
import Internal.Rc (Rc)
import Internal.Rpc.Breaker
  ( Client (..)
  , Pipeline (..)
  , makeOpaque
  , reflectOpaque
  )
import Internal.SnocList (SnocList)
import qualified Lifetimes.Gc as Fin (Cell)
import qualified StmContainers.Map as M (Map)
import System.Mem.StableName (StableName, hashStableName)

-- | We use this type often enough that the types get noisy without a shorthand:
type RawMPtr = Maybe (UntypedRaw.Ptr 'Const)

-- | Errors which can be thrown by the rpc system.
data RpcError
  = -- | The remote vat sent us an abort message.
    ReceivedAbort (R.Parsed R.Exception)
  | -- | We sent an abort to the remote vat.
    SentAbort (R.Parsed R.Exception)
  deriving stock (Show, Eq, Generic)

makeAbortExn :: Bool -> SomeException -> RpcError
makeAbortExn debugMode e =
  fromMaybe
    (SentAbort (wrapException debugMode e))
    (fromException e)

instance Exception RpcError

newtype EmbargoId = EmbargoId Word32
  deriving stock (Eq)
  deriving newtype (Hashable)

newtype QAId = QAId Word32
  deriving stock (Eq)
  deriving newtype (Hashable)

newtype IEId = IEId Word32
  deriving stock (Eq)
  deriving newtype (Hashable)

-- We define these to just show the number; the derived instances would include
-- data constructors, which is a bit weird since these show up in output that
-- is sometimes shown to users.
instance Show QAId where
  show = show . qaWord

instance Show IEId where
  show = show . ieWord

-- | A connection to a remote vat
data Conn = Conn
  { stableName :: StableName (MVar ())
  , -- So we can use the connection as a map key. The MVar used to create
    -- this is just an arbitrary value; the only property we care about
    -- is that it is distinct for each 'Conn', so we use something with
    -- reference semantics to guarantee this.

    debugMode :: !Bool
  , -- whether to include extra (possibly sensitive) info in error messages.

    done :: TMVar (Either SomeException ())
  , -- finished when the connection is shut down. Note: this should not be used
    -- by most of the code within this module; see the implementation of acquireConn.
    liveState :: TVar LiveState
  }

data LiveState
  = Live Conn'
  | Dead

data Conn' = Conn'
  { sendQ :: TChan (Message 'Const, Fulfiller ())
  , -- queue of messages to send sent to the remote vat; these are actually
    -- sent by a dedicated thread (see 'sendLoop').
    --
    -- The fulfiller is fulfilled after the message actually hits the transport.
    --
    -- The queue mainly exists for the sake of messages that are sent *while
    -- processing incomming messages*, since we cannot block in those cases,
    -- but it is used for all message sends to enforce ordering. The fulfiller
    -- is used by parts of the code (basically just calls) that want to block
    -- until their message is actually written to the socket.

    availableCallWords :: TVar WordCount
  , -- Semaphore used to limit the memory that can be used by in-progress
    -- calls originating from this connection. We don't just use a TSem
    -- because waitTSem doesn't let us wait for more than one token with a
    -- single call.

    questionIdPool :: IdPool
  , exportIdPool :: IdPool
  , -- Pools of identifiers for new questions and exports

    questions :: M.Map QAId EntryQA
  , answers :: M.Map QAId EntryQA
  , exports :: M.Map IEId EntryE
  , imports :: M.Map IEId EntryI
  , embargos :: M.Map EmbargoId (Fulfiller ())
  , -- Outstanding embargos. When we receive a 'Disembargo' message with its
    -- context field set to receiverLoopback, we look up the embargo id in
    -- this table, and fulfill the promise.

    pendingCallbacks :: TQueue (IO ())
  , -- See Note [callbacks]

    myBootstrap :: Maybe Client
    -- The capability which should be served as this connection's bootstrap
    -- interface (if any).
  }

instance Eq Conn where
  Conn {stableName = x} == Conn {stableName = y} = x == y

instance Hashable Conn where
  hash Conn {stableName} = hashStableName stableName
  hashWithSalt _ = hash

-- | Configuration information for a connection.
data ConnConfig = ConnConfig
  { maxQuestions :: !Word32
  -- ^ The maximum number of simultanious outstanding requests to the peer
  -- vat. Once this limit is reached, further questsions will block until
  -- some of the existing questions have been answered.
  --
  -- Defaults to 128.
  , maxExports :: !Word32
  -- ^ The maximum number of objects which may be exported on this connection.
  --
  -- Defaults to 8192.
  , maxCallWords :: !WordCount
  -- ^ The maximum total size of outstanding call messages that will be
  -- accepted; if this limit is reached, the implementation will not read
  -- more messages from the connection until some calls have completed
  -- and freed up enough space.
  --
  -- Defaults to 32MiB in words.
  , debugMode :: !Bool
  -- ^ In debug mode, errors reported by the RPC system to its peers will
  -- contain extra information. This should not be used in production, as
  -- it is possible for these messages to contain sensitive information,
  -- but it can be useful for debugging.
  --
  -- Defaults to 'False'.
  , bootstrap :: Maybe Client
  -- ^ The bootstrap interface we should serve for this connection.
  -- If 'bootstrap' is 'Nothing' (the default), we will respond to
  -- bootstrap messages with an exception.
  }

instance Default ConnConfig where
  def =
    ConnConfig
      { maxQuestions = 128
      , maxExports = 8192
      , maxCallWords = bytesToWordsFloor $ 32 * 1024 * 1024
      , debugMode = False
      , bootstrap = Nothing
      }

-- | A pool of ids; used when choosing identifiers for questions and exports.
newtype IdPool = IdPool (TVar [Word32])

-- | @'newIdPool' size@ creates a new pool of ids, with @size@ available ids.
newIdPool :: Word32 -> STM IdPool
newIdPool size = IdPool <$> newTVar [0 .. size - 1]

-- | Get a new identifier from the pool. Retries if the pool is empty.
newId :: IdPool -> STM Word32
newId (IdPool pool) =
  readTVar pool >>= \case
    [] -> retry
    (identifier : ids) -> do
      writeTVar pool $! ids
      pure identifier

-- | Return an identifier to the pool.
freeId :: IdPool -> Word32 -> STM ()
freeId (IdPool pool) identifier = modifyTVar' pool (identifier :)

-- | An entry in our questions or answers table.
data EntryQA
  = NewQA (SnocList (R.Parsed R.Finish -> STM ())) (SnocList (Return -> STM ()))
  | HaveReturn Return (SnocList (R.Parsed R.Finish -> STM ()))
  | HaveFinish (R.Parsed R.Finish) (SnocList (Return -> STM ()))

-- | An entry in our imports table.
data EntryI = EntryI
  { localRc :: Rc ()
  -- ^ A refcount cell with a finalizer attached to it; when the finalizer
  -- runs it will remove this entry from the table and send a release
  -- message to the remote vat.
  , remoteRc :: !Word32
  -- ^ The reference count for this object as understood by the remote
  -- vat. This tells us what to send in the release message's count field.
  , proxies :: ExportMap
  -- ^ See Note [proxies]
  , promiseState
      :: Maybe
          ( TVar PromiseState
          , TmpDest -- origTarget field. TODO(cleanup): clean this up a bit.
          )
  -- ^ If this entry is a promise, this will contain the state of that
  -- promise, so that it may be used to create PromiseClients and
  -- update the promise when it resolves.
  }

-- | An entry in our exports table.
data EntryE = EntryE
  { client :: Client'
  -- ^ The client. We cache it in the table so there's only one object
  -- floating around, which lets us attach a finalizer without worrying
  -- about it being run more than once.
  , refCount :: !Word32
  -- ^ The refcount for this entry. This lets us know when we can drop
  -- the entry from the table.
  }

-- | Types which may be converted to and from 'Client's. Typically these
-- will be simple type wrappers for capabilities.
class IsClient a where
  -- | Convert a value to a client.
  toClient :: a -> Client

  -- | Convert a client to a value.
  fromClient :: Client -> a

instance IsClient Client where
  toClient = id
  fromClient = id

-- | See Note [Breaker]
wrapClient :: Maybe Client' -> Client
wrapClient = Client . makeOpaque

-- | See Note [Breaker]
unwrapClient :: Client -> Maybe Client'
unwrapClient (Client o) =
  join $ fromDynamic $ reflectOpaque o

data Client'
  = LocalClient ExportMap (Rc (Server.CallInfo -> STM ())) (Fin.Cell ()) (forall a. (Typeable a) => Maybe a)
  | PromiseClient (TVar PromiseState) ExportMap TmpDest
  | ImportClient (Fin.Cell ImportRef)

data Pipeline' = Pipeline'
  { state :: TVar PipelineState
  , steps :: SnocList Word16
  }
  deriving stock (Eq)

-- | See Note [Breaker]
wrapPipeline :: Pipeline' -> Pipeline
wrapPipeline = Pipeline . makeOpaque

-- | See Note [Breaker]
unwrapPipeline :: Pipeline -> Pipeline'
unwrapPipeline (Pipeline o) =
  case fromDynamic (reflectOpaque o) of
    Nothing -> error "invalid pipeline; dynamic unwrap failed"
    Just p -> p

data PipelineState
  = PendingRemotePipeline !QAId (M.Map (SnocList Word16) Client) Conn
  | PendingLocalPipeline (SnocList (Fulfiller RawMPtr))
  | ReadyPipeline (Either (R.Parsed R.Exception) RawMPtr)

-- | The current state of a 'PromiseClient'.
data PromiseState
  = Ready Client
  | Embargo (TQueue Server.CallInfo)
  | Pending TmpDest
  | Error (R.Parsed R.Exception)

-- | A temporary destination for calls on an unresolved promise.
data TmpDest
  = -- | A destination that is local to this vat.
    LocalDest LocalDest
  | -- | A destination in another vat.
    RemoteDest RemoteDest

newtype LocalDest
  = -- | Queue the calls in a buffer.
    LocalBuffer {callBuffer :: TQueue Server.CallInfo}

data RemoteDest
  = AnswerDest Conn PromisedAnswer
  | ImportDest (Fin.Cell ImportRef)

-- | A reference to a capability in our import table/a remote vat's export
-- table.
data ImportRef = ImportRef
  { conn :: Conn
  -- ^ The connection to the remote vat.
  , importId :: !IEId
  -- ^ The import id for this capability.
  , proxies :: ExportMap
  -- ^ Export ids to use when this client is passed to a vat other than
  -- the one identified by 'conn'. See Note [proxies]
  }

-- Ideally we could just derive these, but stm-containers doesn't have Eq
-- instances, so neither does ExportMap. not all of the fields are actually
-- necessary to check equality though. See also
-- https://github.com/nikita-volkov/stm-hamt/pull/1
instance Eq ImportRef where
  ImportRef {conn = cx, importId = ix} == ImportRef {conn = cy, importId = iy} =
    cx == cy && ix == iy

instance Eq Client' where
  (LocalClient _ x _ _) == (LocalClient _ y _ _) =
    x == y
  (PromiseClient x _ _) == (PromiseClient y _ _) =
    x == y
  ImportClient x == ImportClient y =
    x == y
  _ == _ =
    False

-- | an 'ExportMap' tracks a mapping from connections to export IDs; it is
-- used to ensure that we re-use export IDs for capabilities when passing
-- them to remote vats. This used for locally hosted capabilities, but also
-- by proxied imports (see Note [proxies]).
newtype ExportMap = ExportMap (M.Map Conn IEId)

-- The below correspond to the similarly named types in
-- rpc.capnp, except:
--

-- * They use our newtype wrappers for ids

-- * They don't have unknown variants

-- * AnyPointers are left un-parsed

-- * PromisedAnswer's transform field is just a list of pointer offsets,

--   rather than a union with no other actually-useful variants.

-- * PromisedAnswer's transform field is a SnocList, for efficient appending.

data MsgTarget
  = ImportTgt !IEId
  | AnswerTgt PromisedAnswer

data PromisedAnswer = PromisedAnswer
  { answerId :: !QAId
  , transform :: SnocList Word16
  }

data Call = Call
  { questionId :: !QAId
  , target :: !MsgTarget
  , interfaceId :: !Word64
  , methodId :: !Word16
  , params :: !Payload
  }

data Return = Return
  { answerId :: !QAId
  , releaseParamCaps :: !Bool
  , union' :: Return'
  }

data Return'
  = Return'results Payload
  | Return'exception (R.Parsed R.Exception)
  | Return'canceled
  | Return'resultsSentElsewhere
  | Return'takeFromOtherQuestion QAId
  | Return'acceptFromThirdParty RawMPtr

data Payload = Payload
  { content :: RawMPtr
  , capTable :: [R.Parsed R.CapDescriptor]
  }

qaWord :: QAId -> Word32
qaWord (QAId value) = value

ieWord :: IEId -> Word32
ieWord (IEId value) = value

embargoWord :: EmbargoId -> Word32
embargoWord (EmbargoId value) = value
