{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Internal.Untyped.Types (Ptr (..), List (..), NormalList (..), StructList (..), ListOf (..), ListRepOf, Cap (..), Struct (..), ReadCtx, RWCtx, Repr (..), PtrRepr (..), ListRepr (..), NormalListRepr (..), DataSz (..), IgnoreMut (..), MaybePtr (..), Unwrapped, Untyped, UntypedData, UntypedPtr, UntypedSomePtr, UntypedList, UntypedSomeList, ElemRepr, ListReprFor, TraverseMsg (..), TraverseMsgCtx, HasMessage (..), MessageDefault (..)) where

import Capnp.Address (WordAddr (..))
import qualified Capnp.Message as M
import Capnp.Mutability (MaybeMutable (..), Mutability (..))
import Capnp.TraversalLimit (MonadLimit)
import Control.Exception.Safe (impureThrow)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Catch.Pure (CatchT (runCatchT))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Bits (Bits, FiniteBits)
import Data.Kind (Type)
import Data.Word

-- | A an absolute pointer to a value (of arbitrary type) in a message.
-- Note that there is no variant for far pointers, which don't make sense
-- with absolute addressing.
data Ptr mut
  = PtrCap (Cap mut)
  | PtrList (List mut)
  | PtrStruct (Struct mut)

-- | A list of values (of arbitrary type) in a message.
data List mut
  = List0 (ListOf ('Data 'Sz0) mut)
  | List1 (ListOf ('Data 'Sz1) mut)
  | List8 (ListOf ('Data 'Sz8) mut)
  | List16 (ListOf ('Data 'Sz16) mut)
  | List32 (ListOf ('Data 'Sz32) mut)
  | List64 (ListOf ('Data 'Sz64) mut)
  | ListPtr (ListOf ('Ptr 'Nothing) mut)
  | ListStruct (ListOf ('Ptr ('Just 'Struct)) mut)

-- | A "normal" (non-composite) list.
data NormalList mut = NormalList
  { nPtr :: {-# UNPACK #-} !(M.WordPtr mut)
  , nLen :: !Int
  }

data StructList mut = StructList
  { slFirst :: Struct mut
  -- ^ First element. data/ptr sizes are the same for
  -- all elements.
  , slLen :: !Int
  -- ^ Number of elements
  }

-- | A list of values with representation 'r' in a message.
newtype ListOf r mut = ListOf (ListRepOf r mut)

type family ListRepOf (r :: Repr) :: Mutability -> Type where
  ListRepOf ('Ptr ('Just 'Struct)) = StructList
  ListRepOf _r = NormalList

data Cap mut = CapAt (M.Message mut) !Word32

-- | A struct value in a message.
data Struct mut
  = StructAt
      {-# UNPACK #-} !(M.WordPtr mut) -- Start of struct
      !Word16 -- Data section size.
      !Word16 -- Pointer section size.

-- | Type (constraint) synonym for the constraints needed for most read
-- operations.
type ReadCtx m mut = (M.MonadReadMessage mut m, MonadThrow m, MonadLimit m)

-- | Synonym for ReadCtx + WriteCtx
type RWCtx m s = (ReadCtx m ('Mut s), M.WriteCtx m s)

-- | A 'Repr' describes a wire representation for a value. This is
-- mostly used at the type level (using DataKinds); types are
-- parametrized over representations.
data Repr
  = -- | Pointer type. 'Nothing' indicates an AnyPointer, 'Just' describes
    -- a more specific pointer type.
    Ptr (Maybe PtrRepr)
  | -- | Non-pointer type.
    Data DataSz
  deriving stock (Show)

-- | Information about the representation of a pointer type
data PtrRepr
  = -- | Capability pointer.
    Cap
  | -- | List pointer. 'Nothing' describes an AnyList, 'Just' describes
    -- more specific list types.
    List (Maybe ListRepr)
  | -- | A struct (or group).
    Struct
  deriving stock (Show)

-- | Information about the representation of a list type.
data ListRepr where
  -- | A "normal" list
  ListNormal :: NormalListRepr -> ListRepr
  -- | A composite (struct) list
  ListComposite :: ListRepr
  deriving stock (Show)

-- | Information about the representation of a normal (non-composite) list.
data NormalListRepr where
  NormalListData :: DataSz -> NormalListRepr
  NormalListPtr :: NormalListRepr
  deriving stock (Show)

-- | The size of a non-pointer type. @SzN@ represents an @N@-bit value.
data DataSz = Sz0 | Sz1 | Sz8 | Sz16 | Sz32 | Sz64
  deriving stock (Show)

-- | Wrapper for use with 'Untyped'; see docs for 'Untyped'
newtype IgnoreMut a (mut :: Mutability) = IgnoreMut a
  deriving stock (Show, Read, Eq, Ord, Bounded)
  deriving newtype (Enum, Bits, FiniteBits)

-- | Wrapper for use with 'Untyped'; see docs for 'Untyped'.
newtype MaybePtr (mut :: Mutability) = MaybePtr (Maybe (Ptr mut))

-- | Normalizes types returned by 'Untyped'; see docs for 'Untyped'.
type family Unwrapped a where
  Unwrapped (IgnoreMut a _mut) = a
  Unwrapped (MaybePtr mut) = Maybe (Ptr mut)
  Unwrapped a = a

-- | @Untyped r mut@ is an untyped value with representation @r@ stored in
-- a message with mutability @mut@.
--
-- Note that the return type of this type family has kind
-- @'Mutability' -> 'Type'@. This is important, as it allows us
-- to define instances on @'Untyped' r@, and use @'Untyped' r@
-- in constraints.
--
-- This introduces some awkwardnesses though -- we really want
-- this to be @(Maybe (Ptr mut))@ for @'Ptr 'Nothing@, and
-- Int types/Bool/() for @'Data sz@. But we can't because these
-- are the wrong kind.
--
-- So, we hack around this by introducing two newtypes, 'IgnoreMut'
-- and 'MaybePtr', and a type family 'Unwrapped', which lets us
-- use @'Unwrapped' ('Untyped' r mut)@ as the type we really want
-- in some places, though we can't curry it then.
--
-- All this is super super awkward, but this is a low level
-- mostly-internal API; most users will intract with this through
-- the Raw type in "Capnp.Repr", which hides all of this...
type family Untyped (r :: Repr) :: Mutability -> Type where
  Untyped ('Data sz) = IgnoreMut (UntypedData sz)
  Untyped ('Ptr ptr) = UntypedPtr ptr

-- | @UntypedData sz@ is an untyped value with size @sz@.
type family UntypedData (sz :: DataSz) :: Type where
  UntypedData 'Sz0 = ()
  UntypedData 'Sz1 = Bool
  UntypedData 'Sz8 = Word8
  UntypedData 'Sz16 = Word16
  UntypedData 'Sz32 = Word32
  UntypedData 'Sz64 = Word64

-- | Like 'Untyped', but for pointers only.
type family UntypedPtr (r :: Maybe PtrRepr) :: Mutability -> Type where
  UntypedPtr 'Nothing = MaybePtr
  UntypedPtr ('Just r) = UntypedSomePtr r

-- | Like 'UntypedPtr', but doesn't allow AnyPointers.
type family UntypedSomePtr (r :: PtrRepr) :: Mutability -> Type where
  UntypedSomePtr 'Struct = Struct
  UntypedSomePtr 'Cap = Cap
  UntypedSomePtr ('List r) = UntypedList r

-- | Like 'Untyped', but for lists only.
type family UntypedList (r :: Maybe ListRepr) :: Mutability -> Type where
  UntypedList 'Nothing = List
  UntypedList ('Just r) = UntypedSomeList r

-- | Like 'UntypedList', but doesn't allow AnyLists.
type family UntypedSomeList (r :: ListRepr) :: Mutability -> Type where
  UntypedSomeList r = ListOf (ElemRepr r)

-- | @ElemRepr r@ is the representation of elements of lists with
-- representation @r@.
type family ElemRepr (rl :: ListRepr) :: Repr where
  ElemRepr 'ListComposite = 'Ptr ('Just 'Struct)
  ElemRepr ('ListNormal 'NormalListPtr) = 'Ptr 'Nothing
  ElemRepr ('ListNormal ('NormalListData sz)) = 'Data sz

-- | @ListReprFor e@ is the representation of lists with elements
-- whose representation is @e@.
type family ListReprFor (e :: Repr) :: ListRepr where
  ListReprFor ('Data sz) = 'ListNormal ('NormalListData sz)
  ListReprFor ('Ptr ('Just 'Struct)) = 'ListComposite
  ListReprFor ('Ptr _a) = 'ListNormal 'NormalListPtr

-- | N.B. this should mostly be considered an implementation detail, but
-- it is exposed because it is used by generated code.
--
-- 'TraverseMsg' is similar to 'Traversable' from the prelude, but
-- the intent is that rather than conceptually being a "container",
-- the instance is a value backed by a message, and the point of the
-- type class is to be able to apply transformations to the underlying
-- message.
--
-- We don't just use 'Traversable' for this for two reasons:
--
-- 1. While algebraically it makes sense, it would be very unintuitive to
--    e.g. have the 'Traversable' instance for 'List' not traverse over the
--    *elements* of the list.
-- 2. For the instance for WordPtr, we actually need a stronger constraint than
--    Applicative in order for the implementation to type check. A previous
--    version of the library *did* have @tMsg :: Applicative m => ...@, but
--    performance considerations eventually forced us to open up the hood a
--    bit.
class TraverseMsg f where
  tMsg :: (TraverseMsgCtx m mutA mutB) => (M.Message mutA -> m (M.Message mutB)) -> f mutA -> m (f mutB)

type TraverseMsgCtx m mutA mutB =
  ( MonadThrow m
  , M.MonadReadMessage mutA m
  , M.MonadReadMessage mutB m
  )

instance TraverseMsg M.WordPtr where
  tMsg f M.WordPtr {M.pMessage, M.pAddr = pAddr@WordAt {segIndex}} = do
    msg' <- f pMessage
    seg' <- M.getSegment msg' segIndex
    pure
      M.WordPtr
        { M.pMessage = msg'
        , M.pSegment = seg'
        , M.pAddr
        }

instance TraverseMsg Ptr where
  tMsg f = \case
    PtrCap cap ->
      PtrCap <$> tMsg f cap
    PtrList l ->
      PtrList <$> tMsg f l
    PtrStruct s ->
      PtrStruct <$> tMsg f s

instance TraverseMsg Cap where
  tMsg f (CapAt msg n) = CapAt <$> f msg <*> pure n

instance TraverseMsg Struct where
  tMsg f (StructAt ptr dataSz ptrSz) =
    StructAt
      <$> tMsg f ptr
      <*> pure dataSz
      <*> pure ptrSz

instance TraverseMsg List where
  tMsg f = \case
    List0 l -> List0 <$> tMsg f l
    List1 l -> List1 <$> tMsg f l
    List8 l -> List8 <$> tMsg f l
    List16 l -> List16 <$> tMsg f l
    List32 l -> List32 <$> tMsg f l
    List64 l -> List64 <$> tMsg f l
    ListPtr l -> ListPtr <$> tMsg f l
    ListStruct l -> ListStruct <$> tMsg f l

instance (TraverseMsg (ListRepOf r)) => TraverseMsg (ListOf r) where
  tMsg f (ListOf l) = ListOf <$> tMsg f l

instance TraverseMsg NormalList where
  tMsg f NormalList {..} = do
    ptr <- tMsg f nPtr
    pure NormalList {nPtr = ptr, ..}

instance TraverseMsg StructList where
  tMsg f StructList {..} = do
    s <- tMsg f slFirst
    pure StructList {slFirst = s, ..}

-------------------------------------------------------------------------------

-- | Types whose storage is owned by a message..
class HasMessage (f :: Mutability -> Type) where
  -- | Get the message in which the value is stored.
  message :: Unwrapped (f mut) -> M.Message mut

-- | Types which have a "default" value, but require a message
-- to construct it.
--
-- The default is usually conceptually zero-size. This is mostly useful
-- for generated code, so that it can use standard decoding techniques
-- on default values.
class (HasMessage f) => MessageDefault f where
  messageDefault :: (ReadCtx m mut) => M.Message mut -> m (Unwrapped (f mut))

instance HasMessage M.WordPtr where
  message M.WordPtr {M.pMessage} = pMessage

instance HasMessage Ptr where
  message (PtrCap cap) = message @Cap cap
  message (PtrList list) = message @List list
  message (PtrStruct struct) = message @Struct struct

instance HasMessage Cap where
  message (CapAt msg _) = msg

instance HasMessage Struct where
  message (StructAt ptr _ _) = message @M.WordPtr ptr

instance MessageDefault Struct where
  messageDefault msg = do
    pSegment <- M.getSegment msg 0
    pure $ StructAt M.WordPtr {M.pMessage = msg, M.pSegment, M.pAddr = WordAt 0 0} 0 0

instance HasMessage List where
  message (List0 list) = message @(ListOf ('Data 'Sz0)) list
  message (List1 list) = message @(ListOf ('Data 'Sz1)) list
  message (List8 list) = message @(ListOf ('Data 'Sz8)) list
  message (List16 list) = message @(ListOf ('Data 'Sz16)) list
  message (List32 list) = message @(ListOf ('Data 'Sz32)) list
  message (List64 list) = message @(ListOf ('Data 'Sz64)) list
  message (ListPtr list) = message @(ListOf ('Ptr 'Nothing)) list
  message (ListStruct list) = message @(ListOf ('Ptr ('Just 'Struct))) list

instance HasMessage (ListOf ('Ptr ('Just 'Struct))) where
  message (ListOf list) = message @StructList list

instance MessageDefault (ListOf ('Ptr ('Just 'Struct))) where
  messageDefault msg = ListOf <$> messageDefault @StructList msg

instance {-# OVERLAPS #-} (ListRepOf r ~ NormalList) => HasMessage (ListOf r) where
  message (ListOf list) = message @NormalList list

instance {-# OVERLAPS #-} (ListRepOf r ~ NormalList) => MessageDefault (ListOf r) where
  messageDefault msg = ListOf <$> messageDefault @NormalList msg

instance HasMessage NormalList where
  message = M.pMessage . nPtr

instance MessageDefault NormalList where
  messageDefault msg = do
    pSegment <- M.getSegment msg 0
    pure
      NormalList
        { nPtr = M.WordPtr {M.pMessage = msg, M.pSegment, M.pAddr = WordAt 0 0}
        , nLen = 0
        }

instance HasMessage StructList where
  message (StructList s _) = message @Struct s

instance MessageDefault StructList where
  messageDefault msg =
    StructList
      <$> messageDefault @Struct msg
      <*> pure 0

instance (MaybeMutable (ListRepOf r)) => MaybeMutable (ListOf r) where
  thaw (ListOf l) = ListOf <$> thaw l
  freeze (ListOf l) = ListOf <$> freeze l
  unsafeThaw (ListOf l) = ListOf <$> unsafeThaw l
  unsafeFreeze (ListOf l) = ListOf <$> unsafeFreeze l

-------------------------------------------------------------------------------
-- Helpers generated MaybeMutable instances
-------------------------------------------------------------------------------

-- trivial wrapaper around CatchT, so we can add a PrimMonad instance.
newtype CatchTWrap m a = CatchTWrap {runCatchTWrap :: CatchT m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadThrow)

instance (PrimMonad m) => PrimMonad (CatchTWrap m) where
  type PrimState (CatchTWrap m) = PrimState m
  primitive = lift . primitive

-- | @runCatchImpure m@ runs @m@, and if it throws, raises the
-- exception with 'impureThrow'.
runCatchImpure :: (Monad m) => CatchTWrap m a -> m a
{-# INLINEABLE runCatchImpure #-}
runCatchImpure m = do
  res <- runCatchT $ runCatchTWrap m
  pure $ case res of
    Left e -> impureThrow e
    Right v -> v

-------------------------------------------------------------------------------
-- Generated MaybeMutable instances
-------------------------------------------------------------------------------

instance MaybeMutable Ptr where
  thaw = runCatchImpure . tMsg thaw
  freeze = runCatchImpure . tMsg freeze
  unsafeThaw = runCatchImpure . tMsg unsafeThaw
  unsafeFreeze = runCatchImpure . tMsg unsafeFreeze

instance MaybeMutable Struct where
  thaw = runCatchImpure . tMsg thaw
  freeze = runCatchImpure . tMsg freeze
  unsafeThaw = runCatchImpure . tMsg unsafeThaw
  unsafeFreeze = runCatchImpure . tMsg unsafeFreeze

instance MaybeMutable NormalList where
  thaw = runCatchImpure . tMsg thaw
  freeze = runCatchImpure . tMsg freeze
  unsafeThaw = runCatchImpure . tMsg unsafeThaw
  unsafeFreeze = runCatchImpure . tMsg unsafeFreeze
