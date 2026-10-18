{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Internal.Untyped.Allocate (Allocate (..), AllocateNormalList (..), allocStruct, allocCompositeList, allocList0, allocList1, allocList8, allocList16, allocList32, allocList64, allocListPtr, appendCap) where

import Capnp.Address (WordAddr (..))
import Capnp.Bits (BitCount (..), WordCount (..), bitsToBytesCeil, bytesToWordsCeil)
import qualified Capnp.Message as M
import Capnp.Mutability (Mutability (..))
import qualified Capnp.Pointer as P
import Data.Word
import Internal.Untyped.Types

-- | An instace of @'Allocate'@ specifies how to allocate a value with a given representation.
-- This only makes sense for pointers of course, so it is defined on PtrRepr. Of the well-kinded
-- types, only @'List 'Nothing@ is missing an instance.
class Allocate (r :: PtrRepr) where
  -- | Extra information needed to allocate a value:
  --
  -- * For structs, the sizes of the sections.
  -- * For capabilities, the client to attach to the messages.
  -- * For lists, the length, and for composite lists, the struct sizes as well.
  type AllocHint r

  -- | Allocate a value of the given type.
  alloc :: (RWCtx m s) => M.Message ('Mut s) -> AllocHint r -> m (Unwrapped (UntypedSomePtr r ('Mut s)))

instance Allocate 'Struct where
  type AllocHint 'Struct = (Word16, Word16)
  alloc msg = uncurry (allocStruct msg)

instance Allocate ('List ('Just 'ListComposite)) where
  type AllocHint ('List ('Just 'ListComposite)) = (Int, AllocHint 'Struct)
  alloc msg (len, (nWords, nPtrs)) = allocCompositeList msg nWords nPtrs len

instance (AllocateNormalList r) => Allocate ('List ('Just ('ListNormal r))) where
  type AllocHint ('List ('Just ('ListNormal r))) = Int
  alloc = allocNormalList @r

-- | Like 'Allocate', but specialized to normal (non-composite) lists.
--
-- Instead of an 'AllocHint' type family, the hint is always an 'Int',
-- which is the number of elements.
class AllocateNormalList (r :: NormalListRepr) where
  allocNormalList
    :: (RWCtx m s)
    => M.Message ('Mut s)
    -> Int
    -> m (UntypedSomeList ('ListNormal r) ('Mut s))

instance AllocateNormalList ('NormalListData 'Sz0) where allocNormalList = allocList0

instance AllocateNormalList ('NormalListData 'Sz1) where allocNormalList = allocList1

instance AllocateNormalList ('NormalListData 'Sz8) where allocNormalList = allocList8

instance AllocateNormalList ('NormalListData 'Sz16) where allocNormalList = allocList16

instance AllocateNormalList ('NormalListData 'Sz32) where allocNormalList = allocList32

instance AllocateNormalList ('NormalListData 'Sz64) where allocNormalList = allocList64

instance AllocateNormalList 'NormalListPtr where allocNormalList = allocListPtr

-- | Allocate a struct in the message.
allocStruct :: (M.WriteCtx m s) => M.Message ('Mut s) -> Word16 -> Word16 -> m (Struct ('Mut s))
{-# INLINEABLE allocStruct #-}
allocStruct msg dataSz ptrSz = do
  let totalSz = fromIntegral dataSz + fromIntegral ptrSz
  ptr <- M.alloc msg totalSz
  pure $ StructAt ptr dataSz ptrSz

-- | Allocate a composite list.
allocCompositeList
  :: (M.WriteCtx m s)
  => M.Message ('Mut s)
  -- ^ The message to allocate in.
  -> Word16
  -- ^ The size of the data section
  -> Word16
  -- ^ The size of the pointer section
  -> Int
  -- ^ The length of the list in elements.
  -> m (ListOf ('Ptr ('Just 'Struct)) ('Mut s))
{-# INLINEABLE allocCompositeList #-}
allocCompositeList msg dataSz ptrSz len = do
  let eltSize = fromIntegral dataSz + fromIntegral ptrSz
  ptr@M.WordPtr {M.pSegment, M.pAddr = addr@WordAt {wordIndex}} <-
    M.alloc msg (WordCount $ len * eltSize + 1) -- + 1 for the tag word.
  M.write pSegment wordIndex $ P.serializePtr' $ P.StructPtr (fromIntegral len) dataSz ptrSz
  let firstStruct =
        StructAt
          ptr {M.pAddr = addr {wordIndex = wordIndex + 1}}
          dataSz
          ptrSz
  pure $ ListOf $ StructList firstStruct len

-- | Allocate a list of capnproto @Void@ values.
allocList0 :: (M.WriteCtx m s) => M.Message ('Mut s) -> Int -> m (ListOf ('Data 'Sz0) ('Mut s))
{-# INLINEABLE allocList0 #-}

-- | Allocate a list of booleans
allocList1 :: (M.WriteCtx m s) => M.Message ('Mut s) -> Int -> m (ListOf ('Data 'Sz1) ('Mut s))
{-# INLINEABLE allocList1 #-}

-- | Allocate a list of 8-bit values.
allocList8 :: (M.WriteCtx m s) => M.Message ('Mut s) -> Int -> m (ListOf ('Data 'Sz8) ('Mut s))
{-# INLINEABLE allocList8 #-}

-- | Allocate a list of 16-bit values.
allocList16 :: (M.WriteCtx m s) => M.Message ('Mut s) -> Int -> m (ListOf ('Data 'Sz16) ('Mut s))
{-# INLINEABLE allocList16 #-}

-- | Allocate a list of 32-bit values.
allocList32 :: (M.WriteCtx m s) => M.Message ('Mut s) -> Int -> m (ListOf ('Data 'Sz32) ('Mut s))
{-# INLINEABLE allocList32 #-}

-- | Allocate a list of 64-bit words.
allocList64 :: (M.WriteCtx m s) => M.Message ('Mut s) -> Int -> m (ListOf ('Data 'Sz64) ('Mut s))
{-# INLINEABLE allocList64 #-}

-- | Allocate a list of pointers.
allocListPtr :: (M.WriteCtx m s) => M.Message ('Mut s) -> Int -> m (ListOf ('Ptr 'Nothing) ('Mut s))
{-# INLINEABLE allocListPtr #-}

allocList0 msg len = ListOf <$> allocNormalList' 0 msg len

allocList1 msg len = ListOf <$> allocNormalList' 1 msg len

allocList8 msg len = ListOf <$> allocNormalList' 8 msg len

allocList16 msg len = ListOf <$> allocNormalList' 16 msg len

allocList32 msg len = ListOf <$> allocNormalList' 32 msg len

allocList64 msg len = ListOf <$> allocNormalList' 64 msg len

allocListPtr msg len = ListOf <$> allocNormalList' 64 msg len

-- | Allocate a NormalList
allocNormalList'
  :: (M.WriteCtx m s)
  => Int
  -- ^ The number bits per element
  -> M.Message ('Mut s)
  -- ^ The message to allocate in
  -> Int
  -- ^ The number of elements in the list.
  -> m (NormalList ('Mut s))
{-# INLINEABLE allocNormalList' #-}
allocNormalList' bitsPerElt msg len = do
  -- round 'len' up to the nearest word boundary.
  let totalBits = BitCount (len * bitsPerElt)
      totalWords = bytesToWordsCeil $ bitsToBytesCeil totalBits
  ptr <- M.alloc msg totalWords
  pure NormalList {nPtr = ptr, nLen = len}

appendCap :: (M.WriteCtx m s) => M.Message ('Mut s) -> M.Client -> m (Cap ('Mut s))
{-# INLINEABLE appendCap #-}
appendCap msg client = do
  i <- M.appendCap msg client
  pure $ CapAt msg (fromIntegral i)
