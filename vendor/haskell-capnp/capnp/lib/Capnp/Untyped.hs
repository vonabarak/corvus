{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Capnp.Untyped
-- Description: Utilities for reading capnproto messages with no schema.
--
-- The types and functions in this module know about things like structs and
-- lists, but are not schema aware.
--
-- Each of the data types exported by this module is parametrized over the
-- mutability of the message it contains (see "Capnp.Message").
module Capnp.Untyped
  ( -- * Type-level descriptions of wire representations.
    Repr (..)
  , PtrRepr (..)
  , ListRepr (..)
  , NormalListRepr (..)
  , DataSz (..)

    -- * Mapping representations to value types.
  , Untyped
  , UntypedData
  , UntypedPtr
  , UntypedSomePtr
  , UntypedList
  , UntypedSomeList
  , IgnoreMut (..)
  , MaybePtr (..)
  , Unwrapped

    -- * Relating the representations of lists & their elements.
  , Element (..)
  , ListItem (..)
  , ElemRepr
  , ListReprFor

    -- * Working with pointers
  , IsPtrRepr (..)
  , IsListPtrRepr (..)

    -- * Allocating values
  , Allocate (..)
  , AllocateNormalList (..)
  , Ptr (..)
  , List (..)
  , Struct
  , ListOf
  , Cap
  , structByteCount
  , structWordCount
  , structPtrCount
  , structListWordCount
  , structListPtrCount
  , getData
  , getPtr
  , setData
  , setPtr
  , copyStruct
  , copyPtr
  , copyList
  , copyCap
  , getClient
  , get
  , index
  , setIndex
  , take
  , rootPtr
  , setRoot
  , rawBytes
  , marshalBytes
  , ReadCtx
  , RWCtx
  , HasMessage (..)
  , MessageDefault (..)
  , allocStruct
  , allocCompositeList
  , allocList0
  , allocList1
  , allocList8
  , allocList16
  , allocList32
  , allocList64
  , allocListPtr
  , appendCap
  , TraverseMsg (..)
  )
where

import Capnp.Address
  ( WordAddr (..)
  )
import Capnp.Bits
  ( BitCount (..)
  , ByteCount (..)
  , Word1 (..)
  , WordCount (..)
  , bitsToBytesCeil
  , bytesToWordsCeil
  , replaceBits
  , wordsToBytes
  )
import qualified Capnp.Errors as E
import qualified Capnp.Message as M
import Capnp.Mutability (Mutability (..))
import qualified Capnp.Pointer as P
import Capnp.TraversalLimit (LimitT, MonadLimit (invoice))
import Control.Monad (forM_, unless)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.ST (RealWorld)
import Data.Bits hiding (shift)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Function ((&))
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV
import Data.Word
import Internal.BuildPure (PureBuilder)
import Internal.Untyped.Allocate
import Internal.Untyped.Read
import Internal.Untyped.Types
import Internal.Untyped.Write (setPointerTo)
import qualified Language.Haskell.TH as TH
import Prelude hiding (length, take)

-------------------------------------------------------------------------------
-- Untyped refernces to values in a message.
-------------------------------------------------------------------------------

-- | @'ListItem' r@ indicates that @r@ is a representation for elements of some list
-- type. Not every representation is covered; instances exist only for @r@ where
-- @'ElemRepr' ('ListReprFor' r) ~ r@.
class (Element r) => ListItem (r :: Repr) where
  -- | Returns the length of a list
  length :: ListOf r mut -> Int

  -- underlying implementations of index, setIndex and take, but
  -- without bounds checking. Don't call these directly.
  unsafeIndex :: (ReadCtx m mut) => Int -> ListOf r mut -> m (Unwrapped (Untyped r mut))
  unsafeSetIndex
    :: (RWCtx m s, a ~ Unwrapped (Untyped r ('Mut s)))
    => a
    -> Int
    -> ListOf r ('Mut s)
    -> m ()
  unsafeTake :: Int -> ListOf r mut -> ListOf r mut

  checkListOf :: (ReadCtx m mut) => ListOf r mut -> m ()

  -- | Make a copy of the list, in the target message.
  copyListOf :: (RWCtx m s) => ListOf r ('Mut s) -> ListOf r ('Mut s) -> m ()
  {-# INLINE copyListOf #-}
  copyListOf dest src =
    forM_ [0 .. length src - 1] $ \i -> do
      value <- index i src
      setIndex value i dest

  default length :: (ListRepOf r ~ NormalList) => ListOf r mut -> Int
  length (ListOf nlist) = nLen nlist
  {-# INLINE length #-}

  default unsafeIndex
    :: forall m mut
     . ( ReadCtx m mut
       , Integral (Unwrapped (Untyped r mut))
       , ListRepOf r ~ NormalList
       , FiniteBits (Unwrapped (Untyped r mut))
       )
    => Int
    -> ListOf r mut
    -> m (Unwrapped (Untyped r mut))
  unsafeIndex i (ListOf nlist) =
    unsafeIndexBits @(Unwrapped (Untyped r mut)) i nlist
  {-# INLINE unsafeIndex #-}

  default unsafeSetIndex
    :: forall m s a
     . ( RWCtx m s
       , a ~ Unwrapped (Untyped r ('Mut s))
       , ListRepOf r ~ NormalList
       , Integral a
       , Bounded a
       , FiniteBits a
       )
    => a
    -> Int
    -> ListOf r ('Mut s)
    -> m ()
  unsafeSetIndex value i (ListOf nlist) =
    unsafeSetIndexBits @(Unwrapped (Untyped r ('Mut s))) value i nlist
  {-# INLINE unsafeSetIndex #-}

  default unsafeTake :: (ListRepOf r ~ NormalList) => Int -> ListOf r mut -> ListOf r mut
  unsafeTake count (ListOf NormalList {..}) = ListOf NormalList {nLen = count, ..}
  {-# INLINE unsafeTake #-}

  default checkListOf
    :: forall m mut
     . ( ReadCtx m mut
       , ListRepOf r ~ NormalList
       , FiniteBits (Untyped r mut)
       )
    => ListOf r mut
    -> m ()
  checkListOf (ListOf l) =
    checkNormalList
      l
      (fromIntegral $ finiteBitSize (undefined :: Untyped r mut))
  {-# INLINE checkListOf #-}

unsafeIndexBits
  :: forall a m mut
   . ( ReadCtx m mut
     , FiniteBits a
     , Integral a
     )
  => Int
  -> NormalList mut
  -> m a
{-# INLINE unsafeIndexBits #-}
unsafeIndexBits i nlist =
  indexNList @a i nlist (64 `div` finiteBitSize (undefined :: a))

unsafeSetIndexBits
  :: forall a m s
   . ( RWCtx m s
     , Bounded a
     , FiniteBits a
     , Integral a
     )
  => a
  -> Int
  -> NormalList ('Mut s)
  -> m ()
{-# INLINE unsafeSetIndexBits #-}
unsafeSetIndexBits value i nlist =
  setNIndex @a i nlist (64 `div` finiteBitSize value) value

indexNList
  :: forall a m mut
   . (ReadCtx m mut, Integral a)
  => Int
  -> NormalList mut
  -> Int
  -> m a
{-# INLINE indexNList #-}
indexNList i (NormalList M.WordPtr {pSegment, pAddr = WordAt {..}} _) eltsPerWord = do
  let wordIndex' = wordIndex + WordCount (i `div` eltsPerWord)
  word <- M.read pSegment wordIndex'
  let shift = (i `mod` eltsPerWord) * (64 `div` eltsPerWord)
  pure $ fromIntegral $ word `shiftR` shift

setNIndex
  :: forall a m s
   . (RWCtx m s, Bounded a, Integral a)
  => Int
  -> NormalList ('Mut s)
  -> Int
  -> a
  -> m ()
{-# INLINE setNIndex #-}
setNIndex i NormalList {nPtr = M.WordPtr {pSegment, pAddr = WordAt {wordIndex}}} eltsPerWord value = do
  let eltWordIndex = wordIndex + WordCount (i `div` eltsPerWord)
  word <- M.read pSegment eltWordIndex
  let shift = (i `mod` eltsPerWord) * (64 `div` eltsPerWord)
  M.write pSegment eltWordIndex $ replaceBits value word shift

setPtrIndex :: (RWCtx m s) => Int -> NormalList ('Mut s) -> Ptr ('Mut s) -> P.Ptr -> m ()
{-# INLINE setPtrIndex #-}
setPtrIndex i NormalList {nPtr = nPtr@M.WordPtr {pAddr = addr@WordAt {wordIndex}}} absPtr relPtr =
  let srcPtr = nPtr {M.pAddr = addr {wordIndex = wordIndex + WordCount i}}
   in setPointerTo srcPtr (ptrAddr absPtr) relPtr

instance ListItem ('Ptr ('Just 'Struct)) where
  length (ListOf (StructList _ len)) = len
  {-# INLINE length #-}
  unsafeIndex i (ListOf (StructList (StructAt ptr@M.WordPtr {pAddr = addr@WordAt {..}} dataSz ptrSz) _)) = do
    let offset = WordCount $ i * (fromIntegral dataSz + fromIntegral ptrSz)
    let addr' = addr {wordIndex = wordIndex + offset}
    return $ StructAt ptr {M.pAddr = addr'} dataSz ptrSz
  {-# INLINE unsafeIndex #-}
  unsafeSetIndex value i list = do
    dest <- unsafeIndex i list
    copyStruct dest value
  unsafeTake count (ListOf (StructList s _)) = ListOf (StructList s count)
  {-# INLINE unsafeTake #-}

  checkListOf (ListOf (StructList s@(StructAt ptr _ _) len)) =
    checkPtrOffset ptr (fromIntegral len * structSize s)
  {-# INLINE checkListOf #-}

instance ListItem ('Data 'Sz0) where
  unsafeIndex _ _ = pure ()
  {-# INLINE unsafeIndex #-}
  unsafeSetIndex _ _ _ = pure ()
  {-# INLINE unsafeSetIndex #-}
  checkListOf _ = pure ()
  {-# INLINE checkListOf #-}
  copyListOf _ _ = pure ()
  {-# INLINE copyListOf #-}

instance ListItem ('Data 'Sz1) where
  unsafeIndex i (ListOf nlist) = do
    Word1 val <- unsafeIndexBits @Word1 i nlist
    pure val
  {-# INLINE unsafeIndex #-}
  unsafeSetIndex value i (ListOf nlist) =
    unsafeSetIndexBits @Word1 (Word1 value) i nlist
  {-# INLINE unsafeSetIndex #-}
  checkListOf (ListOf l) = checkNormalList l 1
  {-# INLINE copyListOf #-}
  copyListOf (ListOf dest) (ListOf src) = copyDataList dest src 1

instance ListItem ('Data 'Sz8) where
  {-# INLINE copyListOf #-}
  copyListOf (ListOf dest) (ListOf src) = copyDataList dest src 8

instance ListItem ('Data 'Sz16) where
  {-# INLINE copyListOf #-}
  copyListOf (ListOf dest) (ListOf src) = copyDataList dest src 16

instance ListItem ('Data 'Sz32) where
  {-# INLINE copyListOf #-}
  copyListOf (ListOf dest) (ListOf src) = copyDataList dest src 32

instance ListItem ('Data 'Sz64) where
  {-# INLINE copyListOf #-}
  copyListOf (ListOf dest) (ListOf src) = copyDataList dest src 64

instance ListItem ('Ptr 'Nothing) where
  unsafeIndex i (ListOf (NormalList ptr@M.WordPtr {pAddr = addr@WordAt {..}} _)) =
    get ptr {M.pAddr = addr {wordIndex = wordIndex + WordCount i}}
  {-# INLINE unsafeIndex #-}
  unsafeSetIndex value i list@(ListOf nlist) = case value of
    Just p | message @Ptr p /= message @(ListOf ('Ptr 'Nothing)) list -> do
      newPtr <- copyPtr (message @(ListOf ('Ptr 'Nothing)) list) value
      unsafeSetIndex newPtr i list
    Nothing ->
      setNIndex i nlist 1 (P.serializePtr Nothing)
    Just (PtrCap (CapAt _ cap)) ->
      setNIndex i nlist 1 (P.serializePtr (Just (P.CapPtr cap)))
    Just p@(PtrList ptrList) ->
      setPtrIndex i nlist p $ P.ListPtr 0 (listEltSpec ptrList)
    Just p@(PtrStruct (StructAt _ dataSz ptrSz)) ->
      setPtrIndex i nlist p $ P.StructPtr 0 dataSz ptrSz
  {-# INLINEABLE unsafeSetIndex #-}

  checkListOf (ListOf l) = checkNormalList l 64
  {-# INLINE checkListOf #-}

-- | A Capability in a message.
-- | 'Element' supports converting between values of representation
-- @'ElemRepr' ('ListReprFor' r)@ and values of representation @r@.
--
-- At a glance, you might expect this to just be a no-op, but it is actually
-- *not* always the case that @'ElemRepr' ('ListReprFor' r) ~ r@; in the
-- case of pointer types, @'ListReprFor' r@ can contain arbitrary pointers,
-- so information is lost, and it is possible for the list to contain pointers
-- of the incorrect type. In this case, 'fromElement' will throw an error.
--
-- 'toElement' is more trivial.
class Element (r :: Repr) where
  fromElement
    :: forall m mut
     . (ReadCtx m mut)
    => M.Message mut
    -> Unwrapped (Untyped (ElemRepr (ListReprFor r)) mut)
    -> m (Unwrapped (Untyped r mut))
  toElement :: Unwrapped (Untyped r mut) -> Unwrapped (Untyped (ElemRepr (ListReprFor r)) mut)

-- | Operations on types with pointer representations.
class IsPtrRepr (r :: Maybe PtrRepr) where
  toPtr :: Unwrapped (Untyped ('Ptr r) mut) -> Maybe (Ptr mut)
  -- ^ Convert an untyped value of this representation to an AnyPointer.

  fromPtr :: (ReadCtx m mut) => M.Message mut -> Maybe (Ptr mut) -> m (Unwrapped (Untyped ('Ptr r) mut))
  -- ^ Extract a value with this representation from an AnyPointer, failing
  -- if the pointer is the wrong type for this representation.

-- | Operations on types with list representations.
class IsListPtrRepr (r :: ListRepr) where
  rToList :: UntypedSomeList r mut -> List mut
  -- ^ Convert an untyped value of this representation to an AnyList.

  rFromList :: (ReadCtx m mut) => List mut -> m (UntypedSomeList r mut)
  -- ^ Extract a value with this representation from an AnyList, failing
  -- if the list is the wrong type for this representation.

  rFromListMsg :: (ReadCtx m mut) => M.Message mut -> m (UntypedSomeList r mut)
  -- ^ Create a zero-length value with this representation, living in the
  -- provided message.

-- helper function for throwing SchemaViolationError "expected ..."
expected :: (MonadThrow m) => String -> m a
expected msg = throwM $ E.SchemaViolationError $ "expected " ++ msg

-------------------------------------------------------------------------------
-- 'Element' instances
-------------------------------------------------------------------------------

instance Element ('Data sz) where
  fromElement _ = pure
  toElement = id
  {-# INLINE fromElement #-}
  {-# INLINE toElement #-}

instance Element ('Ptr ('Just 'Struct)) where
  fromElement _ = pure
  toElement = id
  {-# INLINE fromElement #-}
  {-# INLINE toElement #-}

instance Element ('Ptr 'Nothing) where
  fromElement _ = pure
  toElement = id
  {-# INLINE fromElement #-}
  {-# INLINE toElement #-}

instance (IsPtrRepr ('Just ('List a))) => Element ('Ptr ('Just ('List a))) where
  fromElement = fromPtr @('Just ('List a))
  toElement = toPtr @('Just ('List a))
  {-# INLINE fromElement #-}
  {-# INLINE toElement #-}

-------------------------------------------------------------------------------
-- 'IsPtrRepr' instances
-------------------------------------------------------------------------------

instance IsPtrRepr 'Nothing where
  toPtr p = p
  fromPtr _ = pure
  {-# INLINE toPtr #-}
  {-# INLINE fromPtr #-}

instance IsPtrRepr ('Just 'Struct) where
  toPtr s = Just (PtrStruct s)
  fromPtr msg Nothing = messageDefault @Struct msg
  fromPtr _ (Just (PtrStruct s)) = pure s
  fromPtr _ _ = expected "pointer to struct"
  {-# INLINE toPtr #-}
  {-# INLINE fromPtr #-}

instance IsPtrRepr ('Just 'Cap) where
  toPtr c = Just (PtrCap c)

  -- A null pointer is the canonical encoding for an omitted /
  -- absent capability in Cap'n Proto (pycapnp and capnproto-c++
  -- both emit it that way when a Capability field isn't set). Map
  -- it to a Cap whose lookup falls through 'M.getCap' to
  -- 'invalidClient' — i.e. a client that errors on use rather than
  -- failing the whole parse. This matches the behaviour of the
  -- 'Just 'Struct' instance above, which returns a default empty
  -- struct on null instead of throwing.
  fromPtr msg Nothing = pure (CapAt msg maxBound)
  fromPtr _ (Just (PtrCap c)) = pure c
  fromPtr _ _ = expected "pointer to capability"
  {-# INLINE toPtr #-}
  {-# INLINE fromPtr #-}

instance (IsListPtrRepr r) => IsPtrRepr ('Just ('List ('Just r))) where
  toPtr l = Just (PtrList (rToList @r l))
  fromPtr msg Nothing = rFromListMsg @r msg
  fromPtr _ (Just (PtrList l)) = rFromList @r l
  fromPtr _ (Just _) = expected "pointer to list"
  {-# INLINE toPtr #-}
  {-# INLINE fromPtr #-}

-- | Return the EltSpec needed for a pointer to the given list.
listEltSpec :: List msg -> P.EltSpec
listEltSpec (ListStruct list@(ListOf (StructList (StructAt _ dataSz ptrSz) _))) =
  P.EltComposite $ fromIntegral (length list) * (fromIntegral dataSz + fromIntegral ptrSz)
listEltSpec (List0 list) = P.EltNormal P.Sz0 $ fromIntegral (length list)
listEltSpec (List1 list) = P.EltNormal P.Sz1 $ fromIntegral (length list)
listEltSpec (List8 list) = P.EltNormal P.Sz8 $ fromIntegral (length list)
listEltSpec (List16 list) = P.EltNormal P.Sz16 $ fromIntegral (length list)
listEltSpec (List32 list) = P.EltNormal P.Sz32 $ fromIntegral (length list)
listEltSpec (List64 list) = P.EltNormal P.Sz64 $ fromIntegral (length list)
listEltSpec (ListPtr list) = P.EltNormal P.SzPtr $ fromIntegral (length list)

-- | Return the starting address of the list.
listAddr :: List msg -> WordAddr
listAddr (ListStruct (ListOf (StructList (StructAt M.WordPtr {pAddr} _ _) _))) =
  -- pAddr is the address of the first element of the list, but
  -- composite lists start with a tag word:
  pAddr {wordIndex = wordIndex pAddr - 1}
listAddr (List0 (ListOf NormalList {nPtr = M.WordPtr {pAddr}})) = pAddr
listAddr (List1 (ListOf NormalList {nPtr = M.WordPtr {pAddr}})) = pAddr
listAddr (List8 (ListOf NormalList {nPtr = M.WordPtr {pAddr}})) = pAddr
listAddr (List16 (ListOf NormalList {nPtr = M.WordPtr {pAddr}})) = pAddr
listAddr (List32 (ListOf NormalList {nPtr = M.WordPtr {pAddr}})) = pAddr
listAddr (List64 (ListOf NormalList {nPtr = M.WordPtr {pAddr}})) = pAddr
listAddr (ListPtr (ListOf NormalList {nPtr = M.WordPtr {pAddr}})) = pAddr

-- | Return the address of the pointer's target. It is illegal to call this on
-- a pointer which targets a capability.
ptrAddr :: Ptr msg -> WordAddr
ptrAddr (PtrCap _) = error "ptrAddr called on a capability pointer."
ptrAddr (PtrStruct (StructAt M.WordPtr {pAddr} _ _)) = pAddr
ptrAddr (PtrList list) = listAddr list

-- | @'setIndex value i list@ Set the @i@th element of @list@ to @value@.
setIndex
  :: (RWCtx m s, ListItem r)
  => Unwrapped (Untyped r ('Mut s))
  -> Int
  -> ListOf r ('Mut s)
  -> m ()
{-# INLINE setIndex #-}
{-# SPECIALIZE setIndex ::
  (ListItem r)
  => Unwrapped (Untyped r ('Mut RealWorld))
  -> Int
  -> ListOf r ('Mut RealWorld)
  -> LimitT IO ()
  #-}
{-# SPECIALIZE setIndex ::
  (ListItem r)
  => Unwrapped (Untyped r ('Mut s))
  -> Int
  -> ListOf r ('Mut s)
  -> PureBuilder s ()
  #-}
setIndex _ i list
  | i < 0 || length list <= i =
      throwM (E.BoundsError i (length list))
setIndex value i list = unsafeSetIndex value i list

-- | Make a copy of a capability inside the target message.
copyCap :: (RWCtx m s) => M.Message ('Mut s) -> Cap ('Mut s) -> m (Cap ('Mut s))
{-# INLINEABLE copyCap #-}
copyCap dest cap = getClient cap >>= appendCap dest

-- | Make a copy of the value at the pointer, in the target message.
copyPtr :: (RWCtx m s) => M.Message ('Mut s) -> Maybe (Ptr ('Mut s)) -> m (Maybe (Ptr ('Mut s)))
{-# INLINEABLE copyPtr #-}
{-# SPECIALIZE copyPtr :: M.Message ('Mut RealWorld) -> Maybe (Ptr ('Mut RealWorld)) -> LimitT IO (Maybe (Ptr ('Mut RealWorld))) #-}
{-# SPECIALIZE copyPtr :: M.Message ('Mut s) -> Maybe (Ptr ('Mut s)) -> PureBuilder s (Maybe (Ptr ('Mut s))) #-}
copyPtr _ Nothing = pure Nothing
copyPtr dest (Just (PtrCap cap)) = Just . PtrCap <$> copyCap dest cap
copyPtr dest (Just (PtrList src)) = Just . PtrList <$> copyList dest src
copyPtr dest (Just (PtrStruct src)) =
  Just . PtrStruct <$> do
    destStruct <-
      allocStruct
        dest
        (fromIntegral $ structWordCount src)
        (fromIntegral $ structPtrCount src)
    copyStruct destStruct src
    pure destStruct

-- | Make a copy of the list, in the target message.
copyList :: (RWCtx m s) => M.Message ('Mut s) -> List ('Mut s) -> m (List ('Mut s))
{-# INLINEABLE copyList #-}
{-# SPECIALIZE copyList :: M.Message ('Mut RealWorld) -> List ('Mut RealWorld) -> LimitT IO (List ('Mut RealWorld)) #-}
{-# SPECIALIZE copyList :: M.Message ('Mut s) -> List ('Mut s) -> PureBuilder s (List ('Mut s)) #-}
copyList dest sourceList = case sourceList of
  List0 src -> List0 <$> allocList0 dest (length src)
  List1 src -> List1 <$> copyNewListOf dest src allocList1
  List8 src -> List8 <$> copyNewListOf dest src allocList8
  List16 src -> List16 <$> copyNewListOf dest src allocList16
  List32 src -> List32 <$> copyNewListOf dest src allocList32
  List64 src -> List64 <$> copyNewListOf dest src allocList64
  ListPtr src -> ListPtr <$> copyNewListOf dest src allocListPtr
  ListStruct src ->
    ListStruct <$> do
      destList <-
        allocCompositeList
          dest
          (fromIntegral $ structListWordCount src)
          (structListPtrCount src)
          (length src)
      copyListOf destList src
      pure destList

copyNewListOf
  :: (ListItem r, RWCtx m s)
  => M.Message ('Mut s)
  -> ListOf r ('Mut s)
  -> (M.Message ('Mut s) -> Int -> m (ListOf r ('Mut s)))
  -> m (ListOf r ('Mut s))
{-# INLINE copyNewListOf #-}
copyNewListOf destMsg src new = do
  dest <- new destMsg (length src)
  copyListOf dest src
  pure dest

-- | @copyDataList dest src bits@ copies n elements of @src@ to @dest@, where n
-- is the length of the smaller list. @bits@ is the number of bits per element
-- in the two lists.
--
-- This should only used for non-pointer types, as it does not do a deep copy and
-- just copies the raw bytes.
--
-- Warning: if you get the @bits@ argument wrong, you may trample over data outside
-- the intended bounds.
copyDataList :: (RWCtx m s) => NormalList ('Mut s) -> NormalList ('Mut s) -> BitCount -> m ()
copyDataList dest src bits = do
  let unpack NormalList {nLen, nPtr = M.WordPtr {pSegment, pAddr = WordAt {wordIndex}}} =
        (nLen, wordIndex, pSegment)

      (srcLen, srcOff, srcSeg) = unpack src
      (destLen, destOff, destSeg) = unpack dest

      len = min destLen srcLen
      lenWords =
        fromIntegral len * bits
          & bitsToBytesCeil
          & bytesToWordsCeil

      sliceVec off =
        SMV.slice (fromIntegral off) (fromIntegral lenWords)
  srcVec <- M.segToVecMut srcSeg
  destVec <- M.segToVecMut destSeg
  SMV.copy
    (sliceVec destOff destVec)
    (sliceVec srcOff srcVec)

-- | @'copyStruct' dest src@ copies the source struct to the destination struct.
copyStruct :: (RWCtx m s) => Struct ('Mut s) -> Struct ('Mut s) -> m ()
{-# INLINEABLE copyStruct #-}
{-# SPECIALIZE copyStruct :: Struct ('Mut RealWorld) -> Struct ('Mut RealWorld) -> LimitT IO () #-}
{-# SPECIALIZE copyStruct :: Struct ('Mut s) -> Struct ('Mut s) -> PureBuilder s () #-}
copyStruct dest src = do
  -- We copy both the data and pointer sections from src to dest,
  -- padding the tail of the destination section with zeros/null
  -- pointers as necessary. If the destination section is
  -- smaller than the source section, this will raise a BoundsError.
  --
  -- TODO: possible enhancement: allow the destination section to be
  -- smaller than the source section if and only if the tail of the
  -- source section is all zeros (default values).
  copySection (dataSection dest) (dataSection src) 0
  copySection (ptrSection dest) (ptrSection src) Nothing
  where
    copySection destination source pad = do
      -- Copy the source section to the destinationination section:
      copyListOf destination source
      -- Pad the remainder with zeros/default values:
      forM_ [length source .. length destination - 1] $ \i ->
        setIndex pad i destination

-- | @index i list@ returns the ith element in @list@. Deducts 1 from the quota
index :: (ReadCtx m mut, ListItem r) => Int -> ListOf r mut -> m (Unwrapped (Untyped r mut))
{-# INLINE index #-}
{-# SPECIALIZE index :: (ListItem r) => Int -> ListOf r 'Const -> LimitT IO (Unwrapped (Untyped r 'Const)) #-}
{-# SPECIALIZE index :: (ListItem r) => Int -> ListOf r ('Mut RealWorld) -> LimitT IO (Unwrapped (Untyped r ('Mut RealWorld))) #-}
{-# SPECIALIZE index :: (ListItem r) => Int -> ListOf r 'Const -> PureBuilder s (Unwrapped (Untyped r 'Const)) #-}
{-# SPECIALIZE index :: (ListItem r) => Int -> ListOf r ('Mut s) -> PureBuilder s (Unwrapped (Untyped r ('Mut s))) #-}
index i list
  | i < 0 || i >= length list =
      throwM (E.BoundsError i (length list - 1))
  | otherwise = unsafeIndex i list

-- | Return a prefix of the list, of the given length.
{-# INLINEABLE take #-}
take :: (ListItem r, MonadThrow m) => Int -> ListOf r mut -> m (ListOf r mut)
take count list
  | length list < count =
      throwM (E.BoundsError count (length list - 1))
  | otherwise = pure $ unsafeTake count list

-- | The data section of a struct, as a list of Word64
dataSection :: Struct mut -> ListOf ('Data 'Sz64) mut
{-# INLINE dataSection #-}
dataSection (StructAt ptr dataSz _) =
  ListOf $ NormalList ptr (fromIntegral dataSz)

-- | The pointer section of a struct, as a list of Ptr
ptrSection :: Struct mut -> ListOf ('Ptr 'Nothing) mut
{-# INLINE ptrSection #-}
ptrSection (StructAt ptr@M.WordPtr {pAddr = addr@WordAt {wordIndex}} dataSz ptrSz) =
  ListOf $
    NormalList
      { nPtr = ptr {M.pAddr = addr {wordIndex = wordIndex + fromIntegral dataSz}}
      , nLen = fromIntegral ptrSz
      }

-- | Get the size (in words) of a struct's data section.
structWordCount :: Struct mut -> WordCount
structWordCount (StructAt _ptr dataSz _ptrSz) = fromIntegral dataSz

-- | Get the size (in bytes) of a struct's data section.
structByteCount :: Struct mut -> ByteCount
structByteCount = wordsToBytes . structWordCount

-- | Get the size of a struct's pointer section.
structPtrCount :: Struct mut -> Word16
structPtrCount (StructAt _ptr _dataSz ptrSz) = ptrSz

-- | Get the size (in words) of the data sections in a struct list.
structListWordCount :: ListOf ('Ptr ('Just 'Struct)) mut -> WordCount
structListWordCount (ListOf (StructList s _)) = structWordCount s

-- | Get the size of the pointer sections in a struct list.
structListPtrCount :: ListOf ('Ptr ('Just 'Struct)) mut -> Word16
structListPtrCount (ListOf (StructList s _)) = structPtrCount s

-- | @'getData' i struct@ gets the @i@th word from the struct's data section,
-- returning 0 if it is absent.
getData :: (ReadCtx m msg) => Int -> Struct msg -> m Word64
{-# INLINE getData #-}
getData i struct
  | fromIntegral (structWordCount struct) <= i = pure 0
  | otherwise = index i (dataSection struct)

-- | @'getPtr' i struct@ gets the @i@th word from the struct's pointer section,
-- returning Nothing if it is absent.
getPtr :: (ReadCtx m msg) => Int -> Struct msg -> m (Maybe (Ptr msg))
{-# INLINE getPtr #-}
getPtr i struct
  | fromIntegral (structPtrCount struct) <= i = do
      invoice 1
      pure Nothing
  | otherwise = do
      ptr <- index i (ptrSection struct)
      checkPtr ptr
      invoicePtr ptr
      pure ptr

checkPtr :: (ReadCtx m mut) => Maybe (Ptr mut) -> m ()
{-# INLINEABLE checkPtr #-}
checkPtr Nothing = pure ()
checkPtr (Just (PtrCap c)) = checkCap c
checkPtr (Just (PtrList l)) = checkList l
checkPtr (Just (PtrStruct s)) = checkStruct s

checkCap :: (ReadCtx m mut) => Cap mut -> m ()
{-# INLINEABLE checkCap #-}
checkCap (CapAt _ _) = pure ()

-- No need to do anything here; an out of bounds index is just treated
-- as null.

checkList :: (ReadCtx m mut) => List mut -> m ()
{-# INLINEABLE checkList #-}
checkList (List0 l) = checkListOf @('Data 'Sz0) l
checkList (List1 l) = checkListOf @('Data 'Sz1) l
checkList (List8 l) = checkListOf @('Data 'Sz8) l
checkList (List16 l) = checkListOf @('Data 'Sz16) l
checkList (List32 l) = checkListOf @('Data 'Sz32) l
checkList (List64 l) = checkListOf @('Data 'Sz64) l
checkList (ListPtr l) = checkListOf @('Ptr 'Nothing) l
checkList (ListStruct l) = checkListOf @('Ptr ('Just 'Struct)) l

checkNormalList :: (ReadCtx m mut) => NormalList mut -> BitCount -> m ()
{-# INLINEABLE checkNormalList #-}
checkNormalList NormalList {nPtr, nLen} eltSize =
  let nBits = fromIntegral nLen * eltSize
      nWords = bytesToWordsCeil $ bitsToBytesCeil nBits
   in checkPtrOffset nPtr nWords

checkStruct :: (ReadCtx m mut) => Struct mut -> m ()
{-# INLINEABLE checkStruct #-}
checkStruct s@(StructAt ptr _ _) =
  checkPtrOffset ptr (structSize s)

checkPtrOffset :: (ReadCtx m mut) => M.WordPtr mut -> WordCount -> m ()
{-# INLINEABLE checkPtrOffset #-}
checkPtrOffset M.WordPtr {pSegment, pAddr = WordAt {wordIndex}} size = do
  segWords <- M.numWords pSegment
  let maxIndex = fromIntegral segWords - 1
  unless (wordIndex >= 0) $
    throwM (E.BoundsError (fromIntegral wordIndex) maxIndex)
  unless (wordIndex + size <= segWords) $
    throwM
      (E.BoundsError (fromIntegral (wordIndex + size) - 1) maxIndex)

structSize :: Struct mut -> WordCount
structSize s = structWordCount s + fromIntegral (structPtrCount s)

-- | Invoice the traversal limit for all data reachable via the pointer
-- directly, i.e. without following further pointers.
--
-- The minimum possible cost is 1, and for lists will always be proportional
-- to the length of the list, even if the size of the elements is zero.
invoicePtr :: (MonadLimit m) => Maybe (Ptr mut) -> m ()
{-# INLINEABLE invoicePtr #-}
{-# SPECIALIZE invoicePtr :: Maybe (Ptr ('Mut RealWorld)) -> LimitT IO () #-}
{-# SPECIALIZE invoicePtr :: Maybe (Ptr ('Mut s)) -> PureBuilder s () #-}
invoicePtr p = invoice $! ptrInvoiceSize p

ptrInvoiceSize :: Maybe (Ptr mut) -> WordCount
{-# INLINEABLE ptrInvoiceSize #-}
ptrInvoiceSize = \case
  Nothing -> 1
  Just (PtrCap _) -> 1
  Just (PtrStruct s) -> structInvoiceSize s
  Just (PtrList l) -> listInvoiceSize l

listInvoiceSize :: List mut -> WordCount
{-# INLINEABLE listInvoiceSize #-}
listInvoiceSize list =
  max 1 $! case list of
    List0 l -> fromIntegral $! length l
    List1 l -> fromIntegral $! length l `div` 64
    List8 l -> fromIntegral $! length l `div` 8
    List16 l -> fromIntegral $! length l `div` 4
    List32 l -> fromIntegral $! length l `div` 2
    List64 l -> fromIntegral $! length l
    ListPtr l -> fromIntegral $! length l
    ListStruct (ListOf (StructList s len)) ->
      structInvoiceSize s * fromIntegral len

structInvoiceSize :: Struct mut -> WordCount
{-# INLINEABLE structInvoiceSize #-}
structInvoiceSize (StructAt _ dataSz ptrSz) =
  max 1 (fromIntegral dataSz + fromIntegral ptrSz)

-- | @'setData' value i struct@ sets the @i@th word in the struct's data section
-- to @value@.
{-# INLINE setData #-}
setData
  :: (ReadCtx m ('Mut s), M.WriteCtx m s)
  => Word64
  -> Int
  -> Struct ('Mut s)
  -> m ()
setData value i = setIndex value i . dataSection

-- | @'setData' value i struct@ sets the @i@th pointer in the struct's pointer
-- section to @value@.
setPtr :: (ReadCtx m ('Mut s), M.WriteCtx m s) => Maybe (Ptr ('Mut s)) -> Int -> Struct ('Mut s) -> m ()
{-# INLINE setPtr #-}
setPtr value i = setIndex value i . ptrSection

-- | 'rawBytes' returns the raw bytes corresponding to the list.
rawBytes :: (ReadCtx m 'Const) => ListOf ('Data 'Sz8) 'Const -> m BS.ByteString
{-# INLINEABLE rawBytes #-}
-- TODO: we can get away with a more lax context than ReadCtx, maybe even make
-- this non-monadic.
rawBytes (ListOf (NormalList M.WordPtr {pSegment, pAddr = WordAt {wordIndex}} len)) = do
  let bytes = M.toByteString pSegment
  let ByteCount byteOffset = wordsToBytes wordIndex
  pure $ BS.take len $ BS.drop byteOffset bytes

-- | Bulk-copy a 'BS.ByteString' into a mutable @Data Sz8@ list. The
-- destination list must already be allocated with at least
-- @BS.length bytes@ elements. The copy bottoms out in 'SV.unsafeCopy'
-- which is a single @memcpy@.
--
-- Symmetric in spirit to 'rawBytes' on the read side. The
-- @Marshal Data BS.ByteString@ instance in "Capnp.Basics" used to
-- iterate byte-by-byte via 'setIndex' — at large sizes (multi-MiB
-- 'Data' fields, e.g. the 'ByteSink.write' RPC payload in
-- streaming-bulk applications) that loop dominated wall-clock and
-- capped throughput at single-digit MiB/s. This helper makes the
-- write path proportional to a 'memcpy'.
marshalBytes :: (RWCtx m s) => ListOf ('Data 'Sz8) ('Mut s) -> BS.ByteString -> m ()
{-# INLINEABLE marshalBytes #-}
marshalBytes (ListOf (NormalList M.WordPtr {pSegment, pAddr = WordAt {wordIndex}} _)) bytes = do
  destWord64 <- M.segToVecMut pSegment
  let destWord8 = SMV.unsafeCast destWord64
      ByteCount byteOff = wordsToBytes wordIndex
      srcLen = BS.length bytes
      (srcFPtr, srcOff, _) = BSI.toForeignPtr bytes
      srcVec :: SV.Vector Word8
      srcVec = SV.unsafeFromForeignPtr srcFPtr srcOff srcLen
      destSlice = SMV.unsafeSlice byteOff srcLen destWord8
  SV.unsafeCopy destSlice srcVec

-- | Returns the root pointer of a message.
rootPtr :: (ReadCtx m mut) => M.Message mut -> m (Struct mut)
{-# INLINEABLE rootPtr #-}
rootPtr msg = do
  seg <- M.getSegment msg 0
  root <-
    get
      M.WordPtr
        { pMessage = msg
        , pSegment = seg
        , pAddr = WordAt 0 0
        }
  checkPtr root
  invoicePtr root
  case root of
    Just (PtrStruct struct) -> pure struct
    Nothing -> messageDefault @Struct msg
    _ ->
      throwM $
        E.SchemaViolationError
          "Unexpected root type; expected struct."

-- | Make the given struct the root object of its message.
setRoot :: (M.WriteCtx m s) => Struct ('Mut s) -> m ()
{-# INLINEABLE setRoot #-}
setRoot (StructAt M.WordPtr {pMessage, pAddr = addr} dataSz ptrSz) = do
  pSegment <- M.getSegment pMessage 0
  let rootPointer = M.WordPtr {pMessage, pSegment, pAddr = WordAt 0 0}
  setPointerTo rootPointer addr (P.StructPtr 0 dataSz ptrSz)

do
  let mkIsListPtrRepr (r, listC, str) =
        [d|
          instance IsListPtrRepr $r where
            rToList = $(pure $ TH.ConE listC)
            rFromList $(pure $ TH.ConP listC [] [TH.VarP (TH.mkName "l")]) = pure l
            rFromList _ = expected $(pure $ TH.LitE $ TH.StringL $ "pointer to " ++ str)
            rFromListMsg = messageDefault @(Untyped ('Ptr ('Just ('List ('Just $r)))))
          |]
  concat
    <$> traverse
      mkIsListPtrRepr
      [
        ( [t|'ListNormal ('NormalListData 'Sz0)|]
        , 'List0
        , "List(Void)"
        )
      ,
        ( [t|'ListNormal ('NormalListData 'Sz1)|]
        , 'List1
        , "List(Bool)"
        )
      ,
        ( [t|'ListNormal ('NormalListData 'Sz8)|]
        , 'List8
        , "List(UInt8)"
        )
      ,
        ( [t|'ListNormal ('NormalListData 'Sz16)|]
        , 'List16
        , "List(UInt16)"
        )
      ,
        ( [t|'ListNormal ('NormalListData 'Sz32)|]
        , 'List32
        , "List(UInt32)"
        )
      ,
        ( [t|'ListNormal ('NormalListData 'Sz64)|]
        , 'List64
        , "List(UInt64)"
        )
      ,
        ( [t|'ListNormal 'NormalListPtr|]
        , 'ListPtr
        , "List(AnyPointer)"
        )
      ,
        ( [t|'ListComposite|]
        , 'ListStruct
        , "composite list"
        )
      ]
