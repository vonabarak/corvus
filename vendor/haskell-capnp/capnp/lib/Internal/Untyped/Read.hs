{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Internal.Untyped.Read (getClient, get) where

import Capnp.Address (WordAddr (..), resolveOffset)
import qualified Capnp.Errors as E
import qualified Capnp.Message as M
import Capnp.Mutability (Mutability (..))
import qualified Capnp.Pointer as P
import Capnp.TraversalLimit (LimitT)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.ST (RealWorld)
import Data.Word
import Internal.BuildPure (PureBuilder)
import Internal.Untyped.Types

getClient :: (ReadCtx m mut) => Cap mut -> m M.Client
{-# INLINEABLE getClient #-}
getClient (CapAt msg idx) = M.getCap msg (fromIntegral idx)

-- | @get ptr@ returns the Ptr stored at @ptr@.
-- Deducts 1 from the quota for each word read (which may be multiple in the
-- case of far pointers).
get :: (ReadCtx m mut) => M.WordPtr mut -> m (Maybe (Ptr mut))
{-# INLINEABLE get #-}
{-# SPECIALIZE get :: M.WordPtr ('Mut RealWorld) -> LimitT IO (Maybe (Ptr ('Mut RealWorld))) #-}
{-# SPECIALIZE get :: M.WordPtr ('Mut s) -> PureBuilder s (Maybe (Ptr ('Mut s))) #-}
get ptr = do
  word <- M.getWord ptr
  case P.parsePtr word of
    Just (P.FarPtr twoWords offset segment) -> getFar ptr twoWords offset segment
    v -> getNear ptr v

getFar :: (M.MonadReadMessage mut m, MonadThrow m) => M.WordPtr mut -> Bool -> Word32 -> Word32 -> m (Maybe (Ptr mut))
getFar M.WordPtr {M.pMessage} twoWords offset segment = do
  landingSegment <- M.getSegment pMessage (fromIntegral segment)
  let addr' =
        WordAt
          { wordIndex = fromIntegral offset
          , segIndex = fromIntegral segment
          }
  let landingPtr =
        M.WordPtr
          { M.pMessage
          , M.pSegment = landingSegment
          , M.pAddr = addr'
          }
  landingPad <- M.getWord landingPtr
  if not twoWords
    then getNear landingPtr (P.parsePtr landingPad)
    else do
      case P.parsePtr landingPad of
        Just (P.FarPtr False off seg) -> do
          let segIndex = fromIntegral seg
          finalSegment <- M.getSegment pMessage segIndex
          tagWord <-
            M.getWord
              M.WordPtr
                { M.pMessage
                , M.pSegment = landingSegment
                , M.pAddr = addr' {wordIndex = wordIndex addr' + 1}
                }
          let finalPtr =
                M.WordPtr
                  { M.pMessage
                  , M.pSegment = finalSegment
                  , M.pAddr =
                      WordAt
                        { wordIndex = fromIntegral off
                        , segIndex
                        }
                  }
          case P.parsePtr tagWord of
            Just (P.StructPtr 0 dataSz ptrSz) ->
              return $
                Just $
                  PtrStruct $
                    StructAt finalPtr dataSz ptrSz
            Just (P.ListPtr 0 eltSpec) ->
              Just . PtrList <$> getList finalPtr eltSpec
            -- TODO: I'm not sure whether far pointers to caps are
            -- legal; it's clear how they would work, but I don't
            -- see a use, and the spec is unclear. Should check
            -- how the reference implementation does this, copy
            -- that, and submit a patch to the spec.
            Just (P.CapPtr cap) ->
              return $ Just $ PtrCap (CapAt pMessage cap)
            ptr ->
              throwM $
                E.InvalidDataError $
                  "The tag word of a far pointer's "
                    ++ "2-word landing pad should be an intra "
                    ++ "segment pointer with offset 0, but "
                    ++ "we read "
                    ++ show ptr
        ptr ->
          throwM $
            E.InvalidDataError $
              "The first word of a far pointer's 2-word "
                ++ "landing pad should be another far pointer "
                ++ "(with a one-word landing pad), but we read "
                ++ show ptr

getNear :: (M.MonadReadMessage mut m, MonadThrow m) => M.WordPtr mut -> Maybe P.Ptr -> m (Maybe (Ptr mut))
getNear ptr@M.WordPtr {M.pMessage, M.pAddr} = \case
  Nothing -> return Nothing
  Just p -> case p of
    P.CapPtr cap -> return $ Just $ PtrCap (CapAt pMessage cap)
    P.StructPtr off dataSz ptrSz ->
      return $
        Just $
          PtrStruct $
            StructAt ptr {M.pAddr = resolveOffset pAddr off} dataSz ptrSz
    P.ListPtr off eltSpec ->
      Just . PtrList
        <$> getList ptr {M.pAddr = resolveOffset pAddr off} eltSpec
    P.FarPtr {} ->
      throwM $
        E.InvalidDataError
          "Unexpected far pointer where only near pointers were expected."

getList :: (M.MonadReadMessage mut m, MonadThrow m) => M.WordPtr mut -> P.EltSpec -> m (List mut)
getList ptr@M.WordPtr {M.pAddr = addr@WordAt {wordIndex}} eltSpec =
  case eltSpec of
    P.EltNormal sz len -> pure $ case sz of
      P.Sz0 -> List0 (ListOf nlist)
      P.Sz1 -> List1 (ListOf nlist)
      P.Sz8 -> List8 (ListOf nlist)
      P.Sz16 -> List16 (ListOf nlist)
      P.Sz32 -> List32 (ListOf nlist)
      P.Sz64 -> List64 (ListOf nlist)
      P.SzPtr -> ListPtr (ListOf nlist)
      where
        nlist = NormalList ptr (fromIntegral len)
    P.EltComposite _ -> do
      tagWord <- M.getWord ptr
      case P.parsePtr' tagWord of
        P.StructPtr numElts dataSz ptrSz ->
          pure $
            ListStruct $
              ListOf $
                StructList
                  ( StructAt
                      ptr {M.pAddr = addr {wordIndex = wordIndex + 1}}
                      dataSz
                      ptrSz
                  )
                  (fromIntegral numElts)
        tag ->
          throwM $
            E.InvalidDataError $
              "Composite list tag was not a struct-"
                ++ "formatted word: "
                ++ show tag
