{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Internal.Untyped.Write (setPointerTo) where

import Capnp.Address (OffsetError (..), WordAddr (..), pointerFrom)
import qualified Capnp.Message as M
import Capnp.Mutability (Mutability (..))
import qualified Capnp.Pointer as P
import Capnp.TraversalLimit (LimitT)
import Control.Monad.ST (RealWorld)
import Internal.BuildPure (PureBuilder)

-- | @'setPointerTo' msg srcLoc dstAddr relPtr@ sets the word at @srcLoc@ in @msg@ to a
-- pointer like @relPtr@, but pointing to @dstAddr@. @relPtr@ should not be a far pointer.
-- If the two addresses are in different segments, a landing pad will be allocated and
-- @srcLoc@ will contain a far pointer.
setPointerTo :: (M.WriteCtx m s) => M.WordPtr ('Mut s) -> WordAddr -> P.Ptr -> m ()
{-# INLINEABLE setPointerTo #-}
{-# SPECIALIZE setPointerTo :: M.WordPtr ('Mut RealWorld) -> WordAddr -> P.Ptr -> LimitT IO () #-}
{-# SPECIALIZE setPointerTo :: M.WordPtr ('Mut s) -> WordAddr -> P.Ptr -> PureBuilder s () #-}
setPointerTo
  M.WordPtr
    { M.pMessage = msg
    , M.pSegment = srcSegment
    , M.pAddr = srcAddr@WordAt {wordIndex = srcWordIndex}
    }
  dstAddr
  relPtr
    | P.StructPtr _ 0 0 <- relPtr =
        -- We special case zero-sized structs, since (1) we don't have to
        -- really point at the correct offset, since they can "fit" anywhere,
        -- and (2) they cause problems with double-far pointers, where part
        -- of the landing pad needs to have a zero offset, but that makes it
        -- look like a null pointer... so we just avoid that case by cutting
        -- it off here.
        M.write srcSegment srcWordIndex $
          P.serializePtr $
            Just $
              P.StructPtr (-1) 0 0
    | otherwise = case pointerFrom srcAddr dstAddr relPtr of
        Right absPtr ->
          M.write srcSegment srcWordIndex $ P.serializePtr $ Just absPtr
        Left OutOfRange ->
          error "BUG: segment is too large to set the pointer."
        Left DifferentSegments -> do
          -- We need a far pointer; allocate a landing pad in the target segment,
          -- set it to point to the final destination, an then set the source pointer
          -- pointer to point to the landing pad.
          let WordAt {segIndex = targetSegment} = dstAddr
          M.allocInSeg msg targetSegment 1 >>= \case
            Just M.WordPtr {M.pSegment = landingPadSegment, M.pAddr = landingPadAddr} ->
              case pointerFrom landingPadAddr dstAddr relPtr of
                Right landingPad -> do
                  let WordAt {segIndex, wordIndex} = landingPadAddr
                  M.write landingPadSegment wordIndex (P.serializePtr $ Just landingPad)
                  M.write srcSegment srcWordIndex $
                    P.serializePtr $
                      Just $
                        P.FarPtr False (fromIntegral wordIndex) (fromIntegral segIndex)
                Left DifferentSegments ->
                  error "BUG: allocated a landing pad in the wrong segment!"
                Left OutOfRange ->
                  error "BUG: segment is too large to set the pointer."
            Nothing -> do
              -- The target segment is full. We need to do a double-far pointer.
              -- First allocate the 2-word landing pad, wherever it will fit:
              M.WordPtr
                { M.pSegment = landingPadSegment
                , M.pAddr =
                  WordAt
                    { wordIndex = landingPadOffset
                    , segIndex = landingPadSegIndex
                    }
                } <-
                M.alloc msg 2
              -- Next, point the source pointer at the landing pad:
              M.write srcSegment srcWordIndex $
                P.serializePtr $
                  Just $
                    P.FarPtr
                      True
                      (fromIntegral landingPadOffset)
                      (fromIntegral landingPadSegIndex)
              -- Finally, fill in the landing pad itself.
              --
              -- The first word is a far pointer whose offset is the
              -- starting address of our target object:
              M.write landingPadSegment landingPadOffset $
                let WordAt {wordIndex, segIndex} = dstAddr
                 in P.serializePtr $
                      Just $
                        P.FarPtr
                          False
                          (fromIntegral wordIndex)
                          (fromIntegral segIndex)
              -- The second word is a pointer of the right "shape"
              -- for the target, but with a zero offset:
              M.write landingPadSegment (landingPadOffset + 1) $
                P.serializePtr $
                  Just $ case relPtr of
                    P.StructPtr _ nWords nPtrs -> P.StructPtr 0 nWords nPtrs
                    P.ListPtr _ eltSpec -> P.ListPtr 0 eltSpec
                    _ -> relPtr
