{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Simplified implementation of the monad-ste package,
-- with a few extras.
module Internal.STE
  ( STE
  , throwSTE
  , liftST
  , steToST
  )
where

import Control.Exception
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Typeable (Typeable)

newtype InternalErr e = InternalErr e
  deriving stock (Typeable)

instance Show (InternalErr e) where
  show _ = "(InternalErr _)"

instance (Typeable e) => Exception (InternalErr e)

newtype STE e s a = STE (IO a)
  deriving newtype (Functor, Applicative, Monad)

instance PrimMonad (STE e s) where
  type PrimState (STE e s) = s
  primitive = liftST . primitive

liftST :: ST s a -> STE e s a
liftST st = STE (unsafeSTToIO st)

throwSTE :: (Exception e) => e -> STE e s a
throwSTE e = STE (throwIO (InternalErr e))

steToST :: (Typeable e) => STE e s a -> ST s (Either e a)
steToST (STE io) = unsafeIOToST $ do
  res <- try io
  case res of
    Left (InternalErr e) -> pure $ Left e
    Right v -> pure $ Right v

instance MonadThrow (STE SomeException s) where
  throwM = throwSTE . toException
