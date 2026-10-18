{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Capnp.Errors
-- Description: Error handling utilities
module Capnp.Errors
  ( Error (..)
  )
where

import Control.Monad.Catch (Exception)
import Data.Text.Encoding.Error (UnicodeException)

-- | An error that may occur when processing a capnproto message.
data Error
  = -- | A 'BoundsError' indicates an attempt to access an illegal
    -- index within a sequence of the given length.
    BoundsError !Int !Int
  | -- | A 'RecursionLimitError' indicates that the recursion depth limit
    -- was exceeded.
    RecursionLimitError
  | -- | A 'TraversalLimitError' indicates that the traversal limit was
    -- exceeded.
    TraversalLimitError
  | -- | An 'InvalidDataError' indicates that a part of a message being
    -- parsed was malformed. The argument to the data constructor is a
    -- human-readable error message.
    InvalidDataError String
  | -- | A 'SizeError' indicates that an operation would have resulted in
    -- a message that violated the library's limit on either segment size
    -- or number of segments.
    SizeError
  | -- | A 'SchemaViolationError' indicates that part of the message does
    -- not match the schema. The argument to the data construtor is a
    -- human-readable error message.
    SchemaViolationError String
  | -- | An 'InvalidUtf8Error' indicates that a text value in the message
    -- was invalid utf8.
    --
    -- Note well: Most parts of the library don't actually check for valid
    -- utf8 -- don't assume the check is made unless an interface says it is.
    InvalidUtf8Error UnicodeException
  deriving stock (Show, Eq)

instance Exception Error
