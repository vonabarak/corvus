module Corvus.Cancellation (TaskCancelledException (..), VmOperationSuperseded (..)) where

import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException)

-- | Cooperative and asynchronous cancellation of a daemon task tree.
data TaskCancelledException = TaskCancelledException deriving stock (Show)

instance Exception TaskCancelledException where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | A synchronous rejection of stale nodeagent authorization.
data VmOperationSuperseded = VmOperationSuperseded deriving stock (Show)

instance Exception VmOperationSuperseded
