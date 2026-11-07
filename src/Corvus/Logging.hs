{-# LANGUAGE ScopedTypeVariables #-}

-- | Exception classification shared by connection and subscriber logging.
module Corvus.Logging (synchronousException, subscriberFailureLevel, exceptionCategory, withRpcSupervisor) where

import qualified Capnp as C
import qualified Capnp.Gen.Capnp.Rpc as Rpc
import qualified Control.Exception as E
import Control.Monad.Logger (LogLevel (..), logWarnN, runStderrLoggingT)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (typeOf)
import Supervisors (Supervisor, withSupervisorReporting)
import System.IO.Error (ioeGetErrorType)

synchronousException :: E.SomeException -> Maybe E.SomeException
synchronousException e = case E.fromException e :: Maybe E.SomeAsyncException of
  Just _ -> Nothing
  Nothing -> Just e

subscriberFailureLevel :: E.SomeException -> LogLevel
subscriberFailureLevel e = case E.fromException e :: Maybe (C.Parsed Rpc.Exception) of
  Just rpc | Rpc.type_ rpc == Rpc.Exception'Type'disconnected -> LevelDebug
  _ -> LevelWarn

-- | Do not include RPC parameters, guest payloads, or exception messages which
-- may embed them. The concrete exception type and IO category remain useful.
exceptionCategory :: E.SomeException -> Text
exceptionCategory e@(E.SomeException cause) = case E.fromException e :: Maybe IOError of
  Just io -> T.pack (show (ioeGetErrorType io))
  Nothing -> T.pack (show (typeOf cause))

-- | Report unexpected supervised failures through the service logger.
withRpcSupervisor :: Text -> (Supervisor -> IO a) -> IO a
withRpcSupervisor label = withSupervisorReporting $ \e ->
  runStderrLoggingT $ logWarnN (label <> T.pack ": supervised worker failed: " <> exceptionCategory e)
