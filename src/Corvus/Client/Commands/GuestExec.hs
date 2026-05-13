{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Guest execution command handlers for the Corvus client.
module Corvus.Client.Commands.GuestExec
  ( handleVmExec
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Output (emitError, emitOkWith)
import Corvus.Client.Types (OutputFormat)
import Corvus.Wire.Common (entityRefFromText)
import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (stderr)

-- | Handle guest-exec command
handleVmExec :: OutputFormat -> CapnpConnection -> Text -> Text -> IO Bool
handleVmExec fmt conn vmRef command = do
  r <- try (CR.rpcGuestExec conn (entityRefFromText vmRef) command) :: IO (Either SomeException (Int, Text, Text))
  case r of
    Right (exitcode, stdout, stdErr) -> do
      emitOkWith
        fmt
        [ ("exitcode", toJSON exitcode)
        , ("stdout", toJSON stdout)
        , ("stderr", toJSON stdErr)
        ]
        $ do
          TIO.putStr stdout
          unless (T.null stdErr) $ TIO.hPutStr stderr stdErr
      pure (exitcode == 0)
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Guest agent error: " ++ show e)
      pure False
