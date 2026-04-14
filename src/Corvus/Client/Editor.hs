{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Launch $EDITOR (or \"vi\" if unset) on a temporary file pre-filled with
-- some initial content, wait for the editor to exit, and return the edited
-- contents. Used by @crv template edit@ and by @crv template create@ when
-- no file argument is provided.
module Corvus.Client.Editor
  ( editInEditor
  )
where

import Control.Exception (IOException, try)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess)

-- | Write @initial@ to a temp file with a @.yml@ suffix (so editors pick up
-- YAML syntax), launch @$EDITOR@ (falling back to @vi@) on it, wait for the
-- editor to exit, and read the edited contents back.
--
-- Returns 'Left' with an error message if the editor process fails, or 'Right'
-- with the edited content on success.
editInEditor :: Text -> IO (Either Text Text)
editInEditor initial = do
  mEditor <- lookupEnv "EDITOR"
  let editor = case mEditor of
        Just e | not (null e) -> e
        _ -> "vi"
  withSystemTempFile "corvus-template.yml" $ \path handle -> do
    TIO.hPutStr handle initial
    hClose handle
    launched <- try (callProcess editor [path])
    case launched of
      Left (err :: IOException) ->
        pure $
          Left $
            "Failed to launch editor '"
              <> T.pack editor
              <> "': "
              <> T.pack (show err)
      Right () -> do
        edited <- TIO.readFile path
        pure $ Right edited
