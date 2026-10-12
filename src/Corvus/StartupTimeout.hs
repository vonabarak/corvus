{-# LANGUAGE OverloadedStrings #-}

-- | Startup readiness budgets, in seconds at public boundaries.
module Corvus.StartupTimeout
  ( defaultStartupTimeoutSec
  , validateStartupTimeoutSec
  , startupTimeoutField
  ) where

import Data.Aeson (Object, (.:?))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)

defaultStartupTimeoutSec :: Int
defaultStartupTimeoutSec = 120

validateStartupTimeoutSec :: Int -> Either Text ()
validateStartupTimeoutSec seconds
  | seconds < 1 || toInteger seconds > toInteger (maxBound :: Word32) `div` 1000 =
      Left "startupTimeoutSec must be between 1 and 4294967 seconds"
  | otherwise = Right ()

-- | Optional for build overrides; create schemas supply the default.
startupTimeoutField :: Object -> Parser (Maybe Int)
startupTimeoutField object = do
  value <- object .:? "startupTimeoutSec"
  case traverse validateStartupTimeoutSec value of
    Left err -> fail (T.unpack err)
    Right _ -> pure value
