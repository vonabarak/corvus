{-# LANGUAGE OverloadedStrings #-}

-- | Machine-readable output helpers for the Corvus client.
module Corvus.Client.Output
  ( outputValue,
    outputResult,
    outputOk,
    outputOkWith,
    outputError,
    isStructured,
  )
where

import Corvus.Client.Types (OutputFormat (..))
import Data.Aeson (Key, ToJSON, Value, encode, object, toJSON, (.=))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Yaml as Yaml

-- | Whether the output format is structured (json/yaml) vs text
isStructured :: OutputFormat -> Bool
isStructured TextOutput = False
isStructured _ = True

-- | Output a JSON Value in the requested format
outputValue :: OutputFormat -> Value -> IO ()
outputValue JsonOutput v = BL.putStrLn (encode v)
outputValue YamlOutput v = BS.putStr (Yaml.encode v)
outputValue TextOutput _ = pure ()

-- | Output a ToJSON value
outputResult :: (ToJSON a) => OutputFormat -> a -> IO ()
outputResult fmt a = outputValue fmt (toJSON a)

-- | Output {"status":"ok"}
outputOk :: OutputFormat -> IO ()
outputOk fmt = outputValue fmt (object ["status" .= ("ok" :: Text)])

-- | Output {"status":"ok", ...extraFields}
outputOkWith :: OutputFormat -> [(Key, Value)] -> IO ()
outputOkWith fmt fields =
  outputValue fmt (object (("status" .= ("ok" :: Text)) : fields))

-- | Output {"status":"error","error":"<code>","message":"<msg>"}
outputError :: OutputFormat -> Text -> Text -> IO ()
outputError fmt code msg =
  outputValue fmt $
    object
      [ "status" .= ("error" :: Text),
        "error" .= code,
        "message" .= msg
      ]
