{-# LANGUAGE OverloadedStrings #-}

-- | Machine-readable output helpers for the Corvus client.
module Corvus.Client.Output
  ( outputValue
  , outputResult
  , outputOk
  , outputOkWith
  , outputError
  , isStructured

    -- * Dispatching emitters
  , emitRpcError
  , emitOk
  , emitOkWith
  , emitError
  , emitResult

    -- * Table formatting
  , tableFormat
  , printTableHeader
  , printField
  )
where

import Corvus.Client.Types (OutputFormat (..))
import Data.Aeson (Key, ToJSON, Value, encode, object, toJSON, (.=))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Text.Printf (printf)

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
      [ "status" .= ("error" :: Text)
      , "error" .= code
      , "message" .= msg
      ]

--------------------------------------------------------------------------------
-- Dispatching Emitters
--------------------------------------------------------------------------------

-- | Emit a generic RPC error — the canonical @Left err@ branch.
-- In structured mode emits error code @rpc_error@ with @show err@ as message;
-- in text mode prints @Error: <show err>@. Use for @ConnectionError@ and other
-- @Show@-able transport failures returned from 'Corvus.Client.Rpc'.
emitRpcError :: (Show e) => OutputFormat -> e -> IO ()
emitRpcError fmt err
  | isStructured fmt = outputError fmt "rpc_error" (T.pack (show err))
  | otherwise = putStrLn $ "Error: " ++ show err

-- | Emit a plain success response.
-- In structured mode: @{"status":"ok"}@. In text mode: runs the supplied @IO ()@.
--
-- @
-- emitOk fmt $ putStrLn "VM deleted."
-- @
emitOk :: OutputFormat -> IO () -> IO ()
emitOk fmt textAction
  | isStructured fmt = outputOk fmt
  | otherwise = textAction

-- | Emit a success response with extra structured fields.
-- In structured mode: @{"status":"ok", ...fields}@. In text mode: runs the
-- supplied @IO ()@ (so callers can print whatever shape of human-readable
-- message they want, including multi-line output).
--
-- @
-- emitOkWith fmt [("id", toJSON vmId)] $
--   putStrLn $ "VM created with ID: " ++ show vmId
-- @
emitOkWith :: OutputFormat -> [(Key, Value)] -> IO () -> IO ()
emitOkWith fmt fields textAction
  | isStructured fmt = outputOkWith fmt fields
  | otherwise = textAction

-- | Emit a specific error code with a text-mode callback.
-- In structured mode: @{"status":"error","error":"<code>","message":"<msg>"}@.
-- In text mode: runs the supplied @IO ()@.
--
-- @
-- emitError fmt "not_found" ("VM '" <> vmRef <> "' not found") $
--   putStrLn $ "Error: VM '" ++ T.unpack vmRef ++ "' not found."
-- @
emitError :: OutputFormat -> Text -> Text -> IO () -> IO ()
emitError fmt code msg textAction
  | isStructured fmt = outputError fmt code msg
  | otherwise = textAction

-- | Emit a full 'ToJSON' value for list/show commands.
-- In structured mode: serialises the value directly (no wrapping object).
-- In text mode: runs the supplied @IO ()@ (typically a human-readable table
-- or detail view).
--
-- @
-- emitResult fmt disks $
--   if null disks
--     then putStrLn "No disk images found."
--     else printTableHeader cols >> mapM_ printDiskInfo disks
-- @
emitResult :: (ToJSON a) => OutputFormat -> a -> IO () -> IO ()
emitResult fmt val textAction
  | isStructured fmt = outputResult fmt val
  | otherwise = textAction

--------------------------------------------------------------------------------
-- Table Formatting
--------------------------------------------------------------------------------

-- | Build a printf format string and separator line from column specs.
-- Each spec is (headerLabel, width). Negative width = left-aligned.
-- Returns (formatString, separatorLine).
--
-- @
-- let (fmt, sep) = tableFormat [("ID", -6), ("NAME", -20)]
-- printf fmt "ID" "NAME"
-- putStrLn sep
-- @
tableFormat :: [(String, Int)] -> (String, String)
tableFormat cols =
  let fmtParts = [buildColFmt w | (_, w) <- cols]
      fmt = unwords fmtParts ++ "\n"
      totalWidth = sum [abs w | (_, w) <- cols] + length cols - 1
      sep = replicate totalWidth '-'
   in (fmt, sep)
  where
    buildColFmt :: Int -> String
    buildColFmt w = "%" ++ show w ++ "s"

-- | Print a table header row and separator line from column specs.
-- Each spec is (headerLabel, width). Negative width = left-aligned.
printTableHeader :: [(String, Int)] -> IO ()
printTableHeader cols = do
  let (fmt, sep) = tableFormat cols
      row = buildRow fmt (map fst cols)
  putStr row
  putStrLn sep
  where
    buildRow :: String -> [String] -> String
    buildRow fmt' vals = go fmt' vals ""
      where
        go [] _ acc = acc ++ "\n"
        go ('%' : rest) (v : vs) acc =
          let (spec, rest') = span (/= 's') rest
           in case rest' of
                ('s' : rest'') -> go rest'' vs (acc ++ padField (read spec) v)
                _ -> acc ++ "%" ++ rest -- shouldn't happen
        go (c : rest) vs acc = go rest vs (acc ++ [c])
        go _ [] acc = acc ++ "\n"

    padField :: Int -> String -> String
    padField w s
      | w < 0 = take (abs w) (s ++ repeat ' ')
      | otherwise = replicate (w - length s) ' ' ++ take w s

-- | Print a key-value detail line with consistent alignment.
-- Key is printed left-aligned with a colon, value follows.
--
-- @printField "Name" "alma10"@  prints  @Name:           alma10@
printField :: String -> String -> IO ()
printField key = printf "%-16s%s\n" (key ++ ":")
