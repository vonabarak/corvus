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

    -- * Table rendering
  , Align (..)
  , Column (..)
  , BorderStyle (..)
  , TableOpts (..)
  , defaultTableOpts
  , tableOptsFromOptions
  , printTable
  , renderTable

    -- * Detail views
  , printField

    -- * Legacy sub-table helpers (used by detail views)
  , tableFormat
  , printTableHeader

    -- * Internal (exposed for testing)
  , selectColumns
  , computeWidths
  , fitToWidth
  , renderCell
  , BorderChars (..)
  , borderChars
  )
where

import Corvus.Client.Types (BorderStyleOpt (..), Options (..), OutputFormat (..))
import Data.Aeson (Key, ToJSON, Value, encode, object, toJSON, (.=))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml
import System.Console.ANSI (getTerminalSize, hSupportsANSI)
import System.IO (stdout)
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
emitRpcError :: (Show e) => OutputFormat -> e -> IO ()
emitRpcError fmt err
  | isStructured fmt = outputError fmt "rpc_error" (T.pack (show err))
  | otherwise = putStrLn $ "Error: " ++ show err

-- | Emit a plain success response.
emitOk :: OutputFormat -> IO () -> IO ()
emitOk fmt textAction
  | isStructured fmt = outputOk fmt
  | otherwise = textAction

-- | Emit a success response with extra structured fields.
emitOkWith :: OutputFormat -> [(Key, Value)] -> IO () -> IO ()
emitOkWith fmt fields textAction
  | isStructured fmt = outputOkWith fmt fields
  | otherwise = textAction

-- | Emit a specific error code with a text-mode callback.
emitError :: OutputFormat -> Text -> Text -> IO () -> IO ()
emitError fmt code msg textAction
  | isStructured fmt = outputError fmt code msg
  | otherwise = textAction

-- | Emit a full 'ToJSON' value for list/show commands.
emitResult :: (ToJSON a) => OutputFormat -> a -> IO () -> IO ()
emitResult fmt val textAction
  | isStructured fmt = outputResult fmt val
  | otherwise = textAction

--------------------------------------------------------------------------------
-- Table rendering
--------------------------------------------------------------------------------

-- | Cell alignment.
data Align = LeftAlign | RightAlign
  deriving (Show, Eq)

-- | A table column declaration. @colName@ doubles as the header label and the
-- case-insensitive selector key for @--columns@.
data Column a = Column
  { colName :: !String
  , colAlign :: !Align
  , colMaxWidth :: !(Maybe Int)
  -- ^ Per-column cap. @Nothing@ means no cap.
  , colRender :: a -> String
  }

-- | Border style for table drawing.
data BorderStyle = BordersUnicode | BordersAscii | BordersNone
  deriving (Show, Eq)

-- | Presentation options threaded from CLI flags.
data TableOpts = TableOpts
  { toBorders :: !BorderStyle
  , toTruncate :: !Bool
  -- ^ True = ellipsis when a value exceeds the column width.
  -- False = print the full value (scripts reading column positions still work
  -- because alignment is preserved column-by-column).
  , toColumns :: ![String]
  -- ^ Column selector. Empty = all in declared order. Otherwise: filter and
  -- reorder to match (case-insensitive). Unknown names produce an error.
  , toFitWidth :: !Bool
  -- ^ Shrink columns to fit the terminal width. Only applies when @toTruncate@
  -- is also true — there's no point fitting if we're going to overflow anyway.
  }
  deriving (Show, Eq)

-- | Default options: Unicode borders, truncation, fit to terminal, all columns.
defaultTableOpts :: TableOpts
defaultTableOpts =
  TableOpts
    { toBorders = BordersUnicode
    , toTruncate = True
    , toColumns = []
    , toFitWidth = True
    }

-- | Build 'TableOpts' from the parsed global CLI 'Options'.
tableOptsFromOptions :: Options -> TableOpts
tableOptsFromOptions opts =
  TableOpts
    { toBorders = case optBorders opts of
        BordersUnicodeOpt -> BordersUnicode
        BordersAsciiOpt -> BordersAscii
        BordersNoneOpt -> BordersNone
    , toTruncate = optTruncate opts
    , toColumns = optColumns opts
    , toFitWidth = optFitWidth opts
    }

-- | Print a table to stdout using the current terminal width.
-- Queries 'getTerminalSize' when 'toFitWidth' is true; falls back to 120 cols.
printTable :: TableOpts -> [Column a] -> [a] -> IO ()
printTable opts cols rows = do
  termWidth <-
    if toFitWidth opts && toTruncate opts
      then do
        mSize <- getTerminalSize
        pure $ case mSize of
          Just (_, w) -> Just w
          Nothing -> Just 120
      else pure Nothing
  -- Fall back to ASCII borders when stdout doesn't look like a terminal that
  -- can render Unicode (best-effort — most modern terminals handle it).
  supportsAnsi <- hSupportsANSI stdout
  let effectiveOpts =
        if not supportsAnsi && toBorders opts == BordersUnicode
          then opts {toBorders = BordersAscii}
          else opts
  case renderTable effectiveOpts termWidth cols rows of
    Left err -> TIO.putStrLn $ "Error: " <> T.pack err
    Right txt -> TIO.putStr txt

-- | Pure table rendering. Returns @Left err@ on column-selection errors.
--
-- The second argument is an optional terminal width; 'Nothing' disables
-- terminal fitting.
renderTable :: TableOpts -> Maybe Int -> [Column a] -> [a] -> Either String Text
renderTable opts mTermWidth allCols rows = do
  cols <- selectColumns (toColumns opts) allCols
  let natural = computeWidths cols rows
      capped = applyCaps cols natural
      chars = borderChars (toBorders opts)
      overhead = borderOverhead chars (length capped)
      fitted = case mTermWidth of
        Just w | toTruncate opts -> fitToWidth (w - overhead) capped
        _ -> capped
      header = map colName cols
      renderedRows = [[colRender c r | c <- cols] | r <- rows]
      aligns = map colAlign cols
  pure $ assembleTable chars (toTruncate opts) fitted aligns header renderedRows

-- | Filter and reorder columns by selector.
--
-- * Empty selector list ⇒ all columns in declared order.
-- * Selector matching is case-insensitive.
-- * Unknown names produce a 'Left' with an "available columns" message.
-- * Duplicate names are permitted (renders that column twice).
selectColumns :: [String] -> [Column a] -> Either String [Column a]
selectColumns [] cols = Right cols
selectColumns names cols = mapM lookupCol names
  where
    lookupCol n = case filter (\c -> lower (colName c) == lower n) cols of
      (c : _) -> Right c
      [] ->
        Left $
          "unknown column: "
            <> n
            <> "; available columns: "
            <> unwordsCommas (map colName cols)

    lower = map toLower

    unwordsCommas [] = ""
    unwordsCommas [x] = x
    unwordsCommas (x : xs) = x ++ ", " ++ unwordsCommas xs

-- | Natural width for each column: max of header length and longest rendered
-- row value. Empty row list ⇒ width = header length.
computeWidths :: [Column a] -> [a] -> [Int]
computeWidths cols rows =
  [ maximum $ length (colName c) : [length (colRender c r) | r <- rows]
  | c <- cols
  ]

-- | Apply each column's 'colMaxWidth' cap (if any) to its natural width.
applyCaps :: [Column a] -> [Int] -> [Int]
applyCaps = zipWith clamp
  where
    clamp c w = case colMaxWidth c of
      Nothing -> w
      Just cap -> min cap w

-- | Fit a list of column widths into the given total budget by shrinking
-- widest columns first. Every column retains at least 3 characters.
fitToWidth :: Int -> [Int] -> [Int]
fitToWidth available widths
  | available <= 0 = map (const minColWidth) widths
  | total <= available = widths
  | otherwise = restore (go (total - available) indexed)
  where
    minColWidth :: Int
    minColWidth = 3

    total = sum widths
    indexed :: [(Int, Int)]
    indexed = zip [0 :: Int ..] widths

    go :: Int -> [(Int, Int)] -> [(Int, Int)]
    go 0 xs = xs
    go deficit xs =
      let (widestIx, widest) = head (sortOn (Down . snd) xs)
       in if widest <= minColWidth
            then xs
            else
              let shrunk = map (\(i, w) -> if i == widestIx then (i, w - 1) else (i, w)) xs
               in go (deficit - 1) shrunk

    restore :: [(Int, Int)] -> [Int]
    restore xs = map snd (sortOn fst xs)

-- | Render a single cell: pad or truncate to the target width.
--
-- First argument is whether truncation is enabled.
renderCell :: Bool -> Align -> Int -> String -> String
renderCell truncateMode align width s
  | actualLen > width && truncateMode =
      case align of
        LeftAlign -> take (width - 1) s ++ "…"
        RightAlign -> "…" ++ drop (actualLen - width + 1) s
  | actualLen >= width = s
  | otherwise =
      let pad = replicate (width - actualLen) ' '
       in case align of
            LeftAlign -> s ++ pad
            RightAlign -> pad ++ s
  where
    actualLen = length s

-- | Border characters for the three styles.
data BorderChars = BorderChars
  { bcVertical :: !Char
  , bcHorizontal :: !Char
  , bcTopLeft :: !String
  , bcTopRight :: !String
  , bcBottomLeft :: !String
  , bcBottomRight :: !String
  , bcHeaderSepLeft :: !String
  , bcHeaderSepRight :: !String
  , bcTopJoin :: !String
  , bcBottomJoin :: !String
  , bcHeaderSepJoin :: !String
  , bcEllipsis :: !String
  -- ^ Unused here but retained for future ASCII-only truncation marker.
  }

borderChars :: BorderStyle -> BorderChars
borderChars BordersUnicode =
  BorderChars
    { bcVertical = '│'
    , bcHorizontal = '─'
    , bcTopLeft = "┌"
    , bcTopRight = "┐"
    , bcBottomLeft = "└"
    , bcBottomRight = "┘"
    , bcHeaderSepLeft = "├"
    , bcHeaderSepRight = "┤"
    , bcTopJoin = "┬"
    , bcBottomJoin = "┴"
    , bcHeaderSepJoin = "┼"
    , bcEllipsis = "…"
    }
borderChars BordersAscii =
  BorderChars
    { bcVertical = '|'
    , bcHorizontal = '-'
    , bcTopLeft = "+"
    , bcTopRight = "+"
    , bcBottomLeft = "+"
    , bcBottomRight = "+"
    , bcHeaderSepLeft = "+"
    , bcHeaderSepRight = "+"
    , bcTopJoin = "+"
    , bcBottomJoin = "+"
    , bcHeaderSepJoin = "+"
    , bcEllipsis = "…"
    }
borderChars BordersNone =
  BorderChars
    { bcVertical = ' '
    , bcHorizontal = ' '
    , bcTopLeft = ""
    , bcTopRight = ""
    , bcBottomLeft = ""
    , bcBottomRight = ""
    , bcHeaderSepLeft = ""
    , bcHeaderSepRight = ""
    , bcTopJoin = " "
    , bcBottomJoin = " "
    , bcHeaderSepJoin = " "
    , bcEllipsis = "…"
    }

-- | Width eaten by border characters and inter-column padding.
-- Format: "│ c1 │ c2 │ c3 │" for Unicode. One border+space on each side of
-- each cell, with the interior separators shared, i.e. @(n+1)@ borders and
-- @2n@ spaces for @n@ columns.
borderOverhead :: BorderChars -> Int -> Int
borderOverhead chars n
  | bcVertical chars == ' ' = max 0 (n - 1)
  -- Borderless mode: just single spaces between cells.
  | otherwise = (n + 1) + 2 * n

-- Bordered: one vertical per border, two spaces of padding per cell.

-- | Build the final table string.
assembleTable
  :: BorderChars
  -> Bool -- truncateMode
  -> [Int] -- widths
  -> [Align] -- alignments
  -> [String] -- header cells
  -> [[String]] -- row cells
  -> Text
assembleTable chars truncateMode widths aligns header rows =
  T.pack $ concat $ filter (not . null) parts
  where
    parts =
      [ topBorder
      , headerRow
      , headerSep
      , dataRows
      , bottomBorder
      ]
    hasBorders = not (null (bcTopLeft chars))
    topBorder
      | hasBorders = borderLine (bcTopLeft chars) (bcTopJoin chars) (bcTopRight chars) ++ "\n"
      | otherwise = ""
    bottomBorder
      | hasBorders = borderLine (bcBottomLeft chars) (bcBottomJoin chars) (bcBottomRight chars) ++ "\n"
      | otherwise = ""
    headerSep
      | hasBorders = borderLine (bcHeaderSepLeft chars) (bcHeaderSepJoin chars) (bcHeaderSepRight chars) ++ "\n"
      | otherwise =
          -- Borderless: a whitespace "separator" line is ugly; use dashes for
          -- readability but keep it plain ASCII so awk-scripts still work.
          replicate (sum widths + max 0 (length widths - 1)) '-' ++ "\n"
    headerRow = dataLine header ++ "\n"
    dataRows = concatMap (\r -> dataLine r ++ "\n") rows

    dataLine cells =
      let padded = zipWith3 (renderCell truncateMode) aligns widths cells
          vert = [bcVertical chars]
       in if hasBorders
            then vert ++ " " ++ intercalateStr (" " ++ vert ++ " ") padded ++ " " ++ vert
            else intercalateStr " " padded

    borderLine left join right =
      left ++ intercalateStr join (map (\w -> replicate (w + 2) (bcHorizontal chars)) widths) ++ right

    -- 'intercalate' for a list of 'String's — safe on empty lists.
    intercalateStr _ [] = ""
    intercalateStr _ [x] = x
    intercalateStr sep (x : xs) = x ++ sep ++ intercalateStr sep xs

--------------------------------------------------------------------------------
-- Detail views
--------------------------------------------------------------------------------

-- | Print a key-value detail line with consistent alignment.
printField :: String -> String -> IO ()
printField key = printf "%-16s%s\n" (key ++ ":")

--------------------------------------------------------------------------------
-- Legacy sub-table helpers
--
-- These remain because detail printers (e.g. 'printTemplateDetails') render
-- indented sub-tables inside a larger detail view, a shape that the new
-- 'printTable' API (which writes whole tables to stdout at column 0) does
-- not directly express. List commands should NOT use these.
--------------------------------------------------------------------------------

-- | Build a printf format string and separator line from column specs.
-- Each spec is (headerLabel, width). Negative width = left-aligned.
tableFormat :: [(String, Int)] -> (String, String)
tableFormat cols =
  let fmtParts = ["%" ++ show w ++ "s" | (_, w) <- cols]
      fmt = unwords fmtParts ++ "\n"
      totalWidth = sum [abs w | (_, w) <- cols] + length cols - 1
      sep = replicate totalWidth '-'
   in (fmt, sep)

-- | Print a table header row and separator line from column specs.
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
                _ -> acc ++ "%" ++ rest
        go (c : rest) vs acc = go rest vs (acc ++ [c])
        go _ [] acc = acc ++ "\n"

    padField :: Int -> String -> String
    padField w s
      | w < 0 = take (abs w) (s ++ repeat ' ')
      | otherwise = replicate (w - length s) ' ' ++ take w s
