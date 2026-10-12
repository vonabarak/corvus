-- | Local column selection validation and shell completion, with no RPC calls.
module Corvus.Client.Output.Selection
  ( parseColumnSelection
  , validateColumnSelection
  , listColumnNames
  , columnCompleter
  , completeColumns
  , columnCompletionRequest
  )
where

import Corvus.Client.Output (colName)
import Corvus.Client.Output.Columns
import Corvus.Client.Types
import Data.Char (toLower)
import Data.List (isPrefixOf, nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Options.Applicative.Types (Completer (..))
import System.Environment (getArgs)
import Text.Read (readMaybe)

parseColumnSelection :: String -> Either String ColumnSelection
parseColumnSelection raw
  | any null names = Left "Column selection must contain nonempty comma-separated names."
  | names == ["all"] = Right AllColumns
  | "all" `elem` names = Left "The special column selection 'all' must be used alone."
  | otherwise = case NE.nonEmpty names of
      Just ns -> Right (NamedColumns ns)
      Nothing -> Left "Column selection must not be empty."
  where
    names = map (map toLower . T.unpack . T.strip) (splitComma raw)

splitComma :: String -> [T.Text]
splitComma = T.splitOn (T.singleton ',') . T.pack

-- | Names derive from the actual table declarations, including extra columns.
listColumnNames :: Command -> Maybe [String]
listColumnNames command = case command of
  VmList -> Just (map colName (vmColumns epoch))
  DiskList -> Just (map colName diskColumns)
  SnapshotList _ -> Just (map colName snapshotColumns)
  VmSnapshotList _ -> Just (map colName vmSnapshotColumns)
  NetworkList -> Just (map colName networkColumns)
  NodeList -> Just (map colName nodeColumns)
  TemplateList -> Just (map colName templateVmColumns)
  TaskList {} -> Just (map colName (taskColumns epoch))
  SshKeyList -> Just (map colName sshKeyColumns)
  SshKeyListForVm _ -> Just (map colName sshKeyVmColumns)
  NetIfList _ -> Just (map colName netIfColumns)
  SharedDirList _ -> Just (map colName sharedDirColumns)
  AudioDeviceList _ -> Just (map colName audioDeviceColumns)
  _ -> Nothing
  where
    epoch = posixSecondsToUTCTime 0

validateColumnSelection :: Options -> Either String ()
validateColumnSelection opts = case optColumns opts of
  DefaultColumns -> Right ()
  selection
    | optOutput opts /= TextOutput -> Left "-O/--columns requires -o text."
    | otherwise -> case listColumnNames (optCommand opts) of
        Nothing -> Left "-O/--columns is only supported for list commands."
        Just names -> case selection of
          NamedColumns selected -> mapM_ (validate names) selected
          _ -> Right ()
  where
    validate names n
      | map toLower n `elem` map (map toLower) names = Right ()
      | otherwise = Left ("unknown column: " ++ n ++ "; available columns: " ++ T.unpack (T.intercalate (T.pack ", ") (map T.pack names)))

columnCompleter :: Completer
columnCompleter = Completer $ \token -> do
  args <- getArgs
  let commandWords = completionWords args
  pure (completeColumns commandWords token)

-- | Handle attached option values that optparse-applicative's completion
-- protocol otherwise treats as flag names. Bash may also split '=' into its
-- own word; Zsh and Fish generally keep the full option together.
columnCompletionRequest :: [String] -> Maybe [String]
columnCompletionRequest args = do
  (index, requestWords) <- completionRequest args
  current <- atIndex index requestWords
  let prior = take index requestWords
      finish optionPrefix token = Just (map (optionPrefix ++) (completeColumns (completionWords args) token))
  if "--columns=" `isPrefixOf` current
    then finish "--columns=" (drop 10 current)
    else
      if "-O" `isPrefixOf` current && length current > 2
        then finish "-O" (drop 2 current)
        else case reverse prior of
          "=" : "--columns" : _ -> finish "" current
          "--columns" : _ | "=" `isPrefixOf` current -> finish "=" (drop 1 current)
          _ -> Nothing

-- | Decode the common request carried by generated shell completion scripts.
completionRequest :: [String] -> Maybe (Int, [String])
completionRequest args = do
  index <- findIndexArg args
  if index < 0 then Nothing else Just (index, collect args)
  where
    collect ("--bash-completion-word" : w : rest) = w : collect rest
    collect (_ : rest) = collect rest
    collect [] = []
    findIndexArg ("--bash-completion-index" : i : _) = readMaybe i
    findIndexArg (_ : rest) = findIndexArg rest
    findIndexArg [] = Nothing

atIndex :: Int -> [a] -> Maybe a
atIndex index xs = case drop index xs of
  x : _ -> Just x
  [] -> Nothing

completionWords :: [String] -> [String]
completionWords args = case completionRequest args of
  Nothing -> []
  Just (index, requestWords) -> drop 1 (removeEquals (zipWith (maskCurrent index) [0 ..] requestWords))
  where
    maskCurrent index i word
      | i /= index = word
      | "--columns=" `isPrefixOf` word = "--columns="
      | "-O" `isPrefixOf` word && length word > 2 = "-O="
      | otherwise = ""
    removeEquals ("--columns" : "=" : rest) = "--columns" : removeEquals rest
    removeEquals (word : rest) = word : removeEquals rest
    removeEquals [] = []

-- | Complete only the current comma segment, preserving earlier selections.
completeColumns :: [String] -> String -> [String]
completeColumns wordsInCommand token
  | structured = []
  | "all" `elem` selected = []
  | otherwise = [prefix ++ name | name <- candidates, lower (T.unpack (T.strip (T.pack current))) `isPrefixOf` lower name, lower name `notElem` selected]
  where
    lower = map toLower
    segments = map T.unpack (splitComma token)
    current = last segments
    earlier = init segments
    selected = map (lower . T.unpack . T.strip . T.pack) earlier
    prefix = concatMap (++ ",") earlier
    (commandWords, outputFormats) = stripOptions wordsInCommand
    structured = case reverse outputFormats of
      fmt : _ -> lower fmt `elem` ["json", "yaml"]
      [] -> False
    candidates = case commandContext commandWords of
      Just (Just command) -> fromMaybe [] (listColumnNames command) ++ ["all" | null earlier]
      Just Nothing -> []
      Nothing -> unionNames ++ ["all" | null earlier]
    unionNames = nub (concatMap (fromMaybe [] . listColumnNames) allListCommands)

allListCommands :: [Command]
allListCommands = [VmList, DiskList, SnapshotList T.empty, VmSnapshotList T.empty, NetworkList, NodeList, TemplateList, TaskList 0 Nothing Nothing False, SshKeyList, SshKeyListForVm T.empty, NetIfList T.empty, SharedDirList T.empty, AudioDeviceList T.empty]

-- Global options can precede or follow the command. Ignore their values so an
-- address or column name cannot be mistaken for a command word.
stripOptions :: [String] -> ([String], [String])
stripOptions [] = ([], [])
stripOptions (word : rest)
  | word `elem` ["-o", "--output"] = case rest of
      value : xs -> let (ws, fs) = stripOptions xs in (ws, value : fs)
      [] -> ([], [])
  | "--output=" `isPrefixOf` word = let (ws, fs) = stripOptions rest in (ws, drop 9 word : fs)
  | "-o" `isPrefixOf` word && length word > 2 = let (ws, fs) = stripOptions rest in (ws, drop 2 word : fs)
  | word `elem` ["-O", "--columns", "-s", "--socket", "-H", "--host", "-p", "--port", "--borders", "--tls-cert-dir"] = stripOptions (drop 1 rest)
  | "-" `isPrefixOf` word = stripOptions rest
  | otherwise = let (ws, fs) = stripOptions rest in (word : ws, fs)

commandContext :: [String] -> Maybe (Maybe Command)
commandContext [] = Nothing
commandContext [resource]
  | resource `elem` ["vm", "disk", "network", "node", "template", "task", "ssh-key", "net-if", "shared-dir", "audio-device"] = Nothing
commandContext ("disk" : "snapshot" : rest) = snapshotContext SnapshotList rest
commandContext ("vm" : "snapshot" : rest) = snapshotContext VmSnapshotList rest
commandContext (resource : operation : _) = Just $ case (resource, operation) of
  ("vm", "list") -> Just VmList
  ("disk", "list") -> Just DiskList
  ("network", "list") -> Just NetworkList
  ("node", "list") -> Just NodeList
  ("template", "list") -> Just TemplateList
  ("task", "list") -> Just (TaskList 0 Nothing Nothing False)
  ("ssh-key", "list") -> Just SshKeyList
  ("ssh-key", "list-vm") -> Just (SshKeyListForVm T.empty)
  ("net-if", "list") -> Just (NetIfList T.empty)
  ("shared-dir", "list") -> Just (SharedDirList T.empty)
  ("audio-device", "list") -> Just (AudioDeviceList T.empty)
  _ -> Nothing
commandContext _ = Just Nothing

snapshotContext :: (T.Text -> Command) -> [String] -> Maybe (Maybe Command)
snapshotContext _ [] = Nothing
snapshotContext constructor ("list" : _) = Just (Just (constructor T.empty))
snapshotContext _ _ = Just Nothing
