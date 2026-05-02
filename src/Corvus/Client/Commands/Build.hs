{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Build command handler for the Corvus client.
--
-- Reads a YAML build pipeline file, **preprocesses** it (inlines any
-- referenced @shell.script@ files and @file.from@ files as
-- @shell.inline@ / @file.content@ base64), and forwards it to the daemon.
-- Path inlining means the daemon never has to access the operator's
-- working directory; relative paths resolve against the YAML file's
-- directory.
module Corvus.Client.Commands.Build
  ( handleBuild
  )
where

import Control.Exception (SomeException, try)
import Corvus.Client.Connection
import Corvus.Client.Output (emitError, emitResult, emitRpcError)
import Corvus.Client.Rpc
import Corvus.Client.Types (OutputFormat (..), WaitOptions (..))
import Corvus.Model (TaskResult (..))
import Corvus.Protocol (BuildEvent (..), BuildOne (..), BuildResult (..))
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Value (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)

-- | Top-level handler for @crv build FILE@.
--
-- Async by default: the daemon returns a parent task id and the build
-- runs in the background. With @--wait@ (encoded by 'WaitOptions') the
-- client blocks until completion and prints per-build artifact disk ids.
handleBuild :: OutputFormat -> Connection -> FilePath -> WaitOptions -> IO Bool
handleBuild fmt conn path waitOpts = do
  exists <- doesFileExist path
  if not exists
    then do
      emitError fmt "file_not_found" (T.pack $ "File not found: " ++ path) $
        putStrLn $
          "Error: File not found: " ++ path
      pure False
    else do
      raw <- TIO.readFile path
      case Yaml.decodeEither' (TE.encodeUtf8 raw) of
        Left err -> do
          emitError fmt "yaml_parse_error" (T.pack $ show err) $
            putStrLn $
              "Error parsing YAML: " ++ show err
          pure False
        Right (root :: Value) -> do
          let baseDir = takeDirectory path
          inlined <- try (preprocessRoot baseDir root)
          case inlined of
            Left (ex :: SomeException) -> do
              emitError fmt "preprocess_error" (T.pack $ show ex) $
                putStrLn $
                  "Error inlining file references: " ++ show ex
              pure False
            Right rebuilt -> do
              let yamlOut = TE.decodeUtf8 (Yaml.encode rebuilt)
                  wait = woWait waitOpts
              -- Stream events live in human/text mode while @--wait@ is
              -- in effect. Structured output modes (json/yaml) suppress
              -- live output so the caller's parser sees only the final
              -- 'BuildOk' payload.
              let onEvent = case (wait, fmt) of
                    (True, TextOutput) -> renderBuildEvent
                    _ -> \_ -> pure ()
              -- Line-buffer stdout so each StepOutput event appears
              -- immediately rather than being held in the libc buffer.
              hSetBuffering stdout LineBuffering
              resp <- runBuild conn yamlOut wait onEvent
              case resp of
                Left err -> do
                  emitRpcError fmt err
                  pure False
                Right (BuildOk result) -> do
                  emitResult fmt result $ printBuildResult result
                  pure (all (isNothing . boError) (brBuilds result))
                Right (BuildAsync taskId) -> do
                  emitResult fmt (T.pack $ show taskId) $
                    putStrLn $
                      "Build started (task ID: " ++ show taskId ++ ")"
                  pure True
                Right (BuildFailed msg) -> do
                  emitError fmt "build_failed" msg $
                    putStrLn $
                      "Build failed: " ++ T.unpack msg
                  pure False

-- | Render a streamed 'BuildEvent' to the operator's terminal.
-- Format choices:
--
--   * Daemon-level progress lines get a @==>@ prefix so they stand out
--     from per-step output.
--   * Each step is wrapped between @---@ banners showing index, kind,
--     and a one-line description (typically the file path or the first
--     line of an inline shell body).
--   * 'StepOutput' lines are tagged with the step index so output from
--     concurrent steps could be disambiguated in future (today the
--     daemon runs steps serially, but the tag is cheap).
--   * Failures and errors go to stderr so @&&@ chaining still works
--     against the caller's exit code.
--
-- 'PipelineEnd' is consumed silently here; the human summary printed
-- by 'printBuildResult' (driven by the final 'BuildOk' result) renders
-- after the relay loop unwinds.
renderBuildEvent :: BuildEvent -> IO ()
renderBuildEvent ev = case ev of
  BuildLogLine t -> putStrLn $ "==> " <> T.unpack t
  StepStart i kind desc -> do
    let descPart = if T.null desc then "" else ": " <> T.unpack desc
    putStrLn $ "--- step " <> show i <> " (" <> T.unpack kind <> ")" <> descPart
  StepOutput i line -> putStrLn $ "[" <> show i <> "] " <> T.unpack line
  StepEnd i TaskSuccess _ -> putStrLn $ "--- step " <> show i <> " ok"
  StepEnd i TaskError mErr -> do
    let suffix = case mErr of
          Just e -> ": " <> T.unpack e
          Nothing -> ""
    hPutStrLn stderr $ "--- step " <> show i <> " FAILED" <> suffix
  StepEnd i taskResult mMsg -> do
    -- Cancelled / running shouldn't ordinarily appear, but render
    -- something rather than stay silent.
    let extra = maybe "" ((": " <>) . T.unpack) mMsg
    putStrLn $ "--- step " <> show i <> " (" <> show taskResult <> ")" <> extra
  BuildEnd (Right diskId) ->
    putStrLn $ "==> built disk #" <> show diskId
  BuildEnd (Left err) ->
    hPutStrLn stderr $ "==> build failed: " <> T.unpack err
  PipelineEnd _ -> pure ()

-- | Print a 'BuildResult' in human form.
--
-- The headline distinguishes three cases (all-pass, all-fail, mixed) so a
-- caller skimming for "did my build work?" can tell at a glance — earlier
-- the line @Built N image(s):@ was emitted unconditionally, which read
-- like success even when every entry below it said @FAILED@.
printBuildResult :: BuildResult -> IO ()
printBuildResult result = do
  let entries = brBuilds result
      total = length entries
      failed = length (filter (isJust . boError) entries)
      succeeded = total - failed
  if null entries
    then putStrLn "No builds in pipeline."
    else do
      let headline
            | failed == 0 = "Built " ++ show total ++ " image(s):"
            | succeeded == 0 = "Build failed (" ++ show total ++ " image(s)):"
            | otherwise =
                "Build partially succeeded: "
                  ++ show succeeded
                  ++ " ok, "
                  ++ show failed
                  ++ " failed of "
                  ++ show total
                  ++ ":"
      putStrLn headline
      mapM_ printOne entries
  where
    printOne b = case (boArtifactDiskId b, boError b) of
      (Just did, Nothing) ->
        putStrLn $
          "  - " ++ T.unpack (boName b) ++ " -> disk #" ++ show did
      (_, Just err) ->
        putStrLn $
          "  - " ++ T.unpack (boName b) ++ " FAILED: " ++ T.unpack err
      _ ->
        putStrLn $
          "  - " ++ T.unpack (boName b) ++ " (unknown state)"

--------------------------------------------------------------------------------
-- YAML preprocessing
--------------------------------------------------------------------------------

-- | Walk the parsed YAML root looking for @builds[].provisioners[].shell@
-- and @builds[].provisioners[].file@ entries; for each one with a
-- file-path reference, read the file and embed it inline.
preprocessRoot :: FilePath -> Value -> IO Value
preprocessRoot baseDir = walkBuilds
  where
    walkBuilds (Object o) = case KM.lookup "builds" o of
      Just (Array bs) -> do
        bs' <- mapM walkBuild (V.toList bs)
        pure $ Object (KM.insert "builds" (Array (V.fromList bs')) o)
      _ -> pure (Object o)
    walkBuilds v = pure v

    walkBuild (Object o) = case KM.lookup "provisioners" o of
      Just (Array ps) -> do
        ps' <- mapM walkProv (V.toList ps)
        pure $ Object (KM.insert "provisioners" (Array (V.fromList ps')) o)
      _ -> pure (Object o)
    walkBuild v = pure v

    walkProv (Object o) = do
      o1 <- adjustField "shell" rewriteShell o
      o2 <- adjustField "file" rewriteFile o1
      pure (Object o2)
    walkProv v = pure v

    adjustField :: AK.Key -> (Value -> IO Value) -> KM.KeyMap Value -> IO (KM.KeyMap Value)
    adjustField key f km = case KM.lookup key km of
      Nothing -> pure km
      Just v -> do
        v' <- f v
        pure (KM.insert key v' km)

    rewriteShell :: Value -> IO Value
    rewriteShell (Object o) = case KM.lookup "script" o of
      Just (String relPath) -> do
        body <- readScript (T.unpack relPath)
        let withInline = KM.insert "inline" (String body) (KM.delete "script" o)
        pure (Object withInline)
      _ -> pure (Object o)
    rewriteShell other = pure other

    rewriteFile :: Value -> IO Value
    rewriteFile (Object o) = case KM.lookup "from" o of
      Just (String relPath) -> do
        bytes <- readFromFile (T.unpack relPath)
        let withContent =
              KM.insert
                "content"
                (String (TE.decodeUtf8 (B64.encode bytes)))
                (KM.delete "from" o)
        pure (Object withContent)
      _ -> pure (Object o)
    rewriteFile other = pure other

    readScript rel = do
      let p = if isAbsolute rel then rel else baseDir </> rel
      TIO.readFile p
    readFromFile rel = do
      let p = if isAbsolute rel then rel else baseDir </> rel
      BS.readFile p
    isAbsolute ('/' : _) = True
    isAbsolute _ = False
