{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Build command handler for the Corvus client.
--
-- @crv build FILE@ reads a YAML build pipeline file, preprocesses
-- it (inlines any referenced @shell.script@ files as
-- @shell.inline@, @file.from@ files as @file.content@, and
-- @floppy.from@ files as @floppy.contentBase64@), and forwards
-- it to the daemon via the Cap'n Proto @daemon.build@ method.
-- With @--wait@ it streams each 'BuildEvent' to stdout via the
-- @BuildEventSink@ cap until the pipeline finishes.
module Corvus.Client.Commands.Build
  ( handleBuild
  )
where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Corvus.Client.BuildVars (applyBuildVars, renderVarError)
import Corvus.Client.Capnp.Connection (CapnpConnection)
import qualified Corvus.Client.Capnp.Rpc as CR
import Corvus.Client.Output (emitError, emitOkWith)
import Corvus.Client.Types (BuildClientOptions (..), OutputFormat, WaitOptions (..))
import Corvus.Model (EnumText (..), TaskResult (..))
import Corvus.Protocol.Build (BuildEvent (..), BuildOne (..), BuildResult (..))
import Data.Aeson (toJSON)
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Value (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import System.FilePath (takeDirectory, takeFileName, (</>))

-- | Top-level handler for @crv build FILE@.
--
-- With @--wait@: streams 'BuildEvent's in real time and blocks until
-- the daemon closes the sink. Without @--wait@: kicks off the build
-- and returns immediately with the parent task id.
handleBuild :: OutputFormat -> CapnpConnection -> FilePath -> BuildClientOptions -> WaitOptions -> IO Bool
handleBuild fmt conn path bcOpts waitOpts = do
  parsed <- try (Yaml.decodeFileEither path) :: IO (Either SomeException (Either Yaml.ParseException Value))
  case parsed of
    Left e -> do
      emitError fmt "io_error" (T.pack (show e)) $
        putStrLn ("Error reading " <> path <> ": " <> show e)
      pure False
    Right (Left perr) -> do
      let msg = "YAML parse error: " <> T.pack (Yaml.prettyPrintParseException perr)
      emitError fmt "yaml_parse" msg $ TIO.putStrLn msg
      pure False
    Right (Right root) -> do
      varsResult <- applyBuildVars (bcoVars bcOpts) (bcoVarFiles bcOpts) root
      case varsResult of
        Left e -> do
          let msg = renderVarError e
          emitError fmt "build_vars" msg $ TIO.putStrLn msg
          pure False
        Right substituted -> do
          let baseDir = takeDirectory path
          inlinedResult <- try (preprocessRoot baseDir substituted) :: IO (Either SomeException Value)
          case inlinedResult of
            Left e -> do
              let msg = "preprocess error: " <> T.pack (show e)
              emitError fmt "preprocess_error" msg $ TIO.putStrLn msg
              pure False
            Right inlined -> do
              let yaml = TE.decodeUtf8 (Yaml.encode inlined)
              runBuild fmt conn yaml bcOpts (woWait waitOpts)

runBuild :: OutputFormat -> CapnpConnection -> T.Text -> BuildClientOptions -> Bool -> IO Bool
runBuild fmt conn yaml bcOpts wait = do
  done <- newEmptyMVar :: IO (MVar ())
  -- Aggregate state surfaced after the stream ends: did any
  -- top-level step fail, and the final per-build summary.
  successVar <- newEmptyMVar :: IO (MVar Bool)
  let onEvent ev = when wait (renderEvent ev)
      onEnd = do
        _ <- tryPutMVar successVar True
        putMVar done ()
      useCache = bcoUseCache bcOpts
      buildCacheFlag = bcoBuildCache bcOpts
      rebuildFromInt = fromIntegral (bcoRebuildFrom bcOpts)
  r <-
    try (CR.rpcBuild conn yaml useCache buildCacheFlag rebuildFromInt onEvent onEnd)
      :: IO (Either SomeException Int64)
  case r of
    Left e -> do
      emitError fmt "rpc_error" (T.pack (show e)) $
        putStrLn ("Error invoking build: " <> show e)
      pure False
    Right tid -> do
      if wait
        then do
          takeMVar done
          _ <- takeMVar successVar
          pure True
        else do
          emitOkWith fmt [("taskId", toJSON tid)] $
            putStrLn ("Build started; task id: " <> show tid)
          pure True
  where
    -- Render one BuildEvent to stdout. Mirrors the pre-Phase-5
    -- legacy renderer: a one-line summary per event.
    renderEvent ev = case ev of
      BuildLogLine t ->
        TIO.putStrLn t
      StepStart idx kind msg ->
        TIO.putStrLn ("[step " <> T.pack (show idx) <> " " <> kind <> "] " <> msg)
      StepOutput _ line ->
        TIO.putStrLn line
      StepEnd idx result mMsg ->
        TIO.putStrLn
          ( "[step "
              <> T.pack (show idx)
              <> " "
              <> enumToText result
              <> "] "
              <> maybe "" (" - " <>) mMsg
          )
      BuildEnd (Left err) ->
        TIO.putStrLn ("Build error: " <> err)
      BuildEnd (Right diskId) ->
        TIO.putStrLn ("Build artifact disk id: " <> T.pack (show diskId))
      PipelineEnd (BuildResult builds) -> do
        TIO.putStrLn "Pipeline finished:"
        mapM_ renderOne builds
      StepCacheHit idx _ ->
        TIO.putStrLn ("[step " <> T.pack (show idx) <> " cache-hit] reusing cached snapshot")
      StepCacheStore idx _ ->
        TIO.putStrLn ("[step " <> T.pack (show idx) <> " cache-store] snapshot written")
      StepCacheRestore prefix _ ->
        TIO.putStrLn ("[cache] resuming from step " <> T.pack (show prefix))
    renderOne bo = do
      let prefix = "  - " <> boName bo
      case (boArtifactDiskId bo, boError bo) of
        (Just did, _) ->
          TIO.putStrLn (prefix <> " -> disk " <> T.pack (show did))
        (_, Just err) ->
          TIO.putStrLn (prefix <> " FAILED: " <> err)
        _ ->
          TIO.putStrLn (prefix <> " (no result)")

--------------------------------------------------------------------------------
-- YAML preprocessing
--------------------------------------------------------------------------------

-- | Walk the parsed YAML root looking for
-- @pipeline[].build.provisioners[].shell@ /
-- @pipeline[].build.provisioners[].file@ /
-- @pipeline[].build.floppy@ entries; for each one with a
-- file-path reference, read the file and embed it inline. Apply
-- pipeline steps don't have provisioners or floppies, so they're
-- skipped untouched.
preprocessRoot :: FilePath -> Value -> IO Value
preprocessRoot baseDir = walkPipeline
  where
    walkPipeline (Object o) = case KM.lookup "pipeline" o of
      Just (Array steps) -> do
        steps' <- mapM walkStep (V.toList steps)
        pure $ Object (KM.insert "pipeline" (Array (V.fromList steps')) o)
      _ -> pure (Object o)
    walkPipeline v = pure v

    -- A pipeline step is an object with exactly one of @build:@ or
    -- @apply:@. Only build: carries provisioners and an optional
    -- floppy that the client preprocesses.
    walkStep (Object o) = do
      o' <- adjustField "build" rewriteBuild o
      pure (Object o')
    walkStep v = pure v

    rewriteBuild (Object bo) = do
      bo1 <- case KM.lookup "provisioners" bo of
        Just (Array ps) -> do
          ps' <- mapM walkProv (V.toList ps)
          pure $ KM.insert "provisioners" (Array (V.fromList ps')) bo
        _ -> pure bo
      bo2 <- adjustField "floppy" rewriteFloppy bo1
      pure (Object bo2)
    rewriteBuild v = pure v

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

    -- Inline a build's autounattend / kickstart floppy. Read the
    -- source file, base64-encode its raw bytes, and rewrite
    -- @floppy.from@ into @floppy.contentBase64@. The default
    -- @floppy.filename@ (the entry name on the FAT12 floppy
    -- itself) is the basename of the source path; the daemon
    -- wraps the content into a 1.44 MB FAT12 image and attaches
    -- it to the bake VM.
    rewriteFloppy :: Value -> IO Value
    rewriteFloppy (Object o) = case KM.lookup "from" o of
      Just (String relPath) -> do
        bytes <- readFromFile (T.unpack relPath)
        let basename = T.pack (takeFileName (T.unpack relPath))
            withFilename = case KM.lookup "filename" o of
              Just _ -> o
              Nothing -> KM.insert "filename" (String basename) o
            withContent =
              KM.insert
                "contentBase64"
                (String (TE.decodeUtf8 (B64.encode bytes)))
                (KM.delete "from" withFilename)
        pure (Object withContent)
      _ -> pure (Object o)
    rewriteFloppy other = pure other

    readScript rel = do
      let p = if isAbsolute rel then rel else baseDir </> rel
      TIO.readFile p
    readFromFile rel = do
      let p = if isAbsolute rel then rel else baseDir </> rel
      BS.readFile p
    isAbsolute ('/' : _) = True
    isAbsolute _ = False
