{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Corvus.CodeMetrics
  ( BindingMetric (..)
  , ModuleMetric (..)
  , Report (..)
  , analyzeProject
  , limitViolations
  , renderReport
  )
where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (filterM, forM)
import Corvus.CodeMetrics.Settings (maximumModuleLines, maximumTopLevelDefinitionLines)
import qualified Data.ByteString as BS
import Data.Either (lefts)
import Data.List (isPrefixOf, maximumBy, sortBy)
import Data.Maybe (mapMaybe)
import Distribution.Compiler (AbiTag (NoAbiTag), buildCompilerId, unknownCompilerInfo)
import Distribution.PackageDescription (BuildInfo (..), allBuildInfo)
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.System (buildPlatform)
import Distribution.Types.ComponentRequestedSpec (defaultComponentRequestedSpec)
import Distribution.Utils.Path (getSymbolicPath)
import qualified GHC
import GHC.Hs (HsBind, HsDecl (..), SrcSpanAnnA, hsmodDecls)
import GHC.Hs.Utils (CollectFlag (CollNoDictBinders), collectHsBindBinders)
import GHC.Parser.Annotation (locA)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc
  ( GenLocated
  , SrcSpan (RealSrcSpan)
  , getLoc
  , noLoc
  , srcSpanEndLine
  , srcSpanStartLine
  , unLoc
  )
import Language.Haskell.Extension (Extension (DisableExtension, EnableExtension))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (makeRelative, normalise, takeExtension, (</>))
import System.Process (readProcess)

data ModuleMetric = ModuleMetric
  { modulePath :: FilePath
  , moduleLines :: Int
  }
  deriving (Eq, Show)

data BindingMetric = BindingMetric
  { bindingPath :: FilePath
  , bindingName :: String
  , bindingStartLine :: Int
  , bindingEndLine :: Int
  , bindingLines :: Int
  }
  deriving (Eq, Show)

data Report = Report
  { reportModules :: [ModuleMetric]
  , reportBindings :: [BindingMetric]
  }
  deriving (Eq, Show)

data ComponentSettings = ComponentSettings
  { componentRoots :: [FilePath]
  , componentFlags :: [String]
  }

analyzeProject :: FilePath -> IO (Either [String] Report)
analyzeProject projectRoot = do
  settingsResult <- loadComponentSettings projectRoot
  case settingsResult of
    Left errorMessage -> pure (Left [errorMessage])
    Right settings -> do
      files <- authoredHaskellFiles projectRoot
      ghcLibDir <- compilerLibDir
      let includePaths = concatMap componentRoots settings
      results <- forM files $ \path -> do
        let flags = componentFlags (settingsFor settings path)
        analyzeFile projectRoot ghcLibDir includePaths flags path
      let errors = lefts results
      if null errors
        then
          pure . Right $
            Report
              { reportModules = [moduleMetric | Right (moduleMetric, _) <- results]
              , reportBindings = concat [bindingMetrics | Right (_, bindingMetrics) <- results]
              }
        else pure (Left errors)

renderReport :: Report -> String
renderReport Report {reportModules, reportBindings} =
  unlines $
    [ "CodeMetrics"
    , "  modules: " <> show (length reportModules)
    , "  top-level value definitions: " <> show (length reportBindings)
    , ""
    , "Largest modules (physical LOC)"
    ]
      <> renderModules (take 10 (sortModules reportModules))
      <> ["", "Largest top-level value definitions (source-span LOC)"]
      <> renderBindings (take 10 (sortBindings reportBindings))

limitViolations :: Report -> [String]
limitViolations Report {reportModules, reportBindings} = moduleViolations <> bindingViolations
  where
    moduleViolations =
      [ modulePath <> ": " <> show moduleLines <> " lines exceeds the module limit of " <> show maximumModuleLines
      | ModuleMetric {modulePath, moduleLines} <- reportModules
      , moduleLines > maximumModuleLines
      ]
    bindingViolations =
      [ bindingPath <> ":" <> show bindingStartLine <> "-" <> show bindingEndLine <> " " <> bindingName <> ": " <> show bindingLines <> " lines exceeds the top-level definition limit of " <> show maximumTopLevelDefinitionLines
      | BindingMetric {bindingPath, bindingName, bindingStartLine, bindingEndLine, bindingLines} <- reportBindings
      , bindingLines > maximumTopLevelDefinitionLines
      ]

loadComponentSettings :: FilePath -> IO (Either String [ComponentSettings])
loadComponentSettings projectRoot = do
  let cabalFile = projectRoot </> "corvus.cabal"
  cabalExists <- doesFileExist cabalFile
  if not cabalExists
    then pure (Left "corvus.cabal not found; run make code-metrics from the repository root")
    else do
      contents <- BS.readFile cabalFile
      case parseGenericPackageDescriptionMaybe contents of
        Nothing -> pure (Left "could not parse corvus.cabal")
        Just packageDescription ->
          case finalizePD mempty defaultComponentRequestedSpec (const True) buildPlatform (unknownCompilerInfo buildCompilerId NoAbiTag) [] packageDescription of
            Left dependencies -> pure (Left ("could not finalize corvus.cabal: " <> show dependencies))
            Right (finalizedPackage, _) -> pure . Right . map (toSettings projectRoot) $ allBuildInfo finalizedPackage

toSettings :: FilePath -> BuildInfo -> ComponentSettings
toSettings projectRoot buildInfo =
  ComponentSettings
    { componentRoots = map ((projectRoot </>) . getSymbolicPath) (hsSourceDirs buildInfo)
    , componentFlags = map extensionFlag (defaultExtensions buildInfo) <> cppOptions buildInfo
    }

extensionFlag :: Extension -> String
extensionFlag = \case
  EnableExtension extension -> "-X" <> show extension
  DisableExtension extension -> "-XNo" <> show extension

authoredHaskellFiles :: FilePath -> IO [FilePath]
authoredHaskellFiles projectRoot = concat <$> mapM (findHaskellFiles . (projectRoot </>)) ["src", "app"]

findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles directory = do
  directoryExists <- doesDirectoryExist directory
  if not directoryExists
    then pure []
    else do
      names <- listDirectory directory
      let paths = map (directory </>) names
      directories <- filterM doesDirectoryExist paths
      files <- filterM doesFileExist paths
      nestedFiles <- concat <$> mapM findHaskellFiles directories
      pure (filter ((== ".hs") . takeExtension) files <> nestedFiles)

settingsFor :: [ComponentSettings] -> FilePath -> ComponentSettings
settingsFor settings path = maximumBy compareRoots (filter matchesPath settings)
  where
    normalizedPath = normalise path
    matchesPath settings' = any (`isPathPrefixOf` normalizedPath) (componentRoots settings')
    compareRoots left right = compare (maximumRootLength left) (maximumRootLength right)
    maximumRootLength = maximum . map length . componentRoots
    isPathPrefixOf root candidate = normalise root `isPrefixOf` candidate

analyzeFile :: FilePath -> FilePath -> [FilePath] -> [String] -> FilePath -> IO (Either String (ModuleMetric, [BindingMetric]))
analyzeFile projectRoot ghcLibDir includePaths flags path = do
  source <- readFile path
  parsed <- try (parseWithGhc ghcLibDir includePaths flags path) :: IO (Either SomeException GHC.ParsedSource)
  pure $ case parsed of
    Left errorValue -> Left (path <> ": " <> displayException errorValue)
    Right parsedSource ->
      Right
        ( ModuleMetric reportPath (physicalLines source)
        , mapMaybe (bindingMetric reportPath) (hsmodDecls (unLoc parsedSource))
        )
  where
    reportPath = makeRelative projectRoot path

compilerLibDir :: IO FilePath
compilerLibDir =
  readProcess "ghc" ["--print-libdir"] "" >>= \output -> case lines output of
    libDir : _ -> pure libDir
    [] -> fail "ghc --print-libdir returned no path"

parseWithGhc :: FilePath -> [FilePath] -> [String] -> FilePath -> IO GHC.ParsedSource
parseWithGhc ghcLibDir includePaths flags path = GHC.runGhc (Just ghcLibDir) $ do
  initialFlags <- GHC.getSessionDynFlags
  logger <- GHC.getLogger
  let parserFlags = flags <> map ("-i" <>) includePaths
  (configuredFlags, _, _) <- GHC.parseDynamicFlags logger initialFlags (map noLoc parserFlags)
  _ <- GHC.setSessionDynFlags configuredFlags
  target <- GHC.guessTarget path Nothing Nothing
  GHC.setTargets [target]
  moduleGraph <- GHC.depanal [] False
  let summaries = filter isTargetSummary (GHC.mgModSummaries moduleGraph)
      isTargetSummary summary = Just (normalise path) == (normalise <$> GHC.ml_hs_file (GHC.ms_location summary))
  case summaries of
    [summary] -> GHC.parsedSource <$> GHC.parseModule summary
    [] -> fail "GHC did not produce a module summary for the target"
    _ -> fail "GHC produced multiple module summaries for the target"

physicalLines :: String -> Int
physicalLines "" = 0
physicalLines source = length (lines source)

bindingMetric :: FilePath -> GenLocated SrcSpanAnnA (HsDecl GHC.GhcPs) -> Maybe BindingMetric
bindingMetric path declaration = do
  binding <- case unLoc declaration of
    ValD _ valueBinding -> Just valueBinding
    _ -> Nothing
  name <- bindingNameOf binding
  (startLine, endLine) <- spanLines (locA (getLoc declaration))
  pure
    BindingMetric
      { bindingPath = path
      , bindingName = name
      , bindingStartLine = startLine
      , bindingEndLine = endLine
      , bindingLines = endLine - startLine + 1
      }

bindingNameOf :: HsBind GHC.GhcPs -> Maybe String
bindingNameOf = \case
  GHC.FunBind {GHC.fun_id = functionId} -> Just (occNameString (rdrNameOcc (unLoc functionId)))
  binding@GHC.PatBind {} -> case collectHsBindBinders CollNoDictBinders binding of
    [name] -> Just (occNameString (rdrNameOcc name))
    _ -> Nothing
  _ -> Nothing

spanLines :: GHC.SrcSpan -> Maybe (Int, Int)
spanLines = \case
  RealSrcSpan span _ -> Just (srcSpanStartLine span, srcSpanEndLine span)
  _ -> Nothing

sortModules :: [ModuleMetric] -> [ModuleMetric]
sortModules = sortBy $ \left right ->
  compare (moduleLines right) (moduleLines left) <> compare (modulePath left) (modulePath right)

sortBindings :: [BindingMetric] -> [BindingMetric]
sortBindings = sortBy $ \left right ->
  compare (bindingLines right) (bindingLines left)
    <> compare (bindingPath left) (bindingPath right)
    <> compare (bindingName left) (bindingName right)

renderModules :: [ModuleMetric] -> [String]
renderModules = map (\ModuleMetric {modulePath, moduleLines} -> "  " <> padLeft 5 (show moduleLines) <> "  " <> modulePath)

renderBindings :: [BindingMetric] -> [String]
renderBindings = map renderBinding
  where
    renderBinding BindingMetric {bindingPath, bindingName, bindingStartLine, bindingEndLine, bindingLines} =
      "  " <> padLeft 5 (show bindingLines) <> "  " <> bindingPath <> ":" <> show bindingStartLine <> "-" <> show bindingEndLine <> "  " <> bindingName

padLeft :: Int -> String -> String
padLeft width value = replicate (max 0 (width - length value)) ' ' <> value
