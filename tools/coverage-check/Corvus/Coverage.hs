{-# LANGUAGE CPP #-}

module Corvus.Coverage
  ( Coverage (..)
  , Baseline (..)
  , calculateCoverage
  , cliCoverage
  , meetsBaseline
  , isAuthoredSource
  , mergeTraces
  , loadCoverage
  , readBaseline
  ) where

import Control.Monad (foldM, forM, unless, when)
import Data.Aeson (eitherDecodeFileStrict', withObject, (.:))
import Data.Aeson.Types (Parser, parseEither)
import Data.List (isPrefixOf, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Version (showVersion)
import System.Directory (doesDirectoryExist, getCurrentDirectory, getModificationTime, listDirectory)
import System.FilePath (isRelative, makeRelative, normalise, splitDirectories, takeBaseName, takeExtension, takeFileName, (</>))
import System.Info (compilerVersion)
import Text.Read (readMaybe)
import Trace.Hpc.Mix (BoxLabel (ExpBox), Mix (..))
import Trace.Hpc.Tix (Tix (..), TixModule (..))

data Coverage = Coverage
  { coveredExpressions :: Integer
  , totalExpressions :: Integer
  }
  deriving stock (Eq, Show)

data Baseline = Baseline
  { overallMinimum :: Coverage
  , cliMinimumPercent :: Integer
  , commandMinimumPercent :: Integer
  }
  deriving stock (Eq, Show)

-- The trace has already been validated and zero-filled by calculateCoverage.
-- Match complete module components, so ClientOther never enters CLI scope.
cliCoverage :: Tix -> [(String, Mix)] -> Either String (Coverage, [(String, Coverage)])
cliCoverage (Tix traces) mixes = do
  measured <- forM traces $ \(TixModule name _ _ values) -> do
    Mix _ _ _ _ entries <- maybe (Left ("Missing scoped metadata: " ++ name)) Right (Map.lookup name metadata)
    let expressions = [tick | ((_, ExpBox _), tick) <- zip entries values]
        coverage = Coverage (fromIntegral (length (filter (> 0) expressions))) (fromIntegral (length expressions))
        moduleName = drop 1 (dropWhile (/= '/') name)
    pure (moduleName, coverage)
  let clients = filter (inScope "Corvus.Client" . fst) measured
      commands = filter (\(name, c) -> inScope "Corvus.Client.Commands" name && totalExpressions c > 0) clients
      aggregate = Coverage (sum (map (coveredExpressions . snd) clients)) (sum (map (totalExpressions . snd) clients))
  unless (totalExpressions aggregate > 0 && not (null commands)) $ Left "Missing CLI or command coverage inventory"
  pure (aggregate, commands)
  where
    metadata = Map.fromList mixes
    inScope root name = name == root || (root ++ ".") `isPrefixOf` name

meetsBaseline :: Coverage -> Coverage -> Bool
meetsBaseline actual baseline =
  valid actual
    && valid baseline
    && coveredExpressions actual * totalExpressions baseline
      >= coveredExpressions baseline * totalExpressions actual
  where
    valid c = totalExpressions c > 0 && coveredExpressions c >= 0 && coveredExpressions c <= totalExpressions c

isAuthoredSource :: FilePath -> Bool
isAuthoredSource path =
  isRelative path
    && takeExtension path `elem` [".hs", ".lhs"]
    && ".." `notElem` splitDirectories path
    && any
      (`isPrefixOf` normalise path)
      ["src/", "vendor/haskell-capnp/capnp/lib/", "vendor/lifetimes/src/"]

-- Missing trace modules are unexecuted, rather than absent from the denominator.
calculateCoverage :: [FilePath] -> [(String, Mix)] -> Tix -> Either String (Coverage, Tix)
calculateCoverage sources mixes (Tix traces) = do
  unless (length mixes == Map.size mixMap) $ Left "Duplicate coverage modules"
  unless (length traces == Map.size traceMap) $ Left "Duplicate trace modules"
  unless (length authored == Set.size (Set.fromList (map sourceOf authored))) $
    Left "Duplicate authored source metadata"
  unless (Set.fromList (map normalise sources) == Set.fromList (map sourceOf authored)) $
    Left "Authored source inventory does not match compiled coverage modules; rebuild the quality directory"
  unless (all (`Map.member` mixMap) (Map.keys traceMap)) $
    Left "Trace contains modules with missing coverage metadata"
  measured <- forM mixes $ \(name, Mix _ _ hash _ entries) -> do
    let ticks = Map.findWithDefault (TixModule name hash (length entries) (replicate (length entries) 0)) name traceMap
        TixModule _ traceHash count values = ticks
    unless (hash == traceHash && count == length entries && length values == count && all (>= 0) values) $
      Left ("Coverage hash or tick count mismatch: " ++ name)
    let expressions = [tick | ((_, ExpBox _), tick) <- zip entries values]
    pure (name, Coverage (fromIntegral (length (filter (> 0) expressions))) (fromIntegral (length expressions)), ticks)
  let included = [(c, ticks) | (name, c, ticks) <- measured, Set.member name authoredNames]
      actual = Coverage (sum (map (coveredExpressions . fst) included)) (sum (map (totalExpressions . fst) included))
  unless (totalExpressions actual > 0) $ Left "No authored expression coverage found"
  pure (actual, Tix (map snd included))
  where
    mixMap = Map.fromList mixes
    traceMap = Map.fromList [(name, t) | t@(TixModule name _ _ _) <- traces]
    authored = filter (isAuthoredSource . sourceOf) mixes
    authoredNames = Set.fromList (map fst authored)
    sourceOf (_, Mix source _ _ _ _) = normalise source

readBaseline :: FilePath -> IO Baseline
readBaseline path = do
  value <- eitherDecodeFileStrict' path >>= either (fail . ("Invalid coverage baseline: " ++)) pure
  either fail pure $
    parseEither
      ( withObject "coverage baseline" $ \o -> do
          ghc <- o .: "ghc"
          backends <- o .: "backends" :: Parser [String]
          backend <- o .: "test_backend"
          seed <- o .: "seed" :: Parser Integer
          jobs <- o .: "jobs" :: Parser Integer
          unless
            ( ghc == showVersion compilerVersion ++ "." ++ show (__GLASGOW_HASKELL_PATCHLEVEL1__ :: Int)
                && sort backends == ["postgresql", "sqlite"]
                && backend == ("sqlite" :: String)
                && seed == 20261010
                && jobs == 1
            )
            $ fail "Coverage baseline configuration does not match the canonical quality build"
          packages <- o .: "packages" :: Parser [String]
          unless (sort packages == ["capnp", "corvus", "lifetimes"]) $
            fail "Coverage baseline package scope does not match the quality build"
          baseline <- Coverage <$> o .: "covered_expressions" <*> o .: "total_expressions"
          unless (meetsBaseline baseline baseline) $ fail "Invalid coverage baseline counts"
          cliPercent <- o .: "cli_minimum_percent"
          commandPercent <- o .: "cli_command_module_minimum_percent"
          unless (all (\p -> p > 0 && p <= 100) [cliPercent, commandPercent]) $ fail "Invalid CLI coverage minimum"
          pure (Baseline baseline cliPercent commandPercent)
      )
      value

-- Every suite can exercise multiple libraries. Validate each trace before
-- summing, retaining package unit IDs so equal module names never collide.
mergeTraces :: [Tix] -> Either String Tix
mergeTraces traces = Tix . Map.elems <$> foldM addTrace Map.empty traces
  where
    addTrace accumulated (Tix modules) = do
      unless (length modules == Set.size (Set.fromList [name | TixModule name _ _ _ <- modules])) $
        Left "Duplicate coverage modules within a suite trace"
      foldM addModule accumulated modules
    addModule accumulated entry@(TixModule name hash count values) = do
      unless (count == length values && all (>= 0) values) $
        Left ("Malformed coverage trace: " ++ name)
      case Map.lookup name accumulated of
        Nothing -> pure (Map.insert name entry accumulated)
        Just (TixModule _ previousHash previousCount previousValues) -> do
          unless (hash == previousHash && count == previousCount) $
            Left ("Incompatible suite coverage traces: " ++ name)
          pure (Map.insert name (TixModule name hash count (zipWith (+) previousValues values)) accumulated)

-- Package roots and metadata directories are paired explicitly because GHC
-- records package-relative source filenames in vendored .mix files.
loadCoverage :: [(FilePath, FilePath)] -> [FilePath] -> IO (Coverage, Tix, Coverage, [(String, Coverage)], [(String, Mix)])
loadCoverage packages tracePaths = do
  when (null packages || null tracePaths) $ fail "Missing coverage packages or suite traces"
  traces <- mapM readArtifact tracePaths
  traceTimes <- mapM getModificationTime tracePaths
  sources <- concatMap (filter isAuthoredSource) <$> mapM (filesUnder . sourceRoot . fst) packages
  mixes <- concat <$> mapM (loadPackage (minimum traceTimes)) packages
  let units = Set.fromList [takeWhile (/= '/') name | (name, _) <- mixes]
      libraryTrace (Tix modules) = Tix [entry | entry@(TixModule name _ _ _) <- modules, Set.member (takeWhile (/= '/') name) units]
  trace <- either fail pure (mergeTraces (map libraryTrace traces))
  (actual, authoredTrace) <- either fail pure (calculateCoverage sources mixes trace)
  (clients, commands) <- either fail pure (cliCoverage authoredTrace mixes)
  pure (actual, authoredTrace, clients, commands, mixes)
  where
    sourceRoot root = normalise (root </> if normalise root == "vendor/haskell-capnp/capnp" then "lib" else "src")
    loadPackage traceTime (root, mixDirectory) = do
      paths <- filter ((== ".mix") . takeExtension) <$> filesUnder mixDirectory
      when (null paths) $ fail ("No coverage metadata found: " ++ mixDirectory)
      current <- getCurrentDirectory
      forM paths $ \path -> do
        Mix source stamp hash tab entries <- readArtifact path
        let resolved = normalise (if isRelative source then root </> source else makeRelative current source)
        when (not (isRelative resolved) || ".." `elem` splitDirectories resolved) $
          fail ("Coverage source escapes repository: " ++ source)
        when (isAuthoredSource resolved) $ do
          modified <- getModificationTime resolved
          compiled <- getModificationTime path
          unless (traceTime >= compiled && traceTime >= modified) $
            fail ("Stale coverage artifact: " ++ path)
        pure (takeFileName mixDirectory ++ "/" ++ takeBaseName path, Mix resolved stamp hash tab entries)

readArtifact :: (Read a) => FilePath -> IO a
readArtifact path = readFile path >>= maybe (fail ("Malformed coverage artifact: " ++ path)) pure . readMaybe

filesUnder :: FilePath -> IO [FilePath]
filesUnder root = do
  names <- sort <$> listDirectory root
  concat
    <$> forM
      names
      ( \name -> do
          let path = root </> name
          directory <- doesDirectoryExist path
          if directory then filesUnder path else pure [path]
      )
