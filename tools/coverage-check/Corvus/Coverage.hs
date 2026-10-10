{-# LANGUAGE CPP #-}

module Corvus.Coverage
  ( Coverage (..)
  , calculateCoverage
  , meetsBaseline
  , isAuthoredSource
  , loadCoverage
  , readBaseline
  ) where

import Control.Monad (forM, unless, when)
import Data.Aeson (eitherDecodeFileStrict', withObject, (.:))
import Data.Aeson.Types (Parser, parseEither)
import Data.List (isPrefixOf, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Version (showVersion)
import System.Directory (doesDirectoryExist, getModificationTime, listDirectory)
import System.FilePath (isRelative, normalise, splitDirectories, takeBaseName, takeDirectory, takeExtension, takeFileName, (</>))
import System.Info (compilerVersion)
import Text.Read (readMaybe)
import Trace.Hpc.Mix (BoxLabel (ExpBox), Mix (..))
import Trace.Hpc.Tix (Tix (..), TixModule (..))

data Coverage = Coverage
  { coveredExpressions :: Integer
  , totalExpressions :: Integer
  }
  deriving stock (Eq, Show)

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
    && case splitDirectories (normalise path) of
      "src" : rest -> not (null rest) && ".." `notElem` rest
      _ -> False

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

readBaseline :: FilePath -> IO Coverage
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
          baseline <- Coverage <$> o .: "covered_expressions" <*> o .: "total_expressions"
          unless (meetsBaseline baseline baseline) $ fail "Invalid coverage baseline counts"
          pure baseline
      )
      value

loadCoverage :: FilePath -> FilePath -> IO (Coverage, Tix)
loadCoverage mixDirectory tracePath = do
  sources <- filter isAuthoredSource <$> filesUnder "src"
  paths <- filter ((== ".mix") . takeExtension) <$> filesUnder mixDirectory
  when (null paths) $ fail "No coverage metadata found"
  traceTime <- getModificationTime tracePath
  mixes <- forM paths $ \path -> do
    mix <- readArtifact path
    let Mix source _ _ _ _ = mix
    whenAuthored source $ do
      modified <- getModificationTime source
      compiled <- getModificationTime path
      -- Formatters can touch unchanged sources without GHC rebuilding them.
      -- The fresh suite trace must postdate both source and compiled metadata;
      -- Stack checks source contents before the canonical test run.
      unless (traceTime >= compiled && traceTime >= modified) $
        fail ("Stale coverage artifact: " ++ path)
    pure (takeFileName (takeDirectory path) ++ "/" ++ takeBaseName path, mix)
  Tix traces <- readArtifact tracePath
  -- The suite trace also contains executable and test modules. Only this
  -- library unit is in scope, including its generated modules for validation.
  let unitPrefix = takeFileName mixDirectory ++ "/"
      trace = Tix [t | t@(TixModule name _ _ _) <- traces, unitPrefix `isPrefixOf` name]
  either fail pure (calculateCoverage sources mixes trace)
  where
    whenAuthored source = when (isAuthoredSource source)

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
