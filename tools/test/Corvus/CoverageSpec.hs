module Corvus.CoverageSpec (spec) where

import Control.Exception (IOException)
import Corvus.Coverage (Coverage (..), calculateCoverage, isAuthoredSource, loadCoverage, meetsBaseline, readBaseline)
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import System.Directory (setModificationTime)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Trace.Hpc.Mix (BoxLabel (ExpBox, TopLevelBox), Mix (..))
import Trace.Hpc.Tix (Tix (..), TixModule (..))
import Trace.Hpc.Util (toHash)

spec :: Spec
spec = describe "Haskell coverage gate" $ do
  describe "exact threshold comparison" $ do
    it "accepts equality and higher coverage" $ do
      meetsBaseline (Coverage 2 6) (Coverage 1 3) `shouldBe` True
      meetsBaseline (Coverage 3 6) (Coverage 1 3) `shouldBe` True
    it "rejects a decrease hidden by display rounding" $
      meetsBaseline (Coverage 28109 98335) (Coverage 28110 98335) `shouldBe` False
    it "rejects empty or invalid measurements" $ do
      meetsBaseline (Coverage 0 0) (Coverage 1 3) `shouldBe` False
      meetsBaseline (Coverage 1 3) (Coverage 0 0) `shouldBe` False
      meetsBaseline (Coverage 4 3) (Coverage 1 3) `shouldBe` False
  describe "source scope" $ do
    it "includes authored library code" $
      isAuthoredSource "src/Corvus/Database.hs" `shouldBe` True
    it "excludes generated, vendor, executable, test, and escaping paths" $
      map isAuthoredSource ["src-generated/X.hs", "vendor/X.hs", "app/X.hs", "test/X.hs", "src/../app/X.hs", "/src/X.hs"]
        `shouldBe` replicate 6 False
  describe "compiled module inventory" $ do
    it "counts missing trace modules as uncovered and excludes generated expressions" $ do
      time <- getCurrentTime
      let hash = toHash (1 :: Int)
          entries = [(read "1:1-1:2", ExpBox False), (read "1:1-1:2", TopLevelBox ["f"])]
          mix path = Mix path time hash 8 entries
          result =
            calculateCoverage
              ["src/A.hs", "src/B.hs"]
              [("unit/A", mix "src/A.hs"), ("unit/B", mix "src/B.hs"), ("unit/G", mix "src-generated/G.hs")]
              (Tix [TixModule "unit/A" hash 2 [1, 1], TixModule "unit/G" hash 2 [1, 1]])
      case result of
        Left err -> expectationFailure err
        Right (coverage, Tix modules) -> do
          coverage `shouldBe` Coverage 1 2
          modules `shouldContain` [TixModule "unit/B" hash 2 [0, 0]]
          length modules `shouldBe` 2
    it "rejects missing metadata, hash mismatches, and malformed tick arrays" $ do
      time <- getCurrentTime
      let hash = toHash (1 :: Int)
          mix = Mix "src/A.hs" time hash 8 [(read "1:1-1:2", ExpBox False)]
          check = calculateCoverage ["src/A.hs"] [("unit/A", mix)]
      check (Tix [TixModule "unit/A" (toHash (2 :: Int)) 1 [1]]) `shouldSatisfy` isLeft
      check (Tix [TixModule "unit/A" hash 1 []]) `shouldSatisfy` isLeft
      check (Tix [TixModule "unit/A" hash 1 [-1]]) `shouldSatisfy` isLeft
      check (Tix [TixModule "unit/Missing" hash 1 [1]]) `shouldSatisfy` isLeft
      calculateCoverage ["src/Missing.hs"] [("unit/A", mix)] (Tix []) `shouldSatisfy` isLeft
    it "rejects duplicate and empty inventories" $ do
      time <- getCurrentTime
      let hash = toHash (1 :: Int)
          mix = Mix "src/A.hs" time hash 8 [(read "1:1-1:2", ExpBox False)]
          check = calculateCoverage ["src/A.hs"] [("unit/A", mix)]
          trace = TixModule "unit/A" hash 1 [1]
      check (Tix [trace, trace]) `shouldSatisfy` isLeft
      calculateCoverage ["src/A.hs"] [("unit/A", mix), ("unit/A", mix)] (Tix []) `shouldSatisfy` isLeft
      calculateCoverage ["src/A.hs"] [("unit/A", mix), ("unit/Other", mix)] (Tix []) `shouldSatisfy` isLeft
      calculateCoverage [] [] (Tix []) `shouldSatisfy` isLeft
  describe "artifact validation" $ do
    it "accepts the committed baseline for the quality compiler" $
      readBaseline "coverage-baseline.json" `shouldReturn` Coverage 35909 98136
    it "rejects missing artifacts" $
      loadCoverage "/nonexistent-corvus-coverage" "/nonexistent-corvus-trace" `shouldThrow` anyIOException
    it "rejects malformed mix files" $ withSystemTempDirectory "corvus-coverage" $ \dir -> do
      writeFile (dir </> "Bad.mix") "invalid"
      writeFile (dir </> "test.tix") "Tix []"
      loadCoverage dir (dir </> "test.tix") `shouldThrow` anyIOException
    it "rejects stale metadata after a source change" $ withSystemTempDirectory "corvus-coverage" $ \dir -> do
      let mix = Mix "src/Corvus/Database.hs" (UTCTime (fromGregorian 1970 1 1) 0) (toHash (1 :: Int)) 8 []
      writeFile (dir </> "Database.mix") (show mix)
      writeFile (dir </> "test.tix") "Tix []"
      setModificationTime (dir </> "test.tix") (UTCTime (fromGregorian 1970 1 1) 0)
      loadCoverage dir (dir </> "test.tix") `shouldThrow` (\e -> "Stale coverage artifact" `isInfixOf` show (e :: IOException))
    it "rejects missing and malformed baselines" $ withSystemTempDirectory "corvus-baseline" $ \dir -> do
      readBaseline (dir </> "missing.json") `shouldThrow` anyIOException
      writeFile (dir </> "bad.json") "{}"
      readBaseline (dir </> "bad.json") `shouldThrow` anyIOException
