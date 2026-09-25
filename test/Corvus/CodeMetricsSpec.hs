{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Corvus.CodeMetricsSpec (spec) where

import Corvus.CodeMetrics (BindingMetric (..), ModuleMetric (..), Report (..), analyzeProject, limitViolations, renderReport)
import Data.List (isInfixOf, isPrefixOf)
import Test.Hspec

spec :: Spec
spec = do
  describe "GHC parser code metrics" $ do
    it "parses authored source but excludes unit tests" $ do
      analyzeProject "." >>= \case
        Left errors -> expectationFailure (unlines errors)
        Right Report {reportModules, reportBindings} -> do
          let paths = map modulePath reportModules
          paths `shouldContain` ["src/Corvus/Database.hs"]
          paths `shouldNotContain` ["test/Spec.hs"]
          map bindingName reportBindings `shouldContain` ["runDatabaseMigrations"]

    it "sorts rendered entries by descending size and then path" $ do
      let report =
            Report
              { reportModules = [ModuleMetric "z.hs" 12, ModuleMetric "a.hs" 12, ModuleMetric "small.hs" 2]
              , reportBindings = [BindingMetric "z.hs" "z" 1 8 8, BindingMetric "a.hs" "a" 1 8 8]
              }
          rendered = renderReport report
      rendered `shouldSatisfy` ("a.hs" `isInfixOf`)
      rendered `shouldSatisfy` ("z.hs" `isInfixOf`)
      rendered `shouldSatisfy` ("a.hs" `appearsBefore` "z.hs")

    it "renders only the ten largest modules and definitions" $ do
      let report =
            Report
              { reportModules = [ModuleMetric ("module-#" <> show size <> "#") size | size <- [1 .. 11]]
              , reportBindings = [BindingMetric "definitions.hs" ("definition-#" <> show size <> "#") 1 size size | size <- [1 .. 11]]
              }
          rendered = renderReport report
      rendered `shouldSatisfy` ("module-#11#" `isInfixOf`)
      rendered `shouldSatisfy` ("module-#2#" `isInfixOf`)
      rendered `shouldSatisfy` (not . ("module-#1#" `isInfixOf`))
      rendered `shouldSatisfy` ("definition-#11#" `isInfixOf`)
      rendered `shouldSatisfy` ("definition-#2#" `isInfixOf`)
      rendered `shouldSatisfy` (not . ("definition-#1#" `isInfixOf`))

    it "enforces limits for every reported metric" $ do
      let report =
            Report
              { reportModules = [ModuleMetric "src/Large.hs" 1001]
              , reportBindings = [BindingMetric "src/Large.hs" "large" 1 501 501]
              }
      limitViolations report
        `shouldBe` [ "src/Large.hs: 1001 lines exceeds the module limit of 1000"
                   , "src/Large.hs:1-501 large: 501 lines exceeds the top-level definition limit of 500"
                   ]

appearsBefore :: String -> String -> String -> Bool
appearsBefore first second value = case breakOn first value of
  Nothing -> False
  Just afterFirst -> second `isInfixOf` afterFirst

breakOn :: String -> String -> Maybe String
breakOn needle = go
  where
    go [] = Nothing
    go remaining@(_ : rest)
      | needle `isPrefixOf` remaining = Just (drop (length needle) remaining)
      | otherwise = go rest
