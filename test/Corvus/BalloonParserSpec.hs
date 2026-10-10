module Corvus.BalloonParserSpec (spec) where

import Corvus.Client.Parser.Utility (parseSizeBytes)
import Data.Word (Word64)
import Options.Applicative
import Test.Hspec

parseSize :: String -> ParserResult Word64
parseSize raw = execParserPure defaultPrefs (info (argument parseSizeBytes mempty) mempty) [raw]

spec :: Spec
spec = describe "balloon target sizes" $ do
  mapM_
    ( \(raw, bytes) -> it ("parses " ++ raw) $ case parseSize raw of
        Success actual -> actual `shouldBe` bytes
        _ -> expectationFailure "expected a valid byte target"
    )
    [("1B", 1), ("1k", 1024), ("768M", 768 * 1024 ^ (2 :: Int)), ("1g", 1024 ^ (3 :: Int)), ("1T", 1024 ^ (4 :: Int)), ("18446744073709551615B", maxBound)]
  mapM_
    ( \raw -> it ("rejects " ++ raw) $ case parseSize raw of
        Failure _ -> pure ()
        _ -> expectationFailure "expected an invalid byte target"
    )
    ["0B", "-1K", "1", "1.5G", "1KB", "1P", "18446744073709551616B", "18446744073709551615T"]
