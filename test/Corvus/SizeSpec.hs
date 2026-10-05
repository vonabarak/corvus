module Corvus.SizeSpec (spec) where

import Corvus.Size (formatSize, parseBytes, parseRam)
import Data.Either (isLeft)
import Data.Int (Int64)
import Test.Hspec

spec :: Spec
spec = describe "byte sizes" $ do
  it "uses binary suffixes and preserves exact values" $ do
    map (parseBytes :: String -> Either String Int64) ["1B", "1k", "1536M", "1G", "1T"]
      `shouldBe` map Right [1, 1024, 1610612736, 1073741824, 1099511627776]
    map formatSize ([0, 1, 1024, 1537, 1610612736, 1073741824] :: [Int64])
      `shouldBe` ["0B", "1B", "1K", "1537B", "1536M", "1G"]
  it "rejects absent suffixes, fractions, zero and overflow" $
    mapM_
      (\raw -> (parseBytes raw :: Either String Int64) `shouldSatisfy` isLeft)
      ["1", "1.5G", "0B", "-1M", "1KB", "9223372036854775808B", "8388608T"]
  it "round trips the entire signed capacity range without rounding" $
    mapM_
      (\size -> parseBytes (formatSize size) `shouldBe` Right size)
      ([1, 1537, 9007199254740993, maxBound] :: [Int64])
  it "requires RAM to be a whole number of MiB" $ do
    parseRam "1G" `shouldBe` Right 1073741824
    parseRam "1537B" `shouldSatisfy` isLeft
