module LibSpec where

import Test.Hspec ( Spec, hspec, describe, it, shouldBe )
import Test.QuickCheck ( property )

import Lib

main :: IO ()
main = hspec spec

spec =
  describe "Lib Tests" $ do
    it "test chart-unit---------------------------" $ do
      _ <- someFunc
      1 `shouldBe` 1

  -- putStrLn "Test suite not yet implemented"
