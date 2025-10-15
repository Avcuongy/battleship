module Main where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "BattleShip Tests" $ do
    it "placeholder test" $ do
      1 + 1 `shouldBe` 2
