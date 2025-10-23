{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Game.Types
import Game.Ship
import Game.Board
import qualified Game.Rules

main :: IO ()
main = hspec $ do
    gameTypesSpec
    gameShipSpec
    gameBoardSpec
    gameRulesSpec

gameTypesSpec :: Spec
gameTypesSpec = describe "Game.Types" $ do
    it "Position has row and col" $ do
        let pos = Position {posRow = 5, posCol = 5}
        posRow pos `shouldBe` 5
        posCol pos `shouldBe` 5

gameShipSpec :: Spec
gameShipSpec = describe "Game.Ship" $ do
    it "makeShip creates ship with correct type" $ do
        let ship = makeShip Carrier (Position {posRow=0, posCol=0}) Horizontal
        shipType ship `shouldBe` Carrier
    
    it "isSunk returns False for new ship" $ do
        let ship = makeShip Destroyer (Position {posRow=0, posCol=0}) Horizontal
        isSunk ship `shouldBe` False
    
    it "getShipPositions returns correct number of positions" $ do
        let ship = makeShip Cruiser (Position {posRow=0, posCol=0}) Horizontal
            positions = getShipPositions ship
        length positions `shouldBe` 3

gameBoardSpec :: Spec
gameBoardSpec = describe "Game.Board" $ do
    it "allShipsSunk returns True for empty board" $ do
        let board = emptyBoard
        allShipsSunk board `shouldBe` True

gameRulesSpec :: Spec
gameRulesSpec = describe "Game.Rules" $ do
    it "validateFleet accepts valid fleet" $ do
        let fleet = makeFleet
                [ (Carrier, Position {posRow=0, posCol=0}, Horizontal)
                , (Battleship, Position {posRow=1, posCol=0}, Horizontal)
                , (Cruiser, Position {posRow=2, posCol=0}, Horizontal)
                , (Submarine, Position {posRow=3, posCol=0}, Horizontal)
                , (Destroyer, Position {posRow=4, posCol=0}, Horizontal)
                ]
        Game.Rules.validateFleet fleet `shouldBe` True
    
    it "correctShipCount validates fleet size" $ do
        let fleet = makeFleet
                [ (Carrier, Position {posRow=0, posCol=0}, Horizontal)
                , (Battleship, Position {posRow=1, posCol=0}, Horizontal)
                ]
        Game.Rules.correctShipCount fleet `shouldBe` False
