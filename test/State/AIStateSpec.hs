{-# LANGUAGE OverloadedStrings #-}

module State.AIStateSpec (spec) where

import Test.Hspec
import State.AIState
import Game.Types
import Game.Board
import Game.AI (initAIState)

spec :: Spec
spec = do
    describe "AIGamePhase" $ do
        it "has correct equality" $ do
            AISetup `shouldBe` AISetup
            AIInProgress `shouldBe` AIInProgress
            (AIGameOver "player") `shouldBe` (AIGameOver "player")
            (AIGameOver "player") `shouldNotBe` (AIGameOver "ai")
    
    describe "PlayerTurn" $ do
        it "has correct equality" $ do
            HumanTurn `shouldBe` HumanTurn
            AITurn `shouldBe` AITurn
            HumanTurn `shouldNotBe` AITurn
    
    describe "AIGameState" $ do
        it "initializes correctly" $ do
            let playerShips = makeValidShips
            let aiShips = makeValidShips
            let state = initAIGameState "game1" "p1" "Alice" playerShips aiShips
            
            gameId state `shouldBe` "game1"
            playerId state `shouldBe` "p1"
            playerName state `shouldBe` "Alice"
            currentTurn state `shouldBe` HumanTurn
            gamePhase state `shouldBe` AISetup
            length (playerShipsLeft state) `shouldBe` 5
            length (aiShipsLeft state) `shouldBe` 5
        
        it "updates after move" $ do
            let playerShips = makeValidShips
            let aiShips = makeValidShips
            let state = initAIGameState "game1" "p1" "Alice" playerShips aiShips
            
            let result = MoveResult (Position 0 0) Hit Nothing
            let updated = updateAIGameState state "p1" (Position 0 0) result
            
            length (moveHistory updated) `shouldBe` 1
        
        it "sets game phase" $ do
            let playerShips = makeValidShips
            let aiShips = makeValidShips
            let state = initAIGameState "game1" "p1" "Alice" playerShips aiShips
            
            let updated = setAIGamePhase state AIInProgress
            gamePhase updated `shouldBe` AIInProgress
        
        it "switches turn" $ do
            let playerShips = makeValidShips
            let aiShips = makeValidShips
            let state = initAIGameState "game1" "p1" "Alice" playerShips aiShips
            
            currentTurn state `shouldBe` HumanTurn
            
            let switched = switchAITurn state
            currentTurn switched `shouldBe` AITurn
            
            let switchedBack = switchAITurn switched
            currentTurn switchedBack `shouldBe` HumanTurn
    
    describe "Global Storage (IORef)" $ do
        it "adds and retrieves AI game" $ do
            let playerShips = makeValidShips
            let aiShips = makeValidShips
            let state = initAIGameState "game1" "p1" "Alice" playerShips aiShips
            
            addAIGame "game1" state
            
            retrieved <- getAIGame "game1"
            case retrieved of
                Just game -> gameId game `shouldBe` "game1"
                Nothing -> expectationFailure "Game not found"
        
        it "returns Nothing for non-existent game" $ do
            result <- getAIGame "nonexistent"
            result `shouldBe` Nothing
        
        it "removes AI game" $ do
            let playerShips = makeValidShips
            let aiShips = makeValidShips
            let state = initAIGameState "game2" "p1" "Alice" playerShips aiShips
            
            addAIGame "game2" state
            removeAIGame "game2"
            
            result <- getAIGame "game2"
            result `shouldBe` Nothing
        
        it "updates AI game" $ do
            let playerShips = makeValidShips
            let aiShips = makeValidShips
            let state = initAIGameState "game3" "p1" "Alice" playerShips aiShips
            
            addAIGame "game3" state
            
            updateAIGame "game3" $ \g -> setAIGamePhase g AIInProgress
            
            retrieved <- getAIGame "game3"
            case retrieved of
                Just game -> gamePhase game `shouldBe` AIInProgress
                Nothing -> expectationFailure "Game not found"

-- Helper function
makeValidShips :: [Ship]
makeValidShips =
    [ Ship Carrier (Position 0 0) Horizontal
    , Ship Battleship (Position 1 0) Horizontal
    , Ship Cruiser (Position 2 0) Horizontal
    , Ship Submarine (Position 3 0) Horizontal
    , Ship Destroyer (Position 4 0) Horizontal
    ]
