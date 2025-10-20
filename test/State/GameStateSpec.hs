{-# LANGUAGE OverloadedStrings #-}

module State.GameStateSpec (spec) where

import Test.Hspec
import State.GameState
import Game.Types
import Game.Board
import Data.Text (Text)

spec :: Spec
spec = do
    describe "Player" $ do
        it "initializes with correct defaults" $ do
            let player = initPlayer "p1" "Alice"
            playerId player `shouldBe` "p1"
            playerName player `shouldBe` "Alice"
            isReady player `shouldBe` False
            totalMoves player `shouldBe` 0
            length (shipsLeft player) `shouldBe` 5
        
        it "sets ready status" $ do
            let player = initPlayer "p1" "Alice"
            let readyPlayer = setPlayerReady player True
            isReady readyPlayer `shouldBe` True
        
        it "updates board correctly" $ do
            let player = initPlayer "p1" "Alice"
            let newBoard = emptyBoard
            let newShipsLeft = [Destroyer]
            let updated = updatePlayerBoard player newBoard newShipsLeft
            length (shipsLeft updated) `shouldBe` 1
    
    describe "Turn" $ do
        it "identifies current player" $ do
            timer <- startTimer 30
            let turn = Player1Turn timer
            getCurrentPlayer turn `shouldBe` 1
        
        it "identifies opponent" $ do
            timer <- startTimer 30
            let turn = Player1Turn timer
            getOpponentPlayer turn `shouldBe` 2
        
        it "extracts timer state" $ do
            timer <- startTimer 30
            let turn = Player2Turn timer
            isActive (getTurnTimer turn) `shouldBe` True
    
    describe "GamePhase" $ do
        it "has correct equality" $ do
            WaitingForPlayers `shouldBe` WaitingForPlayers
            SetupPhase `shouldBe` SetupPhase
            InProgress `shouldBe` InProgress
        
        it "distinguishes game over states" $ do
            (GameOver "p1") `shouldBe` (GameOver "p1")
            (GameOver "p1") `shouldNotBe` (GameOver "p2")
    
    describe "GameState" $ do
        it "initializes with correct defaults" $ do
            gameState <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            roomId gameState `shouldBe` "room1"
            playerId (player1 gameState) `shouldBe` "p1"
            playerId (player2 gameState) `shouldBe` "p2"
            getCurrentPlayer (currentTurn gameState) `shouldBe` 1
            gamePhase gameState `shouldBe` WaitingForPlayers
            consecutiveHits gameState `shouldBe` 0
        
        it "updates after move correctly" $ do
            gameState <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            let pos = Position 0 0
            let result = MoveResult pos Hit Nothing
            let updated = updateGameState gameState "p1" pos result
            length (moveHistory updated) `shouldBe` 1
            consecutiveHits updated `shouldBe` 1
        
        it "resets consecutive hits on miss" $ do
            gameState <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            let pos = Position 0 0
            let hitResult = MoveResult pos Hit Nothing
            let updated1 = updateGameState gameState "p1" pos hitResult
            consecutiveHits updated1 `shouldBe` 1
            
            let missResult = MoveResult (Position 1 1) Miss Nothing
            let updated2 = updateGameState updated1 "p1" (Position 1 1) missResult
            consecutiveHits updated2 `shouldBe` 0
        
        it "switches turn correctly" $ do
            gameState <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            getCurrentPlayer (currentTurn gameState) `shouldBe` 1
            
            switched <- switchTurn gameState
            getCurrentPlayer (currentTurn switched) `shouldBe` 2
            
            switchedBack <- switchTurn switched
            getCurrentPlayer (currentTurn switchedBack) `shouldBe` 1
        
        it "identifies player turn correctly" $ do
            gameState <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            isPlayerTurn gameState "p1" `shouldBe` True
            isPlayerTurn gameState "p2" `shouldBe` False
            
            switched <- switchTurn gameState
            isPlayerTurn switched "p1" `shouldBe` False
            isPlayerTurn switched "p2" `shouldBe` True
    
    describe "Queries" $ do
        it "gets player by ID" $ do
            gameState <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            case getPlayerById gameState "p1" of
                Just player -> playerName player `shouldBe` "Alice"
                Nothing -> expectationFailure "Player not found"
            
            case getPlayerById gameState "p2" of
                Just player -> playerName player `shouldBe` "Bob"
                Nothing -> expectationFailure "Player not found"
            
            getPlayerById gameState "p3" `shouldBe` Nothing
        
        it "checks both players ready" $ do
            gameState <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            bothPlayersReady gameState `shouldBe` False
            
            let p1Ready = (player1 gameState) { isReady = True }
            let gameState1 = gameState { player1 = p1Ready }
            bothPlayersReady gameState1 `shouldBe` False
            
            let p2Ready = (player2 gameState1) { isReady = True }
            let gameState2 = gameState1 { player2 = p2Ready }
            bothPlayersReady gameState2 `shouldBe` True
        
        it "gets game phase" $ do
            gameState <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            getGamePhase gameState `shouldBe` WaitingForPlayers
