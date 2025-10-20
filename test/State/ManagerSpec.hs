{-# LANGUAGE OverloadedStrings #-}

module State.ManagerSpec (spec) where

import Test.Hspec
import Control.Concurrent.STM
import Control.Concurrent.Async
import State.Manager
import State.GameState
import Game.Types
import Data.Text (Text)

spec :: Spec
spec = do
    describe "PvPState Initialization" $ do
        it "initializes with empty state" $ do
            state <- initPvPState
            queueSize <- atomically $ getQueueSize state
            activeCount <- atomically $ getActiveRoomCount state
            
            queueSize `shouldBe` 0
            activeCount `shouldBe` 0
    
    describe "Matchmaking (STM)" $ do
        it "first player waits in queue" $ do
            state <- initPvPState
            result <- joinQueue state "p1" "Alice"
            
            result `shouldSatisfy` isLeft
            
            queueSize <- atomically $ getQueueSize state
            queueSize `shouldBe` 1
        
        it "second player matches with first" $ do
            state <- initPvPState
            
            -- First player joins
            _ <- joinQueue state "p1" "Alice"
            
            -- Second player joins and gets matched
            result <- joinQueue state "p2" "Bob"
            
            result `shouldSatisfy` isRight
            
            queueSize <- atomically $ getQueueSize state
            queueSize `shouldBe` 0
            
            activeCount <- atomically $ getActiveRoomCount state
            activeCount `shouldBe` 1
        
        it "handles concurrent matchmaking correctly" $ do
            state <- initPvPState
            
            -- Simulate 4 players joining concurrently
            results <- mapConcurrently (\i -> 
                joinQueue state ("p" <> show i) ("Player" <> show i)
                ) [1..4 :: Int]
            
            let matched = length $ filter isRight results
            let waiting = length $ filter isLeft results
            
            -- Should have 2 matches and 0 waiting (or 1 match and 2 waiting)
            matched `shouldSatisfy` (>= 1)
            (matched + waiting) `shouldBe` 4
        
        it "player can leave queue" $ do
            state <- initPvPState
            _ <- joinQueue state "p1" "Alice"
            
            queueSize1 <- atomically $ getQueueSize state
            queueSize1 `shouldBe` 1
            
            atomically $ leaveQueue state "p1"
            
            queueSize2 <- atomically $ getQueueSize state
            queueSize2 `shouldBe` 0
    
    describe "Game Management (STM)" $ do
        it "creates and retrieves game" $ do
            state <- initPvPState
            gameState <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            
            atomically $ createGame state "room1" gameState
            
            retrieved <- atomically $ getGame state "room1"
            case retrieved of
                Just game -> roomId game `shouldBe` "room1"
                Nothing -> expectationFailure "Game not found"
        
        it "updates game atomically" $ do
            state <- initPvPState
            gameState <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            
            atomically $ createGame state "room1" gameState
            
            atomically $ updateGame state "room1" $ \g ->
                g { consecutiveHits = 5 }
            
            retrieved <- atomically $ getGame state "room1"
            case retrieved of
                Just game -> consecutiveHits game `shouldBe` 5
                Nothing -> expectationFailure "Game not found"
        
        it "removes game" $ do
            state <- initPvPState
            gameState <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            
            atomically $ createGame state "room1" gameState
            atomically $ removeGame state "room1"
            
            retrieved <- atomically $ getGame state "room1"
            retrieved `shouldBe` Nothing
        
        it "handles concurrent game updates" $ do
            state <- initPvPState
            gameState <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            
            atomically $ createGame state "room1" gameState
            
            -- Multiple concurrent updates
            _ <- mapConcurrently (\i ->
                atomically $ updateGame state "room1" $ \g ->
                    g { consecutiveHits = consecutiveHits g + 1 }
                ) [1..100 :: Int]
            
            retrieved <- atomically $ getGame state "room1"
            case retrieved of
                Just game -> consecutiveHits game `shouldBe` 100
                Nothing -> expectationFailure "Game not found"
    
    describe "Player Management (STM)" $ do
        it "sets player ships atomically" $ do
            state <- initPvPState
            gameState <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            atomically $ createGame state "room1" gameState
            
            let ships = makeValidShips
            result <- atomically $ setPlayerShips state "room1" "p1" ships
            
            result `shouldBe` Right ()
            
            retrieved <- atomically $ getGame state "room1"
            case retrieved of
                Just game -> length (State.GameState.ships (player1 game)) `shouldBe` 5
                Nothing -> expectationFailure "Game not found"
        
        it "sets player ready status" $ do
            state <- initPvPState
            gameState <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            atomically $ createGame state "room1" gameState
            
            atomically $ setPlayerReady state "room1" "p1" True
            
            retrieved <- atomically $ getGame state "room1"
            case retrieved of
                Just game -> isReady (player1 game) `shouldBe` True
                Nothing -> expectationFailure "Game not found"
        
        it "rejects move when not player's turn" $ do
            state <- initPvPState
            gameState <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            atomically $ createGame state "room1" gameState
            
            -- Player 2 tries to move on Player 1's turn
            result <- atomically $ 
                processPlayerMove state "room1" "p2" (Position 0 0)
            
            result `shouldSatisfy` isLeft
    
    describe "Queries" $ do
        it "checks if room is full" $ do
            gameState <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            isRoomFull gameState `shouldBe` True
        
        it "gets active room count" $ do
            state <- initPvPState
            
            gameState1 <- initGameState "room1" "p1" "Alice" "p2" "Bob" 30
            gameState2 <- initGameState "room2" "p3" "Carol" "p4" "Dave" 30
            
            atomically $ do
                createGame state "room1" gameState1
                createGame state "room2" gameState2
            
            count <- atomically $ getActiveRoomCount state
            count `shouldBe` 2

-- Helper functions
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

makeValidShips :: [Ship]
makeValidShips =
    [ Ship Carrier (Position 0 0) Horizontal
    , Ship Battleship (Position 1 0) Horizontal
    , Ship Cruiser (Position 2 0) Horizontal
    , Ship Submarine (Position 3 0) Horizontal
    , Ship Destroyer (Position 4 0) Horizontal
    ]
