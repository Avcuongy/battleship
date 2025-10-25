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
{-# LANGUAGE OverloadedStrings #-}

{-|
Comprehensive test suite for BattleShip game modules.
Tests: Game logic, State management, Board operations, Ship placement.
-}

module Main (main) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Concurrent.STM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

-- Import modules to test
import Game.Types
import Game.Ship
import Game.Board
import Game.Rules
import qualified State.Manager.Room as RoomMgr
import qualified State.Manager.Player as PlayerMgr
import State.Types

-- ============================================================================
-- Main Test Suite
-- ============================================================================

main :: IO ()
main = hspec $ do
    describe "Game.Types" gameTypesSpec
    describe "Game.Ship" gameShipSpec
    describe "Game.Board" gameBoardSpec
    describe "Game.Rules" gameRulesSpec
    describe "State.Manager.Player" playerManagerSpec
    describe "State.Manager.Room" roomManagerSpec

-- ============================================================================
-- Game.Types Tests
-- ============================================================================

gameTypesSpec :: Spec
gameTypesSpec = do
    describe "Position" $ do
        it "creates valid position" $ do
            let pos = Position { posRow = 5, posCol = 5 }
            posRow pos `shouldBe` 5
            posCol pos `shouldBe` 5
        
        it "should be comparable" $ do
            Position { posRow = 0, posCol = 0 } `shouldBe` Position { posRow = 0, posCol = 0 }
            Position { posRow = 1, posCol = 2 } `shouldNotBe` Position { posRow = 2, posCol = 1 }

    describe "ShipType" $ do
        it "has correct ship lengths" $ do
            shipLength Carrier `shouldBe` 5
            shipLength Battleship `shouldBe` 4
            shipLength Cruiser `shouldBe` 3
            shipLength Submarine `shouldBe` 3
            shipLength Destroyer `shouldBe` 2

    describe "Orientation" $ do
        it "distinguishes horizontal and vertical" $ do
            Horizontal `shouldNotBe` Vertical

-- ============================================================================
-- Game.Ship Tests
-- ============================================================================

gameShipSpec :: Spec
gameShipSpec = do
    describe "makeShip" $ do
        it "creates ship with correct length" $ do
            let ship = makeShip Carrier (Position 0 0) Horizontal
            length (shipHits ship) `shouldBe` 5
            all not (shipHits ship) `shouldBe` True
        
        it "creates ship with all positions unhit" $ do
            let ship = makeShip Destroyer (Position 5 5) Vertical
            shipHits ship `shouldBe` [False, False]

    describe "getShipPositions" $ do
        it "calculates horizontal positions correctly" $ do
            let ship = makeShip Destroyer (Position 0 0) Horizontal
            getShipPositions ship `shouldBe` [Position 0 0, Position 0 1]
        
        it "calculates vertical positions correctly" $ do
            let ship = makeShip Destroyer (Position 0 0) Vertical
            getShipPositions ship `shouldBe` [Position 0 0, Position 1 0]
        
        it "handles carrier length 5" $ do
            let ship = makeShip Carrier (Position 2 3) Horizontal
            length (getShipPositions ship) `shouldBe` 5

    describe "hitShip" $ do
        it "registers hit at correct position" $ do
            let ship = makeShip Destroyer (Position 0 0) Horizontal
                result = hitShip (Position 0 0) ship
            case result of
                Nothing -> expectationFailure "Should hit ship"
                Just hitShip' -> head (shipHits hitShip') `shouldBe` True
        
        it "returns Nothing for miss" $ do
            let ship = makeShip Destroyer (Position 0 0) Horizontal
                result = hitShip (Position 5 5) ship
            result `shouldBe` Nothing
        
        it "tracks multiple hits" $ do
            let ship = makeShip Destroyer (Position 0 0) Horizontal
                Just ship1 = hitShip (Position 0 0) ship
                Just ship2 = hitShip (Position 0 1) ship1
            shipHits ship2 `shouldBe` [True, True]

    describe "isSunk" $ do
        it "returns False for new ship" $ do
            let ship = makeShip Carrier (Position 0 0) Horizontal
            isSunk ship `shouldBe` False
        
        it "returns True when all positions hit" $ do
            let ship = makeShip Destroyer (Position 0 0) Horizontal
                Just ship1 = hitShip (Position 0 0) ship
                Just ship2 = hitShip (Position 0 1) ship1
            isSunk ship2 `shouldBe` True
        
        it "returns False with partial hits" $ do
            let ship = makeShip Carrier (Position 0 0) Horizontal
                Just ship1 = hitShip (Position 0 0) ship
            isSunk ship1 `shouldBe` False

    describe "makeFleet" $ do
        it "creates standard fleet with 5 ships" $ do
            let fleet = makeFleet
                    [ (Carrier, Position 0 0, Horizontal)
                    , (Battleship, Position 1 0, Horizontal)
                    , (Cruiser, Position 2 0, Horizontal)
                    , (Submarine, Position 3 0, Horizontal)
                    , (Destroyer, Position 4 0, Horizontal)
                    ]
            length fleet `shouldBe` 5

-- ============================================================================
-- Game.Board Tests
-- ============================================================================

gameBoardSpec :: Spec
gameBoardSpec = do
    describe "emptyBoard" $ do
        it "creates 10x10 grid of Nothing" $ do
            let board = emptyBoard
            length board `shouldBe` 10
            all (\row -> length row == 10) board `shouldBe` True
            all (all (== Nothing)) board `shouldBe` True

    describe "processAttack" $ do
        it "returns Miss for empty position" $ do
            let board = emptyBoard
                (result, _) = processAttack (Position 0 0) board
            result `shouldBe` ResultMiss
        
        it "returns Hit for ship position" $ do
            let ship = makeShip Destroyer (Position 0 0) Horizontal
                board = placeShipOnBoard ship emptyBoard
                (result, _) = processAttack (Position 0 0) board
            result `shouldBe` ResultHit
        
        it "updates board after hit" $ do
            let ship = makeShip Destroyer (Position 0 0) Horizontal
                board = placeShipOnBoard ship emptyBoard
                (_, newBoard) = processAttack (Position 0 0) board
            -- Board should be updated
            newBoard `shouldNotBe` board

    describe "allShipsSunk" $ do
        it "returns False for new board with ships" $ do
            let ship = makeShip Destroyer (Position 0 0) Horizontal
                board = placeShipOnBoard ship emptyBoard
            allShipsSunk board `shouldBe` False
        
        it "returns True when all ships destroyed" $ do
            let ship = makeShip Destroyer (Position 0 0) Horizontal
                board = placeShipOnBoard ship emptyBoard
                (_, board1) = processAttack (Position 0 0) board
                (_, board2) = processAttack (Position 0 1) board1
            allShipsSunk board2 `shouldBe` True

-- Helper function for board tests
placeShipOnBoard :: Ship -> Board -> Board
placeShipOnBoard ship board =
    foldr placePosition board (zip [0..] (getShipPositions ship))
  where
    placePosition (idx, Position r c) b =
        updateCell b r c (Just ship)
    
    updateCell b r c val =
        let (before, row:after) = splitAt r b
            (beforeCell, _:afterCell) = splitAt c row
            newRow = beforeCell ++ [val] ++ afterCell
        in before ++ [newRow] ++ after

-- ============================================================================
-- Game.Rules Tests
-- ============================================================================

gameRulesSpec :: Spec
gameRulesSpec = do
    describe "validateFleet" $ do
        it "accepts valid non-overlapping fleet" $ do
            let fleet = makeFleet
                    [ (Carrier, Position {posRow=0, posCol=0}, Horizontal)
                    , (Battleship, Position {posRow=1, posCol=0}, Horizontal)
                    , (Cruiser, Position {posRow=2, posCol=0}, Horizontal)
                    , (Submarine, Position {posRow=3, posCol=0}, Horizontal)
                    , (Destroyer, Position {posRow=4, posCol=0}, Horizontal)
                    ]
            Game.Rules.validateFleet fleet `shouldBe` True
        
        it "rejects fleet with ships out of bounds" $ do
            let fleet = makeFleet
                    [ (Carrier, Position {posRow=0, posCol=6}, Horizontal) -- Goes to col 10 (out of bounds)
                    ]
            Game.Rules.validateFleet fleet `shouldBe` False
        
        it "rejects overlapping ships" $ do
            let fleet = makeFleet
                    [ (Carrier, Position {posRow=0, posCol=0}, Horizontal)
                    , (Battleship, Position {posRow=0, posCol=2}, Horizontal) -- Overlaps with Carrier
                    ]
            Game.Rules.validateFleet fleet `shouldBe` False
    
    describe "correctShipCount" $ do
        it "accepts fleet with exactly 5 ships" $ do
            let fleet = makeFleet
                    [ (Carrier, Position {posRow=0, posCol=0}, Horizontal)
                    , (Battleship, Position {posRow=1, posCol=0}, Horizontal)
                    , (Cruiser, Position {posRow=2, posCol=0}, Horizontal)
                    , (Submarine, Position {posRow=3, posCol=0}, Horizontal)
                    , (Destroyer, Position {posRow=4, posCol=0}, Horizontal)
                    ]
            Game.Rules.correctShipCount fleet `shouldBe` True
        
        it "rejects fleet with wrong number of ships" $ do
            let fleet = makeFleet
                    [ (Carrier, Position {posRow=0, posCol=0}, Horizontal)
                    , (Battleship, Position {posRow=1, posCol=0}, Horizontal)
                    ]
            Game.Rules.correctShipCount fleet `shouldBe` False

-- ============================================================================
-- State.Manager.Player Tests
-- ============================================================================

playerManagerSpec :: Spec
playerManagerSpec = do
    describe "PlayerManager" $ do
        it "creates new player manager" $ do
            mgr <- runIO $ PlayerMgr.newPlayerManager
            -- Just verify manager can be created without error
            mgr `shouldNotBe` undefined
        
        it "generates unique player IDs" $ do
            mgr <- runIO $ PlayerMgr.newPlayerManager
            id1 <- runIO $ PlayerMgr.generateUniquePlayerId mgr
            id2 <- runIO $ PlayerMgr.generateUniquePlayerId mgr
            id1 `shouldNotBe` id2
            T.length id1 `shouldBe` 6
            T.length id2 `shouldBe` 6
        
        it "adds and retrieves player" $ do
            mgr <- runIO $ PlayerMgr.newPlayerManager
            let playerId = "test01"
                playerName = "TestPlayer"
            runIO $ PlayerMgr.addPlayer mgr playerId playerName
            maybePlayer <- runIO $ PlayerMgr.getPlayer mgr playerId
            case maybePlayer of
                Nothing -> expectationFailure "Player not found"
                Just player -> do
                    State.Types.playerId player `shouldBe` playerId
                    State.Types.playerName player `shouldBe` playerName
        
        it "removes player" $ do
            mgr <- runIO $ PlayerMgr.newPlayerManager
            let playerId = "test02"
            runIO $ PlayerMgr.addPlayer mgr playerId "TestPlayer"
            runIO $ PlayerMgr.removePlayer mgr playerId
            maybePlayer <- runIO $ PlayerMgr.getPlayer mgr playerId
            maybePlayer `shouldBe` Nothing
        
        it "checks player existence" $ do
            mgr <- runIO $ PlayerMgr.newPlayerManager
            let playerId = "test03"
            exists1 <- runIO $ PlayerMgr.playerExists mgr playerId
            exists1 `shouldBe` False
            runIO $ PlayerMgr.addPlayer mgr playerId "TestPlayer"
            exists2 <- runIO $ PlayerMgr.playerExists mgr playerId
            exists2 `shouldBe` True

-- ============================================================================
-- State.Manager.Room Tests
-- ============================================================================

roomManagerSpec :: Spec
roomManagerSpec = do
    describe "RoomManager" $ do
        it "creates new room manager" $ do
            mgr <- runIO $ RoomMgr.newRoomManager
            -- Just verify manager can be created without error
            mgr `shouldNotBe` undefined
        
        it "generates unique room IDs" $ do
            mgr <- runIO $ RoomMgr.newRoomManager
            id1 <- runIO $ RoomMgr.generateUniqueRoomId
            id2 <- runIO $ RoomMgr.generateUniqueRoomId
            id1 `shouldNotBe` id2
            T.length id1 `shouldBe` 6
            T.length id2 `shouldBe` 6
        
        it "creates room with first player" $ do
            mgr <- runIO $ RoomMgr.newRoomManager
            let roomId = "room01"
                playerId = "player01"
                playerName = "Player1"
            result <- runIO $ RoomMgr.createRoom mgr roomId playerId playerName
            case result of
                Left err -> expectationFailure $ "Create room failed: " ++ T.unpack err
                Right room -> do
                    State.Types.roomId room `shouldBe` roomId
                    State.Types.player1Id room `shouldBe` playerId
                    State.Types.player1Name room `shouldBe` playerName
                    State.Types.player2Id room `shouldBe` Nothing
        
        it "allows second player to join" $ do
            mgr <- runIO $ RoomMgr.newRoomManager
            let roomId = "room02"
            _ <- runIO $ RoomMgr.createRoom mgr roomId "player01" "Player1"
            result <- runIO $ RoomMgr.joinRoom mgr roomId "player02" "Player2"
            case result of
                Left err -> expectationFailure $ "Join room failed: " ++ T.unpack err
                Right room -> do
                    State.Types.player2Id room `shouldBe` Just "player02"
                    State.Types.player2Name room `shouldBe` Just "Player2"
        
        it "retrieves room state" $ do
            mgr <- runIO $ RoomMgr.newRoomManager
            let roomId = "room03"
            _ <- runIO $ RoomMgr.createRoom mgr roomId "player01" "Player1"
            maybeRoom <- runIO $ RoomMgr.getRoomState mgr roomId
            case maybeRoom of
                Nothing -> expectationFailure "Room not found"
                Just room -> State.Types.roomId room `shouldBe` roomId
        
        it "deletes room" $ do
            mgr <- runIO $ RoomMgr.newRoomManager
            let roomId = "room04"
            _ <- runIO $ RoomMgr.createRoom mgr roomId "player01" "Player1"
            runIO $ RoomMgr.deleteRoom mgr roomId
            maybeRoom <- runIO $ RoomMgr.getRoomState mgr roomId
            maybeRoom `shouldBe` Nothing
        
        it "marks player as ready" $ do
            mgr <- runIO $ RoomMgr.newRoomManager
            let roomId = "room05"
                playerId = "player01"
                fleet = makeFleet [(Destroyer, Position 0 0, Horizontal)]
            _ <- runIO $ RoomMgr.createRoom mgr roomId playerId "Player1"
            result <- runIO $ RoomMgr.markPlayerReady mgr roomId playerId fleet
            case result of
                Left err -> expectationFailure $ "Mark ready failed: " ++ T.unpack err
                Right _ -> do
                    maybeRoom <- runIO $ RoomMgr.getRoomState mgr roomId
                    case maybeRoom of
                        Nothing -> expectationFailure "Room not found"
                        Just room -> State.Types.player1Ready room `shouldBe` True
        
        it "checks both players ready" $ do
            mgr <- runIO $ RoomMgr.newRoomManager
            let roomId = "room06"
                fleet = makeFleet [(Destroyer, Position 0 0, Horizontal)]
            _ <- runIO $ RoomMgr.createRoom mgr roomId "player01" "Player1"
            _ <- runIO $ RoomMgr.joinRoom mgr roomId "player02" "Player2"
            
            ready1 <- runIO $ RoomMgr.checkBothReady mgr roomId
            ready1 `shouldBe` False
            
            _ <- runIO $ RoomMgr.markPlayerReady mgr roomId "player01" fleet
            ready2 <- runIO $ RoomMgr.checkBothReady mgr roomId
            ready2 `shouldBe` False
            
            _ <- runIO $ RoomMgr.markPlayerReady mgr roomId "player02" fleet
            ready3 <- runIO $ RoomMgr.checkBothReady mgr roomId
            ready3 `shouldBe` True
        
        it "sets room status" $ do
            mgr <- runIO $ RoomMgr.newRoomManager
            let roomId = "room07"
            _ <- runIO $ RoomMgr.createRoom mgr roomId "player01" "Player1"
            runIO $ RoomMgr.setRoomStatus mgr roomId InProgress
            maybeRoom <- runIO $ RoomMgr.getRoomState mgr roomId
            case maybeRoom of
                Nothing -> expectationFailure "Room not found"
                Just room -> State.Types.status room `shouldBe` InProgress

-- ============================================================================
-- Property-based Tests (QuickCheck)
-- ============================================================================

instance Arbitrary Position where
    arbitrary = Position <$> choose (0, 9) <*> choose (0, 9)

instance Arbitrary ShipType where
    arbitrary = elements [Carrier, Battleship, Cruiser, Submarine, Destroyer]

instance Arbitrary Orientation where
    arbitrary = elements [Horizontal, Vertical]
