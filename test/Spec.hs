{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import qualified Data.Text as T

-- Import game modules
import Game.Types
import Game.Ship
import Game.Board
import qualified Game.Rules
import qualified State.Manager.Player as PlayerMgr
import qualified State.Manager.Room as RoomMgr
import qualified State.Types as ST

main :: IO ()
main = hspec $ do
    gameTypesSpec
    gameShipSpec
    gameBoardSpec
    gameRulesSpec
    playerManagerSpec
    roomManagerSpec

-- ============================================================================
-- Game.Types Tests
-- ============================================================================

gameTypesSpec :: Spec
gameTypesSpec = describe "Game.Types" $ do
    describe "Position" $ do
        it "creates position with row and col" $ do
            let pos = Position {posRow = 5, posCol = 5}
            posRow pos `shouldBe` 5
            posCol pos `shouldBe` 5
        
        it "compares positions for equality" $ do
            let pos1 = Position {posRow = 0, posCol = 0}
                pos2 = Position {posRow = 0, posCol = 0}
                pos3 = Position {posRow = 1, posCol = 1}
            pos1 `shouldBe` pos2
            pos1 `shouldNotBe` pos3

    describe "Orientation" $ do
        it "distinguishes horizontal and vertical" $ do
            Horizontal `shouldNotBe` Vertical

-- ============================================================================
-- Game.Ship Tests
-- ============================================================================

gameShipSpec :: Spec
gameShipSpec = describe "Game.Ship" $ do
    describe "makeShip" $ do
        it "creates ship with correct type" $ do
            let ship = makeShip Carrier (Position {posRow=0, posCol=0}) Horizontal
            shipType ship `shouldBe` Carrier
        
        it "initializes ship with no hits" $ do
            let ship = makeShip Destroyer (Position {posRow=5, posCol=5}) Vertical
            isSunk ship `shouldBe` False

    describe "getShipPositions" $ do
        it "generates correct positions for horizontal ship" $ do
            let ship = makeShip Destroyer (Position {posRow=0, posCol=0}) Horizontal
                positions = getShipPositions ship
            length positions `shouldBe` 2
            head positions `shouldBe` Position {posRow=0, posCol=0}
            positions !! 1 `shouldBe` Position {posRow=0, posCol=1}
        
        it "generates correct positions for vertical ship" $ do
            let ship = makeShip Destroyer (Position {posRow=0, posCol=0}) Vertical
                positions = getShipPositions ship
            length positions `shouldBe` 2
            head positions `shouldBe` Position {posRow=0, posCol=0}
            positions !! 1 `shouldBe` Position {posRow=1, posCol=0}
        
        it "generates 5 positions for Carrier" $ do
            let ship = makeShip Carrier (Position {posRow=2, posCol=3}) Horizontal
            length (getShipPositions ship) `shouldBe` 5
    
    describe "isSunk" $ do
        it "returns False for new ship" $ do
            let ship = makeShip Carrier (Position {posRow=0, posCol=0}) Horizontal
            isSunk ship `shouldBe` False

    describe "makeFleet" $ do
        it "creates fleet with specified ships" $ do
            let fleet = makeFleet
                    [ (Carrier, Position {posRow=0, posCol=0}, Horizontal)
                    , (Battleship, Position {posRow=1, posCol=0}, Horizontal)
                    , (Cruiser, Position {posRow=2, posCol=0}, Horizontal)
                    , (Submarine, Position {posRow=3, posCol=0}, Horizontal)
                    , (Destroyer, Position {posRow=4, posCol=0}, Horizontal)
                    ]
            length fleet `shouldBe` 5

-- ============================================================================
-- Game.Board Tests
-- ============================================================================

gameBoardSpec :: Spec
gameBoardSpec = describe "Game.Board" $ do
    describe "emptyBoard" $ do
        it "creates a board" $ do
            let board = emptyBoard
            board `shouldSatisfy` (\_ -> True)
    
    describe "allShipsSunk" $ do
        it "returns True for empty board" $ do
            let board = emptyBoard
            allShipsSunk board `shouldBe` True

-- ============================================================================
-- Game.Rules Tests
-- ============================================================================

gameRulesSpec :: Spec
gameRulesSpec = describe "Game.Rules" $ do
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
                    [ (Carrier, Position {posRow=0, posCol=6}, Horizontal)
                    ]
            Game.Rules.validateFleet fleet `shouldBe` False
        
        it "rejects overlapping ships" $ do
            let fleet = makeFleet
                    [ (Carrier, Position {posRow=0, posCol=0}, Horizontal)
                    , (Battleship, Position {posRow=0, posCol=2}, Horizontal)
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
playerManagerSpec = describe "State.Manager.Player" $ do
    describe "newPlayerManager" $ do
        it "creates new player manager" $ do
            mgr <- PlayerMgr.newPlayerManager
            -- Just verify it doesn't throw an error
            return ()

    describe "generateUniquePlayerId" $ do
        it "generates unique player IDs" $ do
            mgr <- PlayerMgr.newPlayerManager
            id1 <- PlayerMgr.generateUniquePlayerId mgr
            id2 <- PlayerMgr.generateUniquePlayerId mgr
            id1 `shouldNotBe` id2
            T.length id1 `shouldBe` 6
            T.length id2 `shouldBe` 6

    describe "addPlayer and getPlayer" $ do
        it "stores and retrieves player" $ do
            mgr <- PlayerMgr.newPlayerManager
            let playerId = "test01"
                playerName = "TestPlayer"
            PlayerMgr.addPlayer mgr playerId playerName
            maybePlayer <- PlayerMgr.getPlayer mgr playerId
            case maybePlayer of
                Nothing -> expectationFailure "Player not found"
                Just player -> do
                    ST.playerId player `shouldBe` playerId
                    ST.playerName player `shouldBe` playerName

    describe "removePlayer" $ do
        it "removes player successfully" $ do
            mgr <- PlayerMgr.newPlayerManager
            let playerId = "test02"
            PlayerMgr.addPlayer mgr playerId "TestPlayer"
            PlayerMgr.removePlayer mgr playerId
            maybePlayer <- PlayerMgr.getPlayer mgr playerId
            case maybePlayer of
                Nothing -> return ()
                Just _ -> expectationFailure "Player should be removed"

    describe "playerExists" $ do
        it "checks player existence correctly" $ do
            mgr <- PlayerMgr.newPlayerManager
            let playerId = "test03"
            exists1 <- PlayerMgr.playerExists mgr playerId
            exists1 `shouldBe` False
            PlayerMgr.addPlayer mgr playerId "TestPlayer"
            exists2 <- PlayerMgr.playerExists mgr playerId
            exists2 `shouldBe` True

-- ============================================================================
-- State.Manager.Room Tests
-- ============================================================================

roomManagerSpec :: Spec
roomManagerSpec = describe "State.Manager.Room" $ do
    describe "newRoomManager" $ do
        it "creates new room manager" $ do
            mgr <- RoomMgr.newRoomManager
            -- Just verify it doesn't throw an error
            return ()

    describe "generateUniqueRoomId" $ do
        it "generates unique room IDs" $ do
            id1 <- RoomMgr.generateUniqueRoomId
            id2 <- RoomMgr.generateUniqueRoomId
            id1 `shouldNotBe` id2
            T.length id1 `shouldBe` 6
            T.length id2 `shouldBe` 6

    describe "createRoom" $ do
        it "creates a new room" $ do
            mgr <- RoomMgr.newRoomManager
            let roomId = "room01"
                playerId = "player01"
                playerName = "Player1"
            result <- RoomMgr.createRoom mgr roomId playerId playerName
            case result of
                Left err -> expectationFailure $ "Create room failed: " ++ T.unpack err
                Right room -> do
                    ST.roomId room `shouldBe` roomId
                    ST.player1Id room `shouldBe` playerId
                    ST.player1Name room `shouldBe` playerName
                    ST.player2Id room `shouldBe` Nothing

    describe "joinRoom" $ do
        it "allows second player to join" $ do
            mgr <- RoomMgr.newRoomManager
            let roomId = "room02"
            _ <- RoomMgr.createRoom mgr roomId "player01" "Player1"
            result <- RoomMgr.joinRoom mgr roomId "player02" "Player2"
            case result of
                Left err -> expectationFailure $ "Join room failed: " ++ T.unpack err
                Right _ -> do
                    maybeRoom <- RoomMgr.getRoomState mgr roomId
                    case maybeRoom of
                        Nothing -> expectationFailure "Room not found after join"
                        Just room -> do
                            ST.player2Id room `shouldBe` Just "player02"
                            ST.player2Name room `shouldBe` Just "Player2"

    describe "getRoomState" $ do
        it "retrieves room state" $ do
            mgr <- RoomMgr.newRoomManager
            let roomId = "room03"
            _ <- RoomMgr.createRoom mgr roomId "player01" "Player1"
            maybeRoom <- RoomMgr.getRoomState mgr roomId
            case maybeRoom of
                Nothing -> expectationFailure "Room not found"
                Just room -> ST.roomId room `shouldBe` roomId

    describe "deleteRoom" $ do
        it "deletes room successfully" $ do
            mgr <- RoomMgr.newRoomManager
            let roomId = "room04"
            _ <- RoomMgr.createRoom mgr roomId "player01" "Player1"
            RoomMgr.deleteRoom mgr roomId
            maybeRoom <- RoomMgr.getRoomState mgr roomId
            case maybeRoom of
                Nothing -> return ()
                Just _ -> expectationFailure "Room should be deleted"

    describe "markPlayerReady" $ do
        it "marks player as ready with fleet" $ do
            mgr <- RoomMgr.newRoomManager
            let roomId = "room05"
                playerId = "player01"
                fleet = makeFleet [(Destroyer, Position {posRow=0, posCol=0}, Horizontal)]
            _ <- RoomMgr.createRoom mgr roomId playerId "Player1"
            result <- RoomMgr.markPlayerReady mgr roomId playerId fleet
            case result of
                Left err -> expectationFailure $ "Mark ready failed: " ++ T.unpack err
                Right _ -> do
                    maybeRoom <- RoomMgr.getRoomState mgr roomId
                    case maybeRoom of
                        Nothing -> expectationFailure "Room not found"
                        Just room -> ST.player1Ready room `shouldBe` True

    describe "checkBothReady" $ do
        it "checks both players ready status" $ do
            mgr <- RoomMgr.newRoomManager
            let roomId = "room06"
                fleet = makeFleet [(Destroyer, Position {posRow=0, posCol=0}, Horizontal)]
            _ <- RoomMgr.createRoom mgr roomId "player01" "Player1"
            _ <- RoomMgr.joinRoom mgr roomId "player02" "Player2"
            
            ready1 <- RoomMgr.checkBothReady mgr roomId
            ready1 `shouldBe` False
            
            _ <- RoomMgr.markPlayerReady mgr roomId "player01" fleet
            ready2 <- RoomMgr.checkBothReady mgr roomId
            ready2 `shouldBe` False
            
            _ <- RoomMgr.markPlayerReady mgr roomId "player02" fleet
            ready3 <- RoomMgr.checkBothReady mgr roomId
            ready3 `shouldBe` True
