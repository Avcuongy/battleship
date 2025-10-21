{-# LANGUAGE OverloadedStrings #-}

module API.Handlers
    ( createRoomHandler
    , joinRoomHandler
    , getRoomHandler
    , startAIHandler
    , processAIAttackHandler
    , savePlayerHandler
    ) where

import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Web.Scotty (ActionM, json, status)
import Network.HTTP.Types.Status (status200, status400, status404, status500)

import API.Types
import qualified State.Manager.Room as RoomMgr
import qualified State.Manager.Player as PlayerMgr
import qualified State.Manager.AI as AIMgr
import qualified Game.Rules as Rules
import qualified Game.AI as AI
import qualified Game.Board as Board
import qualified Storage.Player as PlayerStorage
import qualified Utils.Random as Random
import Game.Types (Fleet, Position)

-- ============================================================================
-- Room Handlers (1vs1)
-- ============================================================================

-- POST /api/rooms/create
createRoomHandler :: CreateRoomRequest -> ActionM ()
createRoomHandler req = do
    -- Generate unique room ID (6 chars a-zA-Z case-sensitive)
    roomId <- liftIO $ RoomMgr.generateUniqueRoomId
    
    -- Create room in STM state (status: "ready")
    result <- liftIO $ atomically $ RoomMgr.createRoom roomId (crPlayerId req) (crPlayerName req)
    
    case result of
        Right _ -> do
            status status200
            json $ CreateRoomResponse
                { crrRoomId = roomId
                , crrStatus = "success"
                }
        Left err -> do
            status status400
            json $ ErrorResponse
                { errStatus = "error"
                , errMessage = err
                }

-- POST /api/rooms/join
joinRoomHandler :: JoinRoomRequest -> ActionM ()
joinRoomHandler req = do
    -- Check if room exists and not full
    result <- liftIO $ atomically $ RoomMgr.joinRoom 
        (jrRoomId req) 
        (jrPlayerId req) 
        (jrPlayerName req)
    
    case result of
        Right _ -> do
            status status200
            json $ JoinRoomResponse
                { jrrStatus = "success"
                , jrrMessage = Nothing
                }
        Left err -> do
            status status400
            json $ JoinRoomResponse
                { jrrStatus = "error"
                , jrrMessage = Just err
                }

-- GET /api/rooms/:id
getRoomHandler :: Text -> ActionM ()
getRoomHandler roomId = do
    -- Get room state from STM
    maybeRoom <- liftIO $ atomically $ RoomMgr.getRoomState roomId
    
    case maybeRoom of
        Just room -> do
            status status200
            json $ GetRoomResponse
                { grrRoomId = RoomMgr.roomId room
                , grrGameMode = "1vs1"
                , grrStatus = RoomMgr.roomStatus room
                , grrPlayer1Id = RoomMgr.player1Id room
                , grrPlayer1Name = RoomMgr.player1Name room
                , grrPlayer1Ready = RoomMgr.player1Ready room
                , grrPlayer2Id = RoomMgr.player2Id room
                , grrPlayer2Name = RoomMgr.player2Name room
                , grrPlayer2Ready = RoomMgr.player2Ready room
                }
        Nothing -> do
            status status404
            json $ ErrorResponse
                { errStatus = "error"
                , errMessage = "Room not found"
                }

-- ============================================================================
-- AI Handlers
-- ============================================================================

-- POST /api/ai/start
startAIHandler :: AIStartRequest -> ActionM ()
startAIHandler req = do
    -- Validate player fleet (single fleet, NO parallel)
    let isValid = Rules.validateFleet (aiFleet req)
    
    if not isValid
        then do
            status status400
            json $ AIStartResponse
                { asrGameId = ""
                , asrStatus = "error"
                , asrMessage = Just "Invalid placement"
                }
        else do
            -- Generate game ID
            gameId <- liftIO $ AIMgr.generateUniqueGameId
            
            -- Create AI session in STM (AI ships: fixed preset)
            liftIO $ atomically $ AIMgr.createAISession 
                gameId 
                (aiPlayerId req)
                (aiPlayerName req)
                (aiFleet req)
            
            status status200
            json $ AIStartResponse
                { asrGameId = gameId
                , asrStatus = "success"
                , asrMessage = Nothing
                }

-- POST /api/ai/attack
processAIAttackHandler :: AIAttackRequest -> ActionM ()
processAIAttackHandler req = do
    -- Get AI session from STM
    maybeSession <- liftIO $ atomically $ AIMgr.getAISession (aaGameId req)
    
    case maybeSession of
        Nothing -> do
            status status404
            json $ ErrorResponse
                { errStatus = "error"
                , errMessage = "Game session not found"
                }
        
        Just session -> do
            -- Process player attack
            let (playerResult, updatedAIBoard) = 
                    Board.processAttack (aaPosition req) (AIMgr.aiBoard session)
            
            -- Check if AI lost
            let aiLost = Board.allShipsSunk updatedAIBoard
            
            if aiLost
                then do
                    -- Player wins, game over
                    liftIO $ atomically $ AIMgr.deleteAISession (aaGameId req)
                    status status200
                    json $ AIAttackResponse
                        { aarPlayerResult = makeAttackResult (aaPosition req) playerResult
                        , aarAiResult = makeAttackResult (Position 0 0) "miss" -- Dummy
                        , aarGameOver = True
                        , aarWinner = Just "player"
                        }
                else do
                    -- AI's turn (instant response, no timer)
                    aiMove <- liftIO $ AI.chooseAIMove (AIMgr.playerBoard session)
                    let (aiResult, updatedPlayerBoard) = 
                            Board.processAttack aiMove (AIMgr.playerBoard session)
                    
                    -- Check if player lost
                    let playerLost = Board.allShipsSunk updatedPlayerBoard
                    
                    -- Update AI session
                    liftIO $ atomically $ AIMgr.updateAISession 
                        (aaGameId req)
                        updatedPlayerBoard
                        updatedAIBoard
                    
                    if playerLost
                        then do
                            -- AI wins, game over
                            liftIO $ atomically $ AIMgr.deleteAISession (aaGameId req)
                            status status200
                            json $ AIAttackResponse
                                { aarPlayerResult = makeAttackResult (aaPosition req) playerResult
                                , aarAiResult = makeAttackResult aiMove aiResult
                                , aarGameOver = True
                                , aarWinner = Just "ai"
                                }
                        else do
                            -- Game continues
                            status status200
                            json $ AIAttackResponse
                                { aarPlayerResult = makeAttackResult (aaPosition req) playerResult
                                , aarAiResult = makeAttackResult aiMove aiResult
                                , aarGameOver = False
                                , aarWinner = Nothing
                                }

-- ============================================================================
-- Player Stats Handler
-- ============================================================================

-- POST /api/players/save
savePlayerHandler :: SavePlayerRequest -> ActionM ()
savePlayerHandler req = do
    -- Async save player stats (non-blocking)
    liftIO $ PlayerStorage.asyncSavePlayer
        (spPlayerId req)
        (spPlayerName req)
        (spGamesPlayed req)
        (spWins req)
        (spLosses req)
    
    status status200
    json $ SavePlayerResponse { sprStatus = "success" }

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Convert Board.Result to API.AttackResult
makeAttackResult :: Position -> Board.Result -> AttackResult
makeAttackResult pos result =
    case result of
        Board.Miss -> AttackResult
            { arPosition = pos
            , arResult = "miss"
            , arShipType = Nothing
            }
        Board.Hit -> AttackResult
            { arPosition = pos
            , arResult = "hit"
            , arShipType = Nothing
            }
        Board.ShipSunk shipType -> AttackResult
            { arPosition = pos
            , arResult = "sunk"
            , arShipType = Just (T.pack $ show shipType)
            }
