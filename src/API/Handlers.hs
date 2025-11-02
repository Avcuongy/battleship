{-# LANGUAGE OverloadedStrings #-}

module API.Handlers
    ( createRoomHandler
    , joinRoomHandler
    , getRoomHandler
    , getActiveRoomHandler
    , startAIHandler
    , processAIAttackHandler
    , savePlayerHandler
    , generatePlayerIdHandler
    ) where

import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (object, (.=), Value(Null))
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
import Game.Types (Fleet, Position(..), Result(..))
import qualified State.Types as ST

-- ============================================================================
-- Room Handlers (1vs1)
-- ============================================================================

-- POST /api/rooms/create
createRoomHandler :: RoomMgr.RoomManager -> CreateRoomRequest -> ActionM ()
createRoomHandler roomMgr req = do
    -- Generate unique room ID (6 chars a-zA-Z case-sensitive)
    roomId <- liftIO $ RoomMgr.generateUniqueRoomId
    
    -- Create room in STM state (status: "ready")
    result <- liftIO $ RoomMgr.createRoom roomMgr roomId (crPlayerId req) (crPlayerName req)
    
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
joinRoomHandler :: RoomMgr.RoomManager -> JoinRoomRequest -> ActionM ()
joinRoomHandler roomMgr req = do
    -- Check if room exists and not full
    result <- liftIO $ RoomMgr.joinRoom 
        roomMgr
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
getRoomHandler :: RoomMgr.RoomManager -> Text -> ActionM ()
getRoomHandler roomMgr roomId = do
    -- Get room state from STM
    maybeRoom <- liftIO $ RoomMgr.getRoomState roomMgr roomId
    
    case maybeRoom of
        Just room -> do
            status status200
            json $ GetRoomResponse
                { grrRoomId = ST.roomId room
                , grrGameMode = "1vs1"
                , grrStatus = T.pack $ show (ST.status room)
                , grrPlayer1Id = ST.player1Id room
                , grrPlayer1Name = ST.player1Name room
                , grrPlayer1Ready = ST.player1Ready room
                , grrPlayer2Id = ST.player2Id room
                , grrPlayer2Name = ST.player2Name room
                , grrPlayer2Ready = Just (ST.player2Ready room)
                }

-- GET /api/rooms/active
getActiveRoomHandler :: RoomMgr.RoomManager -> ActionM ()
getActiveRoomHandler roomMgr = do
    rid <- liftIO $ RoomMgr.activeRoomId roomMgr
    status status200
    json $ object
        [ "garStatus" .= (case rid of Just _ -> ("active" :: Text); Nothing -> "none")
        , "garRoomId" .= rid
        ]

-- ============================================================================
-- AI Handlers
-- ============================================================================

-- POST /api/ai/start
startAIHandler :: AIMgr.AIManager -> AIStartRequest -> ActionM ()
startAIHandler aiMgr req = do
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
            liftIO $ AIMgr.createAISession 
                aiMgr
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
processAIAttackHandler :: AIMgr.AIManager -> AIAttackRequest -> ActionM ()
processAIAttackHandler aiMgr req = do
    -- Get AI session from STM
    maybeSession <- liftIO $ AIMgr.getAISession aiMgr (aaGameId req)
    
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
                    liftIO $ AIMgr.deleteAISession aiMgr (aaGameId req)
                    status status200
                    json $ AIAttackResponse
                        { aarPlayerResult = makeAttackResult (aaPosition req) playerResult
                        , aarAiResult = makeAttackResult (Position { posRow = 0, posCol = 0 }) ResultMiss
                        , aarGameOver = True
                        , aarWinner = Just "player"
                        }
                else do
                    -- Check if player hit or missed
                    case playerResult of
                        -- Player MISSED - AI's turn
                        ResultMiss -> do
                            aiMove <- liftIO $ AI.chooseAIMove (AIMgr.playerBoard session)
                            let (aiResult, updatedPlayerBoard) = 
                                    Board.processAttack aiMove (AIMgr.playerBoard session)
                            
                            -- Check if player lost
                            let playerLost = Board.allShipsSunk updatedPlayerBoard
                            
                            -- Update AI session
                            liftIO $ AIMgr.updateAISession 
                                aiMgr
                                (aaGameId req)
                                updatedPlayerBoard
                                updatedAIBoard
                            
                            if playerLost
                                then do
                                    -- AI wins, game over
                                    liftIO $ AIMgr.deleteAISession aiMgr (aaGameId req)
                                    status status200
                                    json $ AIAttackResponse
                                        { aarPlayerResult = makeAttackResult (aaPosition req) playerResult
                                        , aarAiResult = makeAttackResult aiMove aiResult
                                        , aarGameOver = True
                                        , aarWinner = Just "ai"
                                        }
                                else do
                                    -- Game continues, AI attacked
                                    status status200
                                    json $ AIAttackResponse
                                        { aarPlayerResult = makeAttackResult (aaPosition req) playerResult
                                        , aarAiResult = makeAttackResult aiMove aiResult
                                        , aarGameOver = False
                                        , aarWinner = Nothing
                                        }
                        
                        -- Player HIT or SUNK - player continues (no AI attack)
                        _ -> do
                            -- Update AI session (only AI board changed)
                            liftIO $ AIMgr.updateAISession 
                                aiMgr
                                (aaGameId req)
                                (AIMgr.playerBoard session)  -- Player board unchanged
                                updatedAIBoard
                            
                            -- Game continues, player's turn again (no AI result)
                            status status200
                            json $ object
                                [ "aarPlayerResult" .= makeAttackResult (aaPosition req) playerResult
                                , "aarAiResult" .= Null  -- No AI attack
                                , "aarGameOver" .= False
                                , "aarWinner" .= Null
                                ]

-- ============================================================================
-- Player Stats Handler
-- ============================================================================

-- POST /api/players/save
savePlayerHandler :: SavePlayerRequest -> ActionM ()
savePlayerHandler req = do
    -- Async save player stats (non-blocking)
    liftIO $ PlayerStorage.asyncSavePlayerFromRequest
        (spPlayerId req)
        (spPlayerName req)
        (spGamesPlayed req)
        (spWins req)
        (spLosses req)
    
    status status200
    json $ SavePlayerResponse { sprStatus = "success" }

-- GET /api/players/generate-id
generatePlayerIdHandler :: PlayerMgr.PlayerManager -> ActionM ()
generatePlayerIdHandler playerMgr = do
    -- Generate unique player ID using backend logic
    newPlayerId <- liftIO $ PlayerMgr.generateUniquePlayerId playerMgr
    
    status status200
    json $ GeneratePlayerIdResponse { gpiPlayerId = newPlayerId }

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Convert Game.Types.Result to API.AttackResult
makeAttackResult :: Position -> Result -> AttackResult
makeAttackResult pos result =
    case result of
        ResultMiss -> AttackResult
            { arPosition = pos
            , arResult = "miss"
            , arShipType = Nothing
            }
        ResultHit -> AttackResult
            { arPosition = pos
            , arResult = "hit"
            , arShipType = Nothing
            }
        ResultShipSunk shipType -> AttackResult
            { arPosition = pos
            , arResult = "sunk"
            , arShipType = Just (T.pack $ show shipType)
            }
