{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (newTVarIO, atomically, readTVar)
import Control.Exception (bracket, catch, SomeException)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BL

-- Import project modules
import qualified State.Manager.Room as RoomMgr
import qualified State.Manager.Player as PlayerMgr
import qualified Network.WebSocket.Server as WSServer
import Network.WebSocket.Types (WebSocketState(..))
import Network.Protocol
import Game.Types (Position(..), ShipType(..), Orientation(..), Ship(..))

-- ============================================================================
-- Test Configuration
-- ============================================================================

testHost :: String
testHost = "127.0.0.1"

testPort :: Int
testPort = 9160

wsUrl :: String -> String -> String
wsUrl roomId playerId = 
    "ws://" ++ testHost ++ ":" ++ show testPort 
    ++ "?roomId=" ++ roomId ++ "&playerId=" ++ playerId

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Create test fleet (valid 5 ships)
createTestFleet :: [Ship]
createTestFleet =
    [ Ship Carrier (Position 0 0) Horizontal [False,False,False,False,False]
    , Ship Battleship (Position 1 0) Horizontal [False,False,False,False]
    , Ship Cruiser (Position 2 0) Horizontal [False,False,False]
    , Ship Submarine (Position 3 0) Horizontal [False,False,False]
    , Ship Destroyer (Position 4 0) Horizontal [False,False]
    ]

-- | Start test WebSocket server
startTestServer :: IO WebSocketState
startTestServer = do
    roomMgr <- RoomMgr.newRoomManager
    playerMgr <- PlayerMgr.newPlayerManager
    wsConnections <- newTVarIO Map.empty
    
    let state = WebSocketState
            { wsRoomManager = roomMgr
            , wsPlayerManager = playerMgr
            , wsConnections = wsConnections
            }
    
    -- Start server in background thread
    _ <- forkIO $ WSServer.startWebSocketServer state
    threadDelay 500000  -- Wait 0.5s for server to start
    
    return state

-- | Connect WebSocket client
connectClient :: String -> String -> IO WS.Connection
connectClient roomId playerId = do
    let url = wsUrl roomId playerId
    WS.runClient testHost testPort 
        ("/?roomId=" ++ roomId ++ "&playerId=" ++ playerId) 
        return

-- | Send and receive message
sendAndReceive :: WS.Connection -> ClientMessage -> IO (Maybe ServerMessage)
sendAndReceive conn msg = do
    WS.sendTextData conn (encode msg)
    threadDelay 100000  -- Wait 0.1s for response
    
    -- Try to receive (with timeout)
    result <- (Just <$> WS.receiveData conn) 
              `catch` (\(_ :: SomeException) -> return Nothing)
    
    case result of
        Nothing -> return Nothing
        Just bs -> return (decode bs :: Maybe ServerMessage)

-- ============================================================================
-- Main Test Suite
-- ============================================================================

main :: IO ()
main = hspec $ do
    describe "WebSocket Server Tests" $ do
        
        -- ====================================================================
        -- Connection Tests
        -- ====================================================================
        
        describe "Connection Management" $ do
            it "should accept valid WebSocket connection" $ do
                state <- startTestServer
                
                result <- (do
                    conn <- connectClient "ROOM01" "PLAY01"
                    WS.sendClose conn ("Test done" :: T.Text)
                    return True
                    ) `catch` (\(_ :: SomeException) -> return False)
                
                result `shouldBe` True
            
            it "should handle multiple connections" $ do
                state <- startTestServer
                
                result <- (do
                    conn1 <- connectClient "ROOM01" "PLAY01"
                    conn2 <- connectClient "ROOM01" "PLAY02"
                    
                    WS.sendClose conn1 ("Done" :: T.Text)
                    WS.sendClose conn2 ("Done" :: T.Text)
                    return True
                    ) `catch` (\(_ :: SomeException) -> return False)
                
                result `shouldBe` True
        
        -- ====================================================================
        -- Message Protocol Tests
        -- ====================================================================
        
        describe "Message Protocol" $ do
            it "should encode/decode ReadyMessage correctly" $ do
                let fleet = createTestFleet
                let msg = ReadyMsg $ ReadyMessage "PLAY01" fleet
                let encoded = encode msg
                let decoded = decode encoded :: Maybe ClientMessage
                
                decoded `shouldBe` Just msg
            
            it "should encode/decode AttackMessage correctly" $ do
                let pos = Position 3 5
                let msg = AttackMsg $ AttackMessage "PLAY01" pos
                let encoded = encode msg
                let decoded = decode encoded :: Maybe ClientMessage
                
                decoded `shouldBe` Just msg
            
            it "should encode/decode AttackResultMessage correctly" $ do
                let msg = AttackResultMsg $ AttackResultMessage
                        { armAttacker = "PLAY01"
                        , armPosition = Position 2 3
                        , armResult = "hit"
                        , armShipType = Nothing
                        , armNextTurn = "PLAY01"
                        }
                let encoded = encode msg
                let decoded = decode encoded :: Maybe ServerMessage
                
                decoded `shouldBe` Just msg
        
        -- ====================================================================
        -- Game Flow Tests
        -- ====================================================================
        
        describe "Game Flow" $ do
            it "should send ready message successfully" $ do
                state <- startTestServer
                
                -- Create room first
                roomId <- RoomMgr.generateUniqueRoomId
                _ <- RoomMgr.createRoom (wsRoomManager state) roomId "PLAY01" "Player1"
                
                result <- (do
                    conn <- connectClient (T.unpack roomId) "PLAY01"
                    let fleet = createTestFleet
                    let readyMsg = ReadyMsg $ ReadyMessage "PLAY01" fleet
                    
                    WS.sendTextData conn (encode readyMsg)
                    threadDelay 200000
                    
                    WS.sendClose conn ("Done" :: T.Text)
                    return True
                    ) `catch` (\(_ :: SomeException) -> return False)
                
                result `shouldBe` True
            
            it "should handle attack message" $ do
                state <- startTestServer
                
                -- Create room
                roomId <- RoomMgr.generateUniqueRoomId
                _ <- RoomMgr.createRoom (wsRoomManager state) roomId "PLAY01" "Player1"
                _ <- RoomMgr.joinRoom (wsRoomManager state) roomId "PLAY02" "Player2"
                
                result <- (do
                    conn1 <- connectClient (T.unpack roomId) "PLAY01"
                    conn2 <- connectClient (T.unpack roomId) "PLAY02"
                    
                    -- Both ready
                    let fleet = createTestFleet
                    WS.sendTextData conn1 (encode $ ReadyMsg $ ReadyMessage "PLAY01" fleet)
                    WS.sendTextData conn2 (encode $ ReadyMsg $ ReadyMessage "PLAY02" fleet)
                    threadDelay 500000
                    
                    -- Player 1 attacks
                    let attackMsg = AttackMsg $ AttackMessage "PLAY01" (Position 0 0)
                    WS.sendTextData conn1 (encode attackMsg)
                    threadDelay 200000
                    
                    WS.sendClose conn1 ("Done" :: T.Text)
                    WS.sendClose conn2 ("Done" :: T.Text)
                    return True
                    ) `catch` (\(_ :: SomeException) -> return False)
                
                result `shouldBe` True
        
        -- ====================================================================
        -- Concurrency Tests
        -- ====================================================================
        
        describe "Concurrency" $ do
            it "should handle multiple rooms simultaneously" $ do
                state <- startTestServer
                
                -- Create 3 rooms
                room1 <- RoomMgr.generateUniqueRoomId
                room2 <- RoomMgr.generateUniqueRoomId
                room3 <- RoomMgr.generateUniqueRoomId
                
                _ <- RoomMgr.createRoom (wsRoomManager state) room1 "P1A" "Player1A"
                _ <- RoomMgr.createRoom (wsRoomManager state) room2 "P2A" "Player2A"
                _ <- RoomMgr.createRoom (wsRoomManager state) room3 "P3A" "Player3A"
                
                result <- (do
                    -- Connect to all 3 rooms
                    conn1a <- connectClient (T.unpack room1) "P1A"
                    conn2a <- connectClient (T.unpack room2) "P2A"
                    conn3a <- connectClient (T.unpack room3) "P3A"
                    
                    threadDelay 200000
                    
                    WS.sendClose conn1a ("Done" :: T.Text)
                    WS.sendClose conn2a ("Done" :: T.Text)
                    WS.sendClose conn3a ("Done" :: T.Text)
                    return True
                    ) `catch` (\(_ :: SomeException) -> return False)
                
                result `shouldBe` True
        
        -- ====================================================================
        -- Error Handling Tests
        -- ====================================================================
        
        describe "Error Handling" $ do
            it "should handle invalid message gracefully" $ do
                state <- startTestServer
                
                result <- (do
                    conn <- connectClient "ROOM01" "PLAY01"
                    
                    -- Send invalid JSON
                    WS.sendTextData conn ("invalid json{{{" :: T.Text)
                    threadDelay 100000
                    
                    -- Connection should still be alive
                    WS.sendClose conn ("Done" :: T.Text)
                    return True
                    ) `catch` (\(_ :: SomeException) -> return False)
                
                result `shouldBe` True
            
            it "should handle disconnect gracefully" $ do
                state <- startTestServer
                
                result <- (do
                    conn <- connectClient "ROOM01" "PLAY01"
                    
                    -- Abruptly close without proper close frame
                    WS.sendClose conn ("Disconnect" :: T.Text)
                    return True
                    ) `catch` (\(_ :: SomeException) -> return False)
                
                result `shouldBe` True
        
        -- ====================================================================
        -- State Management Tests
        -- ====================================================================
        
        describe "State Management (STM)" $ do
            it "should register connection in PlayerManager" $ do
                state <- startTestServer
                
                -- Add player
                PlayerMgr.addPlayer (wsPlayerManager state) "TEST01" "TestPlayer"
                
                result <- (do
                    conn <- connectClient "ROOM01" "TEST01"
                    threadDelay 200000
                    
                    -- Check if connection registered
                    maybeConn <- PlayerMgr.getConnection 
                        (wsPlayerManager state) "TEST01"
                    
                    WS.sendClose conn ("Done" :: T.Text)
                    -- Check if connection exists (not Nothing)
                    case maybeConn of
                        Nothing -> return False
                        Just _ -> return True
                    ) `catch` (\(_ :: SomeException) -> return False)
                
                result `shouldBe` True
            
            it "should clean up connection on disconnect" $ do
                state <- startTestServer
                
                -- Add player
                PlayerMgr.addPlayer (wsPlayerManager state) "TEST02" "TestPlayer2"
                
                (do
                    conn <- connectClient "ROOM01" "TEST02"
                    threadDelay 100000
                    WS.sendClose conn ("Done" :: T.Text)
                    threadDelay 100000
                    ) `catch` (\(_ :: SomeException) -> return ())
                
                -- Check connection removed
                maybeConn <- PlayerMgr.getConnection 
                    (wsPlayerManager state) "TEST02"
                
                -- Connection should be Nothing after disconnect
                case maybeConn of
                    Nothing -> True `shouldBe` True
                    Just _ -> False `shouldBe` True

-- ============================================================================
-- Property-Based Tests (QuickCheck)
-- ============================================================================

prop_messageRoundTrip :: Bool
prop_messageRoundTrip =
    let msg = ReadyMsg $ ReadyMessage "TEST01" createTestFleet
        encoded = encode msg
        decoded = decode encoded :: Maybe ClientMessage
    in decoded == Just msg

prop_positionValid :: Int -> Int -> Bool
prop_positionValid r c =
    let pos = Position r c
        encoded = encode pos
        decoded = decode encoded :: Maybe Position
    in decoded == Just pos
