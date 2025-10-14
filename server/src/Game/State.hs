{-# LANGUAGE OverloadedStrings #-}

module Game.State
  ( -- * State query functions
    isValidShot
  , recordShot
  , getMoveHistory
  , getPlayerStats
  , setGameStartTime
  , setGameEndTime
  -- * Helper functions
  , bothPlayersReady
  , setPlayerReady
  ) where

import Data.Maybe (isJust)
import Game.Types
import Player.Types (PlayerId)

-- | Kiểm tra nước đi có hợp lệ không (chưa bắn vị trí này)
isValidShot :: GameState -> Position -> Bool
isValidShot game pos =
  case currentTurn game of
    Player1Turn -> pos `notElem` player1Shots game
    Player2Turn -> pos `notElem` player2Shots game

-- | Ghi lại nước đi (được gọi bởi Engine.makeMove)
recordShot :: GameState -> Position -> GameState
recordShot game pos =
  case currentTurn game of
    Player1Turn -> game { player1Shots = pos : player1Shots game }
    Player2Turn -> game { player2Shots = pos : player2Shots game }

-- | Lấy lịch sử nước đi
getMoveHistory :: GameState -> (PlayerId, PlayerId) -> [(PlayerId, Position)]
getMoveHistory game (p1Id, p2Id) =
  let p1Moves = map (\pos -> (p1Id, pos)) (player1Shots game)
      p2Moves = map (\pos -> (p2Id, pos)) (player2Shots game)
  in p1Moves ++ p2Moves

-- | Lấy thống kê người chơi
getPlayerStats :: GameState -> PlayerId -> (Int, Int, Int) -- (shots, hits, sunk)
getPlayerStats game playerId =
  -- TODO: Implement based on board state
  (0, 0, 0)

-- | Set thời gian bắt đầu
setGameStartTime :: GameState -> Integer -> GameState
setGameStartTime game time = game -- TODO: Add timestamp to GameState

-- | Set thời gian kết thúc
setGameEndTime :: GameState -> Integer -> GameState
setGameEndTime game time = game -- TODO: Add timestamp to GameState

-- | Kiểm tra cả hai người chơi đã sẵn sàng chưa
bothPlayersReady :: GameState -> Bool
bothPlayersReady game =
  not (null (player1Ships game)) && not (null (player2Ships game))

-- | Đặt người chơi là sẵn sàng
setPlayerReady :: GameState -> Bool -> GameState
setPlayerReady game isPlayer1 = game -- Ships already placed means ready

-- | Đặt tàu cho người chơi
setPlayerShips :: GameState -> PlayerId -> PlayerId -> [Ship] -> Either String GameState
setPlayerShips gs p1Id p2Id ships playerId =
  if playerId == p1Id
    then Right $ gs { gsPlayer1 = (gsPlayer1 gs) { psShips = ships, psReady = True } }
    else if playerId == p2Id
      then Right $ gs { gsPlayer2 = (gsPlayer2 gs) { psShips = ships, psReady = True } }
      else Left "Invalid player ID"

-- | Kiểm tra cả hai người chơi đã sẵn sàng chưa
bothPlayersReady :: GameState -> Bool
bothPlayersReady gs = psReady (gsPlayer1 gs) && psReady (gsPlayer2 gs)

-- | Bắt đầu game (set thời gian bắt đầu)
startGame :: GameState -> Integer -> GameState
startGame gs timestamp = gs { gsStartTime = Just timestamp }

-- | Kết thúc game (set thời gian kết thúc)
endGame :: GameState -> Integer -> GameState
endGame gs timestamp = gs { gsEndTime = Just timestamp }

-- | Lấy số lượt đi của một người chơi
getPlayerMoveCount :: GameState -> PlayerId -> Int
getPlayerMoveCount gs playerId =
  length $ filter (\(pid, _, _) -> pid == playerId) (gsMoveHistory gs)

-- | Lấy tỷ lệ trúng của một người chơi
getPlayerAccuracy :: GameState -> PlayerId -> Double
getPlayerAccuracy gs playerId =
  let moves = filter (\(pid, _, _) -> pid == playerId) (gsMoveHistory gs)
      hits = length $ filter (\(_, _, result) -> result /= Miss) moves
      total = length moves
  in if total == 0 then 0.0 else fromIntegral hits / fromIntegral total