{-# LANGUAGE OverloadedStrings #-}

module Game.Engine
  ( newGame
  , placePlayerShips
  , makeMove
  , switchTurn
  , checkGameOver
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Game.Types
  ( Position
  , Ship
  , Board
  , GameState(..)
  , Turn(..)
  , ShotResult
  )
import Game.Board (emptyBoard, placeShip, shootAt, allShipsSunk)
import Player.Types (PlayerId)

-- | Create a new game
newGame :: GameState
newGame = GameState
  { player1Board = emptyBoard
  , player2Board = emptyBoard
  , player1Ships = []
  , player2Ships = []
  , player1Shots = []
  , player2Shots = []
  , currentTurn = Player1Turn
  , winner = Nothing
  }

-- | Place ships for a player
placePlayerShips :: GameState -> Bool -> [Ship] -> Either String GameState
placePlayerShips game isPlayer1 ships =
  case foldl placeOne (Right emptyBoard) ships of
    Left err -> Left err
    Right newBoard ->
      if isPlayer1
      then Right $ game { player1Board = newBoard, player1Ships = ships }
      else Right $ game { player2Board = newBoard, player2Ships = ships }
  where
    placeOne :: Either String Board -> Ship -> Either String Board
    placeOne (Left err) _ = Left err
    placeOne (Right board) ship = placeShip board ship

-- | Make a move in the game
makeMove :: GameState -> Position -> (ShotResult, GameState)
makeMove game pos =
  case currentTurn game of
    Player1Turn ->
      let (result, newBoard) = shootAt pos (player2Board game) (player2Ships game)
          newGame = game
            { player2Board = newBoard
            , player1Shots = pos : player1Shots game
            }
      in (result, newGame)
    Player2Turn ->
      let (result, newBoard) = shootAt pos (player1Board game) (player1Ships game)
          newGame = game
            { player1Board = newBoard
            , player2Shots = pos : player2Shots game
            }
      in (result, newGame)

-- | Switch turn to the other player
switchTurn :: GameState -> GameState
switchTurn game = game
  { currentTurn = case currentTurn game of
                    Player1Turn -> Player2Turn
                    Player2Turn -> Player1Turn
  }

-- | Check if the game is over and who won
checkGameOver :: GameState -> PlayerId -> PlayerId -> Maybe PlayerId
checkGameOver game p1Id p2Id
  | allShipsSunk (player1Board game) (player1Ships game) = Just p2Id
  | allShipsSunk (player2Board game) (player2Ships game) = Just p1Id
  | otherwise = Nothing
