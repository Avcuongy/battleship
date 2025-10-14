{-# LANGUAGE OverloadedStrings #-}

module Game.Board
  ( emptyBoard
  , placeShip
  , isValidPlacement
  , shootAt
  , allShipsSunk
  , getShipPositions
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)

import Game.Types
  ( Position
  , CellState(..)
  , ShipType
  , Ship(..)
  , Board
  , ShotResult(..)
  , shipSize
  )

-- | Create an empty board
emptyBoard :: Board
emptyBoard = Map.fromList [((r, c), Empty) | r <- [0..9], c <- [0..9]]

-- | Get all positions for a ship starting at given position
getShipPositions :: Position -> ShipType -> Bool -> [Position]
getShipPositions (r, c) shipType isHoriz =
  let size = shipSize shipType
  in if isHoriz
     then [(r, c + i) | i <- [0..size-1]]
     else [(r + i, c) | i <- [0..size-1]]

-- | Check if a position is on the board
isOnBoard :: Position -> Bool
isOnBoard (r, c) = r >= 0 && r < 10 && c >= 0 && c < 10

-- | Check if all positions are valid for ship placement
isValidPlacement :: Board -> [Position] -> Bool
isValidPlacement board positions =
  all isOnBoard positions &&
  all (\pos -> case Map.lookup pos board of
                 Just Empty -> True
                 _ -> False
      ) positions

-- | Place a ship on the board
placeShip :: Board -> Ship -> Either String Board
placeShip board ship =
  let pos = positions ship
  in if isValidPlacement board pos
     then Right $ foldr (\p b -> Map.insert p (Ship (shipType ship)) b) board pos
     else Left "Invalid ship placement"

-- | Shoot at a position on the board
shootAt :: Position -> Board -> [Ship] -> (ShotResult, Board)
shootAt pos board ships =
  case Map.lookup pos board of
    Nothing -> (ShotMiss, board)
    Just Empty -> (ShotMiss, Map.insert pos Miss board)
    Just Miss -> (ShotMiss, board)
    Just (Ship st) -> 
      let newBoard = Map.insert pos (Hit st) board
          isSunk = isShipSunk st newBoard ships
      in if isSunk
         then (ShotSunk st, newBoard)
         else (ShotHit st, newBoard)
    Just (Hit st) -> (ShotHit st, board)

-- | Check if a ship is completely sunk
isShipSunk :: ShipType -> Board -> [Ship] -> Bool
isShipSunk st board ships =
  case filter (\s -> shipType s == st) ships of
    [] -> False
    (ship:_) -> all (\pos -> case Map.lookup pos board of
                                Just (Hit _) -> True
                                _ -> False
                    ) (positions ship)

-- | Check if all ships are sunk
allShipsSunk :: Board -> [Ship] -> Bool
allShipsSunk board ships =
  all (\ship -> isShipSunk (shipType ship) board ships) ships
