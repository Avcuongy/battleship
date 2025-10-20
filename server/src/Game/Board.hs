{-# LANGUAGE OverloadedStrings #-}

module Game.Board where

import Game.Types
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)

-- Board là Map từ Position -> CellState
type Board = M.Map Position CellState

-- Create empty board (10x10)
emptyBoard :: Board
emptyBoard = M.fromList 
    [(Position r c, Empty) | r <- [0..9], c <- [0..9]]

-- Get cell state
getCell :: Board -> Position -> Maybe CellState
getCell = flip M.lookup

-- Update cell state
updateCell :: Board -> Position -> CellState -> Board
updateCell board pos state = M.insert pos state board

-- Check if position is valid
isValidPosition :: Position -> Bool
isValidPosition (Position r c) = r >= 0 && r <= 9 && c >= 0 && c <= 9

-- Get all positions for a ship
shipPositions :: Ship -> [Position]
shipPositions (Ship sType (Position r c) orient) =
    let len = shipLength sType
    in case orient of
        Horizontal -> [Position r (c + i) | i <- [0..len-1]]
        Vertical   -> [Position (r + i) c | i <- [0..len-1]]

-- Check if ship placement is valid (no overlap)
isValidPlacement :: [Ship] -> Ship -> Bool
isValidPlacement existingShips newShip =
    let newPositions = shipPositions newShip
        existingPositions = concatMap shipPositions existingShips
        allValid = all isValidPosition newPositions
        noOverlap = null $ filter (`elem` existingPositions) newPositions
    in allValid && noOverlap
