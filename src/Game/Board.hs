{-# LANGUAGE OverloadedStrings #-}

module Game.Board
    ( -- * Types
      Board
    , Cell(..)
    
    -- * Board Creation
    , emptyBoard
    , initBoardWithShips
    
    -- * Board Operations
    , getCell
    , updateCell
    , getCellState
    , setCellState
    
    -- * Ship Operations
    , shipPositions
    , getShipCells
    , isShipSunk
    , markShipAsSunk
    
    -- * Queries
    , allShipsSunk
    , getHitCount
    , getMissCount
    , getTotalHits
    , getSunkShips
    , boardToList
    ) where

import Game.Types
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.List (nub)

-- ===== TYPES =====

-- | Cell contains state and optional ship type
data Cell = Cell
    { cellState :: !CellState
    , cellShip  :: !(Maybe ShipType)
    } deriving (Show, Eq)

-- | Board is a map from Position to Cell (100 cells for 10x10)
type Board = M.Map Position Cell

-- ===== BOARD CREATION =====

-- | Create empty 10x10 board
emptyBoard :: Board
emptyBoard = M.fromList
    [ (Position r c, Cell Empty Nothing)
    | r <- [0..9]
    , c <- [0..9]
    ]

-- | Get all positions occupied by a ship
shipPositions :: Ship -> [Position]
shipPositions (Ship sType (Position r c) orient) =
    let len = shipLength sType
    in case orient of
        Horizontal -> [Position r (c + i) | i <- [0..len-1]]
        Vertical   -> [Position (r + i) c | i <- [0..len-1]]

-- | Initialize board with ships placed
initBoardWithShips :: [Ship] -> Board
initBoardWithShips ships = 
    foldl placeShip emptyBoard ships
  where
    placeShip :: Board -> Ship -> Board
    placeShip board ship =
        let positions = shipPositions ship
            sType = shipType ship
        in foldl (\b pos -> M.insert pos (Cell Empty (Just sType)) b) board positions

-- ===== BOARD OPERATIONS =====

-- | Get cell at position
getCell :: Board -> Position -> Maybe Cell
getCell = flip M.lookup

-- | Get cell state at position (returns Empty if not found)
getCellState :: Board -> Position -> CellState
getCellState board pos =
    maybe Empty cellState (getCell board pos)

-- | Update cell state at position
updateCell :: Board -> Position -> CellState -> Board
updateCell board pos newState =
    M.adjust (\cell -> cell { cellState = newState }) pos board

-- | Set cell state (alias for updateCell)
setCellState :: Board -> Position -> CellState -> Board
setCellState = updateCell

-- ===== SHIP OPERATIONS =====

-- | Get all cells belonging to a specific ship type
getShipCells :: Board -> ShipType -> [Cell]
getShipCells board sType =
    mapMaybe getCellIfShip (M.toList board)
  where
    getCellIfShip (_, cell) = 
        case cellShip cell of
            Just s | s == sType -> Just cell
            _ -> Nothing

-- | Check if a ship is completely sunk
isShipSunk :: Board -> ShipType -> Bool
isShipSunk board sType =
    let shipCells = getShipCells board sType
        allHit = all (\cell -> cellState cell `elem` [Hit, Sunk]) shipCells
        notEmpty = not (null shipCells)
    in notEmpty && allHit

-- | Mark all cells of a ship as Sunk
markShipAsSunk :: Board -> ShipType -> Board
markShipAsSunk board shipType =
    M.map markCell board
  where
    markCell cell = 
        case cellShip cell of
            Just s | s == shipType && cellState cell == Hit -> 
                cell { cellState = Sunk }
            _ -> cell

-- ===== QUERIES =====

-- | Check if all ships are sunk
allShipsSunk :: Board -> Bool
allShipsSunk board =
    all (isShipSunk board) allShipTypes

-- | Count hits on board (including sunk)
getHitCount :: Board -> Int
getHitCount board =
    length $ filter (\cell -> cellState cell `elem` [Hit, Sunk]) (M.elems board)

-- | Count misses on board
getMissCount :: Board -> Int
getMissCount board =
    length $ filter (\cell -> cellState cell == Miss) (M.elems board)

-- | Get total number of hit cells (for scoring)
getTotalHits :: Board -> Int
getTotalHits = getHitCount

-- | Get list of sunk ship types
getSunkShips :: Board -> [ShipType]
getSunkShips board =
    filter (isShipSunk board) allShipTypes

-- | Convert board to list (for testing/debugging)
boardToList :: Board -> [(Position, Cell)]
boardToList = M.toList
