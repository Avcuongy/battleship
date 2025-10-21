{-# LANGUAGE OverloadedStrings #-}

module Game.Board
    ( -- * Board creation
      emptyBoard
    , createBoard
    
    -- * Attack operations
    , processAttack
    , getUnattackedCells
    
    -- * Board queries
    , allShipsSunk
    , getCellAt
    , setBoardCell
    
    -- * Board utilities
    , boardToList
    ) where

import Game.Types
import Game.Ship (getShipPositions, isSunk, hitShip)
import Data.Maybe (isNothing)

-- ============================================================================
-- Board Creation
-- ============================================================================

-- | Create empty board (all Water)
emptyBoard :: Board
emptyBoard = Board $ replicate 10 (replicate 10 Water)

-- | Create board from fleet (place ships on board)
createBoard :: Fleet -> Board
createBoard fleet =
    let board = emptyBoard
    in foldl placeShipOnBoard board fleet
  where
    placeShipOnBoard :: Board -> Ship -> Board
    placeShipOnBoard board ship =
        let positions = getShipPositions ship
            sType = shipType ship
        in foldl (\b pos -> setBoardCell pos (ShipCell sType) b) board positions

-- ============================================================================
-- Attack Operations
-- ============================================================================

-- | Process attack at position, return (Result, updated Board)
processAttack :: Position -> Board -> (Result, Board)
processAttack pos board =
    case getCellAt pos board of
        Nothing -> 
            -- Invalid position (shouldn't happen with validation)
            (ResultMiss, board)
        
        Just Water -> 
            -- Miss: mark as Miss
            (ResultMiss, setBoardCell pos Miss board)
        
        Just Miss -> 
            -- Already attacked (miss) - ignore (silent)
            (ResultMiss, board)
        
        Just (Hit _) -> 
            -- Already attacked (hit) - ignore (silent)
            (ResultMiss, board)
        
        Just (ShipCell sType) -> 
            -- Hit ship: mark as Hit, check if sunk
            let newBoard = setBoardCell pos (Hit sType) board
                sunk = isShipSunkOnBoard sType newBoard
            in if sunk
                then (ResultShipSunk sType, newBoard)
                else (ResultHit, newBoard)

-- | Check if specific ship type is completely sunk on board
isShipSunkOnBoard :: ShipType -> Board -> Bool
isShipSunkOnBoard sType board =
    let cells = boardToList board
        shipCells = filter (isShipOrHit sType) cells
        hitCells = filter (isHit sType) cells
    in length shipCells == length hitCells && length hitCells == shipLength sType
  where
    isShipOrHit st (ShipCell t) = t == st
    isShipOrHit st (Hit t) = t == st
    isShipOrHit _ _ = False
    
    isHit st (Hit t) = t == st
    isHit _ _ = False

-- | Get all unattacked cells (for AI move selection)
getUnattackedCells :: Board -> [Position]
getUnattackedCells board =
    [ Position r c
    | r <- [0..9]
    , c <- [0..9]
    , case getCellAt (Position r c) board of
        Just Water -> True
        Just (ShipCell _) -> True
        _ -> False
    ]

-- ============================================================================
-- Board Queries
-- ============================================================================

-- | Check if all ships are sunk (win condition)
allShipsSunk :: Board -> Bool
allShipsSunk board =
    let cells = boardToList board
        hasShipCells = any isShipCell cells
    in not hasShipCells
  where
    isShipCell (ShipCell _) = True
    isShipCell _ = False

-- | Get cell at position (safe, returns Nothing if out of bounds)
getCellAt :: Position -> Board -> Maybe Cell
getCellAt (Position row col) (Board cells)
    | row < 0 || row >= 10 || col < 0 || col >= 10 = Nothing
    | otherwise = Just $ (cells !! row) !! col

-- | Set cell at position (returns updated board)
setBoardCell :: Position -> Cell -> Board -> Board
setBoardCell (Position row col) newCell (Board cells) =
    let rowList = cells !! row
        newRow = take col rowList ++ [newCell] ++ drop (col + 1) rowList
        newCells = take row cells ++ [newRow] ++ drop (row + 1) cells
    in Board newCells

-- ============================================================================
-- Board Utilities
-- ============================================================================

-- | Convert board to flat list of cells (for queries)
boardToList :: Board -> [Cell]
boardToList (Board cells) = concat cells
