{-# LANGUAGE OverloadedStrings #-}

module Game.Ship
    ( -- * Ship creation
      makeShip
    , makeFleet
    
    -- * Ship queries
    , getShipPositions
    , isSunk
    , canPlaceShip
    
    -- * Ship operations
    , hitShip
    ) where

import Game.Types
import Data.List (elemIndex)

-- ============================================================================
-- Ship Creation
-- ============================================================================

-- | Create a ship with no hits
makeShip :: ShipType -> Position -> Orientation -> Ship
makeShip sType pos orient = Ship
    { shipType = sType
    , shipPosition = pos
    , shipOrientation = orient
    , shipHits = replicate (shipLength sType) False  -- No hits initially
    }

-- | Create a complete fleet (helper for testing/AI)
makeFleet :: [(ShipType, Position, Orientation)] -> Fleet
makeFleet = map (\(t, p, o) -> makeShip t p o)

-- ============================================================================
-- Ship Queries
-- ============================================================================

-- | Get all positions occupied by a ship
getShipPositions :: Ship -> [Position]
getShipPositions ship =
    let start = shipPosition ship
        len = shipLength (shipType ship)
        orient = shipOrientation ship
    in case orient of
        Horizontal -> 
            [Position (posRow start) (posCol start + i) | i <- [0..len-1]]
        Vertical -> 
            [Position (posRow start + i) (posCol start) | i <- [0..len-1]]

-- | Check if ship is completely sunk
isSunk :: Ship -> Bool
isSunk ship = all id (shipHits ship)  -- All segments hit

-- | Check if ship can be placed at position (within bounds)
canPlaceShip :: Ship -> Bool
canPlaceShip ship =
    let positions = getShipPositions ship
    in all isValidPosition positions
  where
    isValidPosition (Position r c) = 
        r >= 0 && r < 10 && c >= 0 && c < 10

-- ============================================================================
-- Ship Operations
-- ============================================================================

-- | Hit ship at specific position, return updated ship
-- Returns Nothing if position is not part of this ship
hitShip :: Position -> Ship -> Maybe Ship
hitShip pos ship =
    let positions = getShipPositions ship
        maybeIndex = elemIndex pos positions
    in case maybeIndex of
        Nothing -> Nothing  -- Position not in this ship
        Just idx -> 
            let newHits = updateAt idx True (shipHits ship)
            in Just $ ship { shipHits = newHits }

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Find index of element in list
findIndex :: Eq a => a -> [a] -> Maybe Int
findIndex x xs = go 0 xs
  where
    go _ [] = Nothing
    go i (y:ys)
        | x == y    = Just i
        | otherwise = go (i + 1) ys

-- Update element at index
updateAt :: Int -> a -> [a] -> [a]
updateAt idx val xs = 
    take idx xs ++ [val] ++ drop (idx + 1) xs
