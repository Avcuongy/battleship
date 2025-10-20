{-# LANGUAGE OverloadedStrings #-}

module Game.Ship
    ( -- * Validation
      validateSetup
    , isValidPlacement
    , checkOverlap
    , validateSingleShip
    
    -- * Helpers
    , hasAllShipTypes
    , validateShipBounds
    , getShipEndPosition
    , canPlaceShip
    ) where

import Game.Types
import Game.Board
import Data.List (sort, nub)
import qualified Data.Set as S

-- ===== VALIDATION =====

-- | Validate complete ship setup (all 5 ships)
-- Rules:
--   1. Must have exactly 5 ships
--   2. Must include all ship types (Carrier, Battleship, Cruiser, Submarine, Destroyer)
--   3. All ships must be within board bounds
--   4. Ships cannot overlap
validateSetup :: [Ship] -> Either String [Ship]
validateSetup ships
    | length ships /= 5 = 
        Left "Must have exactly 5 ships"
    | not (hasAllShipTypes ships) = 
        Left "Missing required ship types (Carrier, Battleship, Cruiser, Submarine, Destroyer)"
    | not (all (validateShipBounds . shipPositions) ships) = 
        Left "Ship placement out of bounds"
    | checkOverlap ships = 
        Left "Ships cannot overlap"
    | otherwise = 
        Right ships

-- | Validate a single ship before adding to existing ships
validateSingleShip :: [Ship] -> Ship -> Either String Ship
validateSingleShip existingShips newShip
    | not (validateShipBounds (shipPositions newShip)) =
        Left "Ship placement out of bounds"
    | not (isValidPlacement existingShips newShip) =
        Left "Ship overlaps with existing ship"
    | otherwise =
        Right newShip

-- | Check if all required ship types are present
hasAllShipTypes :: [Ship] -> Bool
hasAllShipTypes ships =
    let shipTypes = sort $ map shipType ships
        expectedTypes = sort allShipTypes
    in shipTypes == expectedTypes

-- | Validate all positions are within board (0-9 for both row and col)
validateShipBounds :: [Position] -> Bool
validateShipBounds = all isValidPosition

-- | Check if ships overlap (any shared positions)
checkOverlap :: [Ship] -> Bool
checkOverlap ships =
    let allPositions = concatMap shipPositions ships
        uniquePositions = S.fromList allPositions
    in length allPositions /= S.size uniquePositions

-- | Check if a new ship placement is valid (no overlap with existing)
isValidPlacement :: [Ship] -> Ship -> Bool
isValidPlacement existingShips newShip =
    let newPositions = S.fromList $ shipPositions newShip
        existingPositions = S.fromList $ concatMap shipPositions existingShips
        allValid = validateShipBounds (shipPositions newShip)
        noOverlap = S.null $ S.intersection newPositions existingPositions
    in allValid && noOverlap

-- ===== HELPERS =====

-- | Get end position of a ship
getShipEndPosition :: Ship -> Position
getShipEndPosition ship =
    last $ shipPositions ship

-- | Check if ship can be placed at position with orientation
canPlaceShip :: [Ship] -> ShipType -> Position -> Orientation -> Bool
canPlaceShip existingShips sType pos orient =
    let newShip = Ship sType pos orient
    in case validateSingleShip existingShips newShip of
        Right _ -> True
        Left _  -> False
