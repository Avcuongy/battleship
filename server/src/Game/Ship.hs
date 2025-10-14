{-# LANGUAGE OverloadedStrings #-}

module Game.Ship
  ( allShipTypes
  , createShip
  , validateShipPlacement
  ) where

import Game.Types
  ( Position
  , ShipType(..)
  , Ship(..)
  , Board
  , shipSize
  )
import Game.Board (getShipPositions, isValidPlacement)

-- | All ship types in the game
allShipTypes :: [ShipType]
allShipTypes = [Carrier, Battleship, Cruiser, Submarine, Destroyer]

-- | Create a ship at given position
createShip :: ShipType -> Position -> Bool -> Ship
createShip st pos isHoriz = Ship
  { shipType = st
  , positions = getShipPositions pos st isHoriz
  , isHorizontal = isHoriz
  }

-- | Validate that a ship placement doesn't overlap with existing ships
validateShipPlacement :: Board -> Ship -> Bool
validateShipPlacement board ship = isValidPlacement board (positions ship)
