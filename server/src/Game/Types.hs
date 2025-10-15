{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types
  ( Position
  , Coordinate(..)
  , CellState(..)
  , ShipType(..)
  , Ship(..)
  , Board
  , GameState(..)
  , Turn(..)
  , ShotResult(..)
  , positionToIndex
  , indexToPosition
  , shipSize
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Player.Types (PlayerId)

-- | Position on the board (row, col) - 0-indexed
type Position = (Int, Int)

-- | Coordinate (A-J, 1-10)
data Coordinate = Coordinate
  { col :: !Char  -- A-J
  , row :: !Int   -- 1-10
  } deriving (Show, Eq, Generic)

instance ToJSON Coordinate
instance FromJSON Coordinate

-- | Cell state on the board
data CellState
  = Empty
  | ShipCell ShipType
  | Hit ShipType
  | Miss
  deriving (Show, Eq, Generic)

instance ToJSON CellState
instance FromJSON CellState

-- | Ship types
data ShipType
  = Carrier    -- 5 cells
  | Battleship -- 4 cells
  | Cruiser    -- 3 cells
  | Submarine  -- 3 cells
  | Destroyer  -- 2 cells
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ShipType
instance FromJSON ShipType

-- | Ship placement
data Ship = Ship
  { shipType :: !ShipType
  , positions :: ![Position]
  , isHorizontal :: !Bool
  } deriving (Show, Eq, Generic)

instance ToJSON Ship
instance FromJSON Ship

-- | Game board (10x10 grid)
type Board = Map Position CellState

-- | Whose turn it is
data Turn = Player1Turn | Player2Turn
  deriving (Show, Eq, Generic)

instance ToJSON Turn
instance FromJSON Turn

-- | Result of a shot
data ShotResult
  = ShotMiss
  | ShotHit ShipType
  | ShotSunk ShipType
  deriving (Show, Eq, Generic)

instance ToJSON ShotResult
instance FromJSON ShotResult

-- | Game state
data GameState = GameState
  { player1Board :: !Board
  , player2Board :: !Board
  , player1Ships :: ![Ship]
  , player2Ships :: ![Ship]
  , player1Shots :: ![Position]
  , player2Shots :: ![Position]
  , currentTurn :: !Turn
  , winner :: !(Maybe PlayerId)
  } deriving (Show, Eq, Generic)

instance ToJSON GameState
instance FromJSON GameState

-- | Get ship size
shipSize :: ShipType -> Int
shipSize Carrier = 5
shipSize Battleship = 4
shipSize Cruiser = 3
shipSize Submarine = 3
shipSize Destroyer = 2

-- | Convert position to linear index
positionToIndex :: Position -> Int
positionToIndex (r, c) = r * 10 + c

-- | Convert linear index to position
indexToPosition :: Int -> Position
indexToPosition idx = (idx `div` 10, idx `mod` 10)
