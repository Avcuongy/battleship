{-# LANGUAGE DeriveGeneric #-}

module Game.Types where

import GHC.Generics
import Data.Aeson

-- Coordinate trên board (0-9)
data Position = Position 
    { row :: Int 
    , col :: Int 
    } deriving (Show, Eq, Generic)

instance ToJSON Position
instance FromJSON Position

-- Hướng đặt tàu
data Orientation = Horizontal | Vertical 
    deriving (Show, Eq, Generic)

instance ToJSON Orientation
instance FromJSON Orientation

-- Ship types với độ dài
data ShipType 
    = Carrier      -- 5 cells
    | Battleship   -- 4 cells
    | Cruiser      -- 3 cells
    | Submarine    -- 3 cells
    | Destroyer    -- 2 cells
    deriving (Show, Eq, Generic)

instance ToJSON ShipType
instance FromJSON ShipType

shipLength :: ShipType -> Int
shipLength Carrier = 5
shipLength Battleship = 4
shipLength Cruiser = 3
shipLength Submarine = 3
shipLength Destroyer = 2

-- Ship placement
data Ship = Ship
    { shipType :: ShipType
    , startPos :: Position
    , orientation :: Orientation
    } deriving (Show, Eq, Generic)

instance ToJSON Ship
instance FromJSON Ship

-- Cell state
data CellState 
    = Empty           -- Chưa bắn
    | Miss            -- Bắn trượt
    | Hit             -- Bắn trúng
    | Sunk            -- Tàu chìm
    deriving (Show, Eq, Generic)

instance ToJSON CellState
instance FromJSON CellState

-- Move result
data MoveResult = MoveResult
    { position :: Position
    , result :: CellState
    , sunkShip :: Maybe ShipType
    } deriving (Show, Generic)

instance ToJSON MoveResult
instance FromJSON MoveResult
