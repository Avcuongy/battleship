{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Game.Types
    ( -- * Position types
      Position(..)
    , Row
    , Col
    
    -- * Ship types
    , ShipType(..)
    , Orientation(..)
    , Ship(..)
    , Fleet
    
    -- * Board types
    , Cell(..)
    , Board(..)
    
    -- * Result types
    , Result(..)
    
    -- * Helper functions
    , shipLength
    , positionToString
    , stringToPosition
    ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Text (Text)
import qualified Data.Text as T

-- ============================================================================
-- Position Types
-- ============================================================================

type Row = Int  -- 0-9 (A-J)
type Col = Int  -- 0-9 (1-10)

-- | Position on the board (0-indexed)
-- Row 0 = A, Row 1 = B, ..., Row 9 = J
-- Col 0 = 1, Col 1 = 2, ..., Col 9 = 10
data Position = Position
    { posRow :: !Row  -- ! = strict field (better performance)
    , posCol :: !Col
    } deriving (Show, Eq, Ord, Generic)

instance NFData Position
instance FromJSON Position
instance ToJSON Position

-- | Convert Position to string (e.g., "A5", "J10")
positionToString :: Position -> Text
positionToString (Position row col) = 
    T.pack $ [rowToChar row] ++ show (col + 1)
  where
    rowToChar r = toEnum (fromEnum 'A' + r) :: Char

-- | Convert string to Position (e.g., "A5" -> Position 0 4)
stringToPosition :: Text -> Maybe Position
stringToPosition txt = 
    case T.unpack txt of
        (r:cs) | r >= 'A' && r <= 'J' -> do
            let row = fromEnum r - fromEnum 'A'
            col <- case reads cs of
                [(c, "")] | c >= 1 && c <= 10 -> Just (c - 1)
                _ -> Nothing
            return $ Position row col
        _ -> Nothing

-- ============================================================================
-- Ship Types
-- ============================================================================

-- | Ship types with their lengths
data ShipType
    = Destroyer   -- Length 2
    | Submarine   -- Length 3
    | Cruiser     -- Length 3
    | Battleship  -- Length 4
    | Carrier     -- Length 5
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData ShipType
instance FromJSON ShipType
instance ToJSON ShipType

-- | Get ship length by type
shipLength :: ShipType -> Int
shipLength Destroyer  = 2
shipLength Submarine  = 3
shipLength Cruiser    = 3
shipLength Battleship = 4
shipLength Carrier    = 5

-- | Ship orientation
data Orientation
    = Horizontal  -- Left to right
    | Vertical    -- Top to bottom
    deriving (Show, Eq, Generic)

instance NFData Orientation
instance FromJSON Orientation
instance ToJSON Orientation

-- | Ship with position and hit tracking
data Ship = Ship
    { shipType        :: !ShipType
    , shipPosition    :: !Position      -- Starting position (top-left)
    , shipOrientation :: !Orientation
    , shipHits        :: ![Bool]        -- Hit status for each segment
    } deriving (Show, Eq, Generic)

instance NFData Ship
instance FromJSON Ship
instance ToJSON Ship

-- | Fleet = collection of 5 ships
type Fleet = [Ship]

-- ============================================================================
-- Board Types
-- ============================================================================

-- | Cell state on the board
data Cell
    = Water                    -- Empty cell
    | ShipCell !ShipType       -- Cell occupied by ship
    | Hit !ShipType            -- Ship cell that was hit
    | Miss                     -- Water cell that was attacked
    deriving (Show, Eq, Generic)

instance NFData Cell
instance FromJSON Cell
instance ToJSON Cell

-- | Board = 10x10 grid of cells
newtype Board = Board
    { boardCells :: [[Cell]]  -- 10 rows × 10 cols
    } deriving (Show, Eq, Generic)

instance NFData Board
instance FromJSON Board
instance ToJSON Board

-- ============================================================================
-- Result Types
-- ============================================================================

-- | Attack result
data Result
    = ResultMiss
    | ResultHit
    | ResultShipSunk !ShipType
    deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

instance NFData Result
instance FromJSON Result
instance ToJSON Result
