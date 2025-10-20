{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types
    ( -- * Position
      Position(..)
    , isValidPosition
    , positionToString
    , stringToPosition
    
    -- * Ship Types
    , Orientation(..)
    , ShipType(..)
    , Ship(..)
    
    -- * Cell State
    , CellState(..)
    
    -- * Move Result
    , MoveResult(..)
    
    -- * Game Status
    , GameStatus(..)
    
    -- * Helper Functions
    , shipLength
    , allShipTypes
    , shipTypeFromInt
    ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (ord, chr)
import GHC.Generics

-- ===== POSITION =====

-- | Position on 10x10 board (0-indexed)
data Position = Position
    { row :: !Int  -- ^ Row (0-9), where 0 = 'A', 9 = 'J'
    , col :: !Int  -- ^ Column (0-9), where 0 = '1', 9 = '10'
    } deriving (Show, Eq, Ord, Generic)

instance ToJSON Position where
    toJSON (Position r c) = object
        [ "row" .= r
        , "col" .= c
        ]

instance FromJSON Position where
    parseJSON = withObject "Position" $ \v ->
        Position <$> v .: "row" <*> v .: "col"

-- | Check if position is valid (within 10x10 board)
isValidPosition :: Position -> Bool
isValidPosition (Position r c) = 
    r >= 0 && r <= 9 && c >= 0 && c <= 9

-- | Convert position to string (e.g., "A1", "J10")
positionToString :: Position -> Text
positionToString (Position r c) =
    T.pack [rowToChar r] <> T.pack (show (c + 1))
  where
    rowToChar i = chr (ord 'A' + i)

-- | Parse position from string (e.g., "A1" -> Position 0 0)
stringToPosition :: Text -> Maybe Position
stringToPosition txt
    | T.length txt < 2 = Nothing
    | otherwise = do
        let (rowChar, colStr) = (T.head txt, T.tail txt)
        r <- charToRow rowChar
        c <- textToCol colStr
        let pos = Position r c
        if isValidPosition pos then Just pos else Nothing
  where
    charToRow ch
        | ch >= 'A' && ch <= 'J' = Just (ord ch - ord 'A')
        | ch >= 'a' && ch <= 'j' = Just (ord ch - ord 'a')
        | otherwise = Nothing
    
    textToCol txt = case reads (T.unpack txt) of
        [(n, "")] | n >= 1 && n <= 10 -> Just (n - 1)
        _ -> Nothing

-- ===== SHIP TYPES =====

-- | Ship orientation
data Orientation 
    = Horizontal  -- ^ Left to right (→)
    | Vertical    -- ^ Top to bottom (↓)
    deriving (Show, Eq, Generic)

instance ToJSON Orientation where
    toJSON Horizontal = String "horizontal"
    toJSON Vertical   = String "vertical"

instance FromJSON Orientation where
    parseJSON = withText "Orientation" $ \case
        "horizontal" -> pure Horizontal
        "vertical"   -> pure Vertical
        _            -> fail "Invalid orientation"

-- | Ship types in BattleShip
data ShipType
    = Carrier      -- ^ 5 cells - Aircraft Carrier
    | Battleship   -- ^ 4 cells
    | Cruiser      -- ^ 3 cells
    | Submarine    -- ^ 3 cells
    | Destroyer    -- ^ 2 cells
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON ShipType where
    toJSON Carrier    = String "carrier"
    toJSON Battleship = String "battleship"
    toJSON Cruiser    = String "cruiser"
    toJSON Submarine  = String "submarine"
    toJSON Destroyer  = String "destroyer"

instance FromJSON ShipType where
    parseJSON = withText "ShipType" $ \case
        "carrier"    -> pure Carrier
        "battleship" -> pure Battleship
        "cruiser"    -> pure Cruiser
        "submarine"  -> pure Submarine
        "destroyer"  -> pure Destroyer
        _            -> fail "Invalid ship type"

-- | Get length of a ship type
shipLength :: ShipType -> Int
shipLength Carrier    = 5
shipLength Battleship = 4
shipLength Cruiser    = 3
shipLength Submarine  = 3
shipLength Destroyer  = 2

-- | All ship types required for a valid game
allShipTypes :: [ShipType]
allShipTypes = [minBound .. maxBound]

-- | Convert integer to ship type (for testing/debugging)
shipTypeFromInt :: Int -> Maybe ShipType
shipTypeFromInt 5 = Just Carrier
shipTypeFromInt 4 = Just Battleship
shipTypeFromInt 3 = Just Cruiser  -- Returns first 3-cell ship
shipTypeFromInt 2 = Just Destroyer
shipTypeFromInt _ = Nothing

-- | Ship placement on board
data Ship = Ship
    { shipType    :: !ShipType     -- ^ Type of ship
    , startPos    :: !Position     -- ^ Starting position (top-left cell)
    , orientation :: !Orientation  -- ^ Horizontal or vertical
    } deriving (Show, Eq, Generic)

instance ToJSON Ship where
    toJSON (Ship sType pos orient) = object
        [ "shipType"    .= sType
        , "startPos"    .= pos
        , "orientation" .= orient
        ]

instance FromJSON Ship where
    parseJSON = withObject "Ship" $ \v ->
        Ship <$> v .: "shipType"
             <*> v .: "startPos"
             <*> v .: "orientation"

-- ===== CELL STATE =====

-- | State of a cell on the board
data CellState
    = Empty      -- ^ Not attacked yet
    | Miss       -- ^ Attacked, no ship present
    | Hit        -- ^ Attacked, ship hit but not sunk
    | Sunk       -- ^ Ship completely destroyed
    deriving (Show, Eq, Generic)

instance ToJSON CellState where
    toJSON Empty = String "empty"
    toJSON Miss  = String "miss"
    toJSON Hit   = String "hit"
    toJSON Sunk  = String "sunk"

instance FromJSON CellState where
    parseJSON = withText "CellState" $ \case
        "empty" -> pure Empty
        "miss"  -> pure Miss
        "hit"   -> pure Hit
        "sunk"  -> pure Sunk
        _       -> fail "Invalid cell state"

-- ===== MOVE RESULT =====

-- | Result of an attack move
data MoveResult = MoveResult
    { movePosition :: !Position         -- ^ Position attacked
    , moveResult   :: !CellState        -- ^ Result (Hit/Miss/Sunk)
    , sunkShip     :: !(Maybe ShipType) -- ^ Ship type if sunk
    } deriving (Show, Eq, Generic)

instance ToJSON MoveResult where
    toJSON (MoveResult pos result ship) = object
        [ "position" .= pos
        , "result"   .= result
        , "sunkShip" .= ship
        ]

instance FromJSON MoveResult where
    parseJSON = withObject "MoveResult" $ \v ->
        MoveResult <$> v .: "position"
                   <*> v .: "result"
                   <*> v .: "sunkShip"

-- ===== GAME STATUS =====

-- | Overall game status
data GameStatus
    = Ongoing    -- ^ Game in progress
    | Player1Won -- ^ Player 1 victory (1v1)
    | Player2Won -- ^ Player 2 victory (1v1)
    | AIWon      -- ^ AI victory (vs AI)
    | PlayerWon  -- ^ Player beat AI (vs AI)
    deriving (Show, Eq, Generic)

instance ToJSON GameStatus where
    toJSON Ongoing    = String "ongoing"
    toJSON Player1Won = String "player1_won"
    toJSON Player2Won = String "player2_won"
    toJSON AIWon      = String "ai_won"
    toJSON PlayerWon  = String "player_won"

instance FromJSON GameStatus where
    parseJSON = withText "GameStatus" $ \case
        "ongoing"      -> pure Ongoing
        "player1_won"  -> pure Player1Won
        "player2_won"  -> pure Player2Won
        "ai_won"       -> pure AIWon
        "player_won"   -> pure PlayerWon
        _              -> fail "Invalid game status"
