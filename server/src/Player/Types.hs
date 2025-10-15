{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Player.Types
  ( PlayerId
  , PlayerName
  , PlayerStats(..)
  , Player(..)
  , newPlayer
  , updatePlayerStats
  ) where

import Data.Aeson (FromJSON, ToJSON, object, (.=), (.:), withObject)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Player unique identifier
type PlayerId = Text

-- | Player display name
type PlayerName = Text

-- | Player statistics
data PlayerStats = PlayerStats
  { gamesPlayed :: !Int
  , wins :: !Int
  , losses :: !Int
  } deriving (Show, Eq, Generic)

instance ToJSON PlayerStats
instance FromJSON PlayerStats

-- | Player data
data Player = Player
  { playerId :: !PlayerId
  , playerName :: !PlayerName
  , stats :: !PlayerStats
  } deriving (Show, Eq, Generic)

instance ToJSON Player
instance FromJSON Player

-- | Create a new player with default stats
newPlayer :: PlayerId -> PlayerName -> Player
newPlayer pid pname = Player
  { playerId = pid
  , playerName = pname
  , stats = PlayerStats 0 0 0
  }

-- | Update player stats after a game
updatePlayerStats :: Bool -> Player -> Player
updatePlayerStats won player = player
  { stats = newStats
  }
  where
    oldStats = stats player
    newStats = oldStats
      { gamesPlayed = gamesPlayed oldStats + 1
      , wins = wins oldStats + if won then 1 else 0
      , losses = losses oldStats + if won then 0 else 1
      }
