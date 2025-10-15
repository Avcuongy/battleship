{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Storage.Player 
  ( -- * Types
    Player(..)
  , PlayerStats(..)
  , PlayerId
    -- * Operations
  , savePlayer
  , loadPlayer
  , updatePlayerStats
  , createNewPlayer
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import System.Directory (doesFileExist, createDirectoryIfMissing)

-- ============ TYPES ============

type PlayerId = Text

data PlayerStats = PlayerStats
  { gamesPlayed :: Int
  , wins :: Int
  , losses :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON PlayerStats
instance FromJSON PlayerStats

data Player = Player
  { name :: Text
  , stats :: PlayerStats
  } deriving (Show, Eq, Generic)

instance ToJSON Player
instance FromJSON Player

-- ============ OPERATIONS ============

-- Create new player with zero stats
createNewPlayer :: Text -> Player
createNewPlayer nickname = Player
  { name = nickname
  , stats = PlayerStats 0 0 0
  }

-- Save player to data/players/<id>.json
savePlayer :: PlayerId -> Player -> IO ()
savePlayer playerId player = do
  createDirectoryIfMissing True "data/players"
  let filepath = "data/players/" <> T.unpack playerId <> ".json"
  BL.writeFile filepath (encode player)

-- Load player from data/players/<id>.json
loadPlayer :: PlayerId -> IO (Maybe Player)
loadPlayer playerId = do
  let filepath = "data/players/" <> T.unpack playerId <> ".json"
  exists <- doesFileExist filepath
  if exists
    then decode <$> BL.readFile filepath
    else return Nothing

-- Update player stats after a match
-- won = True if player won, False if lost
updatePlayerStats :: PlayerId -> Bool -> IO ()
updatePlayerStats playerId won = do
  maybePlayer <- loadPlayer playerId
  case maybePlayer of
    Just (Player playerName (PlayerStats played w l)) -> do
      let newStats = PlayerStats
            { gamesPlayed = played + 1
            , wins = if won then w + 1 else w
            , losses = if won then l else l + 1
            }
      savePlayer playerId (Player playerName newStats)
    Nothing -> return ()  -- Player doesn't exist, do nothing
