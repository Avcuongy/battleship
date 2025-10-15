{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Storage.Player where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import System.Directory

-- Types
type PlayerId = Text

data PlayerStats = PlayerStats
  { gamesPlayed :: Int
  , wins :: Int
  , losses :: Int
  } deriving (Show, Generic)

instance ToJSON PlayerStats
instance FromJSON PlayerStats

data Player = Player
  { name :: Text
  , stats :: PlayerStats
  } deriving (Show, Generic)

instance ToJSON Player
instance FromJSON Player

-- Constants
playerDir :: FilePath
playerDir = "server/data/players"

-- Simple API
save :: PlayerId -> Player -> IO ()
save playerId player = do
  createDirectoryIfMissing True playerDir
  BL.writeFile (playerDir <> "/" <> T.unpack playerId <> ".json") (encode player)

load :: PlayerId -> IO (Maybe Player)
load playerId = do
  let path = playerDir <> "/" <> T.unpack playerId <> ".json"
  exists <- doesFileExist path
  if exists then decode <$> BL.readFile path else return Nothing

-- Update stats after game
wonGame :: PlayerId -> IO ()
wonGame pid = updateStats pid True

lostGame :: PlayerId -> IO ()
lostGame pid = updateStats pid False

updateStats :: PlayerId -> Bool -> IO ()
updateStats pid won = do
  mp <- load pid
  case mp of
    Just (Player n (PlayerStats p w l)) -> 
      save pid (Player n (PlayerStats (p+1) (if won then w+1 else w) (if won then l else l+1)))
    Nothing -> return ()
