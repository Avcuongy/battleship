{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Storage.Room where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import System.Directory
import Control.Concurrent (forkIO)
import qualified Storage.Player as Player

-- Types
type RoomId = Text

data PlayerInfo = PlayerInfo
  { playerId :: Text
  , playerName :: Text
  , ready :: Bool
  } deriving (Show, Generic)

instance ToJSON PlayerInfo where
  toJSON (PlayerInfo i n r) = object ["id" .= i, "name" .= n, "ready" .= r]
instance FromJSON PlayerInfo where
  parseJSON = withObject "PlayerInfo" $ \v -> 
    PlayerInfo <$> v .: "id" <*> v .: "name" <*> v .: "ready"

data Room = Room
  { gameMode :: Text        -- "1vs1" or "ai"
  , status :: Text          -- "ready", "in_progress", "done"
  , player1 :: PlayerInfo
  , player2 :: PlayerInfo
  } deriving (Show, Generic)

instance ToJSON Room where
  toJSON (Room mode stat p1 p2) = object
    [ "gameMode" .= mode
    , "status" .= stat
    , "players" .= object ["player1" .= p1, "player2" .= p2]
    ]

instance FromJSON Room where
  parseJSON = withObject "Room" $ \v -> do
    mode <- v .: "gameMode"
    stat <- v .: "status"
    ps <- v .: "players"
    p1 <- ps .: "player1"
    p2 <- ps .: "player2"
    return $ Room mode stat p1 p2

-- Constants
roomDir :: FilePath
roomDir = "server/data/rooms"

-- Simple API
save :: RoomId -> Room -> IO ()
save roomId room = do
  createDirectoryIfMissing True roomDir
  BL.writeFile (roomDir <> "/" <> T.unpack roomId <> ".json") (encode room)

load :: RoomId -> IO (Maybe Room)
load roomId = do
  let path = roomDir <> "/" <> T.unpack roomId <> ".json"
  exists <- doesFileExist path
  if exists then decode <$> BL.readFile path else return Nothing

delete :: RoomId -> IO ()
delete roomId = do
  let path = roomDir <> "/" <> T.unpack roomId <> ".json"
  exists <- doesFileExist path
  when exists $ removeFile path

-- High-level helpers
create :: RoomId -> Text -> Text -> IO ()
create roomId pid pname = 
  save roomId $ Room "1vs1" "ready" 
    (PlayerInfo pid pname False) 
    (PlayerInfo "" "" False)

-- When game ends: update stats and cleanup
finalize :: RoomId -> Text -> Text -> IO ()
finalize roomId winnerId loserId = do
  _ <- forkIO $ do
    Player.wonGame winnerId
    Player.lostGame loserId
  delete roomId
