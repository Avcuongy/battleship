{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Player(..)
    , PlayerSession
    , savePlayer
    , loadPlayer
    , deletePlayer
    , generatePlayerId
    , validatePlayerName
    , someFunc
    ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics
import System.Directory
import System.FilePath
import Control.Exception
import Data.Char (isAlphaNum)
import System.Random
import qualified Data.ByteString.Lazy as LB

-- | Player data structure matching the JSON format
data Player = Player
  { playerId :: Text    -- ^ Format: 6 alphanumeric characters (no prefix)
  , playerName :: Text  -- ^ Nickname: 2-30 chars, a-z, A-Z, 0-9, _, -
  } deriving (Show, Eq, Generic)

-- | JSON serialization instances
instance ToJSON Player where
  toJSON (Player pid name) = object
    [ "id" .= pid
    , "name" .= name
    ]

instance FromJSON Player where
  parseJSON = withObject "Player" $ \o -> Player
    <$> o .: "id"
    <*> o .: "name"

-- | Type alias for session management
type PlayerSession = Player

-- | Players data directory
playersDir :: FilePath
playersDir = "data/players"

-- | Generate a 6-character alphanumeric player ID
generatePlayerId :: IO Text
generatePlayerId = do
  let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  randomChars <- sequence $ replicate 6 $ do
    idx <- randomRIO (0, length chars - 1)
    return $ chars !! idx
  return $ T.pack randomChars

-- | Validate player name according to requirements
validatePlayerName :: Text -> Bool
validatePlayerName name = 
  let len = T.length name
      validChars = T.all (\c -> isAlphaNum c || c == '_' || c == '-') name
  in len >= 2 && len <= 30 && validChars

-- | Get file path for a player
playerFilePath :: Text -> FilePath
playerFilePath pid = playersDir </> T.unpack pid <.> "json"

-- | Save player data to JSON file
savePlayer :: Player -> IO (Either String ())
savePlayer player = do
  let filepath = playerFilePath (playerId player)
  
  -- Ensure directory exists
  createDirectoryIfMissing True playersDir
  
  -- Validate player data
  if not (validatePlayerName (playerName player))
    then return $ Left "Invalid player name format"
    else do
      result <- try $ LB.writeFile filepath (encode player)
      case result of
        Left ex -> return $ Left $ "Failed to save player: " ++ show (ex :: IOException)
        Right _ -> return $ Right ()

-- | Load player data from JSON file
loadPlayer :: Text -> IO (Either String Player)
loadPlayer pid = do
  let filepath = playerFilePath pid
  
  exists <- doesFileExist filepath
  if not exists
    then return $ Left "Player file not found"
    else do
      result <- try $ LB.readFile filepath
      case result of
        Left ex -> return $ Left $ "Failed to read player file: " ++ show (ex :: IOException)
        Right content -> 
          case decode content of
            Nothing -> return $ Left "Invalid JSON format"
            Just player -> return $ Right player

-- | Delete player data file
deletePlayer :: Text -> IO (Either String ())
deletePlayer pid = do
  let filepath = playerFilePath pid
  
  exists <- doesFileExist filepath
  if not exists
    then return $ Left "Player file not found"
    else do
      result <- try $ removeFile filepath
      case result of
        Left ex -> return $ Left $ "Failed to delete player: " ++ show (ex :: IOException)
        Right _ -> return $ Right ()

-- | Legacy function
someFunc :: IO ()
someFunc = putStrLn "BattleShip Player Management System"
