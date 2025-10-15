{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage.Player
  ( savePlayer
  , loadPlayer
  , playerExists
  , getPlayerFilePath
  ) where

import Control.Exception (catch, IOException)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text, unpack)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))

import Player.Types (Player, PlayerId, playerId)

-- | Base directory for player data
playersDir :: FilePath
playersDir = "data/players"

-- | Get the file path for a player
getPlayerFilePath :: PlayerId -> FilePath
getPlayerFilePath pid = playersDir </> (unpack pid ++ ".json")

-- | Ensure the players directory exists
ensurePlayersDir :: IO ()
ensurePlayersDir = createDirectoryIfMissing True playersDir

-- | Save player data to file
savePlayer :: Player -> IO (Either String ())
savePlayer player = do
  ensurePlayersDir
  let filePath = getPlayerFilePath (playerId player)
  catch
    (do
      BL.writeFile filePath (encode player)
      return $ Right ()
    )
    (\(e :: IOException) -> return $ Left $ "Failed to save player: " ++ show e)

-- | Load player data from file
loadPlayer :: PlayerId -> IO (Either String Player)
loadPlayer pid = do
  let filePath = getPlayerFilePath pid
  exists <- doesFileExist filePath
  if not exists
    then return $ Left "Player not found"
    else catch
      (do
        content <- BL.readFile filePath
        case decode content of
          Just player -> return $ Right player
          Nothing -> return $ Left "Failed to parse player data"
      )
      (\(e :: IOException) -> return $ Left $ "Failed to load player: " ++ show e)

-- | Check if a player exists
playerExists :: PlayerId -> IO Bool
playerExists pid = doesFileExist (getPlayerFilePath pid)
