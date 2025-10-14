{-# LANGUAGE OverloadedStrings #-}

module Player.Manager
  ( createPlayer
  , getPlayer
  , updatePlayer
  , recordGameResult
  ) where

import Data.Text (Text)
import Player.Types (Player, PlayerId, PlayerName, newPlayer, updatePlayerStats)
import Storage.Player (savePlayer, loadPlayer, playerExists)

-- | Create a new player
createPlayer :: PlayerId -> PlayerName -> IO (Either String Player)
createPlayer pid pname = do
  exists <- playerExists pid
  if exists
    then return $ Left "Player already exists"
    else do
      let player = newPlayer pid pname
      result <- savePlayer player
      case result of
        Right () -> return $ Right player
        Left err -> return $ Left err

-- | Get player data
getPlayer :: PlayerId -> IO (Either String Player)
getPlayer = loadPlayer

-- | Update player data
updatePlayer :: Player -> IO (Either String ())
updatePlayer = savePlayer

-- | Record game result for a player
recordGameResult :: PlayerId -> Bool -> IO (Either String Player)
recordGameResult pid won = do
  result <- loadPlayer pid
  case result of
    Left err -> return $ Left err
    Right player -> do
      let updatedPlayer = updatePlayerStats won player
      saveResult <- savePlayer updatedPlayer
      case saveResult of
        Right () -> return $ Right updatedPlayer
        Left err -> return $ Left err
