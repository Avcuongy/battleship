{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module WebAPI
    ( LoginRequest(..)
    , LoginResponse(..)
    , loginHandler
    , logoutHandler
    , getCurrentPlayerHandler
    ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Control.Monad.IO.Class
import Lib
import Session

-- | Login request from frontend
data LoginRequest = LoginRequest
  { requestNickname :: Text
  , requestAvatarIndex :: Int -- 0: captain, 1: daden, 2: female
  } deriving (Show, Generic)

instance FromJSON LoginRequest where
  parseJSON = withObject "LoginRequest" $ \o -> LoginRequest
    <$> o .: "nickname"
    <*> o .: "avatarIndex"

-- | Login response to frontend
data LoginResponse = LoginResponse
  { responseSuccess :: Bool
  , responseMessage :: Text
  , responsePlayer :: Maybe Player
  , responseSessionId :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON LoginResponse where
  toJSON (LoginResponse success msg player sessionId) = object
    [ "success" .= success
    , "message" .= msg
    , "player" .= player
    , "sessionId" .= sessionId
    ]

-- | Available avatars (matching frontend paths)
avatarPaths :: [Text]
avatarPaths = 
  [ "../../assets/images/captain.jpg"
  , "../../assets/images/daden.jpg"
  , "../../assets/images/female.jpg"
  ]

-- | Get avatar path by index with fallback
getAvatarPath :: Int -> Text
getAvatarPath idx 
  | idx >= 0 && idx < length avatarPaths = avatarPaths !! idx
  | otherwise = head avatarPaths -- Default to captain

-- | Handle login request
loginHandler :: LoginRequest -> IO LoginResponse
loginHandler (LoginRequest nickname avatarIdx) = do
  -- Validate nickname
  if not (validatePlayerName nickname)
    then return $ LoginResponse False "Invalid nickname format" Nothing Nothing
    else do
      -- Generate player ID
      playerId <- generatePlayerId
      
      -- Create player object
      let avatar = getAvatarPath avatarIdx
      let player = Player playerId nickname avatar
      
      -- Save player to file
      saveResult <- savePlayer player
      case saveResult of
        Left err -> return $ LoginResponse False (T.pack err) Nothing Nothing
        Right _ -> do
          -- Create session
          sessionId <- createSession player
          
          return $ LoginResponse True "Login successful" (Just player) (Just sessionId)

-- | Handle logout request
logoutHandler :: Text -> IO LoginResponse
logoutHandler sessionId = do
  -- Get player from session
  maybePlayer <- getSession sessionId
  case maybePlayer of
    Nothing -> return $ LoginResponse False "Invalid session" Nothing Nothing
    Just player -> do
      -- Delete player file
      deleteResult <- deletePlayer (playerId player)
      
      -- Clear session
      clearSession sessionId
      
      case deleteResult of
        Left err -> return $ LoginResponse True 
          ("Logged out but failed to cleanup: " <> T.pack err) Nothing Nothing
        Right _ -> return $ LoginResponse True "Logged out successfully" Nothing Nothing

-- | Get current player from session
getCurrentPlayerHandler :: Text -> IO (Maybe Player)
getCurrentPlayerHandler = getSession

-- | Example usage functions for testing
testLogin :: IO ()
testLogin = do
  let request = LoginRequest "TestPlayer" 0
  response <- loginHandler request
  print response

testLogout :: Text -> IO ()
testLogout sessionId = do
  response <- logoutHandler sessionId
  print response