{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Storage.Match
  ( MatchResult(..)
  , saveMatch
  , loadMatch
  , getMatchFilePath
  ) where

import Control.Exception (catch, IOException)
import Data.Aeson (FromJSON, ToJSON, encode, decode, object, (.=), (.:), withObject)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import Player.Types (PlayerId)
import Room.Types (RoomId, GameMode, RoomStatus(..))

-- | Match result data
data MatchResult = MatchResult
  { matchRoomId :: !RoomId
  , matchGameMode :: !GameMode
  , matchStatus :: !RoomStatus
  , winnerId :: !PlayerId
  , loserId :: !PlayerId
  } deriving (Show, Eq, Generic)

instance ToJSON MatchResult where
  toJSON m = object
    [ "roomId" .= matchRoomId m
    , "gameMode" .= matchGameMode m
    , "status" .= matchStatus m
    , "state" .= object
        [ "id_player_win" .= winnerId m
        , "id_player_lose" .= loserId m
        ]
    ]

instance FromJSON MatchResult where
  parseJSON = withObject "MatchResult" $ \v -> do
    rid <- v .: "roomId"
    gm <- v .: "gameMode"
    st <- v .: "status"
    state <- v .: "state"
    wId <- state .: "id_player_win"
    lId <- state .: "id_player_lose"
    return $ MatchResult rid gm st wId lId

-- | Base directory for match data
matchesDir :: FilePath
matchesDir = "data/matches"

-- | Get the file path for a match
getMatchFilePath :: RoomId -> FilePath
getMatchFilePath rid = matchesDir </> (unpack rid ++ ".json")

-- | Ensure the matches directory exists
ensureMatchesDir :: IO ()
ensureMatchesDir = createDirectoryIfMissing True matchesDir

-- | Save match result to file
saveMatch :: MatchResult -> IO (Either String ())
saveMatch match = do
  ensureMatchesDir
  let filePath = getMatchFilePath (matchRoomId match)
  catch
    (do
      BL.writeFile filePath (encode match)
      return $ Right ()
    )
    (\(e :: IOException) -> return $ Left $ "Failed to save match: " ++ show e)

-- | Load match result from file
loadMatch :: RoomId -> IO (Either String MatchResult)
loadMatch rid = do
  let filePath = getMatchFilePath rid
  exists <- doesFileExist filePath
  if not exists
    then return $ Left "Match not found"
    else catch
      (do
        content <- BL.readFile filePath
        case decode content of
          Just match -> return $ Right match
          Nothing -> return $ Left "Failed to parse match data"
      )
      (\(e :: IOException) -> return $ Left $ "Failed to load match: " ++ show e)
