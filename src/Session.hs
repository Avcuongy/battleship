{-# LANGUAGE OverloadedStrings #-}

module Session
    ( PlayerSession
    , createSession
    , getSession
    , clearSession
    , sessionMiddleware
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)
import Lib (Player(..), PlayerSession)
import Data.Time
import Data.UUID
import Data.UUID.V4 (nextRandom)
import System.IO.Unsafe (unsafePerformIO)

-- Import needed for unsafePerformIO
import System.IO.Unsafe (unsafePerformIO)

-- | Session storage type
type SessionStore = Map Text (UTCTime, Player)

-- | Global session store (in production, use Redis or database)
sessionStore :: IORef (Map Text (UTCTime, Player))
{-# NOINLINE sessionStore #-}
sessionStore = unsafePerformIO $ newIORef Map.empty

-- | Session timeout (30 minutes)
sessionTimeout :: NominalDiffTime
sessionTimeout = 30 * 60

-- | Create a new session for a player
createSession :: Player -> IO Text
createSession player = do
  sessionId <- T.pack . show <$> nextRandom
  now <- getCurrentTime
  atomicModifyIORef' sessionStore $ \store ->
    (Map.insert sessionId (now, player) store, ())
  return sessionId

-- | Get player from session
getSession :: Text -> IO (Maybe Player)
getSession sessionId = do
  now <- getCurrentTime
  store <- readIORef sessionStore
  case Map.lookup sessionId store of
    Nothing -> return Nothing
    Just (timestamp, player) -> 
      if diffUTCTime now timestamp > sessionTimeout
        then do
          -- Session expired, clean it up
          atomicModifyIORef' sessionStore $ \s -> 
            (Map.delete sessionId s, ())
          return Nothing
        else do
          -- Update timestamp
          atomicModifyIORef' sessionStore $ \s ->
            (Map.insert sessionId (now, player) s, ())
          return $ Just player

-- | Clear a session
clearSession :: Text -> IO ()
clearSession sessionId = do
  atomicModifyIORef' sessionStore $ \store ->
    (Map.delete sessionId store, ())

-- | Clean up expired sessions
cleanupExpiredSessions :: IO ()
cleanupExpiredSessions = do
  now <- getCurrentTime
  atomicModifyIORef' sessionStore $ \store ->
    let filtered = Map.filter (\(timestamp, _) -> 
          diffUTCTime now timestamp <= sessionTimeout) store
    in (filtered, ())

-- | Middleware placeholder for web frameworks
sessionMiddleware :: Text -> IO (Maybe Player)
sessionMiddleware = getSession