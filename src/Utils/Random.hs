{-# LANGUAGE OverloadedStrings #-}

{-|
Generate random IDs for rooms/players and random positions for AI.
Includes collision detection for unique ID generation.
-}

module Utils.Random
    ( -- * ID generation
      randomId
    , randomIdIO
    , generateUniqueId
    
    -- * Position generation
    , randomPosition
    , randomPositionInBounds
    
    -- * Character sets
    , alphaChars
    , alphaNumChars
    ) where

import System.Random (randomRIO)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T

import Game.Types (Position(..))

-- ============================================================================
-- Character Sets
-- ============================================================================

-- | Alphabet characters a-zA-Z
alphaChars :: String
alphaChars = ['a'..'z'] ++ ['A'..'Z']

-- | Alphanumeric characters a-zA-Z0-9
alphaNumChars :: String
alphaNumChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- ============================================================================
-- ID Generation
-- ============================================================================

{-|
Generate random ID of specified length using a-zA-Z.

Example:
>>> randomId 6
"AbCdEf"

Note: Case-sensitive (A ≠ a)
-}
randomId :: Int -> IO String
randomId len = sequence $ replicate len randomChar
  where
    randomChar :: IO Char
    randomChar = do
        idx <- randomRIO (0, length alphaChars - 1)
        return (alphaChars !! idx)

{-|
Generate random ID and convert to Text.

Example:
>>> randomIdIO 6
"XyZ123"
-}
randomIdIO :: Int -> IO Text
randomIdIO len = T.pack <$> randomId len

{-|
Generate unique ID with collision detection.
Uses STM TVar to track used IDs.

Example:
>>> usedIds <- newTVarIO Set.empty
>>> id1 <- generateUniqueId usedIds 6
>>> id2 <- generateUniqueId usedIds 6
>>> id1 /= id2
True

This ensures no two IDs are the same (handles collision automatically).
-}
generateUniqueId :: TVar (Set String) -> Int -> IO String
generateUniqueId usedIdsVar len = do
    candidate <- randomId len
    
    -- Check if ID already exists
    exists <- atomically $ do
        usedIds <- readTVar usedIdsVar
        return $ Set.member candidate usedIds
    
    if exists
        then generateUniqueId usedIdsVar len  -- Retry with new ID
        else do
            -- Mark as used
            atomically $ do
                usedIds <- readTVar usedIdsVar
                writeTVar usedIdsVar (Set.insert candidate usedIds)
            return candidate

-- ============================================================================
-- Position Generation (for AI)
-- ============================================================================

{-|
Generate random position (0-9 for row and col).

Example:
>>> randomPosition
Position {posRow = 5, posCol = 3}
-}
randomPosition :: IO Position
randomPosition = do
    row <- randomRIO (0, 9)
    col <- randomRIO (0, 9)
    return $ Position row col

{-|
Generate random position within specific bounds.

Example:
>>> randomPositionInBounds 5 5  -- 5x5 grid
Position {posRow = 2, posCol = 4}
-}
randomPositionInBounds :: Int -> Int -> IO Position
randomPositionInBounds maxRow maxCol = do
    row <- randomRIO (0, maxRow - 1)
    col <- randomRIO (0, maxCol - 1)
    return $ Position row col

-- ============================================================================
-- Other Random Utilities
-- ============================================================================

{-|
Generate random boolean (for testing).

Example:
>>> randomBool
True
-}
randomBool :: IO Bool
randomBool = do
    val <- randomRIO (0, 1) :: IO Int
    return (val == 1)

{-|
Pick random element from list.

Example:
>>> randomChoice ['a', 'b', 'c']
'b'
-}
randomChoice :: [a] -> IO a
randomChoice xs
    | null xs = error "randomChoice: empty list"
    | otherwise = do
        idx <- randomRIO (0, length xs - 1)
        return (xs !! idx)

{-|
Shuffle list (Fisher-Yates algorithm).

Example:
>>> shuffle [1,2,3,4,5]
[3,1,5,2,4]
-}
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    let len = length xs
    idx <- randomRIO (0, len - 1)
    let (before, pivot:after) = splitAt idx xs
    rest <- shuffle (before ++ after)
    return (pivot : rest)
