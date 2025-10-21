{-# LANGUAGE OverloadedStrings #-}

{-|
Wrapper functions for STM operations to simplify usage throughout the codebase.
All operations are ATOMIC (thread-safe).
-}

module State.Sync
    ( -- * Basic STM operations
      atomicRead
    , atomicWrite
    , atomicModify
    
    -- * Conditional operations
    , atomicModifyM
    , atomicUpdate
    
    -- * Query operations
    , atomicLookup
    , atomicMember
    ) where

import Control.Concurrent.STM (TVar, STM, atomically, readTVar, writeTVar, modifyTVar')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- ============================================================================
-- Basic Operations
-- ============================================================================

-- | Atomically read from TVar
atomicRead :: TVar a -> IO a
atomicRead = atomically . readTVar
{-# INLINE atomicRead #-}

-- | Atomically write to TVar
atomicWrite :: TVar a -> a -> IO ()
atomicWrite var val = atomically $ writeTVar var val
{-# INLINE atomicWrite #-}

-- | Atomically modify TVar with strict evaluation
atomicModify :: TVar a -> (a -> a) -> IO ()
atomicModify var f = atomically $ modifyTVar' var f
{-# INLINE atomicModify #-}

-- ============================================================================
-- Advanced Operations
-- ============================================================================

-- | Atomically modify TVar with monadic computation
atomicModifyM :: TVar a -> (a -> IO a) -> IO ()
atomicModifyM var f = do
    old <- atomicRead var
    new <- f old
    atomicWrite var new

-- | Atomically update TVar and return result
atomicUpdate :: TVar a -> (a -> (a, b)) -> IO b
atomicUpdate var f = atomically $ do
    old <- readTVar var
    let (new, result) = f old
    writeTVar var new
    return result

-- ============================================================================
-- Map Operations
-- ============================================================================

-- | Atomically lookup in Map TVar
atomicLookup :: Ord k => k -> TVar (Map k v) -> IO (Maybe v)
atomicLookup key var = atomically $ do
    m <- readTVar var
    return $ Map.lookup key m
{-# INLINE atomicLookup #-}

-- | Atomically check membership in Map TVar
atomicMember :: Ord k => k -> TVar (Map k v) -> IO Bool
atomicMember key var = atomically $ do
    m <- readTVar var
    return $ Map.member key m
{-# INLINE atomicMember #-}
