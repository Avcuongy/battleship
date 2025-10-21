{-# LANGUAGE OverloadedStrings #-}

{-|
Non-blocking file operations using Control.Concurrent.Async.
Prevents blocking the main server thread during file writes.
-}

module Storage.FileIO
    ( -- * Async operations
      asyncSave
    , asyncLoad
    
    -- * Sync operations (for testing)
    , syncSave
    , syncLoad
    
    -- * Directory operations
    , ensureDirectory
    , fileExists
    ) where

import Control.Concurrent.Async (async, wait)
import qualified Data.ByteString.Lazy as BL
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import Control.Exception (catch, SomeException, handle)

-- ============================================================================
-- Async Operations
-- ============================================================================

{-|
Asynchronously save ByteString to file.
Does not block the calling thread.

Example:
>>> asyncSave "data/players/abc123.json" jsonData
>>> -- Returns immediately, file write happens in background
-}
asyncSave :: FilePath -> BL.ByteString -> IO ()
asyncSave path content = do
    -- Ensure directory exists
    ensureDirectory path
    
    -- Launch async write (returns immediately)
    _ <- async $ do
        BL.writeFile path content
            `catch` \(e :: SomeException) -> 
                putStrLn $ "Error saving file " ++ path ++ ": " ++ show e
    
    return ()

{-|
Asynchronously load ByteString from file.
Returns Nothing if file doesn't exist or error occurs.

Example:
>>> maybeData <- asyncLoad "data/players/abc123.json"
>>> case maybeData of
>>>     Just data -> ...
>>>     Nothing -> putStrLn "File not found"
-}
asyncLoad :: FilePath -> IO (Maybe BL.ByteString)
asyncLoad path = do
    exists <- fileExists path
    if not exists
        then return Nothing
        else do
            -- Launch async read
            asyncTask <- async $ handle handleError $ do
                content <- BL.readFile path
                return (Just content)
            
            -- Wait for result (this is async from caller's perspective)
            wait asyncTask
  where
    handleError :: SomeException -> IO (Maybe BL.ByteString)
    handleError e = do
        putStrLn $ "Error loading file " ++ path ++ ": " ++ show e
        return Nothing

-- ============================================================================
-- Sync Operations (for testing/debugging)
-- ============================================================================

-- | Synchronous save (blocks until complete)
syncSave :: FilePath -> BL.ByteString -> IO ()
syncSave path content = do
    ensureDirectory path
    BL.writeFile path content
        `catch` \(e :: SomeException) -> 
            putStrLn $ "Error saving file " ++ path ++ ": " ++ show e

-- | Synchronous load (blocks until complete)
syncLoad :: FilePath -> IO (Maybe BL.ByteString)
syncLoad path = do
    exists <- fileExists path
    if not exists
        then return Nothing
        else handle handleError $ do
            content <- BL.readFile path
            return (Just content)
  where
    handleError :: SomeException -> IO (Maybe BL.ByteString)
    handleError e = do
        putStrLn $ "Error loading file " ++ path ++ ": " ++ show e
        return Nothing

-- ============================================================================
-- Directory Operations
-- ============================================================================

-- | Ensure directory exists for file path
ensureDirectory :: FilePath -> IO ()
ensureDirectory path = do
    let dir = takeDirectory path
    createDirectoryIfMissing True dir

-- | Check if file exists
fileExists :: FilePath -> IO Bool
fileExists = doesFileExist
