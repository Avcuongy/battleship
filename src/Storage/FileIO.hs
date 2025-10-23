{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Non-blocking file operations using Control.Concurrent.Async.
Prevents blocking the main server thread during file writes.
-}

module Storage.FileIO
    ( asyncSave
    , asyncLoad
    , syncSave
    , syncLoad
    , ensureDirectory
    , fileExists
    ) where

import Control.Concurrent.Async (async)
import qualified Data.ByteString.Lazy as BL
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import Control.Exception (catch, SomeException)
import Control.Monad (void)

-- Async save — fire and forget
asyncSave :: FilePath -> BL.ByteString -> IO ()
asyncSave path content = do
    ensureDirectory path
    void . async $
        BL.writeFile path content
            `catch` \(e :: SomeException) ->
                putStrLn $ "Error saving file " ++ path ++ ": " ++ show e

-- Async load (truly non-blocking, user handles IO action)
asyncLoad :: FilePath -> IO (IO (Maybe BL.ByteString))
asyncLoad path = do
    exists <- fileExists path
    if not exists then return (return Nothing)
    else do
        return $ catch (Just <$> BL.readFile path)
                       (\(e :: SomeException) -> 
                            putStrLn ("Error loading " ++ path ++ ": " ++ show e) >> return Nothing)

-- Sync save/load
syncSave :: FilePath -> BL.ByteString -> IO ()
syncSave path content = do
    ensureDirectory path
    BL.writeFile path content
        `catch` \(e :: SomeException) ->
            putStrLn $ "Error saving file " ++ path ++ ": " ++ show e

syncLoad :: FilePath -> IO (Maybe BL.ByteString)
syncLoad path = do
    exists <- fileExists path
    if not exists then return Nothing
    else catch (Just <$> BL.readFile path)
               (\(e :: SomeException) ->
                    putStrLn ("Error loading " ++ path ++ ": " ++ show e) >> return Nothing)

ensureDirectory :: FilePath -> IO ()
ensureDirectory = createDirectoryIfMissing True . takeDirectory

fileExists :: FilePath -> IO Bool
fileExists = doesFileExist
