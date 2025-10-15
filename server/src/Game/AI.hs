{-# LANGUAGE OverloadedStrings #-}

module Game.AI
  ( generateAIShips
  , getAIMove
  ) where

import Control.Monad (foldM)
import System.Random (randomRIO)

import Game.Types
  ( Position
  , ShipType
  , Ship(..)
  , Board
  )
import Game.Board (emptyBoard, placeShip, getShipPositions)
import Game.Ship (allShipTypes, createShip)

-- | Generate random ship placements for AI
generateAIShips :: IO (Either String [Ship])
generateAIShips = do
  result <- foldM placeRandomShip (Right (emptyBoard, [])) allShipTypes
  case result of
    Left err -> return $ Left err
    Right (_, ships) -> return $ Right ships

-- | Place a random ship on the board
placeRandomShip :: Either String (Board, [Ship]) -> ShipType -> IO (Either String (Board, [Ship]))
placeRandomShip (Left err) _ = return $ Left err
placeRandomShip (Right (board, ships)) shipType = do
  tryPlacement board ships 0
  where
    tryPlacement :: Board -> [Ship] -> Int -> IO (Either String (Board, [Ship]))
    tryPlacement b s attempts
      | attempts >= 100 = return $ Left "Failed to place AI ships after 100 attempts"
      | otherwise = do
          row <- randomRIO (0, 9)
          col <- randomRIO (0, 9)
          isHoriz <- randomRIO (True, False)
          let ship = createShip shipType (row, col) isHoriz
          case placeShip b ship of
            Right newBoard -> return $ Right (newBoard, ship : s)
            Left _ -> tryPlacement b s (attempts + 1)

-- | Get AI's next move
getAIMove :: [Position] -> IO Position
getAIMove shotPositions = do
  pos <- randomPosition
  if pos `elem` shotPositions
    then getAIMove shotPositions
    else return pos
  where
    randomPosition :: IO Position
    randomPosition = do
      row <- randomRIO (0, 9)
      col <- randomRIO (0, 9)
      return (row, col)
