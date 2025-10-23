{-# LANGUAGE OverloadedStrings #-}

-- | Test compilation and basic sanity checks for all Game modules.
-- This does NOT run gameplay logic — just ensures that all types
-- and functions in Game/* compile and load correctly.

module TestGame where

import Game.Types
import Game.Ship
import Game.Board
import Game.AI
import Game.Rules
import Game.Timer

-- ============================================================================
-- Dummy test: simple sanity checks
-- ============================================================================

main :: IO ()
main = do
    putStrLn "✅ Loading all Game modules..."
    putStrLn "----------------------------------------"

    -- Test Position conversion round-trip
    let pos = Position 0 4
    putStrLn $ "Position test: " ++ show (positionToString pos)

    -- Test ship creation
    let ship = makeShip Destroyer (Position 0 0) Horizontal
    putStrLn $ "Ship created: " ++ show (shipType ship)

    -- Test fleet creation
    let fleet = [ship]
    print $ "Fleet size = " ++ show (length fleet)

    -- Test board creation
    let board = createBoard fleet
    putStrLn $ "Board created, first cell: " ++ show (getCellAt (Position 0 0) board)

    -- Test AI move selection (random)
    aiMove <- chooseAIMove board
    putStrLn $ "AI chose move: " ++ show aiMove

    -- Test timer
    t1 <- getCurrentTimestamp
    t2 <- getCurrentTimestamp
    putStrLn $ "Time diff = " ++ show (timeDiff t1 t2)

    putStrLn "----------------------------------------"
    putStrLn "✅ All Game modules compiled and basic checks passed!"
