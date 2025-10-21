{-# LANGUAGE OverloadedStrings #-}

module Game.AI
    ( -- * AI fleet
      aiDefaultFleet
    
    -- * AI moves
    , chooseAIMove
    ) where

import Game.Types
import Game.Ship (makeShip)
import Game.Board (getUnattackedCells)
import System.Random (randomRIO)

-- ============================================================================
-- AI Fleet (Fixed Preset)
-- ============================================================================

{-|
AI ships are ALWAYS placed at these positions:
  * Destroyer:   A1-A2 (Horizontal)
  * Submarine:   C3-C5 (Vertical)
  * Cruiser:     F1-F3 (Horizontal)
  * Battleship:  H5-H8 (Vertical)
  * Carrier:     B8-F8 (Horizontal)

This avoids needing to validate AI placement and simplifies implementation.
-}
aiDefaultFleet :: Fleet
aiDefaultFleet =
    [ makeShip Destroyer  (Position 0 0) Horizontal  -- A1-A2
    , makeShip Submarine  (Position 2 2) Vertical    -- C3-C5
    , makeShip Cruiser    (Position 5 0) Horizontal  -- F1-F3
    , makeShip Battleship (Position 7 4) Vertical    -- H5-H8
    , makeShip Carrier    (Position 1 7) Horizontal  -- B8-F8
    ]

-- Visual representation:
--    1 2 3 4 5 6 7 8 9 10
-- A  D D . . . . . . . .
-- B  . . . . . . . C C C C C
-- C  . . S . . . . . . .
-- D  . . S . . . . . . .
-- E  . . S . . . . . . .
-- F  R R R . . . . . . .
-- G  . . . . . . . . . .
-- H  . . . . B . . . . .
-- I  . . . . B . . . . .
-- J  . . . . B . . . . .
--           B

-- ============================================================================
-- AI Move Selection
-- ============================================================================

{-|
Choose random position from unattacked cells.

Simple strategy:
  1. Get all cells that haven't been attacked
  2. Pick one randomly
  3. Return immediately (no timer for AI)

Future improvements (not implemented):
  * Hunt mode: After hit, attack adjacent cells
  * Probability map: Prefer cells where ships are likely
  * Pattern recognition: Avoid impossible positions
-}
chooseAIMove :: Board -> IO Position
chooseAIMove board = do
    let available = getUnattackedCells board
    
    if null available
        then error "AI: No unattacked cells available (should not happen)"
        else do
            -- Pick random index
            idx <- randomRIO (0, length available - 1)
            return (available !! idx)
