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
AI ships are ALWAYS placed at these positions (matching client ai-engine.js):
  * Carrier:     Row 0, Col 0-4 (Horizontal) - A1-A5
  * Battleship:  Row 2, Col 0-3 (Horizontal) - C1-C4
  * Cruiser:     Row 4, Col 0-2 (Horizontal) - E1-E3
  * Submarine:   Row 6, Col 0-2 (Horizontal) - G1-G3
  * Destroyer:   Row 8, Col 0-1 (Horizontal) - I1-I2

This matches the client-side AIEngine.aiFleet configuration.
-}
aiDefaultFleet :: Fleet
aiDefaultFleet =
    [ makeShip Carrier    (Position 0 0) Horizontal  -- Row 0: A1-A5 (5 cells)
    , makeShip Battleship (Position 2 0) Horizontal  -- Row 2: C1-C4 (4 cells)
    , makeShip Cruiser    (Position 4 0) Horizontal  -- Row 4: E1-E3 (3 cells)
    , makeShip Submarine  (Position 6 0) Horizontal  -- Row 6: G1-G3 (3 cells)
    , makeShip Destroyer  (Position 8 0) Horizontal  -- Row 8: I1-I2 (2 cells)
    ]

-- Visual representation (matching client):
--    0 1 2 3 4 5 6 7 8 9  (Col index)
-- 0  C C C C C . . . . .  (Row 0 = A)
-- 1  . . . . . . . . . .  (Row 1 = B)
-- 2  B B B B . . . . . .  (Row 2 = C)
-- 3  . . . . . . . . . .  (Row 3 = D)
-- 4  R R R . . . . . . .  (Row 4 = E)
-- 5  . . . . . . . . . .  (Row 5 = F)
-- 6  S S S . . . . . . .  (Row 6 = G)
-- 7  . . . . . . . . . .  (Row 7 = H)
-- 8  D D . . . . . . . .  (Row 8 = I)
-- 9  . . . . . . . . . .  (Row 9 = J)
--
-- Legend: C=Carrier, B=Battleship, R=CRuiser, S=Submarine, D=Destroyer

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
