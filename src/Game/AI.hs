{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Game.AI
    ( -- * Types
      AIState(..)
    
    -- * AI State Management
    , initAIState
    , updateAIState
    
    -- * AI Move (with parallelism)
    , makeAIMove
    
    -- * Helpers
    , getValidMoves
    , calculateMoveQuality
    ) where

import Game.Types
import Game.Board
import System.Random
import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Parallel.Strategies
import Control.DeepSeq
import qualified Data.Set as S

-- ===== TYPES =====

-- | AI state to track previous moves and strategy
data AIState = AIState
    { previousMoves :: !(S.Set Position)   -- ^ Moves already made (Set for O(1) lookup)
    , lastHit       :: !(Maybe Position)   -- ^ Last successful hit
    , targetQueue   :: ![Position]         -- ^ Adjacent cells to check next
    , hitHistory    :: ![Position]         -- ^ All successful hits (for density calculation)
    } deriving (Show)

-- NFData instances for parallelism
instance NFData Position where
    rnf (Position r c) = r `seq` c `seq` ()

-- ===== AI STATE MANAGEMENT =====

-- | Initialize AI state at game start
initAIState :: AIState
initAIState = AIState
    { previousMoves = S.empty
    , lastHit = Nothing
    , targetQueue = []
    , hitHistory = []
    }

-- | Update AI state after a move based on result
updateAIState :: AIState -> Position -> MoveResult -> AIState
updateAIState state pos result =
    let newMoves = S.insert pos (previousMoves state)
    in case moveResult result of
        Hit -> 
            -- Ship hit but not sunk: enter target mode
            state
                { previousMoves = newMoves
                , lastHit = Just pos
                , targetQueue = getUntriedAdjacent state pos
                , hitHistory = pos : hitHistory state
                }
        Sunk -> 
            -- Ship sunk: return to hunt mode
            state
                { previousMoves = newMoves
                , lastHit = Nothing
                , targetQueue = []  -- Clear queue after sinking ship
                , hitHistory = pos : hitHistory state
                }
        Miss -> 
            -- Miss: remove position from queue and continue
            state
                { previousMoves = newMoves
                , targetQueue = filter (/= pos) (targetQueue state)
                }
        _ -> state { previousMoves = newMoves }

-- ===== AI STRATEGY WITH PARALLELISM =====

-- | Make AI move (automatically uses parallelism when appropriate)
makeAIMove :: AIState -> IO Position
makeAIMove aiState
    -- Target mode: attack adjacent to last hit (high priority)
    | not (null validTargets) = 
        return $ chooseBestTarget aiState validTargets
    
    -- Hunt mode: use PARALLEL probability calculation for all positions
    | otherwise = 
        return $ huntWithProbability aiState
  where
    validTargets = targetQueue aiState

-- | Choose best target from queue using parallel scoring
chooseBestTarget :: AIState -> [Position] -> Position
chooseBestTarget aiState targets =
    let -- ===== PARALLEL: Score all targets simultaneously =====
        scores = map (targetScore aiState) targets
            `using` parList rdeepseq
        
        posWithScores = zip targets scores
        bestPos = fst $ maximumBy (comparing snd) posWithScores
    in bestPos

-- | Hunt mode with PARALLEL probability calculation
huntWithProbability :: AIState -> Position
huntWithProbability aiState =
    let available = getValidMoves aiState
        
        -- ===== PARALLELISM: Calculate probability for ALL positions in parallel =====
        probabilities = map (calculateProbability aiState) available
            `using` parList rdeepseq
        
        posWithProbs = zip available probabilities
        bestPos = fst $ maximumBy (comparing snd) posWithProbs
    
    in bestPos

-- ===== SCORING FUNCTIONS =====

-- | Calculate probability score for a position (used in hunt mode)
calculateProbability :: AIState -> Position -> Double
calculateProbability aiState pos =
    let patternScore = checkerboardScore pos
        edgeScore = edgePenalty pos
        densityScore = hitDensity aiState pos
        centerScore = centerBonus pos
    in patternScore * 0.4 + edgeScore * 0.2 + densityScore * 0.3 + centerScore * 0.1

-- | Score a target position (used in target mode)
targetScore :: AIState -> Position -> Double
targetScore aiState pos =
    let adjacencyScore = adjacencyBonus aiState pos
        centerScore = centerBonus pos
    in adjacencyScore * 0.7 + centerScore * 0.3

-- | Checkerboard pattern score (ships often found on checkerboard)
checkerboardScore :: Position -> Double
checkerboardScore (Position r c) =
    if (r + c) `mod` 2 == 0
        then 2.0  -- Strong preference for checkerboard pattern
        else 1.0

-- | Edge penalty (ships less likely at board edges)
edgePenalty :: Position -> Double
edgePenalty (Position r c)
    | r == 0 || r == 9 || c == 0 || c == 9 = 0.5  -- Edge penalty
    | r == 1 || r == 8 || c == 1 || c == 8 = 0.8  -- Near-edge penalty
    | otherwise = 1.0

-- | Hit density score (prefer areas near previous hits)
hitDensity :: AIState -> Position -> Double
hitDensity aiState pos =
    let hits = hitHistory aiState
        distances = map (manhattanDistance pos) hits
        closeHits = length $ filter (<= 3) distances
    in 1.0 + fromIntegral closeHits * 0.5

-- | Center bonus (ships more likely in center of board)
centerBonus :: Position -> Double
centerBonus (Position r c) =
    let distanceFromCenter = abs (r - 5) + abs (c - 5)
    in 1.0 + (10.0 - fromIntegral distanceFromCenter) * 0.05

-- | Adjacency bonus (for target mode - prioritize cells next to hits)
adjacencyBonus :: AIState -> Position -> Double
adjacencyBonus aiState pos =
    case lastHit aiState of
        Nothing -> 1.0
        Just hitPos ->
            let distance = manhattanDistance pos hitPos
            in case distance of
                1 -> 10.0  -- Immediately adjacent (very high priority)
                2 -> 3.0   -- Two steps away
                _ -> 1.0

-- | Manhattan distance between two positions
manhattanDistance :: Position -> Position -> Int
manhattanDistance (Position r1 c1) (Position r2 c2) =
    abs (r1 - r2) + abs (c1 - c2)

-- ===== HELPER FUNCTIONS =====

-- | Get all valid (untried) moves
getValidMoves :: AIState -> [Position]
getValidMoves aiState =
    let allPositions = [Position r c | r <- [0..9], c <- [0..9]]
    in filter (\p -> not $ S.member p (previousMoves aiState)) allPositions

-- | Get adjacent positions not yet tried
getUntriedAdjacent :: AIState -> Position -> [Position]
getUntriedAdjacent state pos =
    filter (\p -> isValidPosition p && not (S.member p (previousMoves state)))
           (getAdjacentPositions pos)

-- | Get adjacent positions (up, down, left, right only)
getAdjacentPositions :: Position -> [Position]
getAdjacentPositions (Position r c) =
    [ Position (r-1) c  -- Up
    , Position (r+1) c  -- Down
    , Position r (c-1)  -- Left
    , Position r (c+1)  -- Right
    ]

-- | Calculate move quality (0.0 to 1.0) for external evaluation
calculateMoveQuality :: AIState -> Position -> Double
calculateMoveQuality aiState pos =
    if not (null (targetQueue aiState))
        then targetScore aiState pos / 10.0  -- Normalize to 0-1
        else calculateProbability aiState pos / 2.0  -- Normalize to 0-1
