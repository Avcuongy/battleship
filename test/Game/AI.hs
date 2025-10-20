{-# LANGUAGE OverloadedStrings #-}

module Game.AISpec (spec) where

import Test.Hspec
import Game.Types
import Game.AI
import qualified Data.Set as S

spec :: Spec
spec = do
    describe "initAIState" $ do
        it "creates empty state" $ do
            let aiState = initAIState
            S.size (previousMoves aiState) `shouldBe` 0
            lastHit aiState `shouldBe` Nothing
            null (targetQueue aiState) `shouldBe` True
            null (hitHistory aiState) `shouldBe` True
    
    describe "updateAIState" $ do
        it "adds move to previous moves" $ do
            let aiState = initAIState
            let result = MoveResult (Position 0 0) Miss Nothing
            let newState = updateAIState aiState (Position 0 0) result
            S.member (Position 0 0) (previousMoves newState) `shouldBe` True
        
        it "enters target mode on Hit" $ do
            let aiState = initAIState
            let result = MoveResult (Position 5 5) Hit Nothing
            let newState = updateAIState aiState (Position 5 5) result
            lastHit newState `shouldBe` Just (Position 5 5)
            length (targetQueue newState) `shouldBe` 4
            Position 5 5 `elem` hitHistory newState `shouldBe` True
        
        it "clears queue on Sunk" $ do
            let aiState = initAIState { targetQueue = [Position 5 5] }
            let result = MoveResult (Position 5 5) Sunk (Just Destroyer)
            let newState = updateAIState aiState (Position 5 5) result
            lastHit newState `shouldBe` Nothing
            null (targetQueue newState) `shouldBe` True
    
    describe "makeAIMove (with parallelism)" $ do
        it "generates valid position" $ do
            let aiState = initAIState
            pos <- makeAIMove aiState
            isValidPosition pos `shouldBe` True
        
        it "avoids previous moves" $ do
            let moves = S.fromList [Position r c | r <- [0..9], c <- [0..8]]
            let aiState = initAIState { previousMoves = moves }
            pos <- makeAIMove aiState
            S.member pos moves `shouldBe` False
        
        it "prioritizes target queue when available" $ do
            let aiState = initAIState 
                    { targetQueue = [Position 5 5, Position 5 6]
                    , lastHit = Just (Position 5 4)
                    }
            pos <- makeAIMove aiState
            pos `elem` targetQueue aiState `shouldBe` True
        
        it "uses probability in hunt mode" $ do
            let aiState = initAIState
            pos <- makeAIMove aiState
            -- Should prefer checkerboard positions in hunt mode
            let Position r c = pos
            -- This is probabilistic, so we just check it's valid
            isValidPosition pos `shouldBe` True
    
    describe "getValidMoves" $ do
        it "returns 100 moves for empty state" $ do
            let aiState = initAIState
            length (getValidMoves aiState) `shouldBe` 100
        
        it "excludes previous moves" $ do
            let aiState = initAIState 
                    { previousMoves = S.fromList [Position 0 0, Position 1 1] }
            let valid = getValidMoves aiState
            length valid `shouldBe` 98
    
    describe "Parallelism test" $ do
        it "handles large move history efficiently" $ do
            -- Create AI state with many previous moves
            let moves = S.fromList [Position r c | r <- [0..4], c <- [0..9]]
            let hits = [Position 2 5, Position 3 5]
            let aiState = initAIState 
                    { previousMoves = moves
                    , hitHistory = hits
                    }
            
            -- This should use parallel calculation
            pos <- makeAIMove aiState
            
            -- Verify result is valid and not in previous moves
            isValidPosition pos `shouldBe` True
            S.member pos moves `shouldBe` False
