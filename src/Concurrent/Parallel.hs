{-# LANGUAGE BangPatterns #-}

module Concurrent.Parallel
    ( -- * Parallel validation
      validateParallel
    
    -- * Generic parallel operations
    , parallelMap
    , parallelFilter
    , sequentialEval

    -- * Re-exports from Control.Parallel.Strategies
    , Strategy
    , rdeepseq
    , rpar
    , rseq
    ) where

import Control.Parallel.Strategies 
    ( Strategy
    , parMap
    , parListChunk
    , rdeepseq
    , rpar
    , rseq
    , using
    , withStrategy
    )
import Control.DeepSeq (NFData)

validateParallel :: NFData b => (a -> b) -> [a] -> [b]
validateParallel f xs = parMap rdeepseq f xs
{-# INLINE validateParallel #-}

parallelMap :: NFData b => Int -> (a -> b) -> [a] -> [b]
parallelMap chunkSize f xs = 
    withStrategy (parListChunk chunkSize rdeepseq) (map f xs)
{-# INLINE parallelMap #-}

parallelFilter :: NFData a => (a -> Bool) -> [a] -> [a]
parallelFilter predicate xs = 
    let results = parMap rdeepseq (\x -> (x, predicate x)) xs
        filtered = [x | (x, True) <- results]
    in filtered `using` rdeepseq
{-# INLINE parallelFilter #-}

sequentialEval :: NFData b => (a -> b) -> [a] -> [b]
sequentialEval f xs = map f xs `using` rdeepseq
{-# INLINE sequentialEval #-}
