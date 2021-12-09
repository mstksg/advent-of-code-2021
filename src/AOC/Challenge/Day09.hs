-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:

module AOC.Challenge.Day09 (
    day09a
  , day09b
  ) where

import           AOC.Common       (freqs, digitToIntSafe)
import           AOC.Common.Point (Point, cardinalNeighbs, parseAsciiMap)
import           AOC.Solver       ((:~>)(..))
import           Control.Monad    (mfilter)
import           Data.Foldable    (toList)
import           Data.List        (sortBy)
import           Data.Map         (Map)
import           Data.Maybe       (mapMaybe)
import           Data.Ord         (comparing)
import           Safe.Foldable    (minimumByMay)
import qualified Data.Map         as M

-- | Find low points and their heights
findLows :: Map Point Int -> [(Point, Int)]
findLows mp = filter go . M.toList $ mp
  where
    go (p, i) = all isLow (cardinalNeighbs p)
      where
        isLow q = case M.lookup q mp of
          Nothing -> True
          Just j  -> j > i

day09a :: Map Point Int :~> Int
day09a = MkSol
    { sParse = Just . parseAsciiMap (mfilter (< 9) . digitToIntSafe)
    , sShow  = show
    , sSolve = Just . sum . map ((+ 1) . snd) . findLows
    }

day09b :: Map Point Int :~> Int
day09b = MkSol
    { sParse = Just . parseAsciiMap (mfilter (< 9) . digitToIntSafe)
    , sShow  = show
    , sSolve = \xs -> Just
        let -- map of points to their associated low points after flowing
            -- all the way downhill
            res = flip M.mapWithKey (flowMap xs) \p -> \case
              Nothing -> p
              Just q  -> res M.! q
        in  product . take 3 . sortBy (flip compare) . toList $ freqs res
    }

-- | Map of each point to the next point downhill.  If Nothing, then it's a low point.
flowMap :: Map Point Int -> Map Point (Maybe Point)
flowMap mp = M.mapWithKey go mp
  where
    go p i = fmap fst . minimumByMay (comparing snd) $
               mapMaybe getGrad (cardinalNeighbs p)
      where
        getGrad q = (q,) <$> mfilter (< i) (M.lookup q mp)
