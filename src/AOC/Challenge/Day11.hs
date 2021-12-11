-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day11 (
    day11a
  , day11b
  ) where

import           AOC.Common       (firstJust, freqs, (!!!), digitToIntSafe)
import           AOC.Common.Point (Point, fullNeighbs, parseAsciiMap)
import           AOC.Solver       ((:~>)(..))
import           Control.Monad    (guard)
import           Data.List        (unfoldr, scanl')
import           Data.Map         (Map)
import           Data.Set         (Set)
import qualified Data.Map         as M
import qualified Data.Set         as S

type SquidGame = Map Point Int
type FreqMap = Map Point Int

day11a :: SquidGame :~> Int
day11a = MkSol
    { sParse = Just . parseAsciiMap digitToIntSafe
    , sShow  = show
    , sSolve = Just . (!!! 100) . scanl' (+) 0 . map sum . doTheThing
    }

doTheThing :: SquidGame -> [FreqMap]
doTheThing = unfoldr (Just . fullStep)

fullStep :: SquidGame -> (FreqMap, SquidGame)
fullStep mp = (fl, mp'')
  where
    (fl, mp') = runAllFlashes $ fmap (+1) mp
    mp'' = (0 <$ fl) `M.union` mp'

runAllFlashes :: SquidGame -> (FreqMap, SquidGame)
runAllFlashes = go M.empty
  where
    go n mp
        | S.null fl = (n, mp')
        | otherwise = go (M.unionWith (+) (M.fromSet (const 1) fl) n) mp'
      where
        (fl, mp') = runFlash mp

runFlash :: SquidGame -> (Set Point, SquidGame)
runFlash mp = (readyToFlash, M.unionWith (+) neighbs noFlash)
  where
    (yesFlash, noFlash) = M.partition (> 9) mp
    readyToFlash = M.keysSet yesFlash
    neighbs = (`M.restrictKeys` M.keysSet noFlash) . freqs $ foldMap fullNeighbs readyToFlash

day11b :: SquidGame :~> Int
day11b = MkSol
    { sParse = sParse day11a
    , sShow  = show
    , sSolve = \mp ->
        firstJust (\(i, q) -> i <$ guard (M.keysSet q == M.keysSet mp))
          . zip [1..]
          . doTheThing
          $ mp
    }
