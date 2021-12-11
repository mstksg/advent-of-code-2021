{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day11 (
    day11a
  , day11b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

day11a :: _ :~> _
day11a = MkSol
    { sParse = Just . parseAsciiMap digitToIntSafe
    -- { sParse = splitOn ","
    -- , sShow  = ('\n':) . unlines . map (displayAsciiMap ' ' . fmap intToDigit . snd)
    , sShow  = show
    -- , sSolve = Just
    -- , sSolve = Just . take 10 . iterate (realFullStep =<<) . (0,)
    , sSolve = Just . (!!! 100) . doTheThing
    }

realFullStep :: Map Point Int -> (Set Point, Map Point Int)
realFullStep mp = (fl, mp'')
  where
    (fl, mp') = fullStep $ fmap (+1) mp
    mp'' = M.unionWith const (M.fromSet (const 0) fl) mp'

doTheThing :: Map Point Int -> [Int]
doTheThing = go 0
  where
    go n mp = n : go (n + S.size q) mp'
      where
        (q, mp') = realFullStep mp

fullStep :: Map Point Int -> (Set Point, Map Point Int)
fullStep = go S.empty
  where
    go n mp
        | S.null fl = (n, mp')
        | otherwise = go (fl `S.union` n) mp'
      where
        (fl, mp') = runFlash n mp

runFlash :: Set Point -> Map Point Int -> (Set Point, Map Point Int)
runFlash alreadyFlashed mp = (readyToFlash, mp')
  where
    (yesFlash, noFlash) = M.partition (> 9) mp
    readyToFlash = M.keysSet yesFlash
    neighbs = (`M.restrictKeys` M.keysSet noFlash) . freqs $ foldMap fullNeighbs readyToFlash
    mp' = M.unionWith (+) neighbs noFlash

-- runFlash' :: Map Point Int -> (Set Point, Map Point Int)
-- runFlash' mp = (neighbs, mp')
--   where
--     readyToFlash = M.keysSet (M.filter (> 9) mp)
--     neighbs = foldMap fullNeighbsSet readyToFlash `S.intersection` M.keysSet mp
--     mp' = M.unionWith (+) (M.fromSet (const 1) neighbs) mp
--     -- mp' = M.unionWith (+) (M.fromSet (const 1) neighbs) mp
--     -- -- mp'' = M.unionWith const (M.fromSet (const 0) readyToFlash) mp'

doTheThing2 :: Map Point Int -> [Set Point]
doTheThing2 mp = q : doTheThing2 mp'
  where
    (q, mp') = realFullStep mp


day11b :: _ :~> _
day11b = MkSol
    { sParse = sParse day11a
    , sShow  = show
    , sSolve = \mp ->
        firstJust (\(i, q) -> i <$ guard (q == M.keysSet mp )
          ) . zip [1..] . doTheThing2 $ mp
    }
