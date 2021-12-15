-- |
-- Module      : AOC.Challenge.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day15 (
    day15a
  , day15b
  ) where

import           AOC.Common        (digitToIntSafe)
import           AOC.Common.Point  (Point, V2(..), parseAsciiMap, mannDist, cardinalNeighbsSet)
import           AOC.Common.Search (aStar)
import           AOC.Solver        ((:~>)(..))
import           Control.Monad     ((<=<))
import           Data.Finite       (Finite, packFinite)
import           Data.Map          (Map)
import qualified Data.Map          as M
import qualified Data.Set          as S

type Risk = Finite 9

day15 :: (Map Point Risk -> Map Point Risk) -> Map Point Risk :~> Int
day15 reMap = MkSol
    { sParse = Just . parseAsciiMap (packFinite . subtract 1 . fromIntegral <=< digitToIntSafe)
    , sShow  = show
    , sSolve = \mp0 ->
        let mp        = reMap mp0
            (targ, _) = M.findMax mp
            cost p    = fromIntegral (mp M.! p) + 1
        in  fst <$> aStar
                (mannDist targ)
                (M.fromSet cost . S.intersection (M.keysSet mp) . cardinalNeighbsSet)
                0
                (== targ)
    }


day15a :: Map Point Risk :~> Int
day15a = day15 id

day15b :: Map Point Risk :~> Int
day15b = day15 \mp0 ->
    let (corner, _) = M.findMax mp0
        shifter     = corner + 1
    in  M.fromList
          [ (k', v + dx + dy)
          | (k, v) <- M.toList mp0
          , dx <- [0,1,2,3,4]
          , dy <- [0,1,2,3,4]
          , let k' = k + (shifter * (fromIntegral <$> V2 dx dy))
          ]
