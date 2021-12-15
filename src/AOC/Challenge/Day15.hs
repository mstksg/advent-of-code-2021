{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day15 (
    day15a
  , day15b
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
import AOC.Common.Search

day15a :: _ :~> _
day15a = MkSol
    { sParse = Just . parseAsciiMap digitToIntSafe
    , sShow  = show
    , sSolve = \mp ->
        let targ = fst $ M.findMax mp
        in  fst <$> aStar
                (mannDist targ)
                (M.fromSet (\x -> mp M.! x) . (`S.intersection` M.keysSet mp) . cardinalNeighbsSet)
                0
                (== targ)
    }

-- aStar
--     :: forall n p. (Ord n, Ord p, Num p)
--     => (n -> p)         -- ^ heuristic
--     -> (n -> Map n p)   -- ^ neighborhood
--     -> n                -- ^ start
--     -> (n -> Bool)      -- ^ target
--     -> Maybe (p, [n])   -- ^ the shortest path, if it exists, and its cost

day15b :: _ :~> _
day15b = MkSol
    { sParse = sParse day15a
    , sShow  = show
    , sSolve = \mp0 ->
        let mp = M.fromList
                [ (k', v')
                | (k, v) <- M.toList mp0
                , dx <- [0,1,2,3,4]
                , dy <- [0,1,2,3,4]
                , let k' = k + V2 (shifter * dx) (shifter * dy)
                , let v' = ((v - 1 + dx+dy) `mod` 9) + 1
                ]
            corner = fst $ M.findMax mp0
            V2 shifter _ = corner + 1
            targ = fst $ M.findMax mp
        in  fst <$> aStar
                (mannDist targ)
                (M.fromSet (\x -> mp M.! x) . (`S.intersection` M.keysSet mp) . cardinalNeighbsSet)
                0
                (== targ)
    }
