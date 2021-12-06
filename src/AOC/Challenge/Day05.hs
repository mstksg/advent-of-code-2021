{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import           AOC.Prelude

import           Data.Distributive
import qualified Data.ExtendedReal              as ER
import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.IntegerInterval           as I
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

-- data LineType = Horiz | Vert | Upwards | Downwards
-- data Line = Line
--     { lType :: LineType
--     , lAxis :: Integer              -- ^ y for horiz, x for vert, x+y for up, x-y for down
--     , lRange :: I.IntegerInterval
--     }

-- overlaps :: Line -> Line -> [V2 Integer]
-- overlaps (Line t1 a1 r1) (Line t2 a2 r2) = case (t1, t2) of
--     (Horiz, Horiz)
--       | a1 == a2  ->
--           let r3 = I.intersection r1 r2
--           in  case (I.lowerBound r3, I.upperBound r3) of
--                 (ER.Finite x, ER.Finite y) -> V2 a1 <$> [x..y]
--                 _                          -> []
--       | otherwise -> []
--     (Horiz, Vert)
--       | a1 `I.member` r2 || a2 `I.member` r1 -> [V2 a1 a2]
--       | otherwise -> []

parseLine :: String -> Maybe (V2 Point)
parseLine = (traverse . traverse) readMaybe
        <=< listV2 . mapMaybe (listV2 . splitOn ",") . words

expander :: V2 Point -> [Point]
expander (V2 a b) = a : b : lineTo a b

day05 :: ([V2 Point] -> [V2 Point]) -> [V2 Point] :~> Int
day05 preFilter = MkSol
    { sParse = traverseLines parseLine
    , sShow  = show
    , sSolve = Just . length . M.filter (> 1) . freqs . concatMap expander . preFilter
    }

day05a :: [V2 Point] :~> Int
day05a = day05 (filter sameLine)
  where
    sameLine = any (\(V2 a b) -> a == b) . distribute

day05b :: [V2 Point] :~> Int
day05b = day05 id
