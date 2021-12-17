{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day17 (
    day17a
  , day17b
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
import Safe
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

day17a :: [Int] :~> _
day17a = MkSol
    { sParse = traverse readMaybe . words . clearOut (\k -> not (isDigit k || k == '-'))
    , sShow  = show
    , sSolve = \[x1,x2,y1,y2] -> maximumMay
        [ mx
        | x <- [-100.. 100]
        , y <- [-100.. 100]
        , let steps = stepUntilTooDeep (min y1 y2) (V2 x y)
        , any (inBoundingBox (V2 (V2 (min x1 x2) (min y1 y2)) (V2 (max x1 x2) (max y1 y2))))
                    steps
        , Just mx <- [maximumMay (map (view _y) steps)]
        ]
-- exponentialSearch
-- inBoundingBox
--     :: (Applicative g, Foldable g, Ord a)
--     => V2 (g a)
--     -> g a
--     -> Bool
--     :: (Int -> Ordering)        -- LT: Too small, GT: Too big
--     -> Int
--     -> Maybe Int
    }

stepUntilTooDeep :: Int -> Point -> [Point]
stepUntilTooDeep ymin v0 = map fst . takeWhile p $ iterate simStep (0, v0)
  where
    p (V2 _ y, _) = y >= ymin

simStep :: (Point, Point) -> (Point, Point)
simStep (pos, vel@(V2 vx vy)) = (pos + vel, vel')
  where
    vel' = V2 (vx - signum vx) (vy - 1)

-- target area: x=248..285, y=-85..-56

day17b :: _ :~> _
day17b = MkSol
    { sParse = sParse day17a
    , sShow  = show
    , sSolve = \[x1,x2,y1,y2] -> Just $ length
        [ mx
        | x <- [-500.. 500]
        , y <- [-500.. 500]
        , let steps = stepUntilTooDeep (min y1 y2) (V2 x y)
        , any (inBoundingBox (V2 (V2 (min x1 x2) (min y1 y2)) (V2 (max x1 x2) (max y1 y2))))
                    steps
        , Just mx <- [maximumMay (map (view _y) steps)]
        ]
    }
