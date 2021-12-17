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
import Data.Complex
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

parseBox :: String -> Maybe (V2 Point)
parseBox = getBounds <=< traverse readMaybe . words . clearOut valid
  where
    valid k = not (isDigit k || k == '-')
    getBounds = \case
      [x1,x2,y1,y2] -> do
        [xMin, xMax] <- pure $ sort [x1,x2]
        [yMin, yMax] <- pure $ sort [y1,y2]
        pure $ V2 (V2 xMin yMin) (V2 xMax yMax)
      _ -> Nothing

-- | Independent tight bounds for each axis
velBounds :: V2 Point -> V2 Point
velBounds (V2 (V2 x1 y1) (V2 x2 _)) = V2 (V2 vx1 y1) (V2 x2 (abs y1))
  where
    vx1 = case quadraticEq (-fromIntegral x1) 0.5 0.5 of
      QReal a b    -> ceiling (max a b)
      QSimul a     -> ceiling a
      QComplex _ _ -> error "invalid box"

data QuadraticSol = QReal    Double Double
                  | QSimul   Double
                  | QComplex (Complex Double) (Complex Double)

quadraticEq :: Double -> Double -> Double -> QuadraticSol
quadraticEq a b c = case compare discr 0 of
    LT -> QComplex (term1 :+ term2) (term1 :+ (-term2))
    EQ -> QSimul term1
    GT -> QReal (term1 + term2) (term1 - term2)
  where
    discr = b*b - 4 * a * c
    term1 = -0.5*b/a
    term2 = 0.5 * sqrt (abs discr) / a

day17a :: V2 Point :~> _
day17a = MkSol
    { sParse = parseBox
    , sShow  = show
    , sSolve = \(bbox@(V2 (V2 _ y1) (V2 x2 _))) ->
        let V2 (V2 vx1 vy1) (V2 vx2 vy2) = velBounds bbox
        in  maximumMay
              [ mx
              | x <- [vx1..vx2]
              , y <- [vy1..vy2]
              , let steps = stepUntilTooDeep x2 y1 (V2 x y)
              , any (inBoundingBox bbox) steps
              , Just mx <- [maximumMay (map (view _y) steps)]
              ]
    }

stepUntilTooDeep :: Int -> Int -> Point -> [Point]
stepUntilTooDeep xmax ymin v0 = map fst . takeWhile p $ iterate simStep (0, v0)
  where
    p (V2 x y, _) = y >= ymin && x <= xmax

simStep :: (Point, Point) -> (Point, Point)
simStep (pos, vel@(V2 vx vy)) = (pos + vel, vel')
  where
    vel' = V2 (vx - signum vx) (vy - 1)

-- target area: x=248..285, y=-85..-56

day17b :: V2 Point :~> _
day17b = MkSol
    { sParse = sParse day17a
    , sShow = show
    , sSolve = \(bbox@(V2 (V2 _ y1) (V2 x2 _))) ->
        let V2 (V2 vx1 vy1) (V2 vx2 vy2) = velBounds bbox
        in  Just $ countTrue (any (inBoundingBox bbox))
              [ stepUntilTooDeep x2 y1 (V2 x y)
              | x <- [vx1..vx2]
              , y <- [vy1..vy2]
              ]
    }
