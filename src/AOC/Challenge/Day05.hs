-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day05 (
    day05a
  , day05b
  ) where

import           AOC.Common           (traverseLines, listV2)
import           AOC.Common.Point     (Point)
import           AOC.Solver           ((:~>)(..))
import           Control.DeepSeq      (NFData)
import           Control.Monad        ((<=<))
import           Data.Foldable        (foldMap')
import           Data.List            (tails)
import           Data.List.Split      (splitOn)
import           Data.Maybe           (mapMaybe, fromJust)
import           GHC.Generics         (Generic)
import           Linear.Matrix        (transpose)
import           Linear.V2            (V2(..))
import           Text.Read            (readMaybe)
import qualified Data.ExtendedReal    as ER
import qualified Data.IntegerInterval as I
import qualified Data.Set             as S

data LineType = Horiz | Vert | Upwards | Downwards
  deriving stock (Show, Generic)
  deriving anyclass NFData
data Line = Line
    { lType :: LineType
    , lAxis :: Integer              -- ^ y for horiz, x for vert, x+y for up, x-y for down
    , lRange :: I.IntegerInterval
    }
  deriving stock (Show, Generic)
  deriving anyclass NFData

classify :: V2 Point -> Maybe Line
classify ((fmap . fmap) fromIntegral->V2 (V2 x1 y1) (V2 x2 y2))
    | x1 == x2 = Just $ Line Vert x1 (fromTo y1 y2)
    | y1 == y2 = Just $ Line Horiz y1 (fromTo x1 x2)
    | (x1+y1) == (x2+y2) = Just $ Line Upwards (x1+y1) (fromTo y1 y2)
    | (x1-y1) == (x2-y2) = Just $ Line Downwards (x1-y1) (fromTo y1 y2)
    | otherwise = Nothing
  where
    fromTo a b
      | a <= b    = ER.Finite a I.<=..<= ER.Finite b
      | otherwise = ER.Finite b I.<=..<= ER.Finite a

_unclassify :: Line -> V2 Point
_unclassify (Line t a r) = fmap (fmap fromInteger) case t of
    Horiz     -> V2 (V2 lb a) (V2 ub a)
    Vert      -> V2 (V2 a lb) (V2 a ub)
    Upwards   -> V2 (V2 (a-lb) lb) (V2 (a-ub) ub)
    Downwards -> V2 (V2 (a+lb) lb) (V2 (a+ub) ub)
  where
    (lb, ub) = fromJust $ intervalBounds r

parseLine :: String -> Maybe (V2 Point)
parseLine = (traverse . traverse) readMaybe
        <=< listV2 . mapMaybe (listV2 . splitOn ",") . words

day05 :: ([V2 Point] -> [V2 Point]) -> [V2 Point] :~> Int
day05 preFilter = MkSol
    { sParse = traverseLines parseLine
    , sShow  = show
    , sSolve = \xs -> do
        ls <- traverse classify (preFilter xs)
        let bigONSquaredSearch = foldMap' S.fromList do
              l:ls' <- tails ls
              overlaps l <$> ls'
        pure $ S.size bigONSquaredSearch
    }

day05a :: [V2 Point] :~> Int
day05a = day05 (filter sameLine)
  where
    sameLine = any (\(V2 a b) -> a == b) . transpose

day05b :: [V2 Point] :~> Int
day05b = day05 id

-- upwards points: z = x+y is constant, so range t=(a,b)
--   = <z-a, a> to <z-b, b>
--   ie, z = 10, from (3,6)
--   goes from <7,3> to <4,6>
--   to find t coord, <z-t, t>
--   so <x,y> = <z-t,t> gives us the horiz and vert intercepts
--     as in x=z-t, t=z-x, also y = t
--
-- downwards points: z = x-y is constant, so range t=(a,b)
--   = <z+a, a> to <z+b, b>
--   to find t coord, <z+t, t>
--   so <x,y> = <x+t,t> gives us horiz and vert intercepts
--     as in x=z+t, t=x-z, also y = t
--
-- upwards-downwards intersection
--   solve <z1-t1, t1> = <z2+t2, t2>
--   so first y = t1 = t2, and t's are in range
--   so we just need to solve z1-y == z2+y
--     or 2y = z1-z2, y = (z1-z2)  `div` 2
--   ie, let's say upwards z1 = 4, downwards z2=0
--   solve y = (4-0)`div`2 = 2
--   and so check if 2 is in range

-- sloped: <t, z+m*t>
--   intersect with other slope:
--   <t, z1 + m1 * t> == <t, z2 + m2 * t> (t = x = equal)
--   solve: z1 + m1 * t == z2 + m2 * t
--          m1 * t = m2 * t + (z2 - z1)
--          t = (m2 * t + z2 - z1) / m1
--

overlaps :: Line -> Line -> [V2 Integer]
overlaps = \case
    Line Horiz y1 rx1 -> \case
      Line Horiz y2 rx2
        | y1 == y2
        , Just (a, b) <- intervalBounds (I.intersection rx1 rx2) -> (`V2` y1) <$> [a..b]
      Line Vert x2 ry2
        | y1 `I.member` ry2 && x2 `I.member` rx1 -> [V2 x2 y1]
      Line Upwards z2 rt2
        | y1 `I.member` rt2 && (z2 - y1) `I.member` rx1 -> [V2 (z2-y1) y1]
      Line Downwards z2 rt2
        | y1 `I.member` rt2 && (z2 + y1) `I.member` rx1 -> [V2 (z2+y1) y1]
      _ -> []
    Line Vert x1 ry1 -> \case
      Line Horiz y2 rx2
        | x1 `I.member` rx2 && y2 `I.member` ry1 -> [V2 x1 y2]
      Line Vert x2 ry2
        | x1 == x2
        , Just (a, b) <- intervalBounds (I.intersection ry1 ry2) -> V2 x1 <$> [a..b]
      Line Upwards z2 rt2
        | (z2 - x1) `I.member` rt2 && (z2 - x1) `I.member` ry1 -> [V2 x1 (z2-x1)]
      Line Downwards z2 rt2
        | (x1 - z2) `I.member` rt2 && (x1 - z2) `I.member` ry1 -> [V2 x1 (x1-z2)]
      _ -> []
    Line Upwards z1 rt1 -> \case
      Line Horiz y2 rx2
        | y2 `I.member` rt1 && (z1 - y2) `I.member` rx2 -> [V2 (z1-y2) y2]
      Line Vert x2 ry2
        | (z1 - x2) `I.member` rt1 && (z1 - x2) `I.member` ry2 -> [V2 x2 (z1-x2)]
      Line Upwards z2 rt2
        | z1 == z2
        , Just (a, b) <- intervalBounds (I.intersection rt1 rt2) -> (\t -> V2 (z1-t) t) <$> [a..b]
      Line Downwards z2 rt2
        | let (t, rm) = (z1 - z2) `divMod` 2
        , rm == 0, t `I.member` rt1, t `I.member` rt2 -> [V2 (z1-t) t]
      _ -> []
    Line Downwards z1 rt1 -> \case
      Line Horiz y2 rx2
        | y2 `I.member` rt1 && (z1 + y2) `I.member` rx2 -> [V2 (z1+y2) y2]
      Line Vert x2 ry2
        | (x2 - z1) `I.member` rt1 && (x2 - z1) `I.member` ry2 -> [V2 x2 (x2-z1)]
      Line Upwards z2 rt2
        | let (t, rm) = (z2 - z1) `divMod` 2
        , rm == 0, t `I.member` rt1, t `I.member` rt2 -> [V2 (z1+t) t]
      Line Downwards z2 rt2
        | z1 == z2
        , Just (a, b) <- intervalBounds (I.intersection rt1 rt2) -> (\t -> V2 (z1+t) t) <$> [a..b]
      _ -> []

intervalBounds :: I.IntegerInterval -> Maybe (Integer, Integer)
intervalBounds i = do
    ER.Finite a <- pure $ I.lowerBound i
    ER.Finite b <- pure $ I.upperBound i
    pure (a, b)

