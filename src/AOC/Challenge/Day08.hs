-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day08 (
    day08a
  , day08b
  ) where

import           AOC.Common      (listTup, traverseLines, countTrue)
import           AOC.Solver      ((:~>)(..))
import           Control.Lens    (Prism', prism', preview)
import           Data.Bifunctor  (first)
import           Data.Char       (chr, ord)
import           Data.Finite     (Finite, finites, packFinite, getFinite)
import           Data.Foldable   (toList)
import           Data.List       (permutations)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import           Data.Maybe      (mapMaybe)
import           Data.Set        (Set)
import qualified Data.Map        as M
import qualified Data.Set        as S

-- way too much type safety

-- | Actual physical segment on the display
newtype Segment = Segment { getSegment :: Finite 7 }
  deriving stock (Eq, Ord, Show)
type Display = Set Segment

-- | abcdefg
newtype Wire = Wire { getWire :: Finite 7 }
  deriving stock (Eq, Ord, Show)
type Wires = Set Wire

-- | Map of wire displays to the digit they represent
type OutputMap = Map Wires Int

day08a :: [(Set Wires, [Wires])] :~> Int
day08a = MkSol
    { sParse = traverseLines parseLine
    , sShow  = show
    , sSolve = Just . sum . map (countTrue isUnique . snd)
    }
  where
    isUnique xs = length xs `S.member` uniques
      where
        uniques = S.fromList [2,4,3,7]

-- | Map of all 9-digit observations to OutputMap they represent
observationsMap :: Map (Set Wires) OutputMap
observationsMap = M.fromList do
    perm <- permutations $ Wire <$> finites
    let mp  = M.fromList $ zip (Segment <$> finites) perm
        visible = (S.map . S.map) (mp M.!) signalSet
        outputMap = M.fromList do
          (a, sig) <- M.toList signals
          pure (S.map (mp M.!) sig, a)
    pure (visible, outputMap)
  where
    signalSet = S.fromList (toList signals)

signals :: Map Int Display
signals = M.fromList . zip [0..] . map (S.fromList . map Segment) $
    [ [0,1,2,4,5,6]
    , [2,5]
    , [0,2,3,4,6]
    , [0,2,3,5,6]
    , [1,2,3,5]
    , [0,1,3,5,6]
    , [0,1,3,4,5,6]
    , [0,2,5]
    , [0,1,2,3,4,5,6]
    , [0,1,2,3,5,6]
    ]

day08b :: [(Set Wires, [Wires])] :~> [Int]
day08b = MkSol
    { sParse = traverseLines parseLine
    , sShow  = show . sum
    , sSolve = traverse $ \(xs, ys) -> do
        outputMap <- M.lookup xs observationsMap
        [a,b,c,d] <- traverse (`M.lookup` outputMap) ys
        pure (a*1000+b*100+c*10+d)
    }





parseLine :: String -> Maybe (Set Wires, [Wires])
parseLine = fmap (first S.fromList)
          . listTup
          . map (map toWires . words)
          . splitOn " | "
  where
    toWires :: String -> Wires
    toWires = S.fromList . mapMaybe (preview _CharWire)
    -- | Parse a Char as a Wire, for type safety
    _CharWire :: Prism' Char Wire
    _CharWire = prism'
                  (\(Wire w) -> chr $ fromIntegral (getFinite w) + ord 'a')
                  (\c -> Wire <$> packFinite (fromIntegral (ord c - ord 'a')))
