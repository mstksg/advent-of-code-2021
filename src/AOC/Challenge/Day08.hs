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

import           AOC.Common      (listTup, traverseLines, mapMaybeSet, countTrue)
import           AOC.Solver      ((:~>)(..))
import           Control.Lens    (Prism', prism', preview)
import           Control.Monad   ((<=<))
import           Data.Bifunctor  (first)
import           Data.Char       (chr, ord)
import           Data.Finite     (Finite, finites, packFinite, getFinite)
import           Data.List       (permutations)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import           Data.Maybe      (mapMaybe)
import           Data.Set        (Set)
import           Text.Read       (readMaybe)
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

-- | Map of wire (abcdefg) to segment
type WireMap = Map Wire Segment

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

-- | Map of all 9-digit observations to the wiremap that cuses them
observationsMap :: Map (Set Wires) WireMap
observationsMap = M.fromList do
      perm <- permutations $ Wire <$> finites
      let mp  = M.fromList $ zip (Segment <$> finites) perm
          mp' = M.fromList $ zip perm (Segment <$> finites)
          visible = (S.map . S.map) (mp M.!) (M.keysSet signals)
      pure (visible, mp')

signals :: Map Display Char
signals = M.fromList . flip zip ['0'..'9'] . map (S.fromList . map Segment) $
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

-- | Use a wiremap to turn a string into the integer it represents
applyWireMap :: WireMap -> [Wires] -> Maybe Int
applyWireMap mp = readMaybe <=< traverse decodeDigit
  where
    decodeDigit = (`M.lookup` signals) . mapMaybeSet (`M.lookup` mp)

day08b :: [(Set Wires, [Wires])] :~> [Int]
day08b = MkSol
    { sParse = traverseLines parseLine
    , sShow  = show . sum
    , sSolve = traverse $ \(xs, ys) -> do
        mp <- M.lookup xs observationsMap
        applyWireMap mp ys
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
