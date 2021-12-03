-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Common               (traverseLines)
import           AOC.Solver               ((:~>)(..))
import           Control.DeepSeq          (NFData)
import           Control.Lens             (Prism', (^?!), review, preview, prism')
import           Control.Monad            ((<=<))
import           Data.Coerce              (coerce)
import           Data.Functor.Foldable    (hylo)
import           Data.Functor.Foldable.TH (makeBaseFunctor)
import           Data.List.NonEmpty       (NonEmpty(..))
import           Data.Semigroup           (Sum(..))
import           Data.These               (mergeThese)
import           GHC.Generics             (Generic)
import           Linear.V2                (V2(..))
import           Numeric.Lens             (base)
import qualified Data.DList               as DL
import qualified Data.List.NonEmpty       as NE
import qualified Data.Zip                 as Z

data Bit = Zero | One
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass NFData

_Bit :: Prism' Char Bit
_Bit = prism' (\case Zero -> '0'; One -> '1')
              (`lookup` [('0', Zero), ('1', One)])

flipBit :: Bit -> Bit
flipBit Zero = One
flipBit One  = Zero

data BinTrie =
    BTLeaf [Bit]
  | BTNode (Maybe BinTrie) (Maybe BinTrie)
  deriving stock Show
makeBaseFunctor ''BinTrie

day03a :: NonEmpty [Bit] :~> _
day03a = MkSol
    { sParse = NE.nonEmpty <=< traverseLines (traverse (preview _Bit))
    , sShow = \xs -> show @Int
        let ys = map flipBit xs
            toBin str = map (review _Bit) str ^?! base 2
        in  toBin xs * toBin ys
    , sSolve = Just . map pickMost . snd . hylo part1Alg buildTrieCoalg
    }
  where
    pickMost (V2 x y)
      | x > y     = Zero
      | otherwise = One

day03b :: NonEmpty [Bit] :~> ([Bit], [Bit])
day03b = MkSol
    { sParse = NE.nonEmpty <=< traverseLines (traverse (preview _Bit))
    , sShow  = \(o2, co2) -> show @Int
        let toBin str = map (review _Bit) str ^?! base 2
        in  toBin o2 * toBin co2
    , sSolve = Just . snd . hylo part2Alg buildTrieCoalg
    }

buildTrieCoalg :: NonEmpty [Bit] -> BinTrieF (NonEmpty [Bit])
buildTrieCoalg (theOne :| theRest)
  | null theRest = BTLeafF theOne
  | otherwise    =
      let V2 zeroes ones = peelOff (theOne : theRest)
      in  BTNodeF (NE.nonEmpty zeroes) (NE.nonEmpty ones)

part1Alg
    :: BinTrieF (Int, [V2 Int])
    -> (Int, [V2 Int])
part1Alg = \case
    BTLeafF xs -> (1, map singleCount xs)
    BTNodeF zeroes ones ->
      let (Sum numZeroes, zeroAmts) = foldMap coerce zeroes
          (Sum numOnes  , oneAmts ) = foldMap coerce ones
          newNum  = numZeroes + numOnes
          newAmts = V2 numZeroes numOnes
                  : Z.alignWith (mergeThese (+)) zeroAmts oneAmts
      in  (newNum, newAmts)
  where
    singleCount Zero = V2 1 0
    singleCount One  = V2 0 1

-- | Collect both the oxygen (fst) and co2 (snd) answers at the same time
--
-- The first item int he tuple is the number of items under the given
-- branch
part2Alg
    :: BinTrieF (Int, ([Bit], [Bit]))
    -> (Int, ([Bit], [Bit]))
part2Alg = \case
    BTLeafF xs -> (1, (xs, xs))
    BTNodeF zeroes ones ->
      let numZeroes = maybe 0 fst zeroes
          numOnes   = maybe 0 fst ones
          keepForO2
            | numZeroes > numOnes = Zero
            | otherwise           = One
          keepFunc fstOrSnd = \case
            Zero -> Zero : foldMap (fstOrSnd . snd) zeroes
            One  -> One  : foldMap (fstOrSnd . snd) ones
          newO2  = keepFunc fst keepForO2
          newCO2 = keepFunc snd (flipBit keepForO2)
      in  (numZeroes + numOnes, (newO2, newCO2))

peelOff
    :: [[Bit]]
    -> V2 [[Bit]]         -- ^ x is zeros, y is ones
peelOff = fmap DL.toList . foldMap go
  where
    go = \case
      []      -> mempty
      Zero:xs -> V2 (DL.singleton xs) mempty
      One :ys -> V2 mempty (DL.singleton ys)
