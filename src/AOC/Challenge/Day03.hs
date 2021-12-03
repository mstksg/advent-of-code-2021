{-# OPTIONS_GHC -Wno-unused-imports   #-}

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import           AOC.Prelude

import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.List                      (transpose, sort, group, sortOn)
import           Numeric.Lens                   (base)
import qualified Data.Fix                       as DF
import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List                      as Li
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

day03a :: [String] :~> Int
day03a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = fmap product
             . traverse (preview (base 2))
             . transpose
             . map (map fst . sortOn snd . zip "01" . map length . group . sort)
             . transpose
    }

day03b :: NonEmpty [Bit] :~> ([Bit], [Bit])
day03b = MkSol
    { sParse = NE.nonEmpty <=< traverseLines (traverse (preview _Bit))
    , sShow  = \(o2, co2) -> show @Int
        let toBin str = map (review _Bit) str ^?! base 2
        in  toBin o2 * toBin co2
    , sSolve = Just . snd . hylo btAlg btCoalg
    }

btCoalg :: NonEmpty [Bit] -> BinTrieF (NonEmpty [Bit])
btCoalg (theOne :| theRest)
  | null theRest = BTLeafF theOne
  | otherwise    =
      let V2 zeroes ones = peelOff (theOne : theRest)
      in  BTNodeF (NE.nonEmpty zeroes) (NE.nonEmpty ones)

-- | Collect both the oxygen (fst) and co2 (snd) answers at the same time
--
-- The first item int he tuple is the number of items under the given
-- branch
btAlg
    :: BinTrieF (Int, ([Bit], [Bit]))
    -> (Int, ([Bit], [Bit]))
btAlg = \case
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
peelOff = foldMap \case
    []      -> mempty
    Zero:xs -> V2 [xs] []
    One :ys -> V2 [] [ys]
