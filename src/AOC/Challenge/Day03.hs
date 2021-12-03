{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
import           Data.List
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

day03a :: _ :~> _
day03a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = fmap product
             . traverse parseBin
             . transpose
             . map (map fst . sortOn snd . zip "01" . map length . group . sort)
             . transpose
    }

day03b :: [[Bit]] :~> _
day03b = MkSol
    { sParse = traverseLines $ traverse (preview _Bit)
    , sShow  = show . maybe 0 product . traverse (parseBin . map (review _Bit))
    , sSolve = \xs -> Just
        let oxy = searchFor id xs
            car = searchFor flipBit xs
        in  V2 oxy car
    }

data Bit = Zero | One
  deriving stock (Eq, Ord, Show, Generic)

_Bit :: Prism' Char Bit
_Bit = prism' (\case Zero -> '0'; One -> '1')
              (`lookup` [('0', Zero), ('1', One)])

flipBit :: Bit -> Bit
flipBit Zero = One
flipBit One  = Zero

instance NFData Bit

searchFor
    :: (Bit -> Bit)     -- ^ modify the picked bit (oxygen = id, co2 = flipBit)
    -> [[Bit]]
    -> [Bit]
searchFor picker = apo (go . peelOff)
  where
    go :: V2 [[Bit]] -> ListF Bit (Either [Bit] [[Bit]])
    go (V2 zeroes ones) = case valsToKeep of
        []       -> Nil     -- this shouldn't ever happen given the problem statement
        [theOne] -> Cons bitToKeep $ Left theOne
        _        -> Cons bitToKeep $ Right valsToKeep
      where
        numZeroes = length zeroes
        numOnes   = length ones
        bitToKeep
          | numZeroes > numOnes = picker Zero
          | otherwise           = picker One
        valsToKeep = case bitToKeep of
          Zero -> zeroes
          One  -> ones

peelOff
    :: [[Bit]]
    -> V2 [[Bit]]         -- ^ x is zeros, y is ones
peelOff = foldMap \case
    []      -> mempty
    Zero:xs -> V2 [xs] []
    One :ys -> V2 [] [ys]
