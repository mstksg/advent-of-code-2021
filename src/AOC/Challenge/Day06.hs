-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where

import           AOC.Common                ()
import           AOC.Solver                ((:~>)(..))
import           Control.Monad             ((<=<))
import           Control.Monad.ST          (runST)
import           Data.Finite               (Finite, packFinite)
import           Data.Foldable             (traverse_)
import           Data.List.Split           (splitOn)
import           Data.Semigroup            (stimes)
import           GHC.TypeNats              (KnownNat)
import           Text.Read                 (readMaybe)
import qualified Data.Vector.Mutable.Sized as SMV
import qualified Data.Vector.Sized         as SV
import qualified Linear                    as L

newtype SquareMat n = SquareMat (SV.Vector n (SV.Vector n Int))
  deriving stock (Show)

applyMat :: KnownNat  n => SquareMat n -> SV.Vector n Int -> SV.Vector n Int
applyMat (SquareMat xs) x = xs L.!* x

step :: SquareMat 9
step = SquareMat $ SV.fromTuple (
    SV.fromTuple (0,1,0,0,0,0,0,0,0)
  , SV.fromTuple (0,0,1,0,0,0,0,0,0)
  , SV.fromTuple (0,0,0,1,0,0,0,0,0)
  , SV.fromTuple (0,0,0,0,1,0,0,0,0)
  , SV.fromTuple (0,0,0,0,0,1,0,0,0)
  , SV.fromTuple (0,0,0,0,0,0,1,0,0)
  , SV.fromTuple (1,0,0,0,0,0,0,1,0)
  , SV.fromTuple (0,0,0,0,0,0,0,0,1)
  , SV.fromTuple (1,0,0,0,0,0,0,0,0)
  )

instance KnownNat n => Semigroup (SquareMat n) where
    SquareMat x <> SquareMat y = SquareMat (x L.!*! y)

stepN :: Int -> SquareMat 9
stepN n = stimes n step

allocate :: forall n. KnownNat n => [Finite n] -> SV.Vector n Int
allocate xs = runST do
    v <- SMV.replicate 0
    traverse_ (SMV.modify v (+1)) xs
    SV.freeze v

day06 :: Int -> [Finite 9] :~> Int
day06 n = MkSol
    { sParse = traverse (packFinite <=< readMaybe) . splitOn ","
    , sShow  = show
    -- this isn't going to bench very accurately because stepN n is auto-cached
    , sSolve = Just . sum . (stepN n `applyMat`) . allocate
    }

day06a :: [Finite 9] :~> Int
day06a = day06 80

day06b :: [Finite 9] :~> Int
day06b = day06 256
