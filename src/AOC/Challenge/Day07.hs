-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day07 (
    day07a
  , day07b
  ) where

import           AOC.Common        (freqs, triangleNumber)
import           AOC.Common.Search (binaryFindMin)
import           AOC.Solver        ((:~>)(..))
import           Control.Monad     (guard)
import           Data.List.Split   (splitOn)
import           Data.Semigroup    (Sum(..))
import           Text.Read         (readMaybe)
import qualified Data.Map          as M
import qualified Data.Vector       as V

day07
    :: (Int -> Int)         -- ^ loss function
    -> [Int] :~> Int
day07 f = MkSol
    { sParse = traverse readMaybe . splitOn ","
    , sShow  = show
    , sSolve = \xs -> do
        let xsMap = freqs xs
            findFuelFor targ = getSum $ M.foldMapWithKey (\x n -> Sum $ f (abs (targ - x)) * n) xsMap
        (minX, _) <- M.lookupMin xsMap
        (maxX, _) <- M.lookupMax xsMap
        let fuelVector = V.generate (maxX + 1 - minX) $ \i -> findFuelFor (i + minX)
        binaryFindMin (\x ->
                let fX  = fuelVector V.! x
                    fX1 = fuelVector V.! (x+1)
                in  fX <$ guard (fX1 > fX)
            )
          minX maxX
    }

day07a :: [Int] :~> Int
day07a = day07 id

day07b :: [Int] :~> Int
day07b = day07 triangleNumber
