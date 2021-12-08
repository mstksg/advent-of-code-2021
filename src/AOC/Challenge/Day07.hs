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

import           AOC.Common      (triangleNumber, freqList)
import           AOC.Solver      ((:~>)(..))
import           Data.List.Split (splitOn)
import           Safe.Foldable   (minimumMay)
import           Text.Read       (readMaybe)

day07
    :: (Int -> Int)         -- ^ loss function
    -> [Int] :~> _
day07 f = MkSol
    { sParse = traverse readMaybe . splitOn ","
    , sShow  = show
    , sSolve = \xs ->
        let xsMap = freqList xs
            findFuelFor targ = sum $ map (\(n,x) -> f (abs (targ - x)) * n) xsMap
        in  minimumMay [ findFuelFor i | i <- [minimum xs .. maximum xs]]
    }

day07a :: [Int] :~> Int
day07a = day07 id

day07b :: [Int] :~> Int
day07b = day07 triangleNumber
