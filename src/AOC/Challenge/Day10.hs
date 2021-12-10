-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day10 (
    day10a
  , day10b
  ) where

import           AOC.Solver ((:~>)(..))
import           Control.DeepSeq (NFData)
import           GHC.Generics (Generic)
import           Data.Maybe (mapMaybe)
import           AOC.Common (traverseLines)
import           Data.List (sort)

data Bracket = Round | Square | Curly | Angle
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData)
data Direction = Open | Close
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData)

data Symbol = Symbol { direction :: Direction, bracket :: Bracket }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (NFData)

-- | Left: error (with offending bracket)
-- Right: no error, but a list of leftover incompletes
runSymbols :: [Symbol] -> Either Bracket [Bracket]
runSymbols = go []
  where
    go stk = \case
      Symbol Close b:xs -> case stk of
        s:ss | s == b    -> go ss xs
        _                -> Left b
      Symbol Open b:xs  -> go (b:stk) xs
      []                -> Right stk

parseString :: String -> Maybe [Symbol]
parseString = traverse lookupSymbol
  where
    lookupSymbol = \case
      '(' -> Just $ Symbol Open Round
      '[' -> Just $ Symbol Open Square
      '{' -> Just $ Symbol Open Curly
      '<' -> Just $ Symbol Open Angle
      ')' -> Just $ Symbol Close Round
      ']' -> Just $ Symbol Close Square
      '}' -> Just $ Symbol Close Curly
      '>' -> Just $ Symbol Close Angle
      _   -> Nothing

day10a :: [[Symbol]] :~> Int
day10a = MkSol
    { sParse = traverseLines parseString
    , sShow  = show
    , sSolve = Just . sum . map (either bracketScore (const 0) . runSymbols)
    }
  where
    bracketScore :: Bracket -> Int
    bracketScore = \case
      Round  -> 3
      Square -> 57
      Curly  -> 1197
      Angle  -> 25137

day10b :: [[Symbol]] :~> Int
day10b = MkSol
    { sParse = sParse day10a
    , sShow  = show
    , sSolve = takeMid . sort . mapMaybe (either (const Nothing) (Just . getScore) . runSymbols)
    }
  where
    getScore :: [Bracket] -> Int
    getScore = go 0
      where
        go n (b:xs) = go (n * 5 + bracketScore b) xs
        go n [] = n
    bracketScore = \case
      Round  -> 1
      Square -> 2
      Curly  -> 3
      Angle  -> 4

-- | Return the middle item in a list.  Step through the list at two
-- different speeds and return when the double-speed one hits the end.
takeMid :: [a] -> Maybe a
takeMid qs = go qs qs
  where
    go (_:xs) (_:_:ys) = go xs ys
    go (x:_) _ = Just x
    go [] _ = Nothing

