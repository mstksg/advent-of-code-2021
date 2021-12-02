-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import           AOC.Common (traverseLines)
import           AOC.Solver ((:~>)(..))
import           Linear.V2
import           Text.Read  (readMaybe)

day02a :: [(V2 Int)] :~> Int
day02a = MkSol
    { sParse = traverseLines $ parseAsDir
            (\x -> V2 x 0)
            (\y -> V2 0 y)
    , sShow  = show
    , sSolve = Just . product @V2 . sum
    }

data SubState = SubState { pX :: !Int, pY :: !Int, aY :: !Int }
  deriving stock Show

-- | semi-direct product
instance Semigroup SubState where
    SubState x y a <> SubState x' y' a' =
      SubState (x + x') (y + y' + x' * a) (a + a')

instance Monoid SubState where
    mempty = SubState 0 0 0

day02b :: [SubState] :~> Int
day02b = MkSol
    { sParse = traverseLines $ parseAsDir
        (\x -> SubState x 0 0)
        (\a -> SubState 0 0 a)
    , sShow  = show
    , sSolve = Just . summarize . mconcat
    }
  where
    summarize (SubState x y _) = x * y

parseAsDir
    :: (Int -> r)   -- ^ forward
    -> (Int -> r)   -- ^ up or down
    -> String
    -> Maybe r
parseAsDir f g ln = do
    dir:n:_ <- Just $ words ln
    amnt    <- readMaybe n
    case dir of
      "forward" -> Just $ f amnt
      "down"    -> Just $ g amnt
      "up"      -> Just $ g (-amnt)
      _         -> Nothing

