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

import           AOC.Common                    (traverseLines)
import           AOC.Common.Point              (Point)
import           AOC.Solver                    ((:~>)(..))
import           Data.Semigroup                (Sum(..))
import           Linear.V2                     (V2(..))
import           Text.Read                     (readMaybe)
import qualified Data.Monoid.Action            as Mo
import qualified Data.Monoid.SemiDirectProduct as Mo

day02a :: [Sum Point] :~> Int
day02a = day02
    (\x -> Sum $ V2 x 0)
    (\y -> Sum $ V2 0 y)
    getSum

day02b :: [Mo.Semi (Sum Point) Aim] :~> Int
day02b = day02
    (\x -> Mo.inject (Sum (V2 x 0)))
    (\a -> Mo.embed  (Aim a))
    (getSum . Mo.untag)



-- | The difference between Part 1 and Part 2 is just a different monoid
day02
    :: Monoid r
    => (Int -> r)       -- ^ forward
    -> (Int -> r)       -- ^ up/down
    -> (r -> Point)     -- ^ re-extract position
    -> [r] :~> Int
day02 f g ext = MkSol
    { sParse = traverseLines parseAsDir
    , sShow  = show
    , sSolve = Just . product @V2 . ext . mconcat
    }
  where
    parseAsDir ln = do
      dir:n:_ <- Just $ words ln
      amnt    <- readMaybe n
      case dir of
        "forward" -> Just $ f amnt
        "down"    -> Just $ g amnt
        "up"      -> Just $ g (-amnt)
        _         -> Nothing


newtype Aim = Aim Int
deriving via Sum Int instance Semigroup Aim
deriving via Sum Int instance Monoid Aim

instance Mo.Action Aim (Sum Point) where
    act (Aim a) (Sum (V2 x y)) = Sum (V2 x (y + a * x))

