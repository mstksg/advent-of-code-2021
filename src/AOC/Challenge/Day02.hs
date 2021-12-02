{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
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

day02a :: [(Point)] :~> _
day02a = MkSol
    { sParse = Just . mapMaybeLines (\x -> case words x of
        "forward":n:_ -> Just $ (V2 (read n) 0)
        "down":n:_ -> Just (V2 0 (read n))
        "up":n:_ -> Just (V2 0 (negate (read n)))
    )
    , sShow  = show
    , sSolve = Just . product . sum
    }

-- -   `down X` *increases* your aim by `X` units.
-- -   `up X` *decreases* your aim by `X` units.
-- -   `forward X` does two things:
--     -   It increases your horizontal position by `X` units.
--     -   It increases your depth by your aim *multiplied by* `X`.

day02b :: [Either Int Int] :~> _
day02b = MkSol
    { sParse = Just . mapMaybeLines (\x -> case words x of
        "forward":n:_ -> Just $ Left (read n)
        "down":n:_ -> Just $ Right (read n)
        "up":n:_ -> Just $ Right . negate $ read n
      )
    , sShow  = show
    , sSolve = Just . product . fst . foldl' go (0, 0)
    }
  where
    go (pos, aim) = \case
      Left i -> (pos + V2 i (i*aim), aim)
      Right i -> (pos, aim + i)
