{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day10 (
    day10a
  , day10b
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

day10a :: _ :~> _
day10a = MkSol
    { sParse = mapMaybeLinesJust Just
    , sShow  = show
    , sSolve = Just . sum . map countScore
    }
  where
    countScore = go []
      where
        go [] (')':_) = 3
        go [] (']':_) = 57
        go [] ('}':_) = 1197
        go [] ('>':_) = 25137
        go (s:ss) (')':xs) = if s == '(' then go ss xs else 3
        go (s:ss) (']':xs) = if s == '[' then go ss xs else 57
        go (s:ss) ('}':xs) = if s == '{' then go ss xs else 1197
        go (s:ss) ('>':xs) = if s == '<' then go ss xs else 25137
        go ss ('(':xs) = go ('(':ss) xs
        go ss ('[':xs) = go ('[':ss) xs
        go ss ('{':xs) = go ('{':ss) xs
        go ss ('<':xs) = go ('<':ss) xs
        go [] [] = 0
        go (_:_) [] = 0
           

    -- \case
    --   '<':xs -> countAngle xs

takeMid xs = xs !! n
  where
    n = length xs `div` 2
day10b :: _ :~> _
day10b = MkSol
    { sParse = sParse day10a
    , sShow  = show
    , sSolve = Just . takeMid . sort . map getScore . mapMaybe countScore
    }
  where
    getScore = go 0
      where
        go n ('(':xs) = go (n * 5 + 1) xs
        go n ('[':xs) = go (n * 5 + 2) xs
        go n ('{':xs) = go (n * 5 + 3) xs
        go n ('<':xs) = go (n * 5 + 4) xs
        go n [] = n
        go n xs = error xs
    countScore = go []
      where
        go [] (')':_) = Nothing
        go [] (']':_) = Nothing
        go [] ('}':_) = Nothing
        go [] ('>':_) = Nothing
        go (s:ss) (')':xs) = if s == '(' then go ss xs else Nothing
        go (s:ss) (']':xs) = if s == '[' then go ss xs else Nothing
        go (s:ss) ('}':xs) = if s == '{' then go ss xs else Nothing
        go (s:ss) ('>':xs) = if s == '<' then go ss xs else Nothing
        go ss ('(':xs) = go ('(':ss) xs
        go ss ('[':xs) = go ('[':ss) xs
        go ss ('{':xs) = go ('{':ss) xs
        go ss ('<':xs) = go ('<':ss) xs
        go [] [] = Nothing
        go ss@(_:_) [] = Just ss
