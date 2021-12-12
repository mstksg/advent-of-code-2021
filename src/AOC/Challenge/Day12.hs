{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day12
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 12.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day12 (
    day12a
  , day12b
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

day12a :: _ :~> _
day12a = MkSol
    { sParse = mapMaybeLinesJust $ \xs -> case splitOn "-" xs of
            [a,b] -> Just (a, b)
    , sShow  = show
    , sSolve = \xs ->  Just
        let allPaths = M.fromListWith (++) $ concatMap (\(a,b) -> [(a,[b]),(b,[a])]) xs
        in  S.size . S.fromList $ findPaths allPaths
    }

findPaths :: Map String [String] -> [[String]]
findPaths mp = go S.empty "start"
  where
    go seen currPos
      | currPos == "end" = [["end"]]
      | otherwise = do
          nextBranch <- mp M.! currPos
          guard $ nextBranch /= "start"
          guard $ if isSmall nextBranch
            then nextBranch `S.notMember` seen
            else True
          (currPos:) <$> go (S.insert nextBranch seen) nextBranch


isSmall = all isLower



day12b :: _ :~> _
day12b = MkSol
    { sParse = sParse day12a
    , sShow  = show
    , sSolve = \xs ->  Just
        let allPaths = M.fromListWith (++) $ concatMap (\(a,b) -> [(a,[b]),(b,[a])]) xs
        in  S.size . S.fromList $ findPaths2 allPaths
    }

findPaths2 :: Map String [String] -> [[String]]
findPaths2 mp = go S.empty Nothing "start"
  where
    go seen seenTwice currPos
      | currPos == "end" = [["end"]]
      | otherwise = do
          nextBranch <- mp M.! currPos
          guard $ nextBranch /= "start"
          newSeenTwice <- if isSmall nextBranch
            then if nextBranch `S.member` seen
              then case seenTwice of
                        Nothing -> [Just nextBranch]
                        Just _  -> []
              else [seenTwice]
            else [seenTwice]
          (currPos:) <$> go (S.insert nextBranch seen) newSeenTwice nextBranch
