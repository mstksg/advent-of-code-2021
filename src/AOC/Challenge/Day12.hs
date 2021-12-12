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

-- False: small
-- True: large
toAdjMatrix :: [(String, String)] -> Map String [(Bool, String)]
toAdjMatrix = fmap (mapMaybe postProcess)
            . M.fromListWith (++)
            . concatMap (uncurry buildLinks)
  where
    buildLinks a b = [(a, [b]), (b, [a])]
    postProcess str = (all isUpper str, str) <$ guard (str /= "start")

day12a :: [(String, String)] :~> Int
day12a = MkSol
    { sParse = traverseLines $ listTup . splitOn "-"
    , sShow  = show
    , sSolve = Just . length . findPaths . toAdjMatrix
    }

findPaths :: Map String [(Bool, String)] -> [[String]]
findPaths mp = go S.empty "start"
  where
    go seen currPos
      | currPos == "end" = pure ["end"]
      | otherwise = do
          (isLarge, nextBranch) <- mp M.! currPos
          guard $ isLarge || (nextBranch `S.notMember` seen)
          (currPos:) <$> go (S.insert nextBranch seen) nextBranch

day12b :: [(String, String)] :~> Int
day12b = MkSol
    { sParse = sParse day12a
    , sShow  = show
    , sSolve = Just . length . findPaths2 . toAdjMatrix
    }

findPaths2 :: Map String [(Bool, String)] -> [[String]]
findPaths2 mp = go S.empty Nothing "start"
  where
    go seen seenTwice currPos
      | currPos == "end" = pure ["end"]
      | otherwise = do
          (isLarge, nextBranch) <- mp M.! currPos
          newSeenTwice <- if not isLarge && (nextBranch `S.member` seen)
            then Just nextBranch <$ guard (isNothing seenTwice)
            else pure seenTwice
          (currPos:) <$> go (S.insert nextBranch seen) newSeenTwice nextBranch
