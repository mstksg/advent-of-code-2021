{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day04 (
    day04a
  , day04b
  , wins
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
import qualified Data.Vector.Sized as SV

day04a :: _ :~> _
day04a = MkSol
    { sParse = parseCards . splitOn "\n\n"
    , sShow  = show
    -- , sSolve = \(picks, bs) -> Just $  picks
    , sSolve = \(picks, bs) -> 
        fmap (\(n,b) -> (* (picks !! (n-1))) . getSum . foldMap (foldMap (go (S.fromList (take n picks)))) $ b) . listToMaybe $
         sortOn fst $ mapMaybe (\b -> (,b) <$> eatUntilVictory b S.empty picks) bs
    }
  where
    go picks i
      | i `S.member` picks = Sum 0
      | otherwise          = Sum i

type Line = SV.Vector 5

eatUntilVictory
    :: Line (Line Int)
    -> Set (Finite 5, Finite 5)
    -> [Int]
    -> Maybe Int
eatUntilVictory _ _ [] = Nothing
eatUntilVictory xs spots (p:ps)
    | isBingo   = Just 0
    | otherwise = (+1) <$> eatUntilVictory xs newBoard ps
  where
    foundIx = do
      i <- SV.findIndex (\x -> p `elem` x) xs
      j <- SV.elemIndex p (xs `SV.index` i)
      pure (i, j)
    newBoard = case foundIx of
      Nothing -> spots
      Just (i, j) -> S.insert (i,j) spots
    -- isBingo = spots `S.member` wins
    isBingo = any (`S.isSubsetOf` spots) wins

wins :: [Set (Finite 5, Finite 5)]
wins = verts ++ map (S.map swap) verts
-- wins = diag1 : diag2 : verts ++ map (S.map swap) verts
  where
    verts =
      [ S.fromList [(i,0),(i,1),(i,2),(i,3),(i,4)]
      | i <- finites
      ]
    -- diag1 = S.fromList $ [ (i,i) | i <- finites ]
    -- diag2 = S.fromList $ [ (i,maxBound - i) | i <- finites ]

-- wins :: Set (Line (Line Bool))
-- wins = S.fromList $ diag1 : fmap SV.reverse diag1 : verts ++ map L.transpose verts
--   where
--     verts = catMaybes
--       [ SV.fromList [pure True, pure False, pure False, pure False, pure False]
--       , SV.fromList [pure False, pure True, pure False, pure False, pure False]
--       , SV.fromList [pure False, pure False, pure True, pure False, pure False]
--       , SV.fromList [pure False, pure False, pure False, pure True, pure False]
--       , SV.fromList [pure False, pure False, pure False, pure False, pure True]
--       ]
--     diag1 :: Line (Line Bool)
--     diag1 = fromJust . SV.fromList . catMaybes $
--       [ SV.fromList [True, False, False, False, False]
--       , SV.fromList [False, True, False, False, False]
--       , SV.fromList [False, False, True, False, False]
--       , SV.fromList [False, False, False, True, False]
--       , SV.fromList [False, False, False, False, True]
--       ]

parseCards :: [String] -> Maybe ([Int], [Line (Line Int)])
parseCards str = do
    pics:rest <- Just str
    picNums <- traverse readMaybe $ splitOn "," pics
    boards <- for rest $ \qtr -> do
      bb <- for (lines qtr) $ \qq ->
        SV.fromList =<< traverse readMaybe (words qq)
      SV.fromList bb
    pure (picNums, boards)

-- 42,44,71,26,70,92,77,45,6,18,79,54,31,34,64,32,16,55,81,11,90,10,21,87,0,84,8,23,1,12,60,20,57,68,61,82,49,59,22,2,63,33,50,39,28,30,88,41,69,72,98,73,7,65,53,35,96,67,36,4,51,75,24,86,97,85,66,29,74,40,93,58,9,62,95,91,80,99,14,19,43,37,27,56,94,25,83,48,17,38,78,15,52,76,5,13,46,89,47,3

-- 48 69 68 49 13
-- 25 14 30 74 89
-- 16 38 19 24 29
-- 56 97 50 65 79
-- 57 52  5 27 76

day04b :: _ :~> _
day04b = MkSol
    { sParse = sParse day04a
    , sShow  = show
    -- , sSolve = \(picks, bs) -> Just $  picks
    , sSolve = \(picks, bs) -> 
        fmap (\(n,b) -> (* (picks !! (n-1))) . getSum . foldMap (foldMap (go (S.fromList (take n picks)))) $ b) . listToMaybe $
         reverse . sortOn fst $ mapMaybe (\b -> (,b) <$> eatUntilVictory b S.empty picks) bs
    }
  where
    go picks i
      | i `S.member` picks = Sum 0
      | otherwise          = Sum i
