{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 9.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day09 (
    day09a
  , day09b
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

day09a :: _ :~> _
day09a = MkSol
    { sParse = Just . parseAsciiMap (Just . digitToInt)
    , sShow  = show
    , sSolve = \xs -> Just . sum $ M.mapWithKey (go xs) xs
    } 
  where
    go mp p i 
        | and $ mapMaybe f (cardinalNeighbs p) = (i+1)
        | otherwise = 0
      where
        f q = case M.lookup q mp of
          Nothing -> Nothing
          Just j -> Just $ j > i
-- cardinalNeighbs :: Point -> [Point]


    -- go xs = sum $ zipWith3 f xs (drop 1 xs) (drop 2 xs)
    --   where
    --     f a b c
    --      | a > b && c > b = 1 + b
    --      | otherwise = 0

day09b :: _ :~> _
day09b = MkSol
    { sParse = Just . parseAsciiMap (Just . digitToInt)
    , sShow  = show
    , sSolve = \xs -> Just $
        let lowpoints = M.keys $ M.filterWithKey (go xs) xs
        in  product . take 3 . reverse . sort . map (S.size . floodFill (spreadout xs) . S.singleton) $ lowpoints
    } 
  where
    go mp p i = and $ mapMaybe f (cardinalNeighbs p)
      where
        f q = case M.lookup q mp of
          Nothing -> Nothing
          Just j -> Just $ j > i
    spreadout mp p = S.fromList . filter valid . cardinalNeighbs $ p
      where
        valid q = case M.lookup q mp of
          Nothing -> False
          Just i  -> i  /= 9


-- -- | Flood fill from a starting set
-- floodFill
--     :: Ord a
--     => (a -> Set a)     -- ^ Expansion (be sure to limit allowed points)
--     -> Set a            -- ^ Start points
--     -> Set a            -- ^ Flood filled
-- floodFill f = snd . floodFillCount f
