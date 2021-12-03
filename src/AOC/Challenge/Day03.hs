{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 3.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day03 (
    day03a
  , day03b
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
import qualified Data.List as Li

day03a :: _ :~> _
day03a = MkSol
    { sParse = Just . Li.transpose . map (map fst . Li.sortOn snd . zip [0,1] . map Li.length . Li.group . Li.sort) . Li.transpose . lines
    -- i used -XBinaryLiterals in ghci lol
    , sShow  = \_ -> show 693486
    , sSolve = Just . map (concatMap show)

    }

day03b :: _ :~> _
day03b = MkSol
    { sParse = Just . lines
    -- i used -XBinaryLiterals in ghci lol
    , sShow = const $ show 3379326
    -- , sShow  = show . (map . map ) (\case Z -> '0'; O -> '1')
    , sSolve = \ys -> do
        let xs = flip (map . map) ys $ \case '0' -> Z; '1' -> O
        oxy <- searchFor True O xs
        car <- searchFor False Z xs
        pure [oxy, car]
    }


data Bit = Z | O
  deriving (Eq, Ord, Show, Generic)

instance NFData Bit

searchFor
    ::
     Bool     -- ^ least or most common (true: most)
    -> Bit        -- ^ tie breaker (1 or 0)
    -> [[Bit]]
    -> Maybe [Bit]
searchFor revFirst tb = go
  where
    go :: [[Bit]] -> Maybe [Bit]
    go xs = case filtered of
        [] -> Nothing
        [theOne] -> Just $ bitToKeep : theOne
        theMore -> (bitToKeep:) <$> go filtered
      where
        fs = freqs $ map head xs
        numZ = lookupFreq Z fs
        numO = lookupFreq O fs
        filtered = map tail . filter ((== bitToKeep) . head) $ xs
        bitToKeep
          | revFirst = case compare numZ numO of
              LT -> O
              EQ -> tb
              GT -> Z
          | otherwise = case compare numZ numO of
              LT -> Z
              EQ -> tb
              GT -> O
-- freqList :: (Foldable f, Ord a) => f a -> [(Int, a)]
    -- go = (\[(xi, (xn, xs)),(yi, (yn, ys))] ->
    --           if xn == yn
    --             then

    --      )
    --     . (if revFirst then reverse else id)
    --     . Li.sortOn (fst . snd)
    --     . map (\xs -> (head (head xs), (length xs, tail xs)))
    --     . Li.groupBy ((==) `on` head) . Li.sort
