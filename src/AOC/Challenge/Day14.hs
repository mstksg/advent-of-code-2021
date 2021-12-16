{-# OPTIONS_GHC -Wno-unused-imports   #-}
-- |
-- Module      : AOC.Challenge.Day14
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 14.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day14 (
    day14a
  , day14b
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
import Data.Bitraversable
import qualified Data.Map.NonEmpty as NEM
import Safe
import qualified Text.Megaparsec.Char.Lexer     as PP

day14 :: Int -> (String, [((Char, Char), Char)]) :~> Int
day14 n = MkSol
    { sParse = traverse (traverseLines (bitraverse listTup listToMaybe <=< listTup . splitOn " -> "))
           <=< listTup . splitOn "\n\n"
    , sShow  = show
    , sSolve = \(str, rs) -> do
        firstChar <- headMay str
        lastChar  <- lastMay str
        let rMap = M.fromList rs
            strPairs   = freqs $ slidingPairs str
            res        = iterate (expand rMap) strPairs !!! n
            compensate = M.fromList [(firstChar, 1), (lastChar, 1)]
            resFreqs   = M.unionWith (+) compensate . fmap (`div` 2) $ M.fromListWith (+)
              [ (k, v)
              | ((x, y), r) <- M.toList res
              , (k, v)      <- [(x,r),(y,r)]
              ]
            resFreqList = sort $ toList resFreqs
        lowFreq  <- headMay resFreqList
        highFreq <- lastMay resFreqList
        pure $ highFreq - lowFreq
    }
  where
    expand rMap strPairs = M.fromListWith (+)
      [ (k, v)
      | ((a, b), r) <- M.toList strPairs
      , (k, v) <- case M.lookup (a, b) rMap of
          Nothing -> [ ((a, b), r) ]
          Just q  -> [ ((a, q), r), ((q, b), r) ]
      ]

day14a :: (String, [((Char, Char), Char)]) :~> Int
day14a = day14 10

day14b :: (String, [((Char, Char), Char)]) :~> Int
day14b = day14 40
