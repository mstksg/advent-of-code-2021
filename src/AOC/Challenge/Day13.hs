{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 13.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day13 (
    day13a
  , day13b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.Set.NonEmpty as NES
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import AOC.Common.OCR
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP
import Data.Bitraversable

day13a :: ([Point], [Point]) :~> _
day13a = MkSol
    { sParse = bitraverse
                    (traverseLines $ traverse readMaybe <=< listV2 . splitOn ",")
                    (traverseLines $ uncurry parseFold <=< listTup . splitOn "=")
                <=< listTup . splitOn "\n\n"
    , sShow  = show
    , sSolve = \(ptList, folds) -> Just
        let ptSet = S.fromList ptList
        in  S.size $ foldr go ptSet [head folds]
    }
  where
    parseFold ax v = do
      xy <- listToMaybe (reverse ax)
      vv <- readMaybe v
      pure $
        if xy == 'x'
          then V2 vv 0
          else V2 0 vv
    go axis = S.map $ \p -> abs (p - axis)

day13b :: ([Point], [(Bool, Int)]) :~> _
day13b = MkSol
    { sParse = bitraverse
                    (traverseLines $ traverse readMaybe <=< listV2 . splitOn ",")
                    (traverseLines $ uncurry parseFold <=< listTup . splitOn "=")
                <=< listTup . splitOn "\n\n"
    -- , sShow  = unlines . map (displayAsciiSet '.' '#' . NES.toSet) . toList . contiguousShapes
    -- , sShow = show
    , sShow = NE.head . parseLettersAll
-- parseLettersAll
--     :: Set Point
--     -> NonEmpty String
    , sSolve = \(ptList, folds) -> Just
        let ptSet = S.fromList ptList
        in  foldr go ptSet (reverse folds)
    }
  where
    parseFold ax v = do
      xy <- listToMaybe (reverse ax)
      vv <- readMaybe v
      pure (xy == 'x', vv)
    go (isX, a) = S.map $
      let axFunc | isX = over _x
                 | otherwise = over _y
      in  axFunc $ \i -> negate (abs (i - a)) + a

-- revAbs x
--   | x > 0     = negate x
--   | otherwise = x
