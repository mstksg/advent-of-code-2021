{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day08 where

-- module AOC.Challenge.Day08 (
--     day08a
--   , day08b
--   , mappings
--   ) where

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

day08a :: _ :~> _
day08a = MkSol
    { sParse = traverseLines $
         listTup . map words . splitOn " | "
    -- { sParse = _ . splitOn ","
    , sShow  = show
    , sSolve = Just . countTrue ((`elem` udigs) . length) . concatMap snd
    }
  where
    udigs = [2,4,3,7]

mappings :: [Map Char (Finite 7)]
mappings = M.fromList . zip "abcdefg" <$> permutations finites
        -- sequence $ M.fromList ((,finites) <$> "abcdefg")

signals :: Map (Set (Finite 7)) Char
signals = M.fromList . flip zip ['0'..'9'] $
    [ S.fromList [0,1,2,4,5,6]
    , S.fromList [2,5]
    , S.fromList [0,2,3,4,6]
    , S.fromList [0,2,3,5,6]
    , S.fromList [1,2,3,5]
    , S.fromList [0,1,3,5,6]
    , S.fromList [0,1,3,4,5,6]
    , S.fromList [0,2,5]
    , S.fromList [0,1,2,3,4,5,6]
    , S.fromList [0,1,2,3,5,6]
    ]

day08b :: _ :~> [Int]
day08b = MkSol
    { sParse = traverseLines $
         listTup . map words . splitOn " | "
    , sShow  = show . sum . traceShowId 
    , sSolve = traverse $ \(xs, ys) -> do
        firstJust (`decodeFromMapping` (xs, ys)) mappings
        -- guard $ length ys == 4
        -- readMaybe $ map ((signals M.!) . S.fromList . map (mapping M.!)) ys
        -- let allWords = traceShowId $ foldMap (S.fromList . uncurry (++)) xs
        -- in  map 
        -- firstJust (\mp -> mp <$ guard
        --                             (all
        --                               ((`M.member` signals) . S.fromList . map (mp M.!)

        --                               )
        --                             allWords)
        --                         ) mappings
    }
  where
decodeDigit :: Map Char (Finite 7) -> String -> Maybe Char
decodeDigit mp = (`M.lookup` signals) . S.fromList . map (mp M.!)
decodeString :: Map Char (Finite 7) -> [String] -> Maybe Int
decodeString mp = readMaybe <=< traverse (decodeDigit mp)
decodeFromMapping :: Map Char (Finite 7) -> ([String], [String]) -> Maybe Int
decodeFromMapping mp (xs, ys) = do
  _ <- traverse (decodeDigit mp) xs
  decodeString mp ys

