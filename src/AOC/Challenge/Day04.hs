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

import           Data.Finite                     (combineProduct)
import           Data.Functor.Foldable    hiding (fold)
import           Data.Functor.Foldable.TH        (makeBaseFunctor)
import qualified Data.Graph.Inductive            as G
import qualified Data.IntMap                     as IM
import qualified Data.IntSet                     as IS
import qualified Data.List.NonEmpty              as NE
import qualified Data.List.PointedList           as PL
import qualified Data.List.PointedList.Circular  as PLC
import qualified Data.Map                        as M
import qualified Data.OrdPSQ                     as PSQ
import qualified Data.Sequence                   as Seq
import qualified Data.Set                        as S
import qualified Data.Text                       as T
import qualified Data.Vector                     as V
import qualified Data.Vector.Sized               as SV
import qualified Linear                          as L
import qualified Text.Megaparsec                 as P
import qualified Text.Megaparsec.Char            as P
import qualified Text.Megaparsec.Char.Lexer      as PP

data Game = Step Game
          | Win Int    -- ^ checksum: sum of unseen numbers * last called
          | Loss

makeBaseFunctor ''Game

day04a :: ([Int], [Board]) :~> Int
day04a = MkSol
    { sParse = parseCards . splitOn "\n\n"
    , sShow  = show
    , sSolve = \(picks, bs) ->
            listToMaybe . map snd . sortOn fst $
                mapMaybe (\b -> hylo gameAlg (gameCoalg b) (GameState picks S.empty)) bs
    }

day04b :: ([Int], [Board]) :~> Int
day04b = MkSol
    { sParse = parseCards . splitOn "\n\n"
    , sShow  = show
    , sSolve = \(picks, bs) ->
            listToMaybe . reverse . map snd . sortOn fst $
                mapMaybe (\b -> hylo gameAlg (gameCoalg b) (GameState picks S.empty)) bs
    }

type Board = SV.Vector 25 Int

-- | All winning lines
wins :: [Set (Finite 25)]
wins = S.fromList . map combineProduct <$> (verts ++ map (map swap) verts)
  where
    verts :: [[(Finite 5, Finite 5)]]
    verts =
      [ [(i, j) | j <- finites]
      | i <- finites
      ]

parseCards :: [String] -> Maybe ([Int], [Board])
parseCards str = do
    pics:rest <- Just str
    picNums   <- traverse readMaybe $ splitOn "," pics
    boards    <- for rest $ \qtr -> do
      let oneBigVec = words . unwords . lines $ qtr
      SV.fromList =<< traverse readMaybe oneBigVec
    pure (picNums, boards)

data GameState = GameState
    { gsQueue  :: [Int]
    , gsMarked :: Set (Finite 25)
    }

gameCoalg
    :: Board
    -> GameState
    -> GameF GameState
gameCoalg board GameState{..} =
  case gsQueue of
    []   -> LossF
    p:ps ->
      let foundIx   = p `SV.elemIndex` board
          newMarked = maybe id S.insert foundIx gsMarked
          boardWon  = any (`S.isSubsetOf` newMarked) wins
          unMarked  = flip ifoldMap' board $ \i q ->
            if i `S.member` newMarked then 0 else Sum q
      in  if boardWon
            then WinF $ getSum unMarked * p
            else StepF $ GameState ps newMarked

-- | returns number of turns taken, and the checksum
gameAlg
    :: GameF (Maybe (Int, Int))
    -> Maybe (Int, Int)
gameAlg = \case
    StepF x -> first (+1) <$> x
    WinF i  -> Just (1, i)
    LossF   -> Nothing
