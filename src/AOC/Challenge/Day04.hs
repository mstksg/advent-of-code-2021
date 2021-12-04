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

module AOC.Challenge.Day04 (
    day04a
  , day04b
  , wins
  ) where

import           AOC.Common               (loopEither)
import           AOC.Solver               ((:~>)(..))
import           Data.Bifunctor           (first)
import           Data.Finite              (Finite, finites, combineProduct)
import           Data.Foldable            (toList)
import           Data.Functor.Foldable    (ana, hylo)
import           Data.Functor.Foldable.TH (makeBaseFunctor)
import           Data.IntMap              (IntMap)
import           Data.IntSet              (IntSet)
import           Data.List.NonEmpty       (NonEmpty)
import           Data.List.Split          (splitOn)
import           Data.Maybe               (catMaybes, mapMaybe)
import           Data.Ord                 (comparing)
import           Data.Traversable         (for)
import           Data.Tuple               (swap)
import           Safe.Foldable            (maximumByMay)
import           Text.Read                (readMaybe)
import qualified Data.IntMap              as IM
import qualified Data.IntSet              as IS
import qualified Data.List.NonEmpty       as NE

type Board = IntMap Int

data Game = Step Game
          | Win Int    -- ^ checksum: sum of unseen numbers * last called
          | Loss

makeBaseFunctor ''Game

day04a :: ([Int], [Board]) :~> Int
day04a = MkSol
    { sParse = parseCards . splitOn "\n\n"
    , sShow  = show
    , sSolve = \(picks, bs) ->
        let initGames = map (\b -> ana (gameCoalg b) (GameState picks IS.empty)) bs
        in  loopEither stepAndWin initGames
    }

day04b :: ([Int], [Board]) :~> Int
day04b = MkSol
    { sParse = parseCards . splitOn "\n\n"
    , sShow  = show
    , sSolve = \(picks, bs) ->
          fmap snd
        . maximumByMay (comparing fst)
        . mapMaybe (\b -> hylo gameAlg (gameCoalg b) (GameState picks IS.empty))
        $ bs
    }

-- | All winning lines
wins :: [IntSet]
wins = IS.fromList . map (fromIntegral . combineProduct) <$> (verts ++ map (map swap) verts)
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
    boards    <- for rest $
          fmap (IM.fromList . flip zip [0..])
        . traverse readMaybe . words . unwords . lines
    pure (picNums, boards)

data GameState = GameState
    { gsQueue  :: [Int]
    , gsMarked :: IntSet
    }

gameCoalg
    :: Board
    -> GameState
    -> GameF GameState
gameCoalg board GameState{..} =
  case gsQueue of
    []   -> LossF
    p:ps ->
      let foundIx   = p `IM.lookup` board
          newMarked = maybe id IS.insert foundIx gsMarked
          boardWon  = any (`IS.isSubsetOf` newMarked) wins
          unMarked  = sum . IM.keys $
            IM.filter (`IS.notMember` newMarked) board
      in  if boardWon
            then WinF $ unMarked * p
            else StepF $ GameState ps newMarked

-- | returns number of turns taken, and the checksum
gameAlg
    :: GameF (Maybe (Int, Int))
    -> Maybe (Int, Int)
gameAlg = \case
    StepF x -> first (+1) <$> x
    WinF i  -> Just (1, i)
    LossF   -> Nothing

-- | Step all games and quit on the first won
stepAndWin :: [Game] -> Either (Maybe Int) [Game]
stepAndWin = traverse $ \case
    Step g' -> Right g'
    Win i   -> Left (Just i)
    Loss    -> Left Nothing     -- assume that one loss means all remaining have lost

-- | Filter all won & lost games.  If none are remaining, show the wins
-- emitted that final round.
_stepAndFilter :: NonEmpty Game -> Either [Int] (NonEmpty Game)
_stepAndFilter gs = case NE.nonEmpty (catMaybes remaining) of
    Nothing  -> Left emitted
    Just gs' -> Right gs'
  where
    (emitted, remaining) = for (toList gs) $ \case
      Step g' -> ([], Just g')
      Win i   -> ([i], Nothing)
      Loss    -> ([], Nothing)
