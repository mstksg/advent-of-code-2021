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

import           Data.Bitraversable
import           Data.Finitary
import qualified AOC.Common.FinitarySet         as FS
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

data SrcNode = StartNode
             | SrcCave Cave
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (Finitary)

data DestNode = DestCave Cave
              | EndNode
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (Finitary)

data FullNode = FullStart
              | FullCave Cave
              | FullEnd
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (Finitary)

data Ident = Ident (Finite 26) (Maybe (Finite 26))
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (Finitary)

data Cave = Big Ident
          | Small Ident
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (Finitary)

classify :: String -> Maybe FullNode
classify "start" = Just FullStart
classify "end" = Just FullEnd
classify [a] = charFinite a <&> \case
    (False, x) -> FullCave $ Small (Ident x Nothing)
    (True , x) -> FullCave $ Big (Ident x Nothing)
classify [a,b] = do
    (cx, x) <- charFinite a
    (cy, y) <- charFinite b
    let ident = Ident x (Just y)
    case (cx, cy) of
      (False, False) -> Just . FullCave $ Small ident
      (True , True ) -> Just . FullCave $ Big ident
      _              -> Nothing
classify _ = Nothing

srcDest :: FullNode -> (Maybe SrcNode, Maybe DestNode)
srcDest = \case
  FullStart  -> (Just StartNode, Nothing)
  FullCave c -> (Just (SrcCave c), Just (DestCave c))
  FullEnd    -> (Nothing, Just EndNode)

toAdjMatrix :: [(FullNode, FullNode)] -> Map SrcNode [DestNode]
toAdjMatrix = M.fromListWith (++)
            . concatMap (uncurry buildLinks)
  where
    toLink a b = (a, [b])
    buildLinks a b = catMaybes
        [ toLink <$> srcA <*> destB
        , toLink <$> srcB <*> destA
        ]
      where
        (srcA, destA) = srcDest a
        (srcB, destB) = srcDest b

day12a :: [(FullNode, FullNode)] :~> Int
day12a = MkSol
    { sParse = traverseLines $ bitraverse classify classify <=< listTup . splitOn "-"
    , sShow  = show
    , sSolve = Just . length . findPaths . toAdjMatrix
    }

findPaths :: Map SrcNode [DestNode] -> [[Cave]]
findPaths mp = do
    nextBranch <- mp M.! StartNode
    case nextBranch of
      EndNode -> pure []
      DestCave v@(Big _)   -> go FS.empty v
      DestCave v@(Small c) -> go (FS.singleton c) v
  where
    go :: FS.FinitarySet Ident -> Cave -> [[Cave]]
    go seen currPos = do
      nextBranch <- mp M.! SrcCave currPos
      case nextBranch of
        EndNode -> pure [currPos]
        DestCave v@(Big _) -> (currPos:) <$> go seen v
        DestCave v@(Small c) -> do
          guard $ c `FS.notMember` seen
          (currPos:) <$> go (c `FS.insert` seen) v

day12b :: [(FullNode, FullNode)] :~> Int
day12b = MkSol
    { sParse = sParse day12a
    , sShow  = show
    , sSolve = Just . length . findPaths2 . toAdjMatrix
    }

findPaths2 :: Map SrcNode [DestNode] -> [[Cave]]
findPaths2 mp = do
    nextBranch <- mp M.! StartNode
    case nextBranch of
      EndNode -> pure []
      DestCave v@(Big _) -> go FS.empty Nothing v
      DestCave v@(Small c) -> go (FS.singleton c) Nothing v
  where
    go :: FS.FinitarySet Ident -> Maybe Ident -> Cave -> [[Cave]]
    go seen seenTwice currPos = do
      nextBranch <- mp M.! SrcCave currPos
      case nextBranch of
        EndNode -> pure [currPos]
        DestCave v@(Big _) -> (currPos:) <$> go seen seenTwice v
        DestCave v@(Small c) -> do
          newSeenTwice <- if c `FS.member` seen
            then Just c <$ guard (isNothing seenTwice)
            else pure seenTwice
          (currPos:) <$> go (c `FS.insert` seen) newSeenTwice v
