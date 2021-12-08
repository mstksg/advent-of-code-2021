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

module AOC.Challenge.Day08 (
    day08a
  , day08b
  , allPossibleWires
  , lookupTrie
  , toWires
  , Trie(..)
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.DList.DNonEmpty as DNE
import qualified Data.List.PointedList.Circular as PLC
import           Data.Tuple.Strict
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import           Data.Functor.Foldable    (ana, hylo)
import           Data.Functor.Foldable.TH (makeBaseFunctor)
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

newtype Segment = Segment { getSegment :: Finite 7 }
  deriving stock (Eq, Ord, Show)
type Display = Set Segment

newtype Wire = Wire { getWire :: Finite 7 }
  deriving stock (Eq, Ord, Show)
type Wires = Set Wire

type WireMap = Map Wire Segment

_CharWire :: Prism' Char Wire
_CharWire = prism'
              (\(Wire w) -> chr $ fromIntegral (getFinite w) + ord 'a')
              (\c -> Wire <$> packFinite (fromIntegral (ord c - ord 'a')))

day08a :: _ :~> _
day08a = MkSol
    { sParse = traverseLines $
         listTup . map words . splitOn " | "
    , sShow  = show
    , sSolve = Just . countTrue ((`elem` udigs) . length) . concatMap snd
    }
  where
    udigs = [2,4,3,7]

mappings :: [Map Wire Segment]
mappings = M.fromList . zip (Wire <$> finites) <$> permutations (Segment <$> finites)

data Trie k a = TNode (Maybe a) (Map k (Trie k a))
  deriving stock (Functor, Show)

makeBaseFunctor ''Trie

insertTrie
    :: Ord k
    => (a -> a -> a)        -- ^ new, old
    -> [k]
    -> a
    -> Trie k a
    -> Trie k a
insertTrie f ks0 x = go ks0
  where
    go [] (TNode y ts) = TNode (Just $ maybe x (f x) y) ts
    go (k:ks) (TNode y ts) = TNode y $
      M.alter (Just . maybe (TNode (Just x) M.empty) (go ks)) k ts

singletonTrie :: [k] -> a -> Trie k a
singletonTrie ks0 x = go ks0
  where
    go [] = TNode (Just x) M.empty
    go (k:ks) = TNode Nothing $ M.singleton k (go ks)

lookupTrie :: Ord k => [k] -> Trie k a -> Maybe a
lookupTrie [] (TNode x _) = x
lookupTrie (k:ks) (TNode _ ts) = do
    t <- M.lookup k ts
    lookupTrie ks t

trieFromListWith :: Ord k => (a -> a -> a) -> NonEmpty ([k], a) -> Trie k a
trieFromListWith f = ana $ \xs ->
    let T2 here there = flip foldMap xs $ \case
          ([]  , a) -> T2 (Endo (a:)) mempty
          (y:ys, a) -> T2 mempty (Endo ((y, DNE.singleton (ys, a)):))
    in  TNodeF
            (foldr1 f <$> NE.nonEmpty (appEndo here []))
            (DNE.toNonEmpty <$> M.fromListWith (<>) (appEndo there []))
    

allPossibleWires :: Trie Wires (Set WireMap)
allPossibleWires = trieFromListWith (<>) $ (second S.singleton) <$> NE.fromList poss
  where
    poss :: [([Wires], WireMap)]
    poss = do
      perm <- permutations $ Wire <$> finites
      let mp = M.fromList $ zip (Segment <$> finites) perm
          mp' = M.fromList $ zip perm (Segment <$> finites)
          sigSet = sort $ S.map (mp M.!) <$> M.keys signals
      visible <- catMaybes <$> traverse (\wrs -> [Nothing, Just wrs]) sigSet
      pure (visible, mp')
    

signals :: Map (Set Segment) Char
signals = M.fromList . flip zip ['0'..'9'] . map (S.fromList . map Segment) $
    [ [0,1,2,4,5,6]
    , [2,5]
    , [0,2,3,4,6]
    , [0,2,3,5,6]
    , [1,2,3,5]
    , [0,1,3,5,6]
    , [0,1,3,4,5,6]
    , [0,2,5]
    , [0,1,2,3,4,5,6]
    , [0,1,2,3,5,6]
    ]

toWires :: String -> Wires
toWires = S.fromList . map (^?! _CharWire)

day08b :: [([Wires], [Wires])] :~> [Int]
day08b = MkSol
    { sParse = traverseLines $
         listTup . map (map (S.fromList . map (^?! _CharWire)) . words) . splitOn " | "
    , sShow  = show . sum
    , sSolve = traverse $ \xs -> firstJust (`decodeFromMapping` xs) mappings
    }
  where
    decodeDigit :: Map Wire Segment -> Wires -> Maybe Char
    decodeDigit mp = (`M.lookup` signals) . mapMaybeSet (`M.lookup` mp)
    decodeString :: Map Wire Segment -> [Wires] -> Maybe Int
    decodeString mp = readMaybe <=< traverse (decodeDigit mp)
    decodeFromMapping :: Map Wire Segment -> ([Wires], [Wires]) -> Maybe Int
    decodeFromMapping mp (xs, ys) = do
      traverse_ (decodeDigit mp) xs
      decodeString mp ys

