{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!
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

module AOC.Challenge.Day16 (
    day16a
  , day16b
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
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import Numeric.Lens
import qualified Text.Megaparsec.Char.Lexer     as PP

type Version = Int
data Packet = Operator Version Int [Packet]
            | Literal Version Int
  deriving stock (Show, Generic)
  deriving anyclass NFData

makeBaseFunctor ''Packet

day16 :: Show a => (PacketF a -> a) -> TokStream Bool :~> a
day16 alg = MkSol
    { sParse = Just . TokStream . concatMap (maybe [] (padzero . map (== '1')) . fmap (review binary) . digitToIntSafe)
    , sShow  = show
    , sSolve = fmap (cata alg . snd) . preview _Right . P.runParser parsePacket ""
    }
  where
    padzero xs = reverse . take 4 $ reverse xs ++ repeat False

day16a :: TokStream Bool :~> Int
day16a = day16 \case
    OperatorF v _ ps -> v + sum ps
    LiteralF v _     -> v

day16b :: TokStream Bool :~> Int
day16b = day16 evalPackF

evalPackF :: PacketF Int -> Int
evalPackF = \case
    OperatorF _ o is -> evalOp o is
    LiteralF _ i -> i
  where
    evalOp = \case
      0 -> sum
      1 -> product
      2 -> minimum
      3 -> maximum
      5 -> \case [a,b] -> if a > b then 1 else 0
                 _ -> -1
      6 -> \case [a,b] -> if a < b then 1 else 0
                 _ -> -1
      7 -> \case [a,b] -> if a == b then 1 else 0
                 _ -> -1
      _ -> const (-1) -- error

type Parser = P.Parsec Void (TokStream Bool)

-- includes the length of items parsed
parsePacket :: Parser (Int, Packet)
parsePacket = do
    v <- replicateM 3 P.anySingle
    Just vn <- pure $ bitToNum v
    t <- replicateM 3 P.anySingle
    Just tn <- pure $ bitToNum t
    case tn of
      4 -> bimap (+ 6) (Literal vn) <$> parseLiteral
      _ -> bimap (+ 6) (Operator vn tn) <$> parseOperator
  where
    parseLiteral = do
      n       <- parseLitChunks
      Just pn <- pure $ bitToNum (concat n)
      pure (length n * 5, pn)
    parseLitChunks = do
      goOn <- P.anySingle
      digs <- replicateM 4 P.anySingle
      if goOn
        then (digs:) <$> parseLitChunks
        else pure [digs]
    parseOperator = do
      lt <- P.anySingle
      if lt
        then do
          Just n <- bitToNum <$> replicateM 11 P.anySingle
          (len, ps) <- unzip <$> replicateM n parsePacket
          pure (sum len + 11 + 1, ps)
        else do
          Just n  <- bitToNum <$> replicateM 15 P.anySingle
          (n+1+15,) <$> parsePacketsLength n
    parsePacketsLength n = do
      (ln, p) <- parsePacket
      if ln == n
        then pure [p]
        else (p:) <$> parsePacketsLength (n-ln)

bitToNum :: [Bool] -> Maybe Int
bitToNum = preview binary . map (\case False -> '0'; True -> '1')
