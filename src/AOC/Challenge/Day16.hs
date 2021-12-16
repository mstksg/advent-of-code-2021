-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day16 (
    day16a
  , day16b
  ) where

import           AOC.Common               (TokStream(..), digitToIntSafe, parseBinary, toBinaryFixed)
import           AOC.Solver               ((:~>)(..))
import           Control.Applicative      (empty)
import           Control.DeepSeq          (NFData)
import           Control.Lens             (preview, _Right)
import           Control.Monad            (replicateM)
import           Data.Bifunctor           (bimap)
import           Data.Functor.Foldable    (cata)
import           Data.Functor.Foldable.TH (makeBaseFunctor)
import           Data.Void                (Void)
import           GHC.Generics             (Generic)
import qualified Text.Megaparsec          as P

type Version = Int
data Packet = Operator Version Op [Packet]
            | Literal Version Int
  deriving stock (Show, Generic)
  deriving anyclass NFData

data Op = OSum
        | OProd
        | OMin
        | OMax
        | OGT
        | OLT
        | OEQ
  deriving stock (Show, Generic)
  deriving anyclass NFData

makeBaseFunctor ''Packet

day16 :: Show a => (PacketF a -> a) -> TokStream Bool :~> a
day16 alg = MkSol
    { sParse = Just . TokStream
             . concatMap (maybe [] id . fmap (toBinaryFixed 4) . digitToIntSafe)
    , sShow  = show
    , sSolve = fmap (cata alg . snd) . preview _Right . P.runParser parsePacket ""
    }

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
      OSum  -> sum
      OProd -> product
      OMin  -> minimum
      OMax  -> maximum
      OGT   -> \case [a,b] -> if a > b then 1 else 0
                     _ -> -1
      OLT   -> \case [a,b] -> if a < b then 1 else 0
                     _ -> -1
      OEQ   -> \case [a,b] -> if a == b then 1 else 0
                     _ -> -1

type Parser = P.Parsec Void (TokStream Bool)

-- includes the length of items parsed
parsePacket :: Parser (Int, Packet)
parsePacket = do
    v <- parseBinary <$> replicateM 3 P.anySingle
    t <- parseType
    case t of
      Nothing -> bimap (+ 6) (Literal v) <$> parseLiteral
      Just o  -> bimap (+ 6) (Operator v o) <$> parseOperator
  where
    parseType :: Parser (Maybe Op)
    parseType = do
      t <- parseBinary <$> replicateM 3 P.anySingle
      case t of
        0 -> pure $ Just OSum
        1 -> pure $ Just OProd
        2 -> pure $ Just OMin
        3 -> pure $ Just OMax
        4 -> pure Nothing
        5 -> pure $ Just OGT
        6 -> pure $ Just OLT
        7 -> pure $ Just OEQ
        _ -> empty
    parseLiteral :: Parser (Int, Int)
    parseLiteral = do
      n  <- parseLitChunks
      pure (length n * 5, parseBinary (concat n))
    parseLitChunks :: Parser [[Bool]]
    parseLitChunks = do
      goOn <- P.anySingle
      digs <- replicateM 4 P.anySingle
      if goOn
        then (digs:) <$> parseLitChunks
        else pure [digs]
    parseOperator :: Parser (Int, [Packet])
    parseOperator = do
      lt <- P.anySingle
      if lt
        then do
          n <- parseBinary <$> replicateM 11 P.anySingle
          (len, ps) <- unzip <$> replicateM n parsePacket
          pure (sum len + 11 + 1, ps)
        else do
          n  <- parseBinary <$> replicateM 15 P.anySingle
          (n+1+15,) <$> parsePacketsLength n
    parsePacketsLength :: Int -> Parser [Packet]
    parsePacketsLength n = do
      (ln, p) <- parsePacket
      if ln == n
        then pure [p]
        else (p:) <$> parsePacketsLength (n-ln)

