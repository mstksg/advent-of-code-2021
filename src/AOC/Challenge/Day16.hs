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

import           AOC.Common               (TokStream(..), digitToIntSafe)
import           AOC.Solver               ((:~>)(..))
import           AOC.Util                 (maybeAlt)
import           Control.Applicative      (empty)
import           Control.DeepSeq          (NFData)
import           Control.Lens             (preview, review, _Right)
import           Control.Monad            (replicateM)
import           Data.Bifunctor           (bimap)
import           Data.Functor.Foldable    (cata)
import           Data.Functor.Foldable.TH (makeBaseFunctor)
import           Data.Void                (Void)
import           GHC.Generics             (Generic)
import           Numeric.Lens             (binary)
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
             . concatMap (maybe [] (padzero . map (== '1')) . fmap (review binary) . digitToIntSafe)
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
    v <- bitToNumP =<< replicateM 3 P.anySingle
    t <- parseType
    case t of
      Nothing -> bimap (+ 6) (Literal v) <$> parseLiteral
      Just o  -> bimap (+ 6) (Operator v o) <$> parseOperator
  where
    parseType :: Parser (Maybe Op)
    parseType = do
      t <- bitToNumP =<< replicateM 3 P.anySingle
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
    bitToNumP :: [Bool] -> Parser Int
    bitToNumP = maybeAlt . bitToNum
    parseLiteral = do
      n  <- parseLitChunks
      pn <- bitToNumP (concat n)
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
          n <- bitToNumP =<< replicateM 11 P.anySingle
          (len, ps) <- unzip <$> replicateM n parsePacket
          pure (sum len + 11 + 1, ps)
        else do
          n  <- bitToNumP =<< replicateM 15 P.anySingle
          (n+1+15,) <$> parsePacketsLength n
    parsePacketsLength n = do
      (ln, p) <- parsePacket
      if ln == n
        then pure [p]
        else (p:) <$> parsePacketsLength (n-ln)

bitToNum :: [Bool] -> Maybe Int
bitToNum = preview binary . map (\case False -> '0'; True -> '1')
