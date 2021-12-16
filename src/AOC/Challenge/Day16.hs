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
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import Numeric.Lens
import qualified Text.Megaparsec.Char.Lexer     as PP

day16a :: _ :~> _
day16a = MkSol
    { sParse = Just . concatMap (maybe [] (padzero . map (== '1')) . fmap (review binary) . digitToIntSafe)
        
    , sShow  = show
    -- , sSolve = Just
    , sSolve = either (const Nothing) (Just . vsum . snd) . P.runParser parsePacket "" . TokStream
    }
  where
    padzero xs = reverse . take 4 $ reverse xs ++ repeat False

type Parser = P.Parsec Void (TokStream Bool)

-- data Literal = Lit { version :: Version, val :: Int }

type Version = Int
data Packet = Operator Version Int [Packet]
            | Literal Version Int
  deriving (Show, Generic)
  deriving NFData

vsum :: Packet -> Int
vsum = \case
  Operator v _ ps -> v + sum (vsum <$> ps)
  Literal v _ -> v

-- length
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


      

    -- case vn of
    --   6 -> do
    --     [_,a1,a2,a3] <- replicateM 4 P.anySingle
    --     [_,b1,b2,b3] <- replicateM 4 P.anySingle
    --     [_,c1,c2,c3] <- replicateM 4 P.anySingle
    --     replicateM_ 3 P.anySingle
    --     Just res <- pure $ bitToNum [a1,a2,a3,b1,b2,b3,c1,c2,c3]
    --     pure res

bitToNum :: [Bool] -> Maybe Int
bitToNum = preview binary . map (\case False -> '0'; True -> '1')
-- parse
-- instance (Ord a, Show a) => P.Stream (TokStream a) where

day16b :: _ :~> _
day16b = MkSol
    { sParse = sParse day16a
    , sShow  = show
    , sSolve = either (const Nothing) (Just . evalPack . snd) . P.runParser parsePacket "" . TokStream
    }

-- -   Packets with type ID `0` are *sum* packets - their value is the sum
--     of the values of their sub-packets. If they only have a single
--     sub-packet, their value is the value of the sub-packet.
-- -   Packets with type ID `1` are *product* packets - their value is the
--     result of multiplying together the values of their sub-packets. If
--     they only have a single sub-packet, their value is the value of the
--     sub-packet.
-- -   Packets with type ID `2` are *minimum* packets - their value is the
--     minimum of the values of their sub-packets.
-- -   Packets with type ID `3` are *maximum* packets - their value is the
--     maximum of the values of their sub-packets.
-- -   Packets with type ID `5` are *greater than* packets - their value is
--     *1* if the value of the first sub-packet is greater than the value
--     of the second sub-packet; otherwise, their value is *0*. These
--     packets always have exactly two sub-packets.
-- -   Packets with type ID `6` are *less than* packets - their value is
--     *1* if the value of the first sub-packet is less than the value of
--     the second sub-packet; otherwise, their value is *0*. These packets
--     always have exactly two sub-packets.
-- -   Packets with type ID `7` are *equal to* packets - their value is *1*
--     if the value of the first sub-packet is equal to the value of the
--     second sub-packet; otherwise, their value is *0*. These packets
--     always have exactly two sub-packets.

evalPack :: Packet -> Int
evalPack = \case
  Literal _ i -> i
  Operator _ o is -> evalOp o is

evalOp :: Int -> [Packet] -> Int
evalOp = \case
  0 -> sum . map evalPack
  1 -> product . map evalPack
  2 -> minimum . map evalPack
  3 -> maximum . map evalPack
  5 -> (\case [a,b] -> if a > b then 1 else 0) . map evalPack
  6 -> (\case [a,b] -> if a < b then 1 else 0) . map evalPack
  7 -> (\case [a,b] -> if a == b then 1 else 0) . map evalPack

-- type Version = Int
-- data Packet = Operator Version Int [Packet]
--             | Literal Version Int
--   deriving (Show, Generic)
--   deriving NFData

