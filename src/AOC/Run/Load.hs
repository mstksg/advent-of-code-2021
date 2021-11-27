{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : AOC.Run.Load
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Loading challenge data and prompts.
--

module AOC.Run.Load (
    ChallengePaths(..), challengePaths
  , ChallengeData(..), challengeData
  , Day(..)
  , countdownConsole
  , timeToRelease
  , showNominalDiffTime
  , charPart
  , showAoCError
  , htmlToMarkdown
  , mkDay, mkDay_, dayInt
  , TestMeta(..)
  -- * Parsers
  , parseMeta
  , parseTests
  -- * Printers
  , printMeta
  , printTests
  ) where

import           AOC.Challenge
import           AOC.Util
import           AOC.Util.DynoMap
import           Advent
import           Advent.API
import           Control.Applicative
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Char
import           Data.Constraint
import           Data.Constraint.Compose
import           Data.Constraint.Extras
import           Data.Dependent.Sum
import           Data.Dynamic
import           Data.Foldable
import           Data.Functor.Identity
import           Data.GADT.Show
import           Data.Kind
import           Data.Map                  (Map)
import           Data.Maybe
import           Data.Text                 (Text)
import           Data.Time hiding          (Day)
import           Data.Void
import           System.Console.ANSI       as ANSI
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Error
import           Text.Printf
import           Text.Read                 (readMaybe)
import qualified Control.Monad.Combinators as MP
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Text.HTML.TagSoup         as H
import qualified Text.HTML.TagSoup.Tree    as H
import qualified Text.Megaparsec           as MP
import qualified Text.Megaparsec.Char      as MP
import qualified Text.Pandoc               as P

-- | A record of paths corresponding to a specific challenge.
data ChallengePaths = CP { _cpPrompt     :: !FilePath
                         , _cpCodeBlocks :: !FilePath
                         , _cpInput      :: !FilePath
                         , _cpAnswer     :: !FilePath
                         , _cpTests      :: !FilePath
                         , _cpLog        :: !FilePath
                         }
  deriving stock Show

-- | A record of data (test inputs, answers) corresponding to a specific
-- challenge.
data ChallengeData = CD { _cdPrompt     :: !(Either [String] Text)
                        , _cdCodeBlocks :: !(Either [String] [Text])
                        , _cdInput      :: !(Either [String] String)
                        , _cdAnswer     :: !(Maybe String)
                        , _cdTests      :: ![(String, TestMeta)]
                        }

-- | Generate a 'ChallengePaths' from a specification of a challenge.
challengePaths :: Integer -> ChallengeSpec -> ChallengePaths
challengePaths y (CS d p) = CP
    { _cpPrompt     = "prompt"           </> printf "%02d%c" d' p' <.> "md"
    , _cpCodeBlocks = "data/code-blocks" </> printf "%02d%c" d' p' <.> "txt"
    , _cpInput      = "data"             </> printf "%02d" d'      <.> "txt"
    , _cpAnswer     = "data/ans"         </> printf "%02d%c" d' p' <.> "txt"
    , _cpTests      = "test-data"        </> printf "%04d/%02d%c" y d' p' <.> "txt"
    , _cpLog        = "logs/submission"  </> printf "%02d%c" d' p' <.> "txt"
    }
  where
    d' = dayInt d
    p' = partChar p

makeChallengeDirs :: ChallengePaths -> IO ()
makeChallengeDirs CP{..} =
    mapM_ (createDirectoryIfMissing True . takeDirectory)
          [_cpPrompt, _cpCodeBlocks, _cpInput, _cpAnswer, _cpTests, _cpLog]

-- | Load data associated with a challenge from a given specification.
-- Will fetch answers online and cache if required (and if giten a session
-- token).
challengeData
    :: Maybe String   -- ^ session key
    -> Integer        -- ^ year
    -> ChallengeSpec
    -> IO ChallengeData
challengeData sess yr spec = do
    makeChallengeDirs ps
    inp   <- runExceptT . asum $
      [ maybeToEither [printf "Input file not found at %s" _cpInput]
          =<< liftIO (readFileMaybe _cpInput)
      , fetchInput
      ]
    blocks <- runExceptT . asum $
      [ maybeToEither [printf "Input file not found at %s" _cpCodeBlocks]
          =<< liftIO (fmap (T.splitOn codeBlockSep . T.pack) <$> readFileMaybe _cpCodeBlocks)
      , fetchCodeBlocks
      ]
    prompt <- runExceptT . asum $
      [ maybeToEither [printf "Prompt file not found at %s" _cpPrompt]
          =<< liftIO (fmap T.pack <$> readFileMaybe _cpPrompt)
      , fetchPrompt
      ]
    ans    <- readFileMaybe _cpAnswer
    ts     <- readFileMaybe _cpTests >>= \case
                Nothing  -> pure []
                Just str -> case MP.parse parseTests _cpTests str of
                  Left e  -> [] <$ putStrLn (MP.errorBundlePretty e)
                  Right r -> pure r
    return CD
      { _cdPrompt     = prompt
      , _cdCodeBlocks = blocks
      , _cdInput      = inp
      , _cdAnswer     = ans
      , _cdTests      = ts
      }
  where
    ps@CP{..} = challengePaths yr spec
    readFileMaybe :: FilePath -> IO (Maybe String)
    readFileMaybe =
        (traverse (evaluate . force) . eitherToMaybe =<<)
       . tryJust (guard . isDoesNotExistError)
       . readFile
    fetchInput :: ExceptT [String] IO String
    fetchInput = do
        s <- maybeToEither ["Session key needed to fetch input"]
              sess
        let opts = defaultAoCOpts yr s
        inp <- liftEither . bimap showAoCError T.unpack
           =<< liftIO (runAoC opts a)
        liftIO $ writeFile _cpInput inp
        pure inp
      where
        a = AoCInput $ _csDay spec
    fetchPrompt :: ExceptT [String] IO Text
    fetchPrompt = do
        prompts <- liftEither . first showAoCError
               =<< liftIO (runAoC opts a)
        promptH  <- maybeToEither [e]
                 . M.lookup (_csPart spec)
                 $ prompts
        prompt   <- liftEither $ htmlToMarkdown True promptH
        liftIO $ T.writeFile _cpPrompt prompt
        pure prompt
      where
        opts = defaultAoCOpts yr $ fold sess
        a = AoCPrompt $ _csDay spec
        e = case sess of
          Just _  -> "Part not yet released"
          Nothing -> "Part not yet released, or may require session key"
    fetchCodeBlocks :: ExceptT [String] IO [Text]
    fetchCodeBlocks = do
        prompts <- liftEither . first showAoCError
               =<< liftIO (runAoC opts a)
        promptH  <- maybeToEither [e]
                 . M.lookup (_csPart spec)
                 $ prompts
        let blocks = pullCodeBlocks promptH
        liftIO $ T.writeFile _cpCodeBlocks $ T.intercalate codeBlockSep blocks
        pure blocks
      where
        opts = defaultAoCOpts yr $ fold sess
        a = AoCPrompt $ _csDay spec
        e = case sess of
          Just _  -> "Part not yet released"
          Nothing -> "Part not yet released, or may require session key"
    codeBlockSep = "\n>>>>>>>>>>>>\n"

showAoCError :: AoCError -> [String]
showAoCError = \case
    AoCClientError e -> [ "Error contacting Advent of Code server to fetch input"
                        , "Possible invalid session key"
                        , printf "Server response: %s" (show e)
                        ]
    AoCReleaseError t -> [ "Challenge not yet released!"
                         , printf "Please wait %s" (showNominalDiffTime t)
                         ]
    AoCThrottleError  -> [ "Too many requests at a time.  Please slow down." ]

-- | Pretty-print a 'NominalDiffTime'
showNominalDiffTime :: NominalDiffTime -> String
showNominalDiffTime (round @Double @Int . realToFrac -> rawSecs) =
    printf "%02dd %02d:%02d:%02d" days hours mins secs
  where
    (rawMins , secs ) = rawSecs  `divMod` 60
    (rawHours, mins ) = rawMins  `divMod` 60
    (days    , hours) = rawHours `divMod` 24

-- | Run a countdown on the console.
countdownConsole
    :: MonadIO m
    => Integer          -- ^ year of challenge
    -> Day              -- ^ day to count down to
    -> m a              -- ^ callback on release
    -> m a
countdownConsole yr d = countdownWith yr d 250000 $ \ttr -> liftIO $ do
    ANSI.clearFromCursorToScreenEnd
    printf "> Day %d release in: %s" (dayInt d) (showNominalDiffTime ttr)
    ANSI.setCursorColumn 0
    hFlush stdout

-- | Run a countdown with a given callback on each tick.
countdownWith
    :: MonadIO m
    => Integer                      -- ^ year of challenge
    -> Day                          -- ^ day to count down to
    -> Int                          -- ^ interval (milliseconds)
    -> (NominalDiffTime -> m ())    -- ^ callback on each tick
    -> m a                          -- ^ callback on release
    -> m a
countdownWith yr d delay callback release = go
  where
    go = do
      ttr <- liftIO $ timeToRelease yr d
      if ttr <= 0
        then release
        else do
          callback ttr
          liftIO $ threadDelay delay
          go

htmlToMarkdown :: Bool -> Text -> Either [String] T.Text
htmlToMarkdown pretty html = first ((:[]) . show) . P.runPure $ do
    p <- P.readHtml (P.def { P.readerExtensions = exts })
            html
    writer (P.def { P.writerExtensions = exts }) p
  where
    writer
      | pretty    = P.writeMarkdown
      | otherwise = P.writePlain
    exts = P.disableExtension P.Ext_header_attributes
         . P.disableExtension P.Ext_smart
         $ P.pandocExtensions

pullCodeBlocks :: Text -> [Text]
pullCodeBlocks = concatMap pullEm . processHTML "code"
  where
    pullEm :: Text -> [Text]
    pullEm str = fmap H.renderTree $ concat cleared : ems
      where
        (ems, cleared) = traverse go (H.parseTree str)
        go br = case br of
          H.TagBranch b _ xs
            | b == "em" -> ([xs], xs)
          _ -> ([], [br])


type Parser = MP.Parsec Void String

data TestMeta = TM { _tmAnswer :: Maybe String
                   , _tmData   :: Map String (DSum TestType Identity)
                   }
  deriving stock Show

parseTests :: Parser [(String, TestMeta)]
parseTests = MP.many parseTest <* MP.eof
  where
    parseTest = do
      inp <- MP.manyTill MP.anySingle $ MP.lookAhead (MP.string "\n>>>")
      _   <- MP.char '\n'
      met <- optional (MP.try parseMeta) MP.<?> "Metadata Block"
      pure (inp, fromMaybe (TM Nothing M.empty) met)

printTests :: [(String, TestMeta)] -> Text
printTests = T.intercalate "\n" . concatMap (\(x, y) -> T.pack x : printMeta y)

parseMeta :: Parser TestMeta
parseMeta = do
    dats <- MP.many (MP.try parseData) MP.<?> "Data Block"
    ans  <- parseAnswer MP.<?> "Expected Answer"
    pure $ TM ans (M.fromList dats)
  where
    parseAnswer =
      MP.string ">>>" *>
      asum
        [ Just <$> MP.try (MP.space1 *> MP.many (MP.noneOf ['\n']) <* "\n")
        , Nothing <$ (MP.space *> "\n")
        ]
    parseData = do
      MP.string ">>>"
      sym <- MP.manyTill (MP.try MP.letterChar)   (MP.try (MP.char ':'))
      val <- MP.manyTill (MP.try MP.alphaNumChar) (MP.try (MP.char ':'))
      typ <- MP.many     (MP.try MP.letterChar)
      MP.space
      case toLower <$> typ of
        "int"    -> maybe (fail "Could not parse metadata value") (pure . (sym,) . (TTInt :=>) . Identity)
                  . readMaybe
                  $ val
        "string" -> pure (sym, TTString :=> Identity val)
        _        -> fail $ "Unrecognized type " ++ typ

printMeta :: TestMeta -> [Text]
printMeta TM{..} =
       map printData (M.toList _tmData)
    <> [case _tmAnswer of
          Just x  -> ">>> " <> T.pack x
          Nothing -> ">>>"
       ]
  where
    printData :: (String, DSum TestType Identity) -> Text
    printData (sym,dynval) = ">>>" <> T.pack sym <> ":" <> val <> ":" <> typ
      where
        (val, typ) = case dynval of
          TTInt    :=> Identity x -> (T.pack (show x), "int")
          TTString :=> Identity x -> (T.pack x, "string")
