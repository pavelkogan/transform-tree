module Options (Options(..), parseOpt, on', parseOpts) where

import BasicPrelude
import Data.Maybe
import Options.Applicative

class Switch a where
  on', off :: a -> Bool
  on' = not . off; off = not . on'

instance Switch Bool       where on' = id
instance Switch (Maybe a)  where on' = isJust

data Options = Options
  { optVerbose  :: Bool
  , optQuiet    :: Bool
  , optRename   :: Maybe String
  , optLink     :: Bool
  , optSymbolic :: Bool
  , optRelative :: Bool
  , optConvert  :: Maybe String
  , optFilter   :: Maybe String
  , optPrune    :: Bool
  , optForce    :: Bool
  , optDryRun   :: Bool
  , optSource   :: String
  , optDest     :: Maybe String
  } deriving (Show)

options :: Parser Options
options = Options
  <$> switch (long "verbose" <> short 'v' <> help "verbose output")
  <*> switch (long "quiet" <> short 'q' <> help "quiet output")
  <*> optional (strOption $ help "pipe to change filenames"
      <> long "rename" <> short 'r' <> metavar "RENAMER")
  <*> switch (long "link" <> short 'l' <> help "create hard links")
  <*> switch (long "symlink" <> short 's'
      <> help "create symbolic links")
  <*> switch (long "relative" <> help "make symlinks relative")
  <*> optional (strOption $ help "command for converting files; \
         \optionally specify '{in}' and '{out}'"
      <> long "convert" <> short 'c' <> metavar "CONVERTER")
  <*> optional (strOption $ help
         "regular expression used to filter files"
      <> long "filter" <> short 'F' <> metavar "FILTER")
  <*> switch (long "prune" <> short 'p'
      <> help "remove empty directories")
  <*> switch (long "force" <> short 'f'
      <> help "overwrite existing files")
  <*> switch (long "dry-run" <> short 'n'
      <> help "perform a trial run with no changes made")
  <*> argument str (metavar "SOURCE")
  <*> optional (argument str (metavar "DEST"))

mutuallyExclusive :: [[Options -> Bool]]
mutuallyExclusive = [ [optLink, optSymbolic, on'.optConvert]
                    , [optVerbose, optQuiet] ]

parseOpts :: [String] -> ParserResult Options
parseOpts argv = parseResult
  where
    parseResult = execParserPure (prefs mempty) parserInfo argv
    parserInfo = info (helper <*> options) $ fullDesc <> header
      "Usage: transform-tree [OPTION]... SOURCE [DEST]\n"

-- |Parses list of command-line options into an Options data
-- structure, a list of non-options and a list of errors.
parseOpt :: [String] -> (Options, [String], [String])
parseOpt argv = (o', n, e')
  where
    o' = fromJust $ getParseResult $ parseOpts argv
    e = []
    n = optSource o' : maybeToList (optDest o')
    e' = if not (collision o') then e
           else "mutually exclusive options\n":e

-- |Tests if more than one option has been used from any of the
-- lists of mutually exclusive options.
collision :: Options -> Bool
collision o = any g mutuallyExclusive
  where g = (>1) . length . filter id . map ($ o)
