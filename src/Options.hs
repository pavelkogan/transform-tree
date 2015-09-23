module Options where

import Imports

import qualified Options.Applicative as Opt

data FileSource = STDIN | SourcePath FilePath
  deriving (Show, Eq)

data FileDestination = PWD | DestinationPath FilePath
  deriving (Show, Eq)

toFileSource :: Maybe FilePath -> FileSource
toFileSource Nothing = STDIN
toFileSource (Just a) = SourcePath a

toFileDestination :: Maybe FilePath -> FileDestination
toFileDestination Nothing = PWD
toFileDestination (Just a) = DestinationPath a

newtype Command = Command Text
  deriving (Show, Read, Eq, Ord)

data Operation = HardLink
               | SoftLink
               | Converter Command
               | Copy
  deriving (Show, Read, Eq, Ord)

data Verbosity = Quiet | Normal | Verbose
  deriving (Show, Read, Eq, Ord)

data Options = Options
  { dryRun    :: Bool
  , verbosity :: Verbosity
  , force     :: Bool
  , prune     :: Bool
  , renamer   :: Maybe Command
  , operation :: Operation
  , relative  :: Bool
  , source    :: FileSource
  , dest      :: FileDestination
  } deriving (Show, Eq)

sourceContentsOnly :: Options -> Bool
sourceContentsOnly Options{ source = SourcePath a } = filename a == ""
sourceContentsOnly _ = $(err "not defined when source is not a filepath")

sourcePrefix :: Options -> FilePath
sourcePrefix Options{ source = STDIN } = ""
sourcePrefix Options{ source = SourcePath a }
  | filename a == "" = a
  | directory a == "./" = ""
  | otherwise = directory a

parser :: Parser Options
parser = Options
  <$> switch "dry-run" 'n' "perform a trial run with no changes made"
  <*> vParser
  <*> switch "force" 'f' "overwrite existing files"
  <*> switch "prune" 'p' "remove empty directories"
  <*> optional (Command <$> optText "renamer" 'r' "pipe to change filenames")
  <*> opParser
  <*> switch_ "relative" "make symlinks relative"
  <*> (toFileSource . maybeStdin <$> argPath "source" "source path or - for STDIN")
  <*> (toFileDestination <$> optional (argPath "dest" "destination path; default is current directory"))
  where
    maybeStdin a = guard (a /= "-") >> Just a
    switch_ l h = Opt.switch $ Opt.long l <> Opt.help h

opParser :: Parser Operation
opParser =
      Converter . Command <$> optText "converter" 'c'
      "command for converting files; optionally specify {in} and {out}"
  <|> flag'' HardLink "link" 'l' "create hard links"
  <|> flag'' SoftLink "symlink" 's' "create symbolic links"
  <|> pure Copy

vParser :: Parser Verbosity
vParser =
      flag'' Quiet "quiet" 'q' "quiet output"
  <|> flag'' Verbose "verbose" 'v' "verbose output"
  <|> pure Normal

flag'' :: a -> String -> Char -> String -> Parser a
flag'' a l s' h = Opt.flag' a (Opt.long l <> Opt.short s' <> Opt.help h)
