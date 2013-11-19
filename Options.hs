module Options (Options(..), parseOpt) where

import System.Console.GetOpt

data Options = Options
  { optVerbose  :: Bool
  , optQuiet    :: Bool
  , optRename   :: Maybe String
  , optLink     :: Bool
  , optSymbolic :: Bool
  , optRelative :: Bool
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options
  { optVerbose  = False
  , optQuiet    = False
  , optRename   = Nothing
  , optLink     = False
  , optSymbolic = False
  , optRelative = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "v" ["verbose"] (NoArg $ \o -> o {optVerbose = True})
      "verbose output"
  , Option "q" ["quiet"] (NoArg $ \o -> o {optQuiet = True})
      "quiet output"
  , Option "r" ["rename"]
      (ReqArg (\arg o -> o {optRename = Just arg}) "RENAMER")
      "pipe to change filenames"
  , Option "l" ["link"] (NoArg $ \o -> o {optLink = True})
      "create hard links"
  , Option "s" ["symlink"] (NoArg $ \o -> o {optSymbolic = True})
      "create symbolic links"
  , Option "" ["relative"] (NoArg $ \o -> o {optRelative = True})
      "make symlinks relative"
  ]

parseOpt :: [String] -> (Options, [String], [String])
parseOpt argv = (foldl (.) id o defaultOptions, n, e)
  where (o, n, e) = getOpt Permute options argv
