module Options (Options(..), parseOpt) where

import System.Console.GetOpt
import Control.Applicative ((<$>), (<*>))
import Control.Exception (assert)
import Data.Maybe (isJust)

class Switch a where
  on, off :: a -> Bool
  on  = not . off; off = not . on

instance Switch Bool       where on = id
instance Switch (Maybe a)  where on = isJust

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

mutuallyExclusive :: [[Options -> Bool]]
mutuallyExclusive = [ [optLink, optSymbolic]
                    , [optVerbose, optQuiet] ]

defaultsOK :: Bool
defaultsOK = and $
  (off .) <$> (concat mutuallyExclusive) <*> [defaultOptions]

parseOpt :: [String] -> (Options, [String], [String])
parseOpt argv = assert defaultsOK $ (o', n, e')
  where
    (o, n, e) = getOpt Permute options argv
    o' = foldl (.) id o defaultOptions
    e' = if not (collision o') then e
           else "mutually exclusive options\n":e

collision :: Options -> Bool
collision o = or $ map g mutuallyExclusive
  where g = (>1) . length . filter id . map (\f -> on $ f o)