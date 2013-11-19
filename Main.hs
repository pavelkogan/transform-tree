module Main where

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Prelude hiding (sequence_, mapM)
import Control.Arrow ((<<<), (&&&))
import Control.Monad (unless, msum, ap)
import Data.List (inits, stripPrefix)
import Data.Maybe (fromJust)
import Data.Tree (Tree, rootLabel, subForest, unfoldTree,
    unfoldTreeM, drawTree)
import Data.Foldable (sequence_)
import Data.Traversable (mapM)
import System.Console.GetOpt
import System.Directory (doesFileExist, doesDirectoryExist,
  copyFile, getDirectoryContents, createDirectoryIfMissing,
  getCurrentDirectory, canonicalizePath)
import System.Environment (getArgs)
import System.IO.Error (mkIOError, doesNotExistErrorType)
import System.FilePath (takeFileName, takeDirectory, (</>), joinPath,
  splitDirectories, isValid, hasTrailingPathSeparator, splitFileName)
import System.Posix.Files (createSymbolicLink, createLink)
import System.Process (readProcess)

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

handleArgs :: [String] -> IO (FilePath, Maybe FilePath)
handleArgs []        = error "missing file operand"
handleArgs (_:_:_:_) = error "too many operands"
handleArgs (source:rest) = do
  let dest = msum $ map Just rest
  isDir <- doesDirectoryExist source
  unless isDir $ ioError $
    mkIOError doesNotExistErrorType "" Nothing (Just source)
  case fmap isValid dest of
    Just False -> error $ fromJust dest ++ ": invalid filepath"
    _          -> return ()
  return (source, dest)

main :: IO ()
main = do
  args <- getArgs
  let (opts, pos, errs) = parseOpt args
  unless (null errs) $ error (head errs)
  (source, d) <- handleArgs pos
  dest <- maybe getCurrentDirectory return d
  transform source dest opts

transform :: FilePath -> FilePath -> Options -> IO ()
transform source dest opt = createDirTree
  =<< case optRename opt of
        Nothing -> return
        Just r  -> renameDirTree (pipeRenameFSO r)
  =<< return . changeRoot dest
             . case chooseFileCreator opt of
                 Nothing -> id
                 Just c  -> changeDirTreeCreators c
             . (if hasTrailingPathSeparator source
                 then (\t -> t {contentsOnly = True}) else id)
  =<< instantiateTreeFromFS source

chooseFileCreator :: Options -> Maybe FileCreator
chooseFileCreator o =
  case ap [optLink, optRelative, optSymbolic] [o] of
    (True:_)     -> Just createLink
    (_:True:_)   -> Just createRelativeLink
    (_:_:True:_) -> Just createSymbolicLink
    _            -> Nothing

type FileCreator = FilePath -> FilePath -> IO ()
type FSOName = String

data Dir  = Dir  { dirname  :: FSOName } deriving (Show)
data File = File { filename :: FSOName
                 , content  :: (FileCreator, FilePath) }
type FSO = Either Dir File

name :: FSO -> FSOName
name = either dirname filename

data DirTree = DirTree { dirRoot      :: FilePath
                       , contentsOnly :: Bool
                       , fsoTree      :: Tree FSO }

stringifyDirTree :: DirTree -> String
stringifyDirTree = drawTree . fmap name . fsoTree

createFile :: File -> FilePath -> IO ()
createFile file dir = creator file $ dir </> filename file
  where creator = uncurry ($) . content

buildNodeFromPath :: FilePath -> IO (FSO, [FilePath])
buildNodeFromPath path = do
  isFile <- doesFileExist path
  isDir  <- doesDirectoryExist path
  if isFile then return $ flip (,) [] $
    Right File {filename = takeFileName path,
                content  = (copyFile, path) }
  else if isDir then do
    contents <- getDirectoryContents path
    let notImplicit x = (x /= ".") && (x /= "..")
        paths = map (path </>) $ filter notImplicit contents
    return (Left Dir {dirname = takeFileName path}, paths)
  else return undefined -- throw error

instantiateTreeFromFS :: FilePath -> IO DirTree
instantiateTreeFromFS path = do
  tree <- unfoldTreeM buildNodeFromPath path
  return DirTree { dirRoot = takeDirectory path,
                   fsoTree = tree, contentsOnly = False }

nodeToIO :: (Tree FSO, FilePath) -> (IO (), [(Tree FSO, FilePath)])
nodeToIO (tree, parentDir) = (action, seeds)
  where action   = either makeDir makeFile fso
        seeds    = map (flip (,) fsoPath) $ subForest tree
        fso      = rootLabel tree
        fsoPath  = (parentDir </>) $ name fso
        makeDir  = const $ createDirectoryIfMissing True fsoPath
        makeFile = flip createFile parentDir

createDirTree :: DirTree -> IO ()
createDirTree dt = sequence_ $ unfoldTree nodeToIO (tree, destDir)
  where destDir = dirRoot dt
        tree = if contentsOnly dt then emptyRoot t else t
        t = fsoTree dt
        emptyRoot x = x{rootLabel = Left Dir {dirname=""}}

changeRoot :: FilePath -> DirTree -> DirTree
changeRoot p t = t { dirRoot = p }

changeDirTreeCreators :: FileCreator -> DirTree -> DirTree
changeDirTreeCreators c t = t { fsoTree = new } where
  new = (fmap . fmap) (replaceFileCreator c) (fsoTree t)

replaceFileCreator :: FileCreator -> File -> File
replaceFileCreator c f@(File {content = (_, p)})
  = f {content = (c, p)}

renameFile' :: FSOName -> File -> File
renameFile' n f = f { filename = n }

renameDir :: FSOName -> Dir -> Dir
renameDir n d = d { dirname = n }

renameFSO :: FSOName -> FSO -> FSO
renameFSO n = either (Left . renameDir n) (Right . renameFile' n)

readProcessString :: String -> String -> IO String
readProcessString = uncurry readProcess <<< head &&& tail <<< words

pipeRenameFSO :: String -> FSO -> IO FSO
pipeRenameFSO p o =
  return . flip renameFSO o =<< readProcessString p (name o)

renameDirTree :: (FSO -> IO FSO) -> DirTree -> IO DirTree
renameDirTree r d = do
  let t = fsoTree d
  n <- mapM r t
  return $ d { fsoTree = n }

createRelativeLink :: FilePath -> FilePath -> IO ()
createRelativeLink orig link = do
  orig' <- canonicalizePath orig
  let (dir, file) = splitFileName link
  dir' <- canonicalizePath dir
  let link' = dir' </> file
      rel = relativePath link' orig'
  createSymbolicLink rel link

relativePath :: FilePath -> FilePath -> FilePath
relativePath start end = joinPath $ map (const "..") up ++ down
  where up     = init $ fromJust $ stripPrefix common s
        down   = fromJust $ stripPrefix common e
        common = last $ takeWhile (`elem` inits e) $ inits s
        e      = splitDirectories end
        s      = splitDirectories start