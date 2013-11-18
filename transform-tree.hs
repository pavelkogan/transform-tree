#!/usr/bin/env runhaskell

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (sequence_, mapM)
import Control.Arrow ((<<<), (&&&))
import Control.Monad (unless, msum)
import Data.List (inits, stripPrefix)
import Data.Maybe (fromJust)
import Data.Tree (Tree, rootLabel, subForest, unfoldTree,
    unfoldTreeM, drawTree)
import Data.Foldable (sequence_)
import Data.Traversable (mapM)
import System.Console.GetOpt
import System.Directory (doesFileExist, doesDirectoryExist,
  copyFile, getDirectoryContents, createDirectoryIfMissing,
  getCurrentDirectory)
import System.Environment (getArgs)
import System.IO.Error (mkIOError, doesNotExistErrorType)
import System.FilePath (takeFileName, takeDirectory, (</>),
  splitDirectories, joinPath, isValid)
import System.Posix.Files (createSymbolicLink)
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
parseOpt argv = (foldl1 (.) o defaultOptions, n, e)
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
  copyTreeContents source dest

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

copyTreeContents :: FilePath -> FilePath -> IO ()
copyTreeContents source dest =
  transformTree source dest True (return . id) Nothing

transformTree
  :: FilePath
  -> FilePath
  -> Bool
  -> (FSO -> IO FSO)
  -> Maybe FileCreator
  -> IO ()
transformTree source dest co renamer creator = do
  sourceTree <- instantiateTreeFromFS source
  renamedTree <- renameDirTree renamer sourceTree
  let contentsTree = renamedTree { contentsOnly = co }
      createdTree = case creator of
        Just c  -> changeDirTreeCreators c contentsTree
        Nothing -> contentsTree
      destTree = changeRoot dest createdTree
  createDirTree destTree

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
createRelativeLink orig link = createSymbolicLink rel link
  where rel = relativePath link orig

relativePath :: FilePath -> FilePath -> FilePath
relativePath start end = joinPath $ map (const "..") up ++ down
  where up     = init $ fromJust $ stripPrefix common s
        down   = fromJust $ stripPrefix common e
        common = last $ takeWhile (`elem` inits e) $ inits s
        e      = splitDirectories end
        s      = splitDirectories start
