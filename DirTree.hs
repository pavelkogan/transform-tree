module DirTree
  ( DirTree(..), createDirTree, renameDirTree, changeRoot
  , changeDirTreeCreators, instantiateTreeFromFS, stringifyDirTree
  , filterDirTreeByFSO, pruneDirs
  ) where

import FSO (FSO, File(..), Dir(..), FileCreator,
  name, createFile, replaceFileCreator, isDir)

import Prelude hiding (sequence_, mapM)
import Data.Maybe (mapMaybe)
import Data.Foldable (sequence_)
import Data.Traversable (mapM)
import Data.Tree (Tree(..), rootLabel, subForest, unfoldTree,
  unfoldTreeM, drawTree)
import System.Directory (doesFileExist, doesDirectoryExist,
  copyFile, getDirectoryContents, createDirectoryIfMissing)
import System.FilePath (takeFileName, takeDirectory, (</>))

data DirTree = DirTree { dirRoot      :: FilePath
                       , contentsOnly :: Bool
                       , fsoTree      :: Tree FSO }

stringifyDirTree :: DirTree -> String
stringifyDirTree = drawTree . fmap name . fsoTree

buildNodeFromPath :: FilePath -> IO (FSO, [FilePath])
buildNodeFromPath path = do
  fileExists <- doesFileExist path
  dirExists  <- doesDirectoryExist path
  if fileExists then return $ flip (,) [] $
    Right File {filename = takeFileName path,
                content  = (copyFile, path) }
  else if dirExists then do
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

renameDirTree :: (FSO -> IO FSO) -> DirTree -> IO DirTree
renameDirTree r d = do
  let t = fsoTree d
  n <- mapM r t
  return $ d { fsoTree = n }

filterTree :: (Tree a -> Bool) -> Tree a -> Maybe (Tree a)
filterTree p t = if p t
  then Just $ t {subForest = mapMaybe (filterTree p) (subForest t)}
  else Nothing

filterDirTree :: (Tree FSO -> Bool) -> DirTree -> DirTree
filterDirTree p d = case filterTree p (fsoTree d) of
  Nothing -> d {fsoTree = Node (Left $ Dir "") []}
  Just t  -> d {fsoTree = t}

filterDirTreeByFSO :: (FSO -> Bool) -> DirTree -> DirTree
filterDirTreeByFSO p = filterDirTree (p . rootLabel)

pruneDirs :: DirTree -> DirTree
pruneDirs = filterDirTree (not . nullFSOTree)
  where nullFSOTree t = and $
          isDir (rootLabel t) : (map nullFSOTree $ subForest t)
