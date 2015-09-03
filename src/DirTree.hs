module DirTree
  ( DirTree(..), createDirTree, renameDirTree, changeRoot
  , changeDirTreeCreators, instantiateTreeFromFS, stringifyDirTree
  , filterDirTreeByFSO, pruneDirs, sortDirTree
  ) where

import FSO (CreateOptions, FSO (..), FileCreator, createFSO, isDir, name,
            replaceFileCreator)

import BasicPrelude
import Data.Tree        (Tree (..), drawTree, rootLabel, subForest, unfoldTree,
                         unfoldTreeM)
import System.Directory (copyFile, doesDirectoryExist, doesFileExist,
                         getDirectoryContents)
import System.FilePath  (takeDirectory, takeFileName)

data DirTree = DirTree
  { dirRoot      :: FilePath
  , contentsOnly :: Bool
-- ^Set to True if root of 'fsoTree' is to be ignored.
  , fsoTree      :: Tree FSO
  } deriving (Show, Eq)

stringifyDirTree :: DirTree -> String
stringifyDirTree = drawTree . fmap name . fsoTree

-- |Helper function for 'instantiateTreeFromFS'.
buildNodeFromPath :: FilePath -> IO (FSO, [FilePath])
buildNodeFromPath path = do
  fileExists <- doesFileExist path
  dirExists  <- doesDirectoryExist path
  if fileExists then return $ (,[])
    File {filename = takeFileName path,
          content  = (("->", copyFile), path) }
  else if dirExists then do
    contents <- getDirectoryContents path
    let notImplicit x = (x /= ".") && (x /= "..")
        paths = map (path </>) $ filter notImplicit contents
    return (Dir {dirname = takeFileName path}, paths)
  else return undefined -- throw error

instantiateTreeFromFS :: FilePath -> IO DirTree
instantiateTreeFromFS path = do
  tree <- unfoldTreeM buildNodeFromPath path
  return DirTree { dirRoot = takeDirectory path,
                   fsoTree = tree, contentsOnly = False }

-- |Helper function for 'createDirTree'.
nodeToIO :: (Tree FSO, FilePath, CreateOptions)
         -> (IO (), [(Tree FSO, FilePath, CreateOptions)])
nodeToIO (tree, parentDir, opts) = (action, seeds)
  where action   = createFSO opts parentDir fso
        seeds    = map (\t -> (t, fsoPath, opts)) $ subForest tree
        fso      = rootLabel tree
        fsoPath  = (parentDir </>) $ name fso

-- |Write out DirTree to filesystem.
createDirTree :: CreateOptions -> DirTree -> IO ()
createDirTree opts dt = sequence_ $
    unfoldTree nodeToIO (tree, destDir, opts)
  where destDir = dirRoot dt
        tree = if contentsOnly dt then emptyRoot t else t
        t = fsoTree dt
        emptyRoot x = x{rootLabel = Dir ""}

changeRoot :: FilePath -> DirTree -> DirTree
changeRoot p t = t { dirRoot = p }

changeDirTreeCreators :: FileCreator -> DirTree -> DirTree
changeDirTreeCreators c t = t { fsoTree = new } where
  new = fmap (replaceFileCreator c) (fsoTree t)

renameDirTree :: (FSO -> IO FSO) -> DirTree -> IO DirTree
renameDirTree r d = do
  let t = fsoTree d
  n <- mapM r t
  return $ d { fsoTree = n }

-- |Applies predicate to successive subtrees when predicate is True
-- for their parent.
filterTree :: (Tree a -> Bool) -> Tree a -> Maybe (Tree a)
filterTree p t = if p t
  then Just $ t {subForest = mapMaybe (filterTree p) (subForest t)}
  else Nothing

filterDirTree :: (Tree FSO -> Bool) -> DirTree -> DirTree
filterDirTree p d = case filterTree p (fsoTree d) of
  Nothing -> d {fsoTree = Node (Dir "") []}
  Just t  -> d {fsoTree = t}

filterDirTreeByFSO :: (FSO -> Bool) -> DirTree -> DirTree
filterDirTreeByFSO p = filterDirTree (p . rootLabel)

-- |Remove empty directories from DirTree.
pruneDirs :: DirTree -> DirTree
pruneDirs = filterDirTree (not . nullFSOTree)
  where nullFSOTree t = and $
          isDir (rootLabel t) : map nullFSOTree (subForest t)

sortDirTree :: DirTree -> DirTree
sortDirTree d = d {fsoTree = sortTree $ fsoTree d}
  where
    sortTree t = t {subForest = sort' $ map sortTree $ subForest t}
    sort' = sortBy $ comparing rootLabel
