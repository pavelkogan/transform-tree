#!/usr/bin/env runhaskell

{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (sequence_, mapM)
import Control.Arrow ((<<<), (&&&))
import Data.Tree (Tree, rootLabel, subForest, unfoldTree,
    unfoldTreeM, drawTree)
import Data.Foldable (sequence_)
import Data.Traversable (mapM)
import System.Directory (doesFileExist, doesDirectoryExist,
    copyFile, getDirectoryContents, createDirectoryIfMissing)
import System.FilePath (takeFileName, takeDirectory, (</>))
import System.Process (readProcess)

main :: IO ()
main = undefined

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
copyTreeContents source dest = do
  sourceTree <- instantiateTreeFromFS source
  let contentsTree = sourceTree { contentsOnly = True }
      destTree = changeRoot contentsTree dest
  createDirTree destTree

changeRoot :: DirTree -> FilePath -> DirTree
changeRoot t p = t { dirRoot = p }

changeDirTreeCreators :: DirTree -> FileCreator -> DirTree
changeDirTreeCreators t c = t { fsoTree = new } where
  new = (fmap . fmap) (`replaceFileCreator` c) (fsoTree t)

replaceFileCreator :: File -> FileCreator -> File
replaceFileCreator f@(File {content = (_, p)}) c
  = f {content = (c, p)}

renameFile' :: File -> FSOName -> File
renameFile' f n = f { filename = n }

renameDir :: Dir -> FSOName -> Dir
renameDir d n = d { dirname = n }

--renameFSO f n = either (Left . renameDir f) (Right . renameFile' f) n
renameFSO :: FSO -> FSOName -> FSO
renameFSO = flip r where
  r n = either (Left . flip renameDir n) (Right . flip renameFile' n)

readProcessString :: String -> String -> IO String
readProcessString = uncurry readProcess <<< head &&& tail <<< words

pipeRenameFSO :: String -> FSO -> IO FSO
pipeRenameFSO p o =
  return . renameFSO o =<< readProcessString p (name o)

renameDirTree :: (FSO -> IO FSO) -> DirTree -> IO DirTree
renameDirTree r d = do
  let t = fsoTree d
  n <- mapM r t
  return $ d { fsoTree = n }
