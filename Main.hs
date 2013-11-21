module Main where

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Options (Options(..), parseOpt)
import FSO (pipeRenameFSO)
import DirTree (DirTree(contentsOnly), createDirTree, renameDirTree,
  changeRoot, changeDirTreeCreators, instantiateTreeFromFS, pruneDirs)
import Utils (handleArgs, chooseFileCreator, filterFiles)

import Control.Monad (unless)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath (hasTrailingPathSeparator)

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
  =<< maybe return (renameDirTree . pipeRenameFSO) (optRename opt)
  =<< return . changeRoot dest
             . maybe id changeDirTreeCreators (chooseFileCreator opt)
             . (if optPrune opt then pruneDirs else id)
             . maybe id filterFiles (optFilter opt)
             . (if hasTrailingPathSeparator source
                 then (\t -> t {contentsOnly = True}) else id)
  =<< instantiateTreeFromFS source
