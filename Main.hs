module Main where

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Options (Options(..), parseOpt)
import FSO (pipeRenameFSO)
import DirTree (DirTree(contentsOnly), createDirTree, renameDirTree,
  changeRoot, changeDirTreeCreators, instantiateTreeFromFS)
import Utils (handleArgs, chooseFileCreator)

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
