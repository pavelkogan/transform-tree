module Main where

import DirTree (DirTree (contentsOnly), changeDirTreeCreators, changeRoot,
                createDirTree, instantiateTreeFromFS, pruneDirs, renameDirTree,
                sortDirTree)
import FSO     (pipeRenameFSO)
import Options (Options (..), parseOpt, parseOpts)
import Utils   (chooseFileCreator, createOptions, filterFiles, handleArgs)

import BasePrelude
import Options.Applicative (handleParseResult)
import System.Directory    (getCurrentDirectory)
import System.FilePath     (hasTrailingPathSeparator)

main :: IO ()
main = do
  args <- getArgs
  _ <- handleParseResult $ parseOpts args
  let (opts, pos, errs) = parseOpt args
  unless (null errs) $ error (head errs)
  (source, d) <- handleArgs pos
  dest <- maybe getCurrentDirectory return d
  transform source dest opts

transform :: FilePath -> FilePath -> Options -> IO ()
transform source dest opt = createDirTree (createOptions opt)
  =<< maybe return (renameDirTree . pipeRenameFSO) (optRename opt)
  =<< return . changeRoot dest . sortDirTree
             . maybe id changeDirTreeCreators (chooseFileCreator opt)
             . (if optPrune opt then pruneDirs else id)
             . maybe id filterFiles (optFilter opt)
             . (if hasTrailingPathSeparator source
                 then (\t -> t {contentsOnly = True}) else id)
  =<< instantiateTreeFromFS source
