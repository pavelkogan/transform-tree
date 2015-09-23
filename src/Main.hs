module Main where

import Imports
import Options

import Extra hiding ((&&&))

main :: IO ()
main = do
  opts <- options "transform-tree" parser
  print $ sourcePrefix opts
  -- stdout $ show <$> inFiles opts
  stdout $ show <$> inOutPaths opts
  void $ exit ExitSuccess
  stdout $ format fp <$> inFiles opts
  sh $ ff opts "test" "test2"
  print opts

ff :: Options -> FilePath -> FilePath -> Shell ()
ff Options{..} src dst = do
  i <- lstree src
  let out = dst </> i
  whenM (testfile i) $ do
    mktree $ directory out
    performOp operation i out
  return ()

getDest :: MonadIO m => Options -> m FilePath
getDest Options{ dest = PWD } = pwd
getDest Options{ dest = DestinationPath p } = return p

inOutPaths :: Options -> Shell (FilePath, FilePath)
inOutPaths opts = do
  let pre = sourcePrefix opts
  dst <- getDest opts
  map (id &&& (dst </>) . $fromJst . stripPrefix pre) $ inFiles opts

inFiles :: Options -> Shell FilePath
inFiles Options{source = SourcePath a} = do
  i <- lstree a
  ifM (testfile i) (return i) mzero
inFiles Options{source = STDIN} = fromText <$> stdin

performOp :: MonadIO m => Operation -> FilePath -> FilePath -> m ()
performOp HardLink = createLink
performOp SoftLink = createSymbolicLink
performOp (Converter c) = undefined c
performOp Copy = cp
