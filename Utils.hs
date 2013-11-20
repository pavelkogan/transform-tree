module Utils (handleArgs, chooseFileCreator) where

import Options (Options(..))
import FSO (FileCreator)

import Control.Monad (unless, msum, ap)
import Data.List (inits, stripPrefix)
import Data.Maybe (fromJust)
import System.Directory (doesDirectoryExist, canonicalizePath)
import System.FilePath ((</>), joinPath,
  splitDirectories, isValid, splitFileName)
import System.IO.Error (mkIOError, doesNotExistErrorType)
import System.Posix.Files (createSymbolicLink, createLink)

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

chooseFileCreator :: Options -> Maybe FileCreator
chooseFileCreator o =
  case ap [optLink, optRelative, optSymbolic] [o] of
    (True:_)        -> Just createLink
    (_:True:True:_) -> Just createRelativeLink
    (_:_:True:_)    -> Just createSymbolicLink
    _               -> Nothing

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
