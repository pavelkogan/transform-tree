module Utils (handleArgs, chooseFileCreator, filterFiles) where

import Options (Options(..), on)
import FSO (FileCreator, filename)
import DirTree (DirTree(..), filterDirTreeByFSO)

import Control.Arrow ((<<<))
import Control.Exception (catch)
import Control.Monad (unless, msum, ap)
import Data.List (inits, stripPrefix)
import Data.Maybe (fromJust, isJust)
import GHC.IO.Exception
import System.Directory (doesDirectoryExist, canonicalizePath,
  getTemporaryDirectory,  copyFile, removeFile, renameFile)
import System.FilePath ((</>), joinPath,
  splitDirectories, isValid, splitFileName)
import System.IO (IOMode(..), openBinaryTempFile, hClose,
  openBinaryFile)
import System.IO.Error (mkIOError, doesNotExistErrorType)
import System.Posix.Files (createSymbolicLink, createLink)
import System.Process (CreateProcess(..), StdStream(..), shell,
  createProcess, waitForProcess)
import Text.Regex (Regex, mkRegex, subRegex, matchRegex)

{- |Parses command-line arguments minus command-line options.
   Expects paths of either one or two directories, the first of
   which must exist. Throws errors when conditions not met.-}
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
  case ap [optLink, optRelative, optSymbolic, on.optConvert] [o] of
    (True:_)        -> Just createLink
    (_:True:True:_) -> Just createRelativeLink
    (_:_:True:_)    -> Just createSymbolicLink
    (_:_:_:True:_)  -> fmap convertFile $ optConvert o
    _               -> Nothing

createRelativeLink :: FilePath -> FilePath -> IO ()
createRelativeLink orig link = do
  orig' <- canonicalizePath orig
  let (dir, file) = splitFileName link
  dir' <- canonicalizePath dir
  let link' = dir' </> file
      rel = relativePath link' orig'
  createSymbolicLink rel link

relativePath :: FilePath -- ^start point
             -> FilePath -- ^end point
             -> FilePath -- ^path from \"start\" to \"end\"
relativePath start end = joinPath $ map (const "..") up ++ down
  where up     = init $ fromJust $ stripPrefix common s
        down   = fromJust $ stripPrefix common e
        common = last $ takeWhile (`elem` inits e) $ inits s
        e      = splitDirectories end
        s      = splitDirectories start

{- |Takes a string containing an external command with optional {in}
   and {out} file markers. If these are present, source and
   destination filepaths are substituted into the command.
   Otherwise, the file contents are piped in and/or out,
   respectively, when the command is run. -}
convertFile :: String -> FilePath -> FilePath -> IO ()
convertFile converter source dest = do
  tempDir <- getTemporaryDirectory
  (tempPath, tempHandle) <- openBinaryTempFile tempDir "ttree"
  inHandle <- openBinaryFile source ReadMode
  let inRegex  = mkRegex "\\{in\\}"
      outRegex = mkRegex "\\{out\\}"
  let command  = flip substitute converter
        [(inRegex, source), (outRegex, tempPath)]
  let process = (if not $ match inRegex converter then
        (\c -> c {std_in = UseHandle inHandle}) else id)
        <<< (if not $ match outRegex converter then
        (\c -> c {std_out = UseHandle tempHandle}) else id)
        <<< shell $ command
  (_,_,_, procHandle) <- createProcess process
  waitForProcess procHandle
  hClose inHandle >> hClose tempHandle
  moveFile tempPath dest

moveFile :: FilePath -> FilePath -> IO ()
moveFile s d = catch (renameFile s d) $ \e ->
  if isUnsupportedOperation e
    then copyFile s d >> removeFile s
    else ioError e

-- |Helper function for 'moveFile'.
isUnsupportedOperation :: IOException -> Bool
isUnsupportedOperation e = case ioe_type e of
  UnsupportedOperation -> True
  _                    -> False

match :: Regex -> String -> Bool
match = curry $ isJust . uncurry matchRegex

substitute :: [(Regex, String)] -> String -> String
substitute = flip $ foldl (\i (p, r) -> subRegex p i r)

filterFiles :: String -- ^regex
            -> DirTree -> DirTree
filterFiles = filterDirTreeByFSO . f
  where f s = either (const True) (match (mkRegex s) . filename)