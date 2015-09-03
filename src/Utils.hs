module Utils
  ( handleArgs, chooseFileCreator, filterFiles, createOptions
  ) where

import DirTree (DirTree (..), filterDirTreeByFSO)
import FSO     (CreateOptions, FSO (..), FileCreator)
import Options (Options (..), on')

import BasePrelude
import System.Directory   (canonicalizePath, copyFile, doesDirectoryExist,
                           getTemporaryDirectory, removeFile, renameFile)
import System.FilePath    (isValid, joinPath, splitDirectories, splitFileName,
                           (</>))
import System.IO          (IOMode (..), openBinaryFile,
                           openBinaryTempFile)
import System.Posix.Files (createLink, createSymbolicLink)
import System.Process     (CreateProcess (..), StdStream (..), createProcess,
                           shell, waitForProcess)
import Text.Regex         (Regex, matchRegex, mkRegex, subRegex)

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
  case ap [optLink, optRelative, optSymbolic, on'.optConvert] [o] of
    (True:_)        -> Just ("=>", createLink)
    (_:True:True:_) -> Just ("<-", createRelativeLink)
    (_:_:True:_)    -> Just ("<-", createSymbolicLink)
    (_:_:_:True:_)  -> (,) "~>" . convertFile <$> optConvert o
    _               -> Nothing

createOptions :: Options -> CreateOptions
createOptions o = (optForce o, optVerbose o, optDryRun o)

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
  let command  = substitute [(inRegex, source), (outRegex, tempPath)] converter
  let process = (if not $ match inRegex converter then
        (\c -> c {std_in = UseHandle inHandle}) else id)
        <<< (if not $ match outRegex converter then
        (\c -> c {std_out = UseHandle tempHandle}) else id)
        <<< shell $ command
  (_,_,_, procHandle) <- createProcess process
  _ <- waitForProcess procHandle
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
  where f _ (Dir _)       = True
        f s (File name _) = match (mkRegex s) name
