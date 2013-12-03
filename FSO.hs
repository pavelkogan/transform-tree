module FSO
  ( FSO(..), FileCreator, FSOName, CreateOptions
  , name, createFSO, replaceFileCreator, pipeRenameFSO
  , isDir, isFile
  ) where

import Control.Arrow ((<<<), (&&&), first)
import Data.Ord (comparing)
import System.Directory (doesFileExist, removeFile,
  createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process (readProcess)

type FileCreator = (String, FilePath -> FilePath -> IO ())
type FSOName = String

data FSO = Dir  { dirname  :: FSOName }
         | File { filename :: FSOName
                , content  :: (FileCreator, FilePath) }

instance Show FSO where
  show (Dir n) = "Dir " ++ show n
  show (File n _) = "File " ++ show n

instance Eq FSO where
  Dir n1 == Dir n2 = n1 == n2
  File n1 (_, p1) == File n2 (_, p2) = n1 == n2 && p1 == p2
  _ == _ = False

instance Ord FSO where
  compare (File _ _) (Dir _) = LT
  compare (Dir _) (File _ _) = GT
  compare o1 o2 = comparing name o1 o2

-- |force, verbose, dry run
type CreateOptions = (Bool, Bool, Bool)

name :: FSO -> FSOName
name (Dir n)    = n
name (File n _) = n

isDir, isFile :: FSO -> Bool
isDir (Dir _) = True; isDir (File _ _) = False
isFile (Dir _) = False; isFile (File _ _) = True

createFSO :: CreateOptions -> FilePath -> FSO -> IO ()
createFSO (force, verbose, dryRun) dir file@(File _ _) = do
  let creator = uncurry ($) . first snd . content
      path = dir </> filename file
      createAction = creator file path
      (sep, origin) = first fst $ content file
  exists <- doesFileExist path
  if not dryRun then
    if not exists then createAction
    else
      if force then removeFile path >> createAction
      else return ()
  else return ()
  if verbose && (force || not exists)
    then putStrLn $ unwords [origin, sep, path]
    else return ()

createFSO (_, verbose, dryRun) parent dir@(Dir _) = do
  let path = parent </> dirname dir
  if not dryRun then
    createDirectoryIfMissing True path
  else return ()
  if verbose then putStrLn path else return ()

replaceFileCreator :: FileCreator -> FSO -> FSO
replaceFileCreator c f@(File {content = (_, p)})
  = f {content = (c, p)}
replaceFileCreator _ d = d

renameFSO :: FSOName -> FSO -> FSO
renameFSO n d@(Dir _)    = d { dirname = n }
renameFSO n f@(File _ _) = f { filename = n }

{- |Takes a string containing an external command, and an FSO. The
   FSO's name is piped through the command, the output of which
   becomes the new FSO name. -}
pipeRenameFSO :: String -> FSO -> IO FSO
pipeRenameFSO p o =
  return . flip renameFSO o =<< readProcessString p (name o)

readProcessString :: String    -- ^command to be run
                  -> String    -- ^standard input
                  -> IO String -- ^standard output
readProcessString = uncurry readProcess <<< head &&& tail <<< words
