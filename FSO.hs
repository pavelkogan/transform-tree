module FSO
  ( File(..), Dir(..), FSO, FileCreator, FSOName, CreateOptions
  , name, createFile, createDir, replaceFileCreator, pipeRenameFSO
  , isDir, isFile
  ) where

import Control.Arrow ((<<<), (&&&), first)
import System.Directory (doesFileExist, removeFile,
  createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process (readProcess)

type FileCreator = (String, FilePath -> FilePath -> IO ())
type FSOName = String

data Dir  = Dir  { dirname  :: FSOName } deriving (Show)
data File = File { filename :: FSOName
                 , content  :: (FileCreator, FilePath) }
type FSO = Either Dir File

-- |force, verbose, dry run
type CreateOptions = (Bool, Bool, Bool)

name :: FSO -> FSOName
name = either dirname filename

isDir, isFile :: FSO -> Bool
isDir  = either (const True) (const False)
isFile = either (const False) (const True)

createFile :: CreateOptions -> FilePath -> File -> IO ()
createFile (force, verbose, dryRun) dir file = do
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

createDir :: CreateOptions -> FilePath -> Dir -> IO ()
createDir (_, verbose, dryRun) parent dir = do
  let path = parent </> dirname dir
  if not dryRun then
    createDirectoryIfMissing True path
  else return ()
  if verbose then putStrLn path else return ()

replaceFileCreator :: FileCreator -> File -> File
replaceFileCreator c f@(File {content = (_, p)})
  = f {content = (c, p)}

renameFile' :: FSOName -> File -> File
renameFile' n f = f { filename = n }

renameDir :: FSOName -> Dir -> Dir
renameDir n d = d { dirname = n }

renameFSO :: FSOName -> FSO -> FSO
renameFSO n = either (Left . renameDir n) (Right . renameFile' n)

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
