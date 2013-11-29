module FSO
  ( File(..), Dir(..), FSO, FileCreator, FSOName
  , name, createFile, replaceFileCreator, pipeRenameFSO
  , isDir, isFile
  ) where

import Control.Arrow ((<<<), (&&&))
import System.FilePath ((</>))
import System.Process (readProcess)

type FileCreator = FilePath -> FilePath -> IO ()
type FSOName = String

data Dir  = Dir  { dirname  :: FSOName } deriving (Show)
data File = File { filename :: FSOName
                 , content  :: (FileCreator, FilePath) }
type FSO = Either Dir File

name :: FSO -> FSOName
name = either dirname filename

isDir, isFile :: FSO -> Bool
isDir  = either (const True) (const False)
isFile = either (const False) (const True)

createFile :: File -> FilePath -> IO ()
createFile file dir = creator file $ dir </> filename file
  where creator = uncurry ($) . content

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
