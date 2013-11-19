module FSO
  ( File(..), Dir(..), FSO, FileCreator, FSOName
  , name, createFile, replaceFileCreator, pipeRenameFSO
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

pipeRenameFSO :: String -> FSO -> IO FSO
pipeRenameFSO p o =
  return . flip renameFSO o =<< readProcessString p (name o)

readProcessString :: String -> String -> IO String
readProcessString = uncurry readProcess <<< head &&& tail <<< words
