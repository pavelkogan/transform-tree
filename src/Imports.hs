module Imports (module Imports, module Export) where

import BasicPrelude            as Export hiding (FilePath, empty, find,
                                          stripPrefix, (<.>), (</>))
import Data.Function.Pointless as Export
import FileLocation            as Export
import Turtle                  as Export hiding (err, relative)

import System.PosixCompat as Compat

import qualified BasicPrelude

createLink :: MonadIO io => FilePath -> FilePath -> io ()
createLink = Compat.createLink $:: fromFilePath ~> fromFilePath ~> liftIO

createSymbolicLink :: MonadIO io => FilePath -> FilePath -> io ()
createSymbolicLink = Compat.createSymbolicLink $:: fromFilePath ~> fromFilePath ~> liftIO

fromFilePath :: FilePath -> BasicPrelude.FilePath
fromFilePath = textToString . format fp

-- lstree' p = if filename p == ""
--               then $fromJst . Turtle.stripPrefix p <$> lstree p
--               else lstree p
