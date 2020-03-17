module StarCache.Archive.Cereal
  ( cerealArchive
  )
where

import RIO

import Conduit
import Data.Serialize
import RIO.Directory
  (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import RIO.FilePath (takeDirectory, (</>))
import RIO.Orphans
import StarCache.Archive
import StarCache.TempFile

data Item = Item
  { itemPath :: FilePath
  , itemContents :: ByteString
  }
  deriving stock Generic
  deriving anyclass Serialize

cerealArchive :: HasResourceMap env => Archive env
cerealArchive = Archive
  { prepareArchive =
    writeTemp <=< traverse readItem . concat <=< traverse expandDirectories
  , extractArchive = either throwString (traverse_ writeItem) <=< readTemp
  }

writeTemp :: MonadResource m => [Item] -> m TempFile
writeTemp = writeStarCacheTempFileBinary . encode

readTemp :: MonadResource m => TempFile -> m (Either String [Item])
readTemp = fmap decode . readStarCacheTempFileBinary

readItem :: MonadIO m => FilePath -> m Item
readItem path = Item path <$> readFileBinary path

writeItem :: MonadIO m => Item -> m ()
writeItem Item {..} = do
  createDirectoryIfMissing True $ takeDirectory itemPath
  writeFileBinary itemPath itemContents

expandDirectories :: MonadIO m => FilePath -> m [FilePath]
expandDirectories = go Nothing
 where
  go mParent path = do
    let path' = maybe path (</> path) mParent

    exists <- doesDirectoryExist path'

    if exists
      then do
        paths <- listDirectory path'
        concat <$> traverse (go $ Just path') paths
      else pure [path']
