module StarCache.Archive
  ( HasArchive(..)
  , Archive(..)
  )
where

import RIO

import StarCache.TempFile

class HasArchive env where
  archiveL :: Lens' env (Archive env)

data Archive env = Archive
  { prepareArchive :: NonEmpty FilePath -> RIO env TempFile
  , extractArchive :: TempFile -> RIO env ()
  }
