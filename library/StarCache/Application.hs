module StarCache.Application
  ( App(..)
  , run
  )
where

import RIO

import RIO.Orphans
import RIO.Process
import StarCache.Archive
import StarCache.Cache
import StarCache.Options hiding (Command(..))
import qualified StarCache.Options as Options
import StarCache.Store

data App = App
  { appLogFunc :: LogFunc
  , appProcessContext :: ProcessContext
  , appResourceMap :: ResourceMap
  , appArchive :: Archive App
  , appStore :: Store App
  , appOptions :: Options
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

instance HasProcessContext App where
  processContextL =
    lens appProcessContext $ \x y -> x { appProcessContext = y }

instance HasResourceMap App where
  resourceMapL = lens appResourceMap $ \x y -> x { appResourceMap = y }

instance HasArchive App where
  archiveL = lens appArchive $ \x y -> x { appArchive = y }

instance HasStore App where
  storeL = lens appStore $ \x y -> x { appStore = y }

run :: RIO App ()
run = do
  Options {..} <- asks appOptions

  case oCommand of
    Options.Store StoreOptions {..} -> storeCache oCacheKey soPointers soPaths
    Options.Restore RestoreOptions {..} -> restoreCache oCacheKey roPointers
