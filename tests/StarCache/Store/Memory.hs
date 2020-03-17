module StarCache.Store.Memory
  ( memoryStore
  )
where

import RIO

import qualified RIO.HashMap as HashMap
import RIO.Orphans
import RIO.Text (unpack)
import StarCache.Store
import StarCache.TempFile

memoryStore :: (MonadIO m, HasResourceMap env) => m (Store env)
memoryStore = do
  ref <- newIORef HashMap.empty

  pure Store
    { checkStore = \(StorePath p) -> HashMap.member p <$> readIORef ref
    , getStore = \(StorePath p) -> do
      m <- readIORef ref
      case HashMap.lookup p m of
        Nothing -> throwString $ "no such item: " <> unpack p
        Just bs -> writeStarCacheTempFileBinary bs
    , putStore = \(StorePath p) tmp -> do
      bs <- readStarCacheTempFileBinary tmp
      atomicModifyIORef' ref $ \m -> (HashMap.insert p bs m, ())
      pure $ URL $ "memory://" <> p
    }
