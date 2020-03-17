module StarCache.Cache
  ( storeCache
  , restoreCache
  )
where

import RIO

import qualified RIO.NonEmpty as NE
import RIO.Orphans
import StarCache.Archive
import StarCache.CacheKey
import StarCache.Pointer
import StarCache.Store
import StarCache.TempFile

storeCache
  :: (HasLogFunc env, HasResourceMap env, HasArchive env, HasStore env)
  => CacheKey
  -> [Pointer]
  -> NonEmpty FilePath
  -> RIO env ()
storeCache key pointers paths = do
  checksum <- resolveCacheKey key

  let path = checksumPath checksum
  archive <- view archiveL
  store <- view storeL

  exists <- checkStore store path
  if exists
    then logInfo $ "Cache exists at " <> displayShow checksum
    else do
      logInfo
        $ "Caching "
        <> displayShow (NE.toList paths)
        <> " to "
        <> displayShow checksum

      tmp <- prepareArchive archive paths
      logDebug $ "Archive prepared to " <> displayShow tmp
      url <- putStore store path tmp
      logInfo $ "Cached at " <> displayShow url

  for_ pointers $ \pointer -> do
    tmp <- writeStarCacheTempFileUtf8 $ checksumToText checksum
    void $ putStore store (pointerPath pointer) tmp

    logInfo
      $ "Created pointer "
      <> displayShow pointer
      <> " -> "
      <> displayShow checksum

restoreCache
  :: (HasLogFunc env, HasResourceMap env, HasArchive env, HasStore env)
  => CacheKey
  -> [Pointer]
  -> RIO env ()
restoreCache key pointers = do
  checksum <- resolveCacheKey key
  logInfo $ "Restoring cache at " <> displayShow checksum

  let path = checksumPath checksum
  archive <- view archiveL
  store <- view storeL

  exists <- checkStore store path
  if exists
    then do
      tmp <- getStore store path
      logDebug $ "Archive downloaded to " <> displayShow tmp
      extractArchive archive tmp
    else do
      logWarn "No cache found"
      tryPointers pointers

tryPointers
  :: (HasLogFunc env, HasResourceMap env, HasArchive env, HasStore env)
  => [Pointer]
  -> RIO env ()
tryPointers [] = pure ()
tryPointers (pointer : rest) = do
  logInfo $ "Following pointer " <> displayShow pointer

  let path = pointerPath pointer
  store <- view storeL

  exists <- checkStore store path
  if exists
    then do
      tmp <- getStore store path
      key <- CacheKey <$> readStarCacheTempFileUtf8 tmp
      restoreCache key []
    else tryPointers rest
