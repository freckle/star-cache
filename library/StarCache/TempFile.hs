module StarCache.TempFile
  ( TempFile
  , unsafeTempFile
  , sinkStarCacheTempFile
  , sourceStarCacheTempFile
  , writeStarCacheTempFileBinary
  , writeStarCacheTempFileUtf8
  , readStarCacheTempFileBinary
  , readStarCacheTempFileUtf8
  )
where

import RIO

import Conduit hiding (sinkTempFile)
import qualified RIO.ByteString.Lazy as BSL

newtype TempFile = TempFile FilePath
  deriving newtype Show

unsafeTempFile :: TempFile -> FilePath
unsafeTempFile (TempFile p) = p

sinkStarCacheTempFile :: MonadResource m => ConduitT ByteString Void m TempFile
sinkStarCacheTempFile = TempFile <$> sinkSystemTempFile "star-cache-"

sourceStarCacheTempFile
  :: MonadResource m => TempFile -> ConduitT () ByteString m ()
sourceStarCacheTempFile (TempFile p) = sourceFile p

writeStarCacheTempFileBinary :: MonadResource m => ByteString -> m TempFile
writeStarCacheTempFileBinary bs =
  runConduit $ sourceLazy (BSL.fromStrict bs) .| sinkStarCacheTempFile

writeStarCacheTempFileUtf8 :: MonadResource m => Text -> m TempFile
writeStarCacheTempFileUtf8 = writeStarCacheTempFileBinary . encodeUtf8

readStarCacheTempFileBinary :: MonadIO m => TempFile -> m ByteString
readStarCacheTempFileBinary (TempFile p) = readFileBinary p

readStarCacheTempFileUtf8 :: MonadIO m => TempFile -> m Text
readStarCacheTempFileUtf8 (TempFile p) = readFileUtf8 p
