module StarCache.CacheKey
  ( CacheKey(..)
  , resolveCacheKey
  , Checksum
  , checksumToText
  )
where

import RIO

import Data.Digest.Pure.MD5
import qualified RIO.ByteString.Lazy as BSL
import RIO.Text (pack, unpack)

data CacheKey
  = CacheKey Text
  | ChecksumFile FilePath
  | ChecksumFileList FilePath

instance IsString CacheKey where
  fromString = CacheKey . pack

resolveCacheKey :: MonadIO m => CacheKey -> m Checksum
resolveCacheKey = \case
  CacheKey x -> pure $ Checksum x
  ChecksumFile path -> checksum <$> readFileBinary path
  ChecksumFileList path -> do
    paths <- lines . unpack <$> readFileUtf8 path
    checksum . mconcat <$> traverse readFileBinary paths

newtype Checksum = Checksum { checksumToText :: Text }
  deriving newtype Show

checksum :: ByteString -> Checksum
checksum = Checksum . pack . show . md5 . BSL.fromStrict
