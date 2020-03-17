module StarCache.CacheKey
  ( CacheKey(..)
  , resolveCacheKey
  , Checksum
  , checksumToText
  , textToChecksum
  , checksumToLbs
  , lbsToChecksum
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
  ChecksumFile path -> checksum <$> BSL.readFile path
  ChecksumFileList path -> do
    paths <- lines . unpack <$> readFileUtf8 path
    checksum . mconcat <$> traverse BSL.readFile paths

newtype Checksum = Checksum { checksumToText :: Text }
  deriving newtype Show

textToChecksum :: Text -> Checksum
textToChecksum = Checksum

checksumToLbs :: Checksum -> BSL.ByteString
checksumToLbs = BSL.fromStrict . encodeUtf8 . checksumToText

lbsToChecksum :: BSL.ByteString -> Checksum
lbsToChecksum = Checksum . decodeUtf8With lenientDecode . BSL.toStrict

checksum :: BSL.ByteString -> Checksum
checksum = Checksum . pack . show . md5
