module StarCache.Options
  ( Options(..)
  , Command(..)
  , StoreOptions(..)
  , RestoreOptions(..)
  , parseOptions
  )
where

import RIO

import Options.Applicative
import RIO.NonEmpty (some1)
import StarCache.Archive.Tar
import StarCache.CacheKey
import StarCache.Pointer
import StarCache.Store.S3

data Options = Options
  { oVerbose :: Bool
  , oCompression :: Compression
  , oBucketName :: BucketName
  , oPrefix :: Maybe Text
  , oCacheKey :: CacheKey
  , oCommand :: Command
  }

data Command
  = Store StoreOptions
  | Restore RestoreOptions

data StoreOptions = StoreOptions
  { soPointers :: [Pointer]
  , soPaths :: NonEmpty FilePath
  }

newtype RestoreOptions = RestoreOptions
  { roPointers :: [Pointer]
  }

parseOptions :: IO Options
parseOptions = execParser $ withInfo parser

-- brittany-disable-next-binding

parser :: Parser Options
parser = Options
  <$> switch (long "verbose")
  <*> flag Gzip Pigz
    ( short 'P'
    <> long "pigz"
    <> help "Use pigz compression"
    )
  <*> option str
    ( short 'b'
    <> long "bucket"
    <> help "S3 bucket name"
    )
  <*> optional (option str
    ( short 'p'
    <> long "prefix"
    <> help "S3 prefix"
    <> value ""
    <> showDefault
    ))
  <*> parseCacheKey
  <*> subparser
    ( command "store" (withInfo (Store <$> parseStoreOptions))
    <> command "restore" (withInfo (Restore <$> parseRetoreOptions))
    )

-- brittany-disable-next-binding

parseStoreOptions :: Parser StoreOptions
parseStoreOptions = StoreOptions
  <$> many (parsePointer "Create pointer from VALUE")
  <*> some1 (argument str (metavar "PATH" <> help "File or directory to cache"))

parseRetoreOptions :: Parser RestoreOptions
parseRetoreOptions =
  RestoreOptions <$> many (parsePointer "Fallback to pointer VALUE")

parsePointer :: String -> Parser Pointer
parsePointer h =
  fmap Pointer
    $ option str
    $ short 'p'
    <> long "pointer"
    <> metavar "VALUE"
    <> help h

-- brittany-disable-next-binding

parseCacheKey :: Parser CacheKey
parseCacheKey = asum
  [ CacheKey <$> option str
    (short 'k'
    <> long "key"
    <> metavar "VALUE"
    <> help "Set the cache key to VALUE"
    )
  , ChecksumFile <$> option str
    ( short 'c'
    <> long "checksum-file"
    <> metavar "PATH"
    <> help "Set the cache key by checksum of PATH"
    )
  , ChecksumFileList <$> option str
    ( short 'C'
    <> long "checksum-list"
    <> metavar "PATH"
    <> help "Like --checksum-file, but read a list of files to checksum from PATH"
    )
  ]

withInfo :: Parser a -> ParserInfo a
withInfo p = info (p <**> helper) fullDesc
