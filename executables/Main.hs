module Main
  ( main
  )
where

import RIO

import RIO.Orphans
import RIO.Process
import StarCache.Application
import StarCache.Archive.Tar
import StarCache.Options
import StarCache.Store.S3

main :: IO ()
main = do
  options <- parseOptions
  lo <- logOptionsHandle stdout $ oVerbose options
  pc <- mkDefaultProcessContext

  withLogFunc lo $ \lf -> withResourceMap $ \rm -> do
    store <- s3Store lf (oBucketName options) (oPrefix options)

    let
      app = App
        { appLogFunc = lf
        , appProcessContext = pc
        , appResourceMap = rm
        , appArchive = tarArchive $ oCompression options
        , appStore = store
        , appOptions = options
        }

    runRIO app run
