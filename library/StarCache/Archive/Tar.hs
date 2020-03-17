module StarCache.Archive.Tar
  ( Compression(..)
  , tarArchive
  )
where

import RIO

import Conduit
import Data.Conduit.Process.Typed (createSource)
import RIO.Directory (canonicalizePath)
import qualified RIO.NonEmpty as NE
import RIO.Orphans
import RIO.Process
import StarCache.Archive
import StarCache.TempFile

data Compression
  = Gzip
  | Pigz

compressOpts :: Compression -> [String]
compressOpts = \case
  Gzip -> ["-z"]
  Pigz -> ["-I", "pigz"]

tarArchive
  :: (HasLogFunc env, HasProcessContext env, HasResourceMap env)
  => Compression
  -> Archive env
tarArchive c = Archive
  { prepareArchive = \relOrAbsPaths -> do
    absPaths <- traverse canonicalizePath relOrAbsPaths
    let args = ["-c", "-f", "-"] <> compressOpts c <> NE.toList absPaths
    proc "tar" args $ withStreamingStdout_ sinkStarCacheTempFile
  , extractArchive = \tmp -> do
    let args = ["-C", "/", "-x", "-f", unsafeTempFile tmp] <> compressOpts c
    proc "tar" args runProcess_
  }

withStreamingStdout_
  :: MonadUnliftIO m
  => ConduitT ByteString Void m a
  -> ProcessConfig stdin stdout stderr
  -> m a
withStreamingStdout_ sink pc = withUnliftIO $ \u -> do
  let pc' = setStdout createSource pc

  withProcessWait pc' $ \p -> do
    a <- unliftIO u $ runConduit $ getStdout p .| sink
    a <$ checkExitCode p
