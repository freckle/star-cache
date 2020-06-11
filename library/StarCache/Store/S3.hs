{-# OPTIONS_GHC -fno-warn-orphans #-}

module StarCache.Store.S3
  ( s3Store

  -- * Re-export for construction
  , BucketName(..)
  )
where

import RIO

import Conduit hiding (sinkTempFile)
import Control.Exception.Lens (handling_)
import Control.Monad.Trans.AWS (AWST')
import Control.Monad.Trans.Resource (liftResourceT)
import Control.Retry (RetryPolicyM, retryPolicyDefault, retrying)
import Network.AWS
import Network.AWS.Data.Body (RsBody(..))
import Network.AWS.S3 hiding (URL, bucket)
import Network.AWS.S3.StreamingUpload
import RIO.Orphans
import StarCache.Store
import StarCache.TempFile

instance MonadFail (AWST' Env (ResourceT IO)) where
  fail = throwString

s3Store
  :: HasResourceMap env => LogFunc -> BucketName -> Maybe Text -> IO (Store env)
s3Store lf bucket mPrefix = do
  let lgr = logFuncToLogger lf
  env <- newEnv Discover <&> set envLogger lgr

  let
    prefix = maybe "" (<> "/") mPrefix
    storePathKey (StorePath p) = ObjectKey $ prefix <> p

  pure Store
    { checkStore = checkStoreS3 env bucket . storePathKey
    , getStore = getStoreS3 env bucket . storePathKey
    , putStore = putStoreS3 env bucket . storePathKey
    }

checkStoreS3
  :: HasResourceMap env => Env -> BucketName -> ObjectKey -> RIO env Bool
checkStoreS3 env bucket key =
  runAWS env $ handling_ _ServiceError (pure False) $ True <$ send
    (headObject bucket key)

getStoreS3
  :: HasResourceMap env => Env -> BucketName -> ObjectKey -> RIO env TempFile
getStoreS3 env bucket key = runAWS env $ do
  resp <- send $ getObject bucket key
  runConduit
    $ transPipe liftResourceT (resp ^. gorsBody ^. to _streamBody)
    .| sinkStarCacheTempFile

-- | Store the archive file on S3 at the given bucket and key
--
-- This operation retries up to 5 times with a 0.05s delay.
--
putStoreS3
  :: HasResourceMap env
  => Env
  -> BucketName
  -> ObjectKey
  -> TempFile
  -> RIO env URL
putStoreS3 env bucket key tmp = do
  let cmu = createMultipartUpload bucket key

  result <-
    retryingEither retryPolicyDefault
    $ runAWS env
    $ runConduit
    $ sourceStarCacheTempFile tmp
    .| streamUpload Nothing cmu

  case result of
    Left (_, ex) -> throwIO ex
    Right _ -> pure ()

  pure $ URL $ "s3://" <> unBucketName bucket <> "/" <> unObjectKey key

logFuncToLogger :: LogFunc -> Logger
logFuncToLogger lf level builder = runRIO lf $ logRIO $ Utf8Builder builder
 where
  logRIO = case level of
    Info -> logInfo
    Error -> logError
    Debug -> logDebug
    Trace -> logDebug

unBucketName :: BucketName -> Text
unBucketName (BucketName b) = b

unObjectKey :: ObjectKey -> Text
unObjectKey (ObjectKey k) = k

-- | 'retrying' an action that returns 'Either' on 'Left's
retryingEither
  :: MonadIO m => RetryPolicyM m -> m (Either e a) -> m (Either e a)
retryingEither policy = retrying policy (const $ pure . isLeft) . const
