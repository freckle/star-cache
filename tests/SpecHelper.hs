module SpecHelper
  ( module X
  , module SpecHelper
  )
where

import RIO as X
import Test.Hspec as X hiding
  ( expectationFailure
  , shouldBe
  , shouldContain
  , shouldEndWith
  , shouldMatchList
  , shouldNotBe
  , shouldNotContain
  , shouldNotReturn
  , shouldNotSatisfy
  , shouldReturn
  , shouldSatisfy
  , shouldStartWith
  )
import Test.Hspec.Expectations.Lifted as X

import qualified Data.ByteString.Lazy.Builder as BSL
import RIO.Directory (withCurrentDirectory)
import RIO.Orphans
import StarCache.Archive
import StarCache.Archive.Cereal
import StarCache.Store
import StarCache.Store.Memory

inTemporaryDirectory :: Spec -> Spec
inTemporaryDirectory = around_ go
 where
  go :: IO () -> IO ()
  go f = withSystemTempDirectory "" $ \tmp -> withCurrentDirectory tmp f

data TestApp = TestApp
  { testAppLogFunc :: LogFunc
  , testAppLogRef :: IORef Builder
  , testAppResourceMap :: ResourceMap
  , testAppArchive :: Archive TestApp
  , testAppStore :: Store TestApp
  }

instance HasLogFunc TestApp where
  logFuncL = lens testAppLogFunc $ \x y -> x { testAppLogFunc = y }

instance HasResourceMap TestApp where
  resourceMapL = lens testAppResourceMap $ \x y -> x { testAppResourceMap = y }

instance HasArchive TestApp where
  archiveL = lens testAppArchive $ \x y -> x { testAppArchive = y }

instance HasStore TestApp where
  storeL = lens testAppStore $ \x y -> x { testAppStore = y }

runTestApp :: RIO TestApp a -> IO a
runTestApp f = do
  (ref, lo) <- logOptionsMemory

  withLogFunc lo $ \lf -> withResourceMap $ \rm -> do
    store <- memoryStore

    let
      app = TestApp
        { testAppLogFunc = lf
        , testAppLogRef = ref
        , testAppResourceMap = rm
        , testAppArchive = cerealArchive
        , testAppStore = store
        }

    runRIO app f

putTestAppLog :: RIO TestApp ()
putTestAppLog = do
  ref <- asks testAppLogRef
  builder <- readIORef ref
  liftIO $ BSL.hPutBuilder stdout builder
