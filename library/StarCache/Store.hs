-- | S3-like interface, as an abstract @'Store'@
module StarCache.Store
  ( HasStore(..)
  , Store(..)

  -- * Types
  , StorePath(..)
  , checksumPath
  , pointerPath
  , URL(..)
  )
where

import RIO

import StarCache.CacheKey
import StarCache.Pointer
import StarCache.TempFile

class HasStore env where
  storeL :: Lens' env (Store env)

data Store env = Store
  { checkStore :: StorePath -> RIO env Bool
  , getStore :: StorePath -> RIO env TempFile
  , putStore :: StorePath -> TempFile -> RIO env URL
  }

newtype StorePath = StorePath Text

checksumPath :: Checksum -> StorePath
checksumPath = StorePath . checksumToText

pointerPath :: Pointer -> StorePath
pointerPath = StorePath . unPointer

newtype URL = URL Text
  deriving newtype Show
