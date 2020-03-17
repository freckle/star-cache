module StarCache.Pointer
  ( Pointer(..)
  )
where

import RIO

newtype Pointer = Pointer { unPointer :: Text }
  deriving newtype (Show, IsString)
