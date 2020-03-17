module StarCache.CacheSpec
  ( spec
  )
where

import SpecHelper

import RIO.Directory
import qualified RIO.NonEmpty.Partial as NE
import StarCache.Cache
import StarCache.CacheKey

spec :: Spec
spec = inTemporaryDirectory $ do
  describe "Cache" $ do
    it "can store/restore by key" $ do
      runTestApp $ do
        writeFileUtf8 "foo.hs" "Original"
        storeCache "x" [] $ NE.fromList ["foo.hs"]

        writeFileUtf8 "foo.hs" "Changed"

        restoreCache "x" []
        readFileUtf8 "foo.hs" `shouldReturn` "Original"

    it "can store/restore a directory" $ do
      runTestApp $ do
        createDirectoryIfMissing True "foo"
        writeFileUtf8 "foo/bar.hs" "Original bar"
        writeFileUtf8 "foo/baz.hs" "Original baz"
        createDirectoryIfMissing True "foo/bat"
        writeFileUtf8 "foo/bat/quix.hs" "Original quix"
        storeCache "x" [] $ NE.fromList ["foo"]

        writeFileUtf8 "foo/bar.hs" "Changed bar"
        writeFileUtf8 "foo/baz.hs" "Changed baz"
        removeDirectoryRecursive "foo/bat"

        restoreCache "x" []
        readFileUtf8 "foo/bar.hs" `shouldReturn` "Original bar"
        readFileUtf8 "foo/baz.hs" `shouldReturn` "Original baz"
        readFileUtf8 "foo/bat/quix.hs" `shouldReturn` "Original quix"

    it "does not overwrite an existing key" $ do
      runTestApp $ do
        writeFileUtf8 "foo.hs" "Cached 1st"
        storeCache "x" [] $ NE.fromList ["foo.hs"]

        writeFileUtf8 "foo.hs" "Cached 2st"
        storeCache "x" [] $ NE.fromList ["foo.hs"]

        restoreCache "x" []
        readFileUtf8 "foo.hs" `shouldReturn` "Cached 1st"

    it "does nothing with missing keys and no pointers" $ do
      runTestApp $ do
        writeFileUtf8 "foo.hs" "Original"
        storeCache "x" [] $ NE.fromList ["foo.hs"]

        writeFileUtf8 "foo.hs" "Changed"

        restoreCache "not-x" []
        readFileUtf8 "foo.hs" `shouldReturn` "Changed"

    it "can store/restore with pointers" $ do
      runTestApp $ do
        writeFileUtf8 "foo.hs" "Original"
        storeCache "x" ["a", "b"] $ NE.fromList ["foo.hs"]

        writeFileUtf8 "foo.hs" "Changed"

        restoreCache "not-x" ["a"]
        readFileUtf8 "foo.hs" `shouldReturn` "Original"

        writeFileUtf8 "foo.hs" "Changed"

        restoreCache "not-x" ["b"]
        readFileUtf8 "foo.hs" `shouldReturn` "Original"

        writeFileUtf8 "foo.hs" "Changed"

        restoreCache "not-x" []
        readFileUtf8 "foo.hs" `shouldReturn` "Changed"

    it "always updates pointers" $ do
      runTestApp $ do
        -- Cache at x and y, point a to y
        writeFileUtf8 "foo.hs" "Original X"
        storeCache "x" [] $ NE.fromList ["foo.hs"]
        writeFileUtf8 "foo.hs" "Original Y"
        storeCache "y" ["a"] $ NE.fromList ["foo.hs"]

        -- Cache at x (a no-op), but point a to x
        storeCache "x" ["a"] $ NE.fromList ["foo.hs"]

        -- Follow a, which should get x
        restoreCache "not-x" ["a"]
        readFileUtf8 "foo.hs" `shouldReturn` "Original X"

    it "can store by md5 checksum" $ do
      runTestApp $ do
        writeFileUtf8 "foo.hs" "Original"
        storeCache (ChecksumFile "foo.hs") [] $ NE.fromList ["foo.hs"]

        writeFileUtf8 "foo.hs" "Changed"

        let checksum = "0a52da7a03a6de3beefe54f8c03ad80d"
        restoreCache checksum []
        readFileUtf8 "foo.hs" `shouldReturn` "Original"

    it "can read files to checksum from another file" $ do
      runTestApp $ do
        writeFileUtf8 "foo.hs" "Original foo"
        writeFileUtf8 "bar.hs" "Original bar"
        writeFileUtf8 "checksum" "foo.hs\nbar.hs\n"
        storeCache (ChecksumFileList "checksum") [] $ NE.fromList ["foo.hs"]

        writeFileUtf8 "foo.hs" "Changed foo"
        writeFileUtf8 "bar.hs" "Changed bar"

        let checksum = "0ab3e8571044caf73ea939d583de197f"
        restoreCache checksum []
        readFileUtf8 "foo.hs" `shouldReturn` "Original foo"
        readFileUtf8 "bar.hs" `shouldReturn` "Changed bar" -- not cached
