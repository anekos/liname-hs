
module LiName.PathSpec where


import LiName.Types
import LiName.Path

import Data.Either
import Test.Hspec
import Text.ParserCombinators.Parsec.Error


spec :: Spec
spec = do
    describe "compactPath'" $ do
      it "Nothing" $ do
        compactPath' Nothing ["/a/b/X", "/a/b/Y"] `shouldBe` ("", ["/a/b/X", "/a/b/Y"])
        compactPath' Nothing ["/Q/b/X", "/a/b/Y"] `shouldBe` ("", ["/Q/b/X", "/a/b/Y"])
      it "Multi files" $ do
        compactPath' (Just 0) ["/a/b/X", "/a/b/Y"] `shouldBe` ("/a/b", ["X", "Y"])
        compactPath' (Just 0) ["/Q/b/X", "/a/b/Y"] `shouldBe` ("/", ["Q/b/X", "a/b/Y"])
      it "Level Specified" $ do
        compactPath' (Just 1) ["/a/b/X", "/a/b/Y"] `shouldBe` ("/a", ["b/X", "b/Y"])
        compactPath' (Just 2) ["/a/b/X", "/a/b/Y"] `shouldBe` ("/", ["a/b/X", "a/b/Y"])
        compactPath' (Just 3) ["/a/b/X", "/a/b/Y"] `shouldBe` ("", ["/a/b/X", "/a/b/Y"])
        compactPath' (Just 4) ["/a/b/X", "/a/b/Y"] `shouldBe` ("", ["/a/b/X", "/a/b/Y"])
        compactPath' (Just 5) ["/a/b/X", "/a/b/Y"] `shouldBe` ("", ["/a/b/X", "/a/b/Y"])
      it "Single file" $ do
        compactPath' (Just 0) ["/a/b/X"] `shouldBe` ("/a/b", ["X"])
        compactPath' (Just 0) ["/a"] `shouldBe` ("/", ["a"])
        compactPath' (Just 0) ["a"] `shouldBe` ("", ["a"])
      it "Same files" $ do
        let x = "/a/b/X"
        compactPath' (Just 0) [x, x, x] `shouldBe` ("/a/b", ["X", "X", "X"])
        compactPath' (Just 1) [x, x, x] `shouldBe` ("/a", ["b/X", "b/X", "b/X"])
        compactPath' (Just 2) [x, x, x] `shouldBe` ("/", ["a/b/X", "a/b/X", "a/b/X"])
