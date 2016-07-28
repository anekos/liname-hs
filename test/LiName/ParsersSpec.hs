
module LiName.ParsersSpec where


import LiName.Types
import LiName.Parsers

import Data.Either
import Test.Hspec
import Text.ParserCombinators.Parsec.Error


fn = "<HSpec>"

spec :: Spec
spec = do
    describe "parseEntry" $ do
      it "Rename" $ do
        parseEntry fn "023\tCAT" `shouldBe` (Right $ LiNameEntry (LiNameKey 23) (DoRename "CAT"))
        parseEntry fn "023\t\tCAT" `shouldBe` (Right $ LiNameEntry (LiNameKey 23) (DoRename "\tCAT"))
        parseEntry fn "023\t CAT" `shouldBe` (Right $ LiNameEntry (LiNameKey 23) (DoRename " CAT"))
      it "Copy" $ do
        parseEntry fn "=023\tCAT" `shouldBe` (Right $ LiNameEntry (LiNameKey 23) (DoCopy "CAT"))
        parseEntry fn "=023\t\tCAT" `shouldBe` (Right $ LiNameEntry (LiNameKey 23) (DoCopy "\tCAT"))
        parseEntry fn "=023\t CAT" `shouldBe` (Right $ LiNameEntry (LiNameKey 23) (DoCopy " CAT"))
      it "Trash" $ do
        parseEntry fn "!023\tCAT" `shouldBe` (Right $ LiNameEntry (LiNameKey 23) DoTrash)
        parseEntry fn "!023\t\tCAT" `shouldBe` (Right $ LiNameEntry (LiNameKey 23) DoTrash)
        parseEntry fn "!023\t CAT" `shouldBe` (Right $ LiNameEntry (LiNameKey 23) DoTrash)
      it "Delete" $ do
        parseEntry fn "!!023\tCAT" `shouldBe` (Right $ LiNameEntry (LiNameKey 23) DoDelete)
        parseEntry fn "!!023\t\tCAT" `shouldBe` (Right $ LiNameEntry (LiNameKey 23) DoDelete)
        parseEntry fn "!!023\t CAT" `shouldBe` (Right $ LiNameEntry (LiNameKey 23) DoDelete)
      it "ParseError" $ do
        parseEntry fn "!! 023\tCAT" `shouldSatisfy` isLeft
        parseEntry fn "\tCAT" `shouldSatisfy` isLeft
        parseEntry fn "a\tCAT" `shouldSatisfy` isLeft
        parseEntry fn "123\t" `shouldSatisfy` isLeft
