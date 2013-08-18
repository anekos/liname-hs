
module LiName.ParsersSpec where


import LiName.Types
import LiName.Parsers

import Test.Hspec
import Text.ParserCombinators.Parsec.Error


instance Eq ParseError where
  _ == _ = True


fn = "<HSpec>"

spec :: Spec
spec = do
    describe "parseEntry" $ do
      it "Rename" $ do
        parseEntry fn "000\tCAT" `shouldBe` (Right $ LiNameEntry (LiNameKey 0) "CAT" (DoRename "CAT"))
        parseEntry fn "000\t\tCAT" `shouldBe` (Right $ LiNameEntry (LiNameKey 0) "\tCAT" (DoRename "\tCAT"))
        parseEntry fn "000\t CAT" `shouldBe` (Right $ LiNameEntry (LiNameKey 0) " CAT" (DoRename " CAT"))
