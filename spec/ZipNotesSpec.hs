module ZipNotesSpec where

import Test.Hspec
import Data.ListZipper

import ZipNotes
import Note

spec :: Spec
spec = 
  describe "ZipNotes" . it "" $ do
      toList (fromList (note False) [] `append` Note True `append` Note False)
        `shouldBe`
        [Note False, Note True, Note False]

      toList (fromList (Note False) [Note True, Note False])
        `shouldBe`
        [Note False, Note True, Note False] 

      toList (copy $ fromList (Note True) [Note False] )
        `shouldBe`
        [Note True, Note True, Note False]
      toList (copy $ moveEnd $ fromList (Note True) [Note False] )
        `shouldBe`
        [Note True, Note False, Note False]
