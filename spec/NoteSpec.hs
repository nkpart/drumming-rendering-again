module NoteSpec where

import Test.Hspec
import Note

spec :: Spec
spec =
  describe "Note" $
   it "builds" $
      Note True `shouldBe` note False

