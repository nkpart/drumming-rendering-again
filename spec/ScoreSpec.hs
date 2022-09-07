module ScoreSpec where

import Test.Hspec

import Score
import Measure
import Note
import Data.ListZipper (list)

spec :: Spec
spec = 
  describe "Score" $ do
    it "appends" $
      let start = fromMeasures [Measure [Note False]]
          modified = list $ appendMeasure start
      in modified `shouldBe` [emptyMeasure]
