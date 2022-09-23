{-# language OverloadedStrings #-}
module ZipNotesSpec where

import Data.ListZipper
import Measure
import Note
import Hedgehog

shouldBe :: (Eq a, Show a) => a -> a -> PropertyT IO ()
shouldBe = (===)

hprop_spec :: Property
hprop_spec = withTests 1 . property $ do
      (append (Note "p") $ zipper0L (Just $ Note "P") [])
        `shouldBe`
        ListZipper [Just "P"] (Just "p") []

      list (zipper0L (Note "P") [Note "p", Note "P"])
        `shouldBe`
        [Note "P", Note "p", Note "P"] 

      list (copy $ zipper0L (Just $ Note "p") [Just $ Note "P"] )
        `shouldBe`
        (fmap Just [Note "p", Note "p", Note "P"])
      list (copy $ moveEnd $ zipper0L (Just $ Note "p") [Just $ Note "P"] )
        `shouldBe`
        (fmap Just [Note "p", Note "P", Note "P"])
