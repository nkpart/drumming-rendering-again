{-# language OverloadedStrings #-}
module ZipNotesSpec where

import Data.ListZipper
import Measure
import Hedgehog


hprop_spec :: Property
hprop_spec = withTests 1 . property $
      list (zipper0L (1::Int) [2,3]) === [1,2,3]


hprop_copy :: Property
hprop_copy = withTests 1 . property $ do
      list (copy $ zipper0L (1::Int) [2] ) === [1,1,2]
      list (copy $ moveEnd $ zipper0L (1::Int) [2] ) === [1,2,2]
