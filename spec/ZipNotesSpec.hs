{-# language OverloadedStrings #-}
module ZipNotesSpec where

import Data.List.NonEmpty.Zipper
import Measure (copy)
import Hedgehog
import RIO

hprop_spec :: Property
hprop_spec = withTests 1 . property $
      toList (zipper0L (1::Int) [2,3]) === [1,2,3]

hprop_copy :: Property
hprop_copy = withTests 1 . property $ do
      toList (copy $ zipper0L (1::Int) [2] ) === [1,1,2]
      toList (copy $ end $ zipper0L (1::Int) [2] ) === [1,2,2]

zipper0L :: a -> [a] -> Zipper a
zipper0L x xs = fromNonEmpty (x :| xs)