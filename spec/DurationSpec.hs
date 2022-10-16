module DurationSpec where


import Duration
import Hedgehog
import Hedgehog.Gen as Gen

genDuration :: Gen Duration
genDuration =
  Gen.element [d1,d2,d4,d8,d16,d32]

hprop_increaseDuration :: Property
hprop_increaseDuration = withTests 1 . property $ do 
      doubleDuration d8 === d4
      doubleDuration d16 === d8