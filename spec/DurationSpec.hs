module DurationSpec where


import Duration
import Hedgehog
import Hedgehog.Gen as Gen
import Control.Lens

genDuration :: Gen Duration
genDuration = do
  v <- Gen.element [d1,d2,d4,d8,d16,d32]
  d <- Gen.bool
  pure $ v & dotted .~ d

hprop_increaseDuration :: Property
hprop_increaseDuration = withTests 1 . property $ do 
      doubleDuration d8 === d4
      doubleDuration d16 === d8

-- hprop_subtractDuration :: Property
-- hprop_subtractDuration = withTests 1 . property $ do 
--       subtractDuration d8 === d4
