module ElemSpec where

import Elem
import Hedgehog
import Hedgehog.Gen as Gen

genHand :: Gen Hand
genHand = Gen.enumBounded

hprop_swapHand :: Property
hprop_swapHand = property $ do 
      h <- forAll genHand
      swapHand h /== h

hprop_increaseDVal :: Property
hprop_increaseDVal = property $ do 
      increaseDVal d8 === d4
      increaseDVal d16 === d8