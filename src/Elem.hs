{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Elem where

import Control.Lens.TH
import Control.Lens ((%~))

data Note
  = Note { _hand :: Hand, _duration :: Duration, _tripletStart :: Bool, _tripletEnd :: Bool }
  deriving (Eq, Show)

data Hand = 
    LeftHand 
  | RightHand
  | Rest
   deriving (Eq, Show, Enum, Bounded)

data Duration = Duration { _dval :: DVal, _dotted :: Bool } deriving (Eq, Show)

data DVal = D1 | D2 | D4 | D8 | D16 | D32 deriving (Eq, Show)

makeLenses ''Note
makeLenses ''Duration

left :: Note
left = Note LeftHand (Duration D4 False) False False

right :: Note
right = Note RightHand (Duration D4 False) False False

swapHand :: Hand -> Hand
swapHand LeftHand = RightHand
swapHand RightHand = LeftHand
swapHand Rest = RightHand

decreaseDVal :: DVal -> DVal
decreaseDVal D1 = D2
decreaseDVal D2 = D4
decreaseDVal D4 = D8
decreaseDVal D8 = D16
decreaseDVal D16 = D32
decreaseDVal D32 = D32

increaseDVal :: DVal -> DVal
increaseDVal D1 = D1
increaseDVal D2 = D1
increaseDVal D4 = D2
increaseDVal D8 = D4
increaseDVal D16 = D8
increaseDVal D32 = D16

d1,d2,d4,d8,d16,d32 :: Duration
d1 = Duration D1 False
d2 = Duration D2 False
d4 = Duration D4 False
d8 = Duration D8 False
d16 = Duration D16 False
d32 = Duration D32 False

toggleDotted :: Duration -> Duration
toggleDotted = dotted %~ not 