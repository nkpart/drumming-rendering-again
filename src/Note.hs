{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Note (
  Note(..), hand, duration, mods,
  Mod(..), toggleMod,
  Duration, d1,d2,d4,d8,d16,d32,
  Hand(..), swapHand,
  noteDotted, doubleDuration, halveDuration, toggleDotted,
  noteValue
 , note)
 where

import Control.Lens.TH
import Control.Lens ()
import RIO
import RIO.Set as S
import Duration
    (Duration,
     duration,
     durationDenom,
     halveDuration,
     doubleDuration,
     d1,
     d2,
     d4,
     d8,
     d16,
     d32,
     HasDuration)

data Note =
    Note    { _hand :: Hand, _noteDuration :: Duration, _noteDotted :: Bool, _mods :: Set Mod }
  deriving (Eq)

instance Show Note where
  show (Note hand noteD dd mods) =
       show hand
    <> show noteD
    <> if dd then "." else ""
    <> foldMap show mods

note :: Note
note = Note RightHand d8 False mempty

data Mod = Roll
  deriving (Eq, Ord)

instance Show Mod where
  show Roll = "z"

data Hand =
    LeftHand
  | RightHand
  | Rest
  | Space
   deriving (Eq, Enum, Bounded)

instance Show Hand where
  show RightHand = "R"
  show LeftHand = "L"
  show Space = "-"
  show Rest = "_"

makeLenses ''Note

instance HasDuration Note where
  duration = noteDuration

noteValue :: Note -> Int
noteValue = durationDenom . view duration

swapHand :: Hand -> Hand
swapHand LeftHand = RightHand
swapHand RightHand = LeftHand
swapHand Rest = Rest
swapHand Space = Space

toggleMod :: Mod -> Set Mod -> Set Mod
toggleMod m s =
  if S.member m s
    then S.delete m s
    else S.insert m s

toggleDotted :: Note -> Note
toggleDotted = noteDotted %~ not