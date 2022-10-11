{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Note (
  Note(..), hand, duration, mods,
  Mod(..), toggleMod,
  Duration, d1,d2,d4,d8,d16,d32,
  Hand(..), swapHand,
  dotted, doubleDuration, halveDuration, toggleDotted,
  noteValue
 , addDurations, addNotes, note)
 where

import Control.Lens.TH
import Control.Lens ()
import RIO
import RIO.Set as S
import Duration
    (Duration,
     duration,
     dotted,
     noteValue,
     halveDuration,
     doubleDuration,
     d1,
     d2,
     d4,
     d8,
     d16,
     d32,
     toggleDotted,
     addDurations, HasDuration)

data Note =
    Note    { _hand :: Hand, _noteDuration :: Duration, _mods :: Set Mod }
  deriving (Eq)

instance Show Note where
  show (Note hand noteD mods) =
       show hand
    <> show noteD
    <> foldMap show mods

note :: Note
note = Note RightHand d8 mempty

data Mod = Roll
  deriving (Eq, Ord)

instance Show Mod where
  show Roll = "z"

data Hand =
    LeftHand
  | RightHand
  | Rest
   deriving (Eq, Enum, Bounded)

instance Show Hand where
  show RightHand = "R"
  show LeftHand = "L"
  show Rest = "_"

makeLenses ''Note

instance HasDuration Note where
  duration = noteDuration

addNotes :: Note -> Note -> Maybe Note
addNotes (Note hand1 duration1 mods1) (Note _ duration2 mods2) = do
  newD <- addDurations duration1 duration2
  newM <- addMods mods1 mods2
  pure $ Note hand1 newD newM

addMods :: Set Mod -> Set Mod -> Maybe (Set Mod)
addMods a b = Just (mappend a b)

swapHand :: Hand -> Hand
swapHand LeftHand = RightHand
swapHand RightHand = LeftHand
swapHand Rest = RightHand

toggleMod :: Mod -> Set Mod -> Set Mod
toggleMod m s =
  if S.member m s
    then S.delete m s
    else S.insert m s
