{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Note (
  Note(..), hand, duration, mods,
  Mod(..), toggleMod,
  Duration, d1,d2,d4,d8,d16,d32,
  Hand(..), swapHand,
  noteDotted, doubleDuration, halveDuration, toggleDotted,
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
     durationValue,
     halveDuration,
     doubleDuration,
     d1,
     d2,
     d4,
     d8,
     d16,
     d32,
     addDurations, HasDuration)

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
   deriving (Eq, Enum, Bounded)

instance Show Hand where
  show RightHand = "R"
  show LeftHand = "L"
  show Rest = "_"

makeLenses ''Note

instance HasDuration Note where
  duration = noteDuration

noteValue :: Note -> Int
noteValue = durationValue . view duration

addNotes :: Note -> Note -> Maybe Note
addNotes (Note hand1 duration1 dd1 mods1) (Note _ duration2 dd2 mods2) = do
  -- todo, can combine certain combinations of dots and durations 
  guard $ dd1 == dd2
  newD <- addDurations duration1 duration2
  newM <- addMods mods1 mods2
  pure $ Note hand1 newD dd1 newM

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

toggleDotted :: Note -> Note
toggleDotted = noteDotted %~ not