{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Elem (
  Note(..), hand, duration, tripletState,
  TripletState(..),
  Mod(..), toggleMod, mods,
  Duration,
  d1,d2,d4,d8,d16,d32,
  Hand(..), swapHand,
  dotted, doubleDuration, halveDuration, toggleDotted,
  noteValue
 , addDurations, addNotes)
 where

import Control.Lens.TH
import Control.Lens ()
import RIO
import RIO.Set as S
import Duration

data Note =
  Note {
   _hand :: Hand,
   _duration :: Duration,
   _tripletState :: TripletState,
   _mods :: Set Mod
  }
  deriving (Eq, Show)

data Mod = Roll
  deriving (Eq, Show, Ord)

data TripletState =
  None | Start | End | Covered
  deriving (Eq, Show)

data Hand =
    LeftHand
  | RightHand
  | Rest
   deriving (Eq, Show, Enum, Bounded)

makeLenses ''Note

addNotes :: Note -> Note -> Maybe Note
addNotes n1 n2 = do
  guard $ n1 ^. tripletState /= End
  newD <- addDurations (view duration n1) (view duration n2)
  newS <- addStates (view tripletState n1) (view tripletState n2)
  newM <- addMods (view mods n1) (view mods n2)
  pure $ Note (view hand n1) newD newS newM

addMods :: Set Mod -> Set Mod -> Maybe (Set Mod)
addMods a b = Just (mappend a b)

addStates :: TripletState -> TripletState -> Maybe TripletState
addStates None None = Just None
addStates _ None = Nothing
addStates None _ = Nothing
addStates _ Start = Nothing -- TODO Impossible!
addStates End _ = Nothing
addStates Start End = Nothing
addStates Start Covered = Just Start
addStates Covered Covered = Just Covered
addStates Covered End = Just End
 

swapHand :: Hand -> Hand
swapHand LeftHand = RightHand
swapHand RightHand = LeftHand
swapHand Rest = RightHand


toggleMod :: Mod -> Set Mod -> Set Mod
toggleMod m s =
  if S.member m s
    then S.delete m s
    else S.insert m s
