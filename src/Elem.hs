{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Elem (
  Note(..), hand, duration, tripletState,
  TripletState(..),
  Duration(..),
  d1,d2,d4,d8,d16,d32,
  Hand(..), swapHand,
  dotted, increaseDVal, decreaseDVal, toggleDotted,
  noteValue
 , addDurations, addNotes)
 where

import Control.Lens.TH
import Control.Lens ()
import RIO

data Note
  = Note { _hand :: Hand, _duration :: Duration, _tripletState :: TripletState }
  deriving (Eq, Show)

data TripletState =
  None | Start | End | Covered
  deriving (Eq, Show)

data Hand =
    LeftHand
  | RightHand
  | Rest
   deriving (Eq, Show, Enum, Bounded)

data Duration = Duration { dval :: Int, _dotted :: Bool } deriving (Eq, Show)

makeLenses ''Note
makeLenses ''Duration

noteValue :: Duration -> Int
noteValue = dval

-- left :: Note
-- left = Note LeftHand d4 None

-- right :: Note
-- right = Note RightHand d4 None

addNotes :: Note -> Note -> Maybe Note
addNotes n1 n2 = do
  guard $ n1 ^. tripletState /= End
  newD <- addDurations (view duration n1) (view duration n2)
  newS <- addStates (view tripletState n1) (view tripletState n2)
  pure $ Note (view hand n1) newD newS

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

decreaseDVal :: Duration -> Duration
decreaseDVal (Duration n t)
  | n <= 16   = Duration (n * 2) t
  | otherwise = Duration n t

increaseDVal :: Duration -> Duration
increaseDVal (Duration n t)
  | n > 1 = Duration (n `div` 2) t
  | otherwise = Duration n t

d1,d2,d4,d8,d16,d32 :: Duration
d1 = Duration 1 False
d2 = Duration 2 False
d4 = Duration 4 False
d8 = Duration 8 False
d16 = Duration 16 False
d32 = Duration 32 False

toggleDotted :: Duration -> Duration
toggleDotted = dotted %~ not

addDurations :: Duration -> Duration -> Maybe Duration
addDurations p q 
 -- TODO we can combine a p with decreasedval p
 | p == q 
   = Just (increaseDVal p)
 | otherwise
   = Nothing