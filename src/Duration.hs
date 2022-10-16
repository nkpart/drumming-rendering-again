{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Duration (
  Duration,
  d1,d2,d4,d8,d16,d32,
  HasDuration,
  duration,
  durationValue,
  addDurations,
  doubleDuration,
  halveDuration
  ) where

import Control.Lens.TH
import Control.Lens ()

newtype Duration = Duration { 
   dval :: Int
  } deriving (Eq, Ord)

makeClassy ''Duration

instance Show Duration where
  show (Duration d) = show d

durationValue :: Duration -> Int
durationValue = dval

halveDuration :: Duration -> Duration
halveDuration (Duration n)
  | n <= 16   = Duration (n * 2)
  | otherwise = Duration n

doubleDuration :: Duration -> Duration
doubleDuration (Duration n)
  | n > 1 = Duration (n `div` 2)
  | otherwise = Duration n

d1,d2,d4,d8,d16,d32 :: Duration
d1 = Duration 1 
d2 = Duration 2 
d4 = Duration 4 
d8 = Duration 8 
d16 = Duration 16
d32 = Duration 32 

addDurations :: Duration -> Duration -> Maybe Duration
addDurations p q 
 -- TODO we can combine a p with decreasedval p into a dotted p
 | p == q 
   = Just (doubleDuration p)
 | otherwise
   = Nothing