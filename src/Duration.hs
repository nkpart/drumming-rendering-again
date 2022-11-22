{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Duration (
  Duration,
  d1,d2,d4,d8,d16,d32,
  HasDuration,
  duration,
  durationDenom,
  durationRational,
  doubleDuration,
  halveDuration
  ) where

import Control.Lens.TH
import Control.Lens ()
import Data.Ratio

newtype Duration = Duration { 
   dval :: Int
  } deriving (Eq, Ord)

makeClassy ''Duration

instance Show Duration where
  show (Duration d) = show d

durationDenom :: Duration -> Int
durationDenom = dval

durationRational :: Duration -> Ratio Int
durationRational = (1 %) . dval

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