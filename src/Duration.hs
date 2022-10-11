{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Duration (
  Duration,
  d1,d2,d4,d8,d16,d32,
  HasDuration,
  duration,
  noteValue,
  toggleDotted,
  addDurations,
  doubleDuration,
  halveDuration,
  -- subtractDuration,
  dotted
  ) where

import Control.Lens.TH
import Control.Lens ()
import RIO

data Duration = Duration { 
   dval :: Int,
  _dotted :: Bool 
  } deriving (Eq, Ord)

makeClassy ''Duration

instance Show Duration where
  show (Duration d dt) =
    show d <> if dt then "." else ""

noteValue :: Duration -> Int
noteValue = dval

halveDuration :: Duration -> Duration
halveDuration (Duration n t)
  | n <= 16   = Duration (n * 2) t
  | otherwise = Duration n t

doubleDuration :: Duration -> Duration
doubleDuration (Duration n t)
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
 -- TODO we can combine a p with decreasedval p into a dotted p
 | p == q 
   = Just (doubleDuration p)
 | otherwise
   = Nothing

-- if a > b
--   returns how much is left over
-- if a < b
--   returns 

-- data SubResult = AllDone (Maybe Duration) | KeepDeleting Duration

-- subtractDuration :: Duration -> Duration -> SubResult
-- subtractDuration p q | p == q = AllDone Nothing
-- subtractDuration p q | p > q = AllDone (Just $ diff p q)
-- subtractDuration p q | p < q = KeepDeleting (diff p q)

-- rationalDuration :: Duration -> Ratio Int
-- rationalDuration d =
--   if view dotted d
--     then (1 :% noteValue d) * (1 :% 2)
--     else (1 :% noteValue d)

-- diff :: Duration -> Duration -> Duration
-- diff p q = undefined