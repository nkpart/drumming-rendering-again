{-# language OverloadedStrings #-}
module ScoreSpec where

import Score
import Elem (right, left)
import RIO
import Hedgehog

hprop_Score_appends :: Property
hprop_Score_appends = withTests 1 . property $
      let start = score Metadata []
          modified = (insertNote . insertNote) start
      in do
        allNotes modified === [right, left]
        (start ^. notes . focus) === Nothing
        (modified ^. notes . focus) === Just left
