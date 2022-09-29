{-# language OverloadedStrings #-}
module ScoreSpec where

import Score
import RIO
import Hedgehog
import Elem
import Control.Lens (_Just)

hprop_Score_appends :: Property
hprop_Score_appends = withTests 1 . property $
      let start = score Metadata []
          modified = (insertNote . insertNote) start
      in do
        allNotes modified === [right, left]
        (start ^. notes) === Nothing
        (modified ^.. notes . _Just . focus) === [left]

left :: Note
left = Note LeftHand d4 None

right :: Note
right = Note RightHand d4 None