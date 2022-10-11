{-# language OverloadedStrings, OverloadedLists #-}
module ScoreSpec where

import Score
import RIO
import Hedgehog
import Elem
import Note

trip :: ElemSeq -> Elem
trip = Triplet Note.d8
note :: Elem
note = Single Note.note
noted :: Duration -> Elem
noted d = Single $ Note.note & Note.duration .~ d

note16 :: Elem
note16 = noted Note.d16

hprop_Score_appends :: Property
hprop_Score_appends = withTests 1 . property $
      let start = score []
          modified = execThis (insertNote >> insertNote) start
      in do
        view notes modified === [Single right, Single left]
        -- (start ^. notes) === Nothing -- TODO
        -- (modified ^.. notes . _Just . focus) === [left] -- TODO

left :: Note
left = Note LeftHand d4 mempty

right :: Note
right = Note RightHand d4 mempty