{-# language OverloadedLists #-}
module ElemSpec where

import Elem
import Hedgehog
import qualified Note
import RIO
import Control.Lens (ix)

-- genHand :: Gen Hand
-- genHand = Gen.enumBounded

-- hprop_swapHand :: Property
-- hprop_swapHand = property $ do 
--       h <- forAll genHand
--       swapHand h /== h

trip :: ElemSeq -> Elem
trip = Triplet Note.d8
note :: Elem
note = Single Note.note
noted :: Note.Duration -> Elem
noted d = Single $ Note.note & Note.duration .~ d

note16 :: Elem
note16 = noted Note.d16

elemAt :: Cursor -> ElemSeq -> Maybe Elem
elemAt c n = n RIO.^? ix c

hprop_elemAt :: Property
hprop_elemAt = withTests 1 . property $ do
         elemAt [0] [note, trip [note], note] === Just note
         elemAt [1] [note, trip [note], note] === Just (trip [note])
         elemAt [1,0] [note, trip [note], note] === Just note

hprop_insertElem :: Property
hprop_insertElem = withTests 1 . property $ do
         insertElem note16 [0] [note, trip [note], note] === [note16, note, trip [note], note]
         insertElem note16 [2] [note, trip [note], note] === [note, trip [note], note16, note]
         insertElem note16 [1,0] [note, trip [note], note] === [note, trip [note16, note], note]

         insertElem note16 [1] [note] === [note, note16]

hprop_deleteElem :: Property
hprop_deleteElem = withTests 1 . property $ do
         deleteElem [0] [note, trip [note], note] === [trip [note], note]
         deleteElem [2] [note, trip [note], note] === [note, trip [note]]
         deleteElem [1] [note, trip [note], note] === [note, note]
         deleteElem [1,0] [note, trip [note], note] === [note, trip [], note]
         deleteElem [1,0] [note, trip [note, note], note] === [note, trip [note], note]

hprop_moveCursorRight :: Property
hprop_moveCursorRight = withTests 1 . property $ 
      do moveCursorRight [0] [] === Nothing

         moveCursorRight [0] [note, note, note] === Just [1]
         moveCursorRight [1] [note, note, note] === Just [2]
         moveCursorRight [2] [note, note, note] === Nothing

         moveCursorRight [0,0] [trip [note, note, note]] === Just [0,1]
         moveCursorRight [0,1] [trip [note, note, note]] === Just [0,2]
         moveCursorRight [0,2] [trip [note, note, note]] === Nothing

         -- In and out of trips
         moveCursorRight [0] [note, trip [note], note] === Just [1,0]
         moveCursorRight [1,0] [note, trip [note], note] === Just [2]
         moveCursorRight [2] [note, trip [note], note] === Nothing

hprop_moveCursorLeft :: Property
hprop_moveCursorLeft = withTests 1 . property $ 
      do moveCursorLeft [0] [] === Nothing

         moveCursorLeft [2] [note, note, note] === Just [1]
         moveCursorLeft [1] [note, note, note] === Just [0]
         moveCursorLeft [0] [note, note, note] === Nothing

         moveCursorLeft [0,2] [trip [note, note, note]] === Just [0,1]
         moveCursorLeft [0,1] [trip [note, note, note]] === Just [0,0]
         moveCursorLeft [0,0] [trip [note, note, note]] === Nothing

         -- In and out of trips
         moveCursorLeft [2] [note, trip [note], note] === Just [1,0]
         moveCursorLeft [1,0] [note, trip [note], note] === Just [0]
         moveCursorLeft [0] [note, trip [note], note] === Nothing