{-# language TemplateHaskell #-}
module EditState where

import Elem
import Control.Lens.TH
import RIO

data EditState =
    EditState {
        -- TODO: should this just be a note?
        _hand :: Hand,
        _duration :: Duration
    } deriving (Eq, Show)

makeLenses ''EditState

initState :: EditState
initState = EditState RightHand (Duration D4 False)

createNote :: EditState -> (Note, EditState)
createNote es =
    let thisNote = Note (es^.EditState.hand) (es^.EditState.duration) False False
        nextState = 
          es 
            & EditState.hand %~ swapHand
    in (thisNote, nextState)

(?:) :: Maybe p -> p -> p
Nothing ?: v = v
Just x ?: _ = x