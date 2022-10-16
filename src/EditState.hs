{-# language TemplateHaskell #-}
module EditState where

import Note
import Control.Lens.TH
import RIO
import Duration

data EditState =
    EditState {
        -- TODO: should this just be a note?
        _hand :: Hand,
        _editStateDuration :: Duration,
        _editStateDotted :: Bool
    } deriving (Eq, Show)

makeLenses ''EditState

instance HasDuration EditState where duration = editStateDuration

initState :: EditState
initState = EditState RightHand d4 False

createNote :: EditState -> (Note, EditState)
createNote es =
    let thisNote = Note (es^.EditState.hand) (es^.duration) (es^.editStateDotted) mempty
        nextState = 
          es 
            & EditState.hand %~ swapHand
    in (thisNote, nextState)

(?:) :: Maybe p -> p -> p
Nothing ?: v = v
Just x ?: _ = x