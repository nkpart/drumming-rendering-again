{-# language TemplateHaskellQuotes, OverloadedStrings, OverloadedLists #-}
module Score.RenderSpec where

import Score
import Score.Render
-- import Hedgehog
import RIO.FilePath
import RIO
import Hedgehog
import EditState (initState)
import Elem
import Note

hprop_renderScore_empty :: Property
hprop_renderScore_empty = withTests 1 . property $ do
    v <- readGolden (show 'renderScore <> "blank")
    v === renderScore (score [])

hprop_renderScore_with_notes :: Property
hprop_renderScore_with_notes = withTests 1 . property $
    let
      theNotes = [right&_Single.duration.~d1,  
        left&_Single.duration.~d2, 
        right&_Single.duration.~d4, 
        left&_Single.duration.~d8, 
        right&_Single.duration.~d16]
      thisScore = 
          Score initState (Cursor 2 Nothing) theNotes
      -- thisScore = Score initState Metadata (Just $ fromMaybe theNotes $ Just theNotes >>= Z.right >>= Z.right)
    in do
        v <- readGolden (show 'renderScore <> "-somewhat-complicated") 
        v === renderScore thisScore

hprop_renderNotes_for_edit_shows_focus :: Property
hprop_renderNotes_for_edit_shows_focus =
  withTests 1 . property $ do
    noteMarkup (Note RightHand d4 False mempty) === "P4"
    -- TODO
    -- editMarkup (Note RightHand d4 mempty) === "\n\\override NoteHead.color = \"red\"\n P4 \n\\revert NoteHead.color\n"

----

readGolden :: MonadIO m => FilePath -> m String
readGolden name = liftIO $
  readFile (".golden" </> name </> "golden")

left :: Elem
left = Single $ Note LeftHand d4 False mempty

right :: Elem
right = Single $ Note RightHand d4 False mempty