{-# language TemplateHaskellQuotes, OverloadedStrings #-}
module Score.RenderSpec where

import Score
import Score.Render
import Data.ListZipper
-- import Hedgehog
import RIO.FilePath
import RIO
import Hedgehog
import EditState (initState)
import Elem

hprop_renderScore_empty :: Property
hprop_renderScore_empty = withTests 1 . property $ do
    v <- readGolden (show 'renderScore <> "blank")
    v === renderScore (score Metadata [])

hprop_renderScore_with_notes :: Property
hprop_renderScore_with_notes = withTests 1 . property $
    let
      theseNotes = Just <$> ListZipper [left&duration.~d2, right&duration.~d1] (right&duration.~d4) [left&duration.~d8, right&duration.~d16]
      thisScore = Score initState Metadata theseNotes
    in do
        v <- readGolden (show 'renderScore <> "-somewhat-complicated") 
        v === renderScore thisScore

hprop_renderNotes_orders_left_to_right :: Property
hprop_renderNotes_orders_left_to_right =
  withTests 1 . property $ pure ()
    -- renderNotes ForPresentation (Just <$> ListZipper ["P2", "P1"] "P3" ["P4", "P5"]) === "P1 P2 P3 P4 P5"

hprop_renderNotes_for_edit_shows_focus :: Property
hprop_renderNotes_for_edit_shows_focus =
  withTests 1 . property $ do
    noteMarkup (Note RightHand d4) === "P4"
    editMarkup (Note RightHand d4) === "\n\\override NoteHead.color = \"red\"\n P4 \n\\revert NoteHead.color\n"

----

readGolden :: MonadIO m => FilePath -> m String
readGolden name = liftIO $
  readFile (".golden" </> name </> "golden")