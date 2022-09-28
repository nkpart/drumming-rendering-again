{-# language TemplateHaskellQuotes, OverloadedStrings #-}
module Score.RenderSpec where

import Score
import Score.Render
-- import Hedgehog
import RIO.FilePath
import RIO
import Hedgehog
import EditState (initState)
import Elem
import Data.List.NonEmpty.Zipper (fromNonEmpty)
import qualified Data.List.NonEmpty.Zipper as Z

hprop_renderScore_empty :: Property
hprop_renderScore_empty = withTests 1 . property $ do
    v <- readGolden (show 'renderScore <> "blank")
    v === renderScore (score Metadata [])

hprop_renderScore_with_notes :: Property
hprop_renderScore_with_notes = withTests 1 . property $
    let
      theNotes = fromNonEmpty $ (right&duration.~d1) :| [ left&duration.~d2, right&duration.~d4, left&duration.~d8, right&duration.~d16]
      -- theseNotes = Just <$> ListZipper [left&duration.~d2, right&duration.~d1] (right&duration.~d4) [left&duration.~d8, right&duration.~d16]
      thisScore = Score initState Metadata (Just $ fromMaybe theNotes $ Just theNotes >>= Z.right >>= Z.right)
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
    noteMarkup (Note RightHand d4 None) === "P4"
    editMarkup (Note RightHand d4 None) === "\n\\override NoteHead.color = \"red\"\n P4 \n\\revert NoteHead.color\n"

----

readGolden :: MonadIO m => FilePath -> m String
readGolden name = liftIO $
  readFile (".golden" </> name </> "golden")

left :: Note
left = Note LeftHand d4 None

right :: Note
right = Note RightHand d4 None