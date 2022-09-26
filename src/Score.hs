{-# language TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Score where

import Measure
import Control.Lens.TH
import RIO
import Elem
import Data.ListZipper
import EditState

data Score =
  Score {
    _editState :: EditState,
    _metadata :: Metadata,
    _notes :: ZipNotes
  }
  deriving (Eq, Show)

data Metadata = Metadata
  deriving (Eq, Show)

makeLenses ''Score

allNotes :: Score -> [Note]
allNotes = catMaybes . nelist . view notes

score :: Metadata -> [Note] -> Score
score md ns = Score initState md (editList ns)

insertNote :: Score -> Score
insertNote s =
  let (n, nextState) = createNote (s^.editState)
  in
     s & notes %~ insertMoveLeft (Just n)
       & editState .~ nextState

deleteNote :: Score -> Score
deleteNote s =
  if s^.notes & atEnd
    then s & notes %~ execListZipperOpOr deleteStepLeft
    else s & notes %~ execListZipperOpOr deleteStepRight

toggleDotCut :: Score -> Score
toggleDotCut s =
  let Just (n1, n2) = getPair (view notes s) -- TODO whoopsie
      (d1', d2') = toggleDots (n1 ^. Elem.duration, n2 ^. Elem.duration)
      n1' = n1 & Elem.duration .~ d1'
      n2' = n2 & Elem.duration .~ d2'
   in
    s & notes %~ execListZipperOpOr (do
                   _ <- setFocus (Just n1')
                   _ <- moveRight
                   _ <- setFocus (Just n2')
                   _ <- moveLeft
                   pure ())

-- Crying out for a property based test
-- for all inputs, total duration should be the same across the pair
toggleDots :: (Duration, Duration) -> (Duration, Duration)
toggleDots (n1, n2) =
    case (view dotted n1, view dotted n2, view dval n1 == view dval n2, view dval n2 == decreaseDVal (view dval n1)) of
      (True, False, False, True) ->
        -- proper dotting
        (n1 & dotted .~ False, n2 & dval %~ increaseDVal)
      (False, False, True, _) ->
        -- proper no dotting
        (n1 & dotted .~ True, n2 & dval %~ decreaseDVal)
      (_,_,_,_) ->
        (n1, n2)

splitNote :: Score -> Score
splitNote s =
  let n = (s ^. notes . focus)
  in
    case n of
      Just base ->
         let new = (Elem.duration . dval %~ decreaseDVal) base
         in
          s
            & notes . focus .~ Just new
            & notes %~ insertMoveLeft (Just new)
      Nothing ->
        s

replaceNote :: Note -> Score -> Score
replaceNote n =
 notes %~ insertMoveLeft (Just n)

