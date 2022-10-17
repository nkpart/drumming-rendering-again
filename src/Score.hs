{-# language TemplateHaskell, OverloadedLists #-}
{-# LANGUAGE DeriveFunctor #-}

{-# options_ghc -Wno-deprecations #-}

module Score where

import Elem
import Control.Lens.TH
import RIO
import Note
import EditState
import RIO.State ( StateT, execStateT, MonadState (state) )
import Control.Lens ((%=), use, (.=), ix, zoom)
import GHC.Exts (fromList)
import RIO.Seq (Seq(..))

data Score =
  Score {
    _editState :: EditState,
    _cursor :: Cursor,
    _notes :: ElemSeq
  }
  deriving (Eq, Show)

makeLenses ''Score

score :: [Elem] -> Score
-- TODO The cursor and the content doesn't line up :(
score ns = Score initState (Cursor (-1) Nothing) (fromList ns)

execThis :: StateT s Maybe a -> s -> s
execThis action s =
   fromMaybe s (execStateT action s)

insertNote :: StateT Score Maybe ()
insertNote = do
  n <- zoom editState (state createNote)
  c <- use cursor
  notes %= insertAfterElem (Single n) c
  moveRight

toggleDotCut :: StateT Score Maybe ()
toggleDotCut = do
  c <- use cursor
  n <- use notes
  (Single n1 :<| Single n2 :<| Empty) <- lift $ takeAtLevel 2 c n
  (n1', n2') <- lift $ toggleDots (n1, n2)
  setFocus (Single n1')
  moveRight
  setFocus (Single n2')
  moveLeft

splitNote :: StateT Score Maybe ()
splitNote = do
  Single base <- getFocus
  let new = base & duration %~ halveDuration
  deleteFocus
  insertMoveLeft new
  insertMoveLeft new

insertMoveLeft :: Note -> StateT Score Maybe ()
insertMoveLeft n =
  do 
    c1 <- use cursor
    notes %= insertElem (Single n) c1

makeTriplet :: StateT Score Maybe ()
makeTriplet = do
  Single v <- getFocus
  let halved = Single $ v & Note.duration %~ halveDuration
      origDuration = v ^. Note.duration
  setFocus $ Triplet origDuration [halved, halved, halved]
  moveDown

combineNotes :: StateT Score Maybe ()
combineNotes = do
  ns <- use notes
  c1 <- use cursor
  c2 <- lift $ moveCursorRight c1 ns

  Single n1 <- lift (ns ^? ix c1)
  Single n2 <- lift (ns ^? ix c2)
  added <- lift (addNotes n1 n2)
  notes %= deleteElem c1
  notes . ix c1 .= Single added

-- unTriplet :: Score -> Score

deleteFocus :: StateT Score Maybe ()
deleteFocus =
  do c <- use cursor
     notes %= deleteElem c

modifyFocusNote :: (Note -> Note) -> StateT Score Maybe ()
modifyFocusNote f =
  do Single e <- getFocus
     setFocus (Single $ f e)

getFocus :: StateT Score Maybe Elem
getFocus = 
  do ns <- use notes
     c <- use cursor
     lift $ ns RIO.^? ix c

setFocus :: Elem -> StateT Score Maybe ()
setFocus n = do
   v <- use cursor
   notes . ix v .= n 

moveRight :: StateT Score Maybe ()
moveRight = tryMove moveCursorRight

moveLeft :: StateT Score Maybe ()
moveLeft = tryMove moveCursorLeft

moveDown :: StateT Score Maybe ()
moveDown = tryMove moveCursorDown

moveUp :: StateT Score Maybe ()
moveUp = tryMove (\c _ -> moveCursorUp c)

tryMove :: (Cursor -> ElemSeq -> Maybe Cursor) -> StateT Score Maybe () 
tryMove f = 
  do c <- use cursor
     ns <- use notes
     c2 <- lift $ f c ns
     cursor .= c2

-- Crying out for a property based test
-- for all inputs, total duration should be the same across the pair
toggleDots :: (Note, Note) -> Maybe (Note, Note)
toggleDots (n1, n2) =
    case (view noteDotted n1, view noteDotted n2, noteValue n1 == noteValue n2, noteValue n2 == noteValue (n1 & duration %~ halveDuration)) of
      (True, False, False, True) ->
        -- proper dotting
        Just (n1 & noteDotted .~ False, n2 & duration %~ doubleDuration)
      (False, False, True, _) ->
        -- proper no dotting
        Just (n1 & noteDotted .~ True, n2 & duration %~ halveDuration)
      (_,_,_,_) ->
        Nothing