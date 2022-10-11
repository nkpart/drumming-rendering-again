{-# language TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

{-# options_ghc -Wno-deprecations #-}

module Score where

import Elem
import Control.Lens.TH
import RIO
import Note
import EditState
import RIO.State ( StateT, execStateT )
import Control.Lens ((%=), use, (.=), ix)
import GHC.Exts (fromList)

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
  (n, nextState) <- createNote <$> use editState
  c <- use cursor
  flip traceShow (pure ()) =<< use cursor
  flip traceShow (pure ()) =<< use notes
  notes %= insertAfterElem c (Single n)
  flip traceShow (pure ()) =<< use notes
  moveRight
  flip traceShow (pure ()) =<< use cursor
  editState .= nextState

toggleDotCut :: StateT Score Maybe ()
toggleDotCut = do
  c <- use cursor
  ns <- use notes
  (Single n1, Single n2) <- lift (getPair c ns)
  let dots = (n1 ^. duration, n2 ^. duration)
      (d1', d2') = toggleDots dots
      n1' = n1 & Note.duration .~ d1'
      n2' = n2 & Note.duration .~ d2'
  -- traceShow (dots, (d1',d2'))
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

-- replaceNote :: Note -> StateT Score Maybe ()
-- replaceNote n = do
--   v <- use cursor
--   notes . ix v %= n

insertMoveLeft :: Note -> StateT Score Maybe ()
insertMoveLeft n =
  do 
    c1 <- use cursor
    notes %= insertElem c1 (Single n)

makeTriplet :: StateT Score Maybe ()
makeTriplet = do
  Single v <- getFocus
  let halved = Single $ v & Note.duration %~ halveDuration
      origDuration = v ^. Note.duration
  setFocus $ Triplet origDuration $ fromList [halved, halved, halved]
  cursor %= appendToCursor 0
  -- TODO need to enter the cursor here

combineNotes :: StateT Score Maybe ()
combineNotes = do
  ns <- use notes
  c1 <- use cursor
  c2 <- lift $ moveCursorRight c1 ns

  Single n1 <- lift $ ns ^? ix c1
  Single n2 <- lift $ ns ^? ix c2
  added <- lift $ addNotes n1 n2
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
     lift $ elemAt c ns

setFocus :: Elem -> StateT Score Maybe ()
setFocus n = do
   v <- use cursor
   notes . ix v .= n 

moveRight :: StateT Score Maybe ()
moveRight = 
  do c <- use cursor
     ns <- use notes
     traceShow c $ pure ()
     traceShow ns $ pure ()
     c2 <- lift $ moveCursorRight c ns
     cursor .= c2

moveLeft :: StateT Score Maybe ()
moveLeft = 
  do c <- use cursor
     ns <- use notes
     c2 <- lift $ moveCursorLeft c ns
     cursor .= c2

-- Crying out for a property based test
-- for all inputs, total duration should be the same across the pair
toggleDots :: (Duration, Duration) -> (Duration, Duration)
toggleDots (n1, n2) =
    case (view dotted n1, view dotted n2, noteValue n1 == noteValue n2, noteValue n2 == noteValue (halveDuration n1)) of
      (True, False, False, True) ->
        -- proper dotting
        (n1 & dotted .~ False, n2 & doubleDuration)
      (False, False, True, _) ->
        -- proper no dotting
        (n1 & dotted .~ True, n2 & halveDuration)
      (_,_,_,_) ->
        (n1, n2)