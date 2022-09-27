{-# language TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Score where

import Measure
import Control.Lens.TH
import RIO
import Elem
import EditState
import Data.List.NonEmpty.Zipper
import RIO.State
import Data.Maybe (fromJust)

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
  s & notes . focus . traverse . Elem.hand .~ Rest
  -- if s^.notes & atEnd
  --   then s & notes %~ execListZipperOpOr deleteStepLeft
  --   else s & notes %~ execListZipperOpOr deleteStepRight

toggleDotCut :: Score -> Score
toggleDotCut s =
  let Just (n1, n2) = getPair (view notes s) -- TODO whoopsie
      (d1', d2') = toggleDots (n1 ^. Elem.duration, n2 ^. Elem.duration)
      n1' = n1 & Elem.duration .~ d1'
      n2' = n2 & Elem.duration .~ d2'
   in
    s & notes %~ \v -> fromMaybe v (Just (replace (Just n1') v) 
                      >>= Data.List.NonEmpty.Zipper.right
                      >>= (Just . replace (Just n2'))
                      >>= Data.List.NonEmpty.Zipper.left
    )
                   

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
  let n = (s ^. notes . to current)
  in
    case n of
      Just base ->
         let new = (Elem.duration . dval %~ decreaseDVal) base
         in
          s
            & notes %~ replace (Just new)
            & notes %~ insertMoveLeft (Just new)
      Nothing ->
        s

replaceNote :: Note -> Score -> Score
replaceNote n =
 notes %~ insertMoveLeft (Just n)


insertMoveLeft :: a -> Zipper a -> Zipper a
insertMoveLeft n z = 
  fromMaybe z $
  Data.List.NonEmpty.Zipper.right (unshift n z)

makeTriplet :: Score -> Score
makeTriplet s =
    s & notes %~ execState (do
      v <- fromJust <$> getFocus
      let halved = v & Elem.duration . dval %~ decreaseDVal
      setFocus (Just $ halved & tripletStart .~ True)

      modify (unshift $ Just halved)
      moveRight

      modify (unshift $ Just $ halved & tripletEnd .~ True)
      moveRight
    )

      -- insertMoveRight _ _
      -- insertMoveRight $ Just $ halved & tripletEnd .~ True
      -- pure ()
    -- )

  -- let (n, nextState) = createNote (s^.editState)
  -- in
  --    s & notes %~ insertMoveLeft (Just n)
  --      & editState .~ nextState


getFocus :: State (Zipper (Maybe Note)) (Maybe Note)
getFocus = gets current

setFocus :: Maybe Note -> State (Zipper (Maybe Note)) ()
setFocus = modify . replace

focus :: Lens' (Zipper a) a
focus = lens current (flip replace)

moveRight :: State (Zipper a) ()
moveRight =
  modify $ \v -> fromMaybe v (Data.List.NonEmpty.Zipper.right v)