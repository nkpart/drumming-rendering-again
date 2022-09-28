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
                      >>= right
                      >>= (Just . replace (Just n2'))
                      >>= left
    )
                   

-- Crying out for a property based test
-- for all inputs, total duration should be the same across the pair
toggleDots :: (Duration, Duration) -> (Duration, Duration)
toggleDots (n1, n2) =
    case (view dotted n1, view dotted n2, noteValue n1 == noteValue n2, n2 == decreaseDVal n1) of
      (True, False, False, True) ->
        -- proper dotting
        (n1 & dotted .~ False, n2 & increaseDVal)
      (False, False, True, _) ->
        -- proper no dotting
        (n1 & dotted .~ True, n2 & decreaseDVal)
      (_,_,_,_) ->
        (n1, n2)

splitNote :: Score -> Score
splitNote s =
  let n = (s ^. notes . to current)
  in
    case n of
      Just base ->
         let new = (Elem.duration %~ decreaseDVal) base
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
  fromMaybe z $ right (unshift n z)

makeTriplet :: Score -> Score
makeTriplet s =
    s & notes %~ execThis (do
      v <- fromJust <$> getFocus
      let halved = v & Elem.duration %~ decreaseDVal
      setFocus (Just $ halved & tripletState .~ Start)

      modify (unshift $ Just $ halved & tripletState .~ Covered)
      opOrContinue right

      modify (unshift $ Just $ halved & tripletState .~ End)
      opOrContinue right
    )

execThis :: StateT s Maybe a -> s -> s
execThis action s =
  fromMaybe s (execStateT action s)


combineNotes :: Score -> Score
combineNotes =
  notes %~ execThis (do
      v1 <- getFocus
      opOrCancel right
      v2 <- getFocus
      opOrCancel left
      case addNotes v1 v2 of
        Nothing -> pure Nothing

      pure ()
    )


-- unTriplet :: Score -> Score
-- unTriplet =
--     notes %~ execState (do
--       v <- fromJust <$> getFocus
--       case v ^. tripletState of
--         None -> pure ()
--         _ -> 
--     )

      -- insertMoveRight _ _
      -- insertMoveRight $ Just $ halved & tripletEnd .~ True
      -- pure ()
    -- )

  -- let (n, nextState) = createNote (s^.editState)
  -- in
  --    s & notes %~ insertMoveLeft (Just n)
  --      & editState .~ nextState


getFocus :: StateT (Zipper (Maybe Note)) Maybe (Maybe Note)
getFocus = gets current

setFocus :: Maybe Note -> StateT (Zipper (Maybe Note)) Maybe ()
setFocus = modify . replace

focus :: Lens' (Zipper a) a
focus = lens current (flip replace)

moveRight :: StateT (Zipper a) Maybe ()
moveRight =
  opOrContinue right

moveLeft :: StateT (Zipper a) Maybe ()
moveLeft =
  opOrContinue left

opOrCancel :: (s -> Maybe s) -> StateT s Maybe ()
opOrCancel f = do
     x <- get
     case f x of
       Just v -> put v 
       Nothing -> lift Nothing

opOrContinue :: (s -> Maybe s) -> StateT s Maybe ()
opOrContinue f = do
     x <- get
     case f x of
       Just v -> put v 
       Nothing -> pure ()
