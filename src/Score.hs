{-# language TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

{-# options_ghc -Wno-deprecations #-}

module Score where

import Measure
import Control.Lens.TH
import RIO
import Elem
import EditState
import Data.List.NonEmpty.Zipper
import RIO.State
    ( StateT, MonadState(put, get), gets, execStateT, modify )
import Control.Lens ((?~), _Just)

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
allNotes = maybe [] nelist . view notes

score :: Metadata -> [Note] -> Score
score md ns = Score initState md (editList ns)

insertNote :: Score -> Score
insertNote s =
  let (n, nextState) = createNote (s^.editState)
  in
    s & notes ?~
        (case view notes s of
          Just v -> insertMoveLeft n v
          Nothing -> fromNonEmpty (pure n)
          )
      & editState .~ nextState

deleteNote :: Score -> Score
deleteNote s =
  s & notes %~ (>>= delete)

toggleDotCut :: Score -> Score
toggleDotCut s =
  let Just (n1, n2) = getPair =<< view notes s -- TODO whoopsie
      dots = (n1 ^. Elem.duration, n2 ^. Elem.duration)
      (d1', d2') = toggleDots dots
      n1' = n1 & Elem.duration .~ d1'
      n2' = n2 & Elem.duration .~ d2'
   in traceShow (dots, (d1',d2'))
    s & notes . _Just %~ \v -> fromMaybe v (Just (replace n1' v)
                      >>= right
                      >>= (Just . replace n2')
                      >>= left
    )


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

splitNote :: Score -> Score
splitNote s =
  let n = (s ^? notes . _Just . to current)
  in
    case n of
      Just base ->
         let new = (Elem.duration %~ halveDuration) base
         in
          s
            & notes . _Just %~ replace new
            & notes . _Just %~ insertMoveLeft new
      Nothing ->
        s

replaceNote :: Note -> Score -> Score
replaceNote n =
 notes . _Just %~ insertMoveLeft n

insertMoveLeft :: a -> Zipper a -> Zipper a
insertMoveLeft n z =
  fromMaybe z $ right (unshift n z)

makeTriplet :: Score -> Score
makeTriplet s =
    s & notes . _Just %~ execThis (do
      v <- getFocus
      let halved = v & Elem.duration %~ halveDuration
      setFocus (halved & tripletState .~ Start)

      modify (unshift $ halved & tripletState .~ Covered)
      opOrContinue right

      modify (unshift $ halved & tripletState .~ End)
      opOrContinue right
    )

execThis :: StateT s Maybe a -> s -> s
execThis action s =
  fromMaybe s (execStateT action s)

combineNotes :: Score -> Score
combineNotes =
  notes . _Just %~ execThis (do
      v1 <- getFocus
      opOrCancel right
      v2 <- getFocus
      opOrCancel left
      case addNotes v1 v2 of
        Nothing -> lift Nothing
        Just v -> do
          modify (\s -> fromMaybe s $ delete s)
          modify (\s -> fromMaybe s $ delete s)
          modify (unshift v)

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


getFocus :: StateT (Zipper Note) Maybe Note
getFocus = gets current

setFocus :: Note -> StateT (Zipper Note) Maybe ()
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
