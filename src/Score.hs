{-# language TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Score where

import Measure
import Control.Lens.TH
import Control.Lens
import RIO (Bifoldable, bifoldMap)
import Note
import Data.ListZipper hiding (focus)

data Score =
  Score {
    _metadata :: Metadata,
    _measures :: MeasureZipper
  }
  deriving (Eq, Show)

data Metadata = Metadata
  deriving (Eq, Show)

data EdittingZipper a b = EdittingZipper { _lefts :: [a], _focus :: b, _rights :: [a] } deriving (Functor, Eq, Show)

type MeasureZipper = EdittingZipper Measure ZipNotes

instance Bifoldable EdittingZipper where bifoldMap f g (EdittingZipper l x r) = foldMap f (reverse l) <> g x <> foldMap f r

open :: EdittingZipper Measure Measure -> EdittingZipper Measure ZipNotes
open = fmap startEdit

close :: EdittingZipper Measure ZipNotes -> EdittingZipper Measure Measure
close = fmap finishEdit

makeLenses ''Score
makeLenses ''EdittingZipper

allMeasures :: Score -> [Measure]
allMeasures = bifoldMap pure pure . close . view measures

score :: Metadata -> [Measure] -> Score
score md [] = Score md (EdittingZipper [] (startEdit emptyMeasure) [])
score md (m:ms) = Score md (EdittingZipper [] (startEdit m) ms)

insertMeasure :: Score -> Score
insertMeasure =
 measures %~ insertMeasure' emptyMeasure
  where 
    insertMeasure' m (EdittingZipper l x r) = EdittingZipper (finishEdit x:l) (startEdit m) r

insertNote :: Note -> Score -> Score
insertNote n =
 measures . focus %~ insertMoveLeft (Just n)