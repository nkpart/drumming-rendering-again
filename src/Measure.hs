module Measure where

import Note
import ZipNotes

newtype Measure = Measure [Note]
  deriving (Eq, Show)

emptyMeasure :: Measure
emptyMeasure = Measure []

startEdit :: Measure -> ZipNotes
startEdit (Measure []) = fromList (note False) []
startEdit (Measure (n:ns)) = fromList n ns

finishEdit :: ZipNotes -> Measure
finishEdit = Measure . toList
