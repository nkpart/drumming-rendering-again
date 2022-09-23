module Measure where

import Data.ListZipper
import Note
import Prelude hiding (reverse)
import RIO

newtype Measure = Measure [Note]
  deriving (Eq, Show)

type ZipNotes = ListZipper (Maybe Note)

emptyMeasure :: Measure
emptyMeasure = Measure []

startEdit :: Measure -> ZipNotes
startEdit (Measure (n:ns)) = zipper0L (Just n) (fmap Just ns)
startEdit (Measure []) = zipper0L Nothing []

finishEdit :: ZipNotes -> Measure
finishEdit = Measure . catMaybes . nelist

nelist :: ListZipper a -> [a]
nelist (ListZipper [] x r) = x : r
nelist (ListZipper (x:xs) f r) = RIO.reverse (x : xs) <> (f : r)

append :: Note -> ZipNotes -> ZipNotes
append = insertMoveLeft . Just

copy :: ZipNotes -> ZipNotes
copy x =
    insertMoveRight (x ^. focus) x

