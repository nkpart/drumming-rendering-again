module ZipNotes where

import Data.ListZipper
import Control.Lens
import Note

type ZipNotes = ListZipper Note

init :: Bool -> ZipNotes
init n = fromList (note n) []

append :: ZipNotes -> Note -> ZipNotes
append l n = insertMoveRight n l

toList :: ZipNotes -> [Note]
toList = list

fromList :: Note -> [Note] -> ZipNotes
fromList = zipper0L

copy :: ZipNotes -> ZipNotes
copy x = 
    insertMoveRight (x ^. focus) x

