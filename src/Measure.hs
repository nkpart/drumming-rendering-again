module Measure where

import Data.ListZipper
import Elem
import Prelude hiding (reverse)
import RIO

type ZipNotes = ListZipper (Maybe Note)

editList :: [Note] -> ZipNotes
editList (n:ns) = zipper0L (Just n) (fmap Just ns)
editList [] = zipper0L Nothing []

nelist :: ListZipper a -> [a]
nelist (ListZipper [] x r) = x : r
nelist (ListZipper (x:xs) f r) = RIO.reverse (x : xs) <> (f : r)

copy :: ListZipper a -> ListZipper a
copy x =
    insertMoveRight (x ^. focus) x

getPair :: ListZipper (Maybe a) -> Maybe (a,a)
getPair (ListZipper _ _ []) = 
  Nothing
getPair (ListZipper _ (Just x) ((Just r):_)) =
  Just (x, r)
getPair _ = error "getPair whoopsie" 