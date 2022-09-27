module Measure where

import Data.List.NonEmpty.Zipper
import Elem
import Prelude hiding (reverse)
import RIO

type ZipNotes = Zipper (Maybe Note)

editList :: [Note] -> ZipNotes
editList (n:ns) = fromNonEmpty $ Just n :| fmap Just ns
editList [] = fromNonEmpty $ Nothing :| []

nelist :: Zipper a -> [a]
nelist = toList

copy :: Zipper a -> Zipper a
copy x =
   push (current x) x

getPair :: Zipper (Maybe a) -> Maybe (a,a)
getPair z = do
  x <- current z
  z2 <- Data.List.NonEmpty.Zipper.right z
  y <- current z2
  pure (x, y)