module Measure where

import Data.List.NonEmpty.Zipper
import Elem
import Prelude hiding (reverse)
import RIO

type ZipNotes = Maybe (Zipper Note)

editList :: [Note] -> ZipNotes
editList (n:ns) = Just $ fromNonEmpty $ n :| ns
editList [] = Nothing

nelist :: Zipper a -> [a]
nelist = toList

copy :: Zipper a -> Zipper a
copy x =
   push (current x) x

getPair :: Zipper a -> Maybe (a,a)
getPair z = do
  let x = current z
  z2 <- right z
  let y = current z2
  pure (x, y)