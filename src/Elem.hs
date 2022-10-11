{-# language TemplateHaskell, TypeFamilies, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Elem where

import Note
import Control.Lens.TH
import Control.Lens.At
import Control.Lens (_2, _Wrapped)
import Prelude hiding (reverse)
import RIO
import RIO.Seq
import GHC.Exts (IsList (Item, toList, fromList))
import qualified RIO.Seq as Seq

data Elem =
    Single Note
  | Triplet { _tripletDuration :: Duration,  _tripletNotes :: ElemSeq }
  deriving (Eq)

instance Show Elem where
  show (Single n) = show n
  show (Triplet d n) = "tuplet" <> show d <> "{" <> show n <> show "}"

newtype ElemSeq = ElemSeq (Seq Elem)
  deriving (Eq, IsList)

instance Show ElemSeq where
  show (ElemSeq e) = show (RIO.toList e)

data Cursor = Cursor {_position :: Int,  _tripletCursor :: Maybe Cursor }
  deriving (Eq, Show)

instance IsList Cursor where
  type instance Item Cursor = Int
  fromList [] = error "Cursor.fromList - Need at least one cursor level."
  fromList [x] = Cursor x Nothing
  fromList (x:xs) = Cursor x (Just $ GHC.Exts.fromList xs)
  toList (Cursor i js) = i : maybe [] GHC.Exts.toList js

makeWrapped ''ElemSeq
makeLenses ''Elem
makePrisms ''Elem
makeLenses ''Cursor

type instance IxValue ElemSeq = Elem
type instance Index ElemSeq = Cursor

instance Ixed ElemSeq where
  ix (Cursor i Nothing) =
    _Wrapped . ix i
  ix (Cursor i (Just n)) =
    _Wrapped . ix i . _Triplet . _2 . ix n

-- TODO: monadstate layer

appendToCursor :: Int -> Cursor -> Cursor
appendToCursor n (Cursor p Nothing) = Cursor p (Just $ Cursor n Nothing)
appendToCursor n (Cursor p (Just v)) =
  Cursor p (Just $ appendToCursor n v)

elemAt :: Cursor -> ElemSeq -> Maybe Elem
elemAt c notes =
  notes RIO.^? ix c

insertElem :: Cursor -> Elem -> ElemSeq -> ElemSeq
insertElem (Cursor i nextI) n (ElemSeq notes) = ElemSeq $
  case nextI of
    Nothing -> insertAt i n notes
    Just trips -> notes & ix i . _Triplet . _2 RIO.%~ insertElem trips n

insertAfterElem :: Cursor -> Elem -> ElemSeq -> ElemSeq
insertAfterElem (Cursor i Nothing) n (ElemSeq notes) = ElemSeq $
  insertAt (i+1) n notes
insertAfterElem (Cursor i (Just trips)) n (ElemSeq notes) = ElemSeq (
  notes & ix i . _Triplet . _2 RIO.%~ insertAfterElem trips n)

deleteElem :: Cursor -> ElemSeq -> ElemSeq
deleteElem (Cursor i Nothing) (ElemSeq notes) = ElemSeq $
  deleteAt i notes
deleteElem (Cursor i (Just trips)) (ElemSeq notes) = ElemSeq (
    -- TODO: What happens if we delete the last note in a triplet
    -- we could stick a (validate f) function in here that
    -- checks if the resulting elem is valide
    notes & ix i . _Triplet . _2 RIO.%~ deleteElem trips
  )

moveCursorRight :: Cursor -> ElemSeq -> Maybe Cursor
moveCursorRight (Cursor i Nothing) (ElemSeq notes) =
  case RIO.Seq.lookup (i+1) notes of
    Nothing -> Nothing
    Just (Single _) -> Just (Cursor (i+1) Nothing)
    Just (Triplet _ ns) -> Just (Cursor (i+1) (Just $ enter ns))
moveCursorRight (Cursor i (Just iNext)) (ElemSeq notes) =
  case RIO.Seq.lookup i notes of
    Nothing -> Nothing -- but this is a big whoopsy, we have a cursor pointing at nothing
    Just (Single _) -> Nothing -- this is also a big whoopsy, we have a triplet cursor pointing at a single
    Just (Triplet _ ns) -> 
      case moveCursorRight iNext ns of
        Nothing -> moveCursorRight (Cursor i Nothing) (ElemSeq notes)
        Just v -> Just (Cursor i (Just v))

moveCursorLeft :: Cursor -> ElemSeq -> Maybe Cursor
moveCursorLeft (Cursor i Nothing) (ElemSeq notes) =
  case RIO.Seq.lookup (i-1) notes of
    Nothing -> Nothing
    Just (Single _) -> Just (Cursor (i-1) Nothing)
    Just (Triplet _ ns) -> Just (Cursor (i-1) (Just $ enter ns))
moveCursorLeft (Cursor i (Just iNext)) (ElemSeq notes) =
  case RIO.Seq.lookup i notes of
    Nothing -> Nothing -- but this is a big whoopsy, we have a cursor pointing at nothing
    Just (Single _) -> Nothing -- this is also a big whoopsy, we have a triplet cursor pointing at a single
    Just (Triplet _ ns) -> 
      case moveCursorLeft iNext ns of
        Nothing -> moveCursorLeft (Cursor i Nothing) (ElemSeq notes)
        Just v -> Just (Cursor i (Just v))

enter :: ElemSeq -> Cursor
enter (ElemSeq es) = 
  Cursor 0 $
  case Seq.lookup 0 es of
    Just (Single _) -> Nothing
    Just (Triplet _ ns) -> Just $ enter ns
    Nothing -> error "trying to cursor an empty list"

-- TODO, what should append look like?
-- Append after everything?
-- appendNote :: Cursor -> Note -> ElemSeq -> ElemSeq

copy :: Cursor -> ElemSeq -> Maybe ElemSeq
copy c notes = do
  v <- elemAt c notes
  pure $ insertElem c v notes

-- not sure what this should return, elems or notes
getPair :: Cursor -> ElemSeq -> Maybe (Elem,Elem)
getPair c1 notes = do
  x <- elemAt c1 notes
  c2 <- moveCursorRight c1 notes
  y <- elemAt c2 notes
  pure (x,y)