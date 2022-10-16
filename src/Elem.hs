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

focus :: Int -> Cursor
focus i = Cursor i Nothing

intermediate :: Int -> Cursor -> Cursor
intermediate i = Cursor i . Just

-- insertElem :: Cursor -> ElemSeq -> _ -> ElemSeq
modifying :: (Int -> Seq Elem -> Seq Elem) -> Cursor -> ElemSeq -> ElemSeq
modifying f (Cursor i nextI) (ElemSeq notes) = ElemSeq $
  case nextI of
    Nothing -> f i notes
    Just trips -> notes & ix i . _Triplet . _2 RIO.%~ modifying f trips

insertElem :: Elem -> Cursor -> ElemSeq -> ElemSeq
insertElem n =
  modifying (\i -> insertAt i n)

insertAfterElem :: Elem -> Cursor -> ElemSeq -> ElemSeq
insertAfterElem n =
  modifying (\i -> insertAt (i+1) n)

deleteElem :: Cursor -> ElemSeq -> ElemSeq
deleteElem = modifying deleteAt
    -- TODO: What happens if we delete the last note in a triplet
    -- we could stick a (validate f) function in here that
    -- checks if the resulting elem is valide

thingo :: (Int -> Seq Elem -> Maybe a) -> Cursor -> ElemSeq -> Maybe a
thingo f (Cursor i Nothing) (ElemSeq notes) =
  f i notes
thingo f (Cursor i (Just iNext)) (ElemSeq notes) = do
  Triplet _ ns <- RIO.Seq.lookup i notes
  thingo f iNext ns
    <|> thingo f (Cursor i Nothing) (ElemSeq notes)

moveCursorRight :: Cursor -> ElemSeq -> Maybe Cursor
moveCursorRight (Cursor i Nothing) (ElemSeq notes) =
  focus (i+1) <$ RIO.Seq.lookup (i+1) notes
moveCursorRight (Cursor i (Just iNext)) (ElemSeq notes) = do
  Triplet _ ns <- RIO.Seq.lookup i notes
  (intermediate i <$> moveCursorRight iNext ns)
    <|> moveCursorRight (focus i) (ElemSeq notes)

moveCursorLeft :: Cursor -> ElemSeq -> Maybe Cursor
moveCursorLeft (Cursor i Nothing) (ElemSeq notes) =
  focus (i-1) <$ RIO.Seq.lookup (i-1) notes
moveCursorLeft (Cursor i (Just iNext)) (ElemSeq notes) = do
  Triplet _ ns <- RIO.Seq.lookup i notes
  (intermediate i <$> moveCursorLeft iNext ns)
    <|> moveCursorLeft (focus i) (ElemSeq notes)

moveCursorDown :: Cursor -> ElemSeq -> Maybe Cursor
moveCursorDown (Cursor i nextI) (ElemSeq notes) = do
  Triplet _ ns <- RIO.Seq.lookup i notes
  case nextI of
    Nothing -> pure (intermediate i (focus 0))
    Just v -> intermediate i <$> moveCursorDown v ns

moveCursorUp :: Cursor -> Maybe Cursor
moveCursorUp (Cursor _ Nothing) = Nothing
moveCursorUp (Cursor i (Just v)) = Just (Cursor i (moveCursorUp v))

copy :: Cursor -> ElemSeq -> Maybe ElemSeq
copy c notes = do
  v <- notes RIO.^? ix c
  pure $ insertElem v c notes

-- not sure what this should return, elems or notes
getPair :: Cursor -> ElemSeq -> Maybe (Elem,Elem)
getPair c1 notes = do
  x <- notes RIO.^? ix c1
  c2 <- moveCursorRight c1 notes
  y <- notes RIO.^? ix c2
  pure (x,y)