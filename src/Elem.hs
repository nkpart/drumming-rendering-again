{-# language TemplateHaskell, TypeFamilies, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Elem
  (
    Elem(..), _Single, _Triplet,
    ElemSeq(..),
    Cursor(..),
    insertElem, insertAfterElem,
    deleteElem,
    moveCursorDown, moveCursorUp, moveCursorLeft, moveCursorRight,
    takeAtLevel
  )
  where

import Note
import Control.Lens.TH
import Control.Lens.At ( IxValue, Index, Ixed(..) )
import Control.Lens (_2, _Wrapped)
import Prelude hiding (reverse)
import RIO
import RIO.Seq
import GHC.Exts (IsList (Item, toList, fromList))
import qualified RIO.Seq as Seq

data Elem =
    Single Note
  | Triplet Duration ElemSeq
  deriving (Eq)

instance Show Elem where
  show (Single n) = show n
  show (Triplet d n) = "tuplet" <> show d <> "{" <> show n <> show "}"

newtype ElemSeq = ElemSeq (Seq Elem)
  deriving (Eq, IsList)

instance Show ElemSeq where
  show (ElemSeq e) = show (RIO.toList e)

data Cursor = Cursor Int (Maybe Cursor)
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

withNeighbours :: (Int -> Seq Elem -> a) -> Cursor -> ElemSeq -> Maybe a
withNeighbours f (Cursor i nextI) (ElemSeq notes) =
  case nextI of
    Nothing -> Just $ f i notes
    Just trips -> notes ^? ix i . _Triplet . _2 >>= withNeighbours f trips

insertElem :: Elem -> Cursor -> ElemSeq -> ElemSeq
insertElem n =
  modifying (`insertAt` n)

insertAfterElem :: Elem -> Cursor -> ElemSeq -> ElemSeq
insertAfterElem n =
  modifying (\i -> insertAt (i+1) n)

deleteElem :: Cursor -> ElemSeq -> ElemSeq
deleteElem = modifying deleteAt
    -- TODO: What happens if we delete the last note in a triplet
    -- we could stick a (validate f) function in here that
    -- checks if the resulting elem is valide

takeAtLevel :: Int -> Cursor -> ElemSeq -> Maybe (Seq Elem)
takeAtLevel n =
  withNeighbours (\i-> Seq.take n . Seq.drop i)

moveCursorRight :: Cursor -> ElemSeq -> Maybe Cursor
moveCursorRight (Cursor i Nothing) (ElemSeq notes) = do
  focus (i+1) <$ guard (i+1 < Seq.length notes)
moveCursorRight (Cursor i (Just iNext)) (ElemSeq notes) = do
  Triplet _ ns <- RIO.Seq.lookup i notes
  (intermediate i <$> moveCursorRight iNext ns)
    <|> moveCursorRight (focus i) (ElemSeq notes)

moveCursorLeft :: Cursor -> ElemSeq -> Maybe Cursor
moveCursorLeft (Cursor i Nothing) (ElemSeq notes) =
  focus (i-1) <$ guard (i-1 < Seq.length notes)
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