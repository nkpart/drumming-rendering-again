{-# language TemplateHaskell, TypeFamilies, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Elem
  (
    Elem(..), _Single, _Triplet
  )
  where

import Note
import Control.Lens.TH
import Prelude hiding (reverse)

data Elem =
    Single Note
  | Triplet Duration [Elem]
  deriving (Eq)

instance Show Elem where
  show (Single n) = show n
  show (Triplet d n) = "tuplet" <> show d <> "{" <> show n <> show "}"

makeLenses ''Elem
makePrisms ''Elem