{-# language TemplateHaskell, TypeFamilies, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Elem
  (
    Elem(..), _Single, _Triplet,
    totalDuration
  )
  where

import Note
import Control.Lens.TH
import Prelude hiding (reverse)
import Duration (durationRational)
import Data.Ratio
import RIO
import Control.Lens (sumOf)

data Elem =
    Content String
  | Single Note
  | Triplet Duration [Elem]
  deriving (Eq)

instance Show Elem where
  show (Content n) = show n
  show (Single n) = show n
  show (Triplet d n) = "tuplet" <> show d <> "{" <> show n <> show "}"

makeLenses ''Elem
makePrisms ''Elem

totalDuration :: Elem -> Ratio Int
totalDuration (Content _) = 0
totalDuration (Single e) =
  if view noteDotted e
    then (3/2) * (view duration e & durationRational)
    else view duration e & durationRational
totalDuration (Triplet _ es) = (2%3) * sumOf (traverse . to totalDuration) es
