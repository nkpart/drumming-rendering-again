{-# language TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

{-# options_ghc -Wno-deprecations #-}

module Score where

import Phrase
import Note
import Elem
import Control.Lens.TH

newtype Score =
  Score {
    _notes :: [Token]
  }
  deriving (Eq, Show)

data Token =
    Elem_ Elem
  | BarLine
  | NewLine
  deriving (Eq, Show)

makeLenses ''Score

phrasesToScore :: [Phrase] -> Score
phrasesToScore ps =
  let wats = (=<<) (\p -> fmap Elem_ (renderPhrase p) <> [NewLine]) ps
  in Score wats

phrases_24 :: [Phrase]
phrases_24 = [
  -- 13 roll
    Phrase AnyHand MatchHand (\_ r (m:_) -> [m $ r d4] )
  -- 
  , Phrase AnyHand AnyHand (\_ r (m:_) -> [m $ r d8] )
  --  7

  -- , Phrase AnyHand AnyHand (\_ r mods -> [r d8, r d8 & roll] ) []
  -- , Phrase allN allN (\l r mods -> [r d8, l d8] ) []
  ]