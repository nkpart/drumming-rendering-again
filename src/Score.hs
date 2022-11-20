{-# language TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

{-# options_ghc -Wno-deprecations #-}

module Score where

import Note
import Elem
import Control.Lens.TH
import RIO

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

score :: [Token] -> Score
-- TODO The cursor and the content doesn't line up :(
score = Score


data Phrase = 
  Phrase 
         (Elem -> Bool) -- valid befores
         (Elem -> Bool) -- valid afters
         (NoteSpec -> NoteSpec -> [Elem->Elem] -> [Elem])
         [Int] -- ornament locations

type NoteSpec = Duration -> Elem

allN :: b -> Bool
allN = const True

r4, r8 :: Elem
r4 = Single (Note RightHand d4 False mempty)
r8 = Single (Note RightHand d8 False mempty)

stuff :: [Phrase]
stuff = [
  -- 13 roll
    Phrase allN allN (\_ r (m:_) -> [m $ r d4] ) []
  -- 
  , Phrase allN allN (\_ r mods -> [r d8, r d8] ) []
  , Phrase allN allN (\l r mods -> [r d8, l d8] ) []
-- PpP
-- 

  ]




---