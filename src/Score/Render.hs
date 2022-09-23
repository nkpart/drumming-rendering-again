{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Score.Render where

import Score
import Note
import Measure
import Text.Heterocephalus
import Text.Blaze.Renderer.String -- todo text
import Text.Blaze (Markup, toMarkup)
import RIO
import RIO.List (intersperse)

renderScore :: Score -> String
renderScore (Score _ ms) = renderMarkup [compileText|
    \version "2.20.0"
    \include "pipe-band-drumming.ly"
    \header {
        tagline = ""
        title = "2/4 March Template"
        composer = "Your Name Here"
    }
    notes = \drummode {
      #{ bifoldMap measureMarkup measureMarkup $ close ms }
    }
    \drums {
      \set strictBeatBeaming = ##t
      \time 2/4
      \notes
    }
    |]

measureMarkup :: Measure -> Markup
measureMarkup (Measure m) = fold (intersperse " " $ fmap renderNotes m)

renderNotes :: Note -> Markup
renderNotes (Note c) = toMarkup c
