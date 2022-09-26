{-# LANGUAGE OverloadedStrings #-}
module Score.Render where

import Score
import Elem
import Measure
import Data.ListZipper
import RIO
import RIO.List (intersperse)

renderScore :: Score -> String
renderScore (Score _ _ ms) = unlines [
  "    \\version \"2.20.0\"",
  "    \\include \"pipe-band-drumming.ly\"",
  "    \\header {",
  "        tagline = \"\"",
  "        title = \"2/4 March Template\"",
  "        composer = \"Your Name Here\"",
  "    }",
  "    notes = \\drummode {",
  "      " <> renderNotes ForEdit ms,
  "    }",
  "    \\drums {",
  "      \\set strictBeatBeaming = ##t",
  "      \\time 2/4",
  "      \\notes",
  "    }"
  ]


data RenderTarget =
  ForPresentation | ForEdit

-- TODO: line breaks after measures?
renderNotes :: RenderTarget -> ZipNotes -> String
renderNotes target (ListZipper l x r) =
  let renderSide = fmap noteMarkup . catMaybes
      renderFocus =
        case target of
          ForPresentation -> pure . noteMarkup
          ForEdit -> pure . editMarkup
      rendered = fold . intersperse " " $ renderSide (reverse l) <> maybe [] renderFocus x <> renderSide r
  in if null rendered then "s4" else rendered

-- TODO: Test Me
noteMarkup :: Note -> String
noteMarkup (Rest d) = "r" <> durationMarkup d 
noteMarkup (Note h d) = 
  case h of 
    RightHand -> "P"
    LeftHand -> "p"
  <>
  durationMarkup d

durationMarkup :: Duration -> String
durationMarkup (Duration d dt) =
  case d of
    D1 -> "1"
    D2 -> "2"
    D4 -> "4"
    D8 -> "8"
    D16 -> "16"
    D32 -> "32"
  <> if dt then "." else ""

editMarkup :: Note -> String
editMarkup c =
   "\n" <>
   "\\override NoteHead.color = \"red\"" <>
  "\n" <>
   " " <> noteMarkup c <> " " <>
  "\n" <>
   "\\revert NoteHead.color" <>
  "\n"