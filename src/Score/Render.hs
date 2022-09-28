{-# LANGUAGE OverloadedStrings #-}
module Score.Render where

import Score
import Elem
import Measure
import Data.List.NonEmpty.Zipper
import RIO hiding (rights, lefts)
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
renderNotes target zz =
  let (l, x, r) = (lefts zz, current zz, rights zz)
      renderSide = fmap noteMarkup . catMaybes
      renderFocus =
        case target of
          ForPresentation -> pure . noteMarkup
          ForEdit -> pure . editMarkup
      rendered = fold . intersperse " " $ renderSide l <> maybe [] renderFocus x <> renderSide r
  in if null rendered then "s4" else rendered

-- TODO: Test Me
noteMarkup :: Note -> String
noteMarkup (Note h d tState) =
  (if tState == Start 
    then " \\tuplet 3/2 { "
    else mempty)
  <>
  case h of
    RightHand -> "P"
    LeftHand -> "p"
    Rest -> "r"
  <>
  durationMarkup d
  <>
  (if tState == End
    then " } "
    else "")

durationMarkup :: Duration -> String
durationMarkup (Duration d dt) =
  show d <> if dt then "." else ""

editMarkup :: Note -> String
editMarkup c =
  let prop =
       case c ^. hand of
        Rest -> "Rest.color"
        _ -> "NoteHead.color"
  in
   "\n" <>
   "\\override " <> prop <> " = \"red\"" <>
  "\n" <>
   " " <> noteMarkup c <> " " <>
  "\n" <>
   "\\revert " <> prop <>
  "\n"