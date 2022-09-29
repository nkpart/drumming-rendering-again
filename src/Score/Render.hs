{-# LANGUAGE OverloadedStrings #-}
module Score.Render where

import Score
import Elem
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
  "      " <> maybe "s4" (renderNotes ForEdit) ms,
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
renderNotes :: RenderTarget -> Zipper Note -> String
renderNotes target zz =
  let (l, x, r) = (lefts zz, current zz, rights zz)
      renderSide = fmap noteMarkup
      renderFocus =
        case target of
          ForPresentation -> pure . noteMarkup
          ForEdit -> pure . editMarkup
      rendered = fold . intersperse " " $ renderSide l <> renderFocus x <> renderSide r
  in rendered

-- TODO: Test Me
noteMarkup :: Note -> String
noteMarkup (Note h d tState m) =
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
  foldMap (renderMod d) m
  <>
  (if tState == End
    then " } "
    else "")

renderMod :: Duration -> Elem.Mod -> String
renderMod (Duration n _) Elem.Roll = "~:" <> show (n*2*2)

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