{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Score.Render (renderScore) where

import Score
import Elem
import Note
import RIO hiding (rights, lefts)
import RIO.Seq (intersperse)

renderScore :: Score -> String
renderScore (Score ms) = unlines [
  "    \\version \"2.20.0\"",
  "    \\include \"pipe-band-drumming.ly\"",
  "    \\header {",
  "        tagline = \"\"",
  "        title = \"2/4 March Template\"",
  "        composer = \"Your Name Here\"",
  "    }",
  "    notes = \\drummode {",
  "      " <> if null ms then "s4" else elemSeqMarkup ms,
  "    }",
  "    \\drums {",
  "      \\set strictBeatBeaming = ##t",
  "      \\time 2/4",
  "      \\notes",
  "    }"
  ]

-- TODO: line breaks after measures?

elemSeqMarkup :: [Elem] -> String
elemSeqMarkup (ElemSeq es) =
      fold . intersperse " " . fmap elemMarkup $ es

elemMarkup :: Elem -> String
elemMarkup (Single n) = noteMarkup n
elemMarkup  (Triplet _ es) =
    " \\tuplet 3/2 { "
    <>
    elemSeqMarkup es -- uh
    <>
    " } "

-- TODO: Test Me
noteMarkup :: Note -> String
noteMarkup n@(Note h _ dt m) =
  case h of
    RightHand -> "P"
    LeftHand -> "p"
    Rest -> "r"
  <>
  durationMarkup n
  <>
  noteDot dt
  <>
  foldMap (renderMod n) m

noteDot :: Bool -> String
noteDot True = "."
noteDot False = ""

renderMod :: Note -> Note.Mod -> String
renderMod d Note.Roll = "~:" <> show (noteValue d * 2 * 2)

durationMarkup :: Note -> String
durationMarkup d =
  show (noteValue d) <> if view noteDotted d then "." else ""
