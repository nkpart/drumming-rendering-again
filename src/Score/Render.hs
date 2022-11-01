{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Score.Render where

import Score
import Elem
import Note
import RIO hiding (rights, lefts)
import RIO.Seq (intersperse)
import Control.Lens (imap)

renderScore :: Score -> String
renderScore (Score _a cur ms) = unlines [
  "    \\version \"2.20.0\"",
  "    \\include \"pipe-band-drumming.ly\"",
  "    \\header {",
  "        tagline = \"\"",
  "        title = \"2/4 March Template\"",
  "        composer = \"Your Name Here\"",
  "    }",
  "    notes = \\drummode {",
  "      " <> if isEmpty ms then "s4" else elemSeqMarkup cur ms,
  "    }",
  "    \\drums {",
  "      \\set strictBeatBeaming = ##t",
  "      \\time 2/4",
  "      \\notes",
  "    }"
  ]

isEmpty :: ElemSeq -> Bool
isEmpty (ElemSeq ms) = null ms

data RenderTarget =
  ForPresentation | ForEdit

-- TODO: line breaks after measures?

elemSeqMarkup :: Cursor -> ElemSeq -> String
elemSeqMarkup (Cursor (ix :| next)) (ElemSeq es) =
      fold . intersperse " " . imap f $ es
    where f elemIx this | elemIx == ix = highlighElemMarkup next this
                        | otherwise    = elemMarkup this

highlighElemMarkup :: [Int] -> Elem -> String
highlighElemMarkup [] e = addFocus (elemMarkup e)
highlighElemMarkup _ (Single _) = error "cursor mismatch, indexing into Single"
highlighElemMarkup (i:is) (Triplet _ es) =
    " \\tuplet 3/2 { "
    <>
   elemSeqMarkup (Cursor (i:|is)) es
    <>
    " } "

elemMarkup :: Elem -> String
elemMarkup (Single n) = noteMarkup n
elemMarkup  (Triplet _ es) =
    " \\tuplet 3/2 { "
    <>
    elemSeqMarkup [-1] es -- uh
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

addFocus :: String -> String
addFocus c =
   "\n" <>
   "\\override " <> "Rest.color" <> " = \"red\"\n" <>
   "\\override " <> "NoteHead.color" <> " = \"red\"\n" <>
  "\n" <>
   " " <> c <> " " <>
  "\n" <>
   "\\revert " <> "NoteHead.color" <> "\n" <>
   "\\revert " <> "Rest.color" <> "\n" <>
  "\n"
