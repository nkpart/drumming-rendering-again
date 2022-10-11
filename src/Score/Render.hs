{-# LANGUAGE OverloadedStrings #-}
module Score.Render where

import Score
import Elem
import Duration
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
elemSeqMarkup (Cursor ix next) (ElemSeq es) =
      fold . intersperse " " . imap f $ es
    where f elemIx this | elemIx == ix = highlighElemMarkup next this
                        | otherwise    = elemMarkup this

highlighElemMarkup :: Maybe Cursor -> Elem -> String
highlighElemMarkup (Just _) (Single _) = error "cursor mismatch, indexing into Single"
highlighElemMarkup Nothing (Triplet _ _) = error "cursor mismatch, no index for triplet"

highlighElemMarkup Nothing (Single n) = addFocus n
highlighElemMarkup (Just i) (Triplet _ es) =
    " \\tuplet 3/2 { "
    <>
   elemSeqMarkup i es
    <>
    " } "

elemMarkup :: Elem -> String
elemMarkup (Single n) = noteMarkup n
elemMarkup  (Triplet _ es) =
    " \\tuplet 3/2 { "
    <>
    elemSeqMarkup (Cursor (-1) Nothing) es -- uh
    <>
    " } "

-- TODO: Test Me
noteMarkup :: Note -> String
noteMarkup (Note h d m) =
  case h of
    RightHand -> "P"
    LeftHand -> "p"
    Rest -> "r"
  <>
  durationMarkup d
  <>
  foldMap (renderMod d) m

renderMod :: Duration -> Note.Mod -> String
renderMod d Note.Roll = "~:" <> show (noteValue d * 2 * 2)

durationMarkup :: Duration -> String
durationMarkup d =
  show (noteValue d) <> if view dotted d then "." else ""

addFocus :: Note -> String
addFocus c =
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
