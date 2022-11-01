{-# OPTIONS_GHC -Wall -Wno-missing-signatures #-}
{-# language FlexibleContexts #-}

module Main where

-- import Gui
-- main = gui

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import RIO.List (findIndices)

data Note = Note Hand Flam Roll NoteDuration
data Hand = LeftHand | RightHand
data Flam = Flam | NoFlam
data Roll = Roll | NoRoll
data NoteDuration = QuarterNote

isRolled (Note _ _ Roll _) = True
isRolled (Note _ _ NoRoll _) = False

aNote :: Int -> Note -> Diagram B
aNote n (Note hand _flam _roll _dur) =
    (positioned base # named n)
  where
    positioned = translateY
        (case hand of
            RightHand -> 5
            LeftHand -> (-5))

    base = hcat ([alignT stem, alignT nh]) # showOrigin

    nh :: Diagram B
    nh = square noteHeadSize # fc red
                   # centerX

    stem :: Diagram B
    stem = rect stemWidth (stemHeight hand) # fc blue

letsGo :: [Note] -> Diagram B
letsGo notes =
  addRolls notes
  (hsep 2.0 . imap aNote $ notes)

  where addRolls :: [Note] -> Diagram B -> Diagram B
        addRolls ns d = findIndices isRolled ns # foldl rolled d
        rolled d n = connect'
                    (with & arrowShaft .~ shaft)
                    n (n+1) d
                --   # frame 0.25
                    where shaft = arc xDir (-1/2 @@ turn)

noteHeadSize = 10

stemWidth = 2
stemHeight RightHand = 50
stemHeight LeftHand = 40

textExample :: Diagram B
textExample = 
    (text "\xE0A4" 
        # fontSizeL 0.5 
        # font "Bravura"
    )
        <> (vrule 0.55 
            # translateX 0.086
            # translateY 0.30
            # lineWidth 7
            # showOrigin
        )
        <> (rect 0.24 0.7 
            # translateY 0.25
            # showOrigin
        )

textExample2 :: Diagram B
textExample2 = 
    (text "\xE0A4" 
        # fontSizeL 0.5 
        # font "Bravura"
    )
     <>
    (text "\xE241" 
        # fontSizeL 0.5 
        # font "Bravura"
        # translateY (-0.6)
    )
        <> (vrule 0.55 
            # translateX (-0.086)
            # translateY (-0.3)
            # lineWidth 7
            # showOrigin
        )
        <> (rect 0.24 0.7 
            # translateY (-0.25)
            # showOrigin
        )


-- main = mainWith (pad 2.1 $ scale 5 textExample)
-- textExample = (text "5\x1D158\&3 \xE0A4\xE210 " # fontSize 32 # font "Bravura")
main = mainWith (scale 10 textExample2 ||| scale 10 textExample2 ||| 
 (scale 10 $ (text "\xE210" # fontSizeL 0.5 # font "Bravura") <> rect 0.5 0.5)
 )
 where _score = [
        Note RightHand Flam NoRoll QuarterNote,
        Note RightHand NoFlam Roll QuarterNote,
        Note LeftHand NoFlam NoRoll QuarterNote,
        Note RightHand NoFlam Roll QuarterNote,
        Note RightHand NoFlam Roll QuarterNote,
        Note RightHand NoFlam NoRoll QuarterNote
        ]