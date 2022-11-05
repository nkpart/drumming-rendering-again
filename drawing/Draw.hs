{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine ( mainWith, B )
import Data.List (tails)

noteThing :: Diagram B
noteThing = 
   (square 1 # fc blue)
   ===
   (rect 0.2 1.5 # fc red)
beams n =
  vsep 0.1
   (replicate n $ rect 1 0.2 # fc yellow)

rNote [] = mempty
rNote (n:[]) = alignB (beams n) `atop` alignB noteThing
rNote (n:_) = alignB (beams n) `atop` alignB noteThing
                 ||| strut 0.2

main = mainWith $
 hsep 0 $ fmap rNote . tails $ [2, 2, 2]