module EditNoteSpec where

import RIO
import EditState as ES
import Hedgehog
import Elem as E
import qualified Elem

hprop_createNote_init :: Property
hprop_createNote_init = withTests 1 . property $
  fst (createNote initState) === Note RightHand (Duration D4 False) False False

hprop_createNote_withDuration :: Property
hprop_createNote_withDuration = withTests 1 . property $
  fst (createNote (initState & ES.duration .~ d8)) === Note RightHand d8 False False

hprop_createNote_switches_hands :: Property
hprop_createNote_switches_hands = withTests 1 . property $ do
  let (n1, s2) = createNote initState
  n1^..Elem.hand === [RightHand]
  let (n2, s3) = createNote s2
  n2^..Elem.hand === [LeftHand]
  let (n3, _) = createNote s3
  n3^..Elem.hand === [RightHand]
