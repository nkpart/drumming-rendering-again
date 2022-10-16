module EditNoteSpec where

import RIO
import qualified EditState as ES
import Hedgehog
import Note

hprop_createNote_init :: Property
hprop_createNote_init = withTests 1 . property $
  fst (ES.createNote ES.initState) === Note RightHand d4 False mempty

hprop_createNote_withDuration :: Property
hprop_createNote_withDuration = withTests 1 . property $
  fst (ES.createNote (ES.initState & duration .~ d8)) === Note RightHand d8 False mempty

hprop_createNote_switches_hands :: Property
hprop_createNote_switches_hands = withTests 1 . property $ do
  let (n1, s2) = ES.createNote ES.initState
  n1^..hand === [RightHand]
  let (n2, s3) = ES.createNote s2
  n2^..hand === [LeftHand]
  let (n3, _) = ES.createNote s3
  n3^..hand === [RightHand]
