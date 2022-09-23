module NoteSpec where

import Note
import Hedgehog


hprop_builds :: Property
hprop_builds = withTests 1 . property $
      note False === Note "p"