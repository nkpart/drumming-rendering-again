module ScoreSpec where

import Score
import Measure
import Note
import RIO
import Data.ListZipper hiding (focus)
import Hedgehog

hprop_Score_appends :: Property
hprop_Score_appends = withTests 1 . property $
      let start = score Metadata [Measure [Note "X"]]
          modified = insertMeasure start
      in do
        allMeasures modified === [Measure [Note "X"], emptyMeasure]
        (modified ^. measures . focus) === ListZipper [] Nothing []
