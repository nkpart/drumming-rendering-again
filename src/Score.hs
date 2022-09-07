
module Score where

import Data.ListZipper
import Measure

type Score = ListZipper Measure

fromMeasures :: [Measure] -> Score
fromMeasures [] = zipper0L emptyMeasure []
fromMeasures (m:ms) = zipper0L m ms

appendMeasure :: ListZipper Measure -> ListZipper Measure
appendMeasure =
 insertMoveLeft emptyMeasure
