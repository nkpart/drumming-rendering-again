{-# language TemplateHaskellQuotes #-}
module Score.RenderSpec where

import Score
import Score.Render
import Note
import Measure
import Data.ListZipper
import Data.Bifoldable
import Hedgehog
import RIO.FilePath
import Test.Tasty.Golden
import Test.Tasty
import RIO (fromString)


hprop_bifoldMap :: Property
hprop_bifoldMap = withTests 1 . property $
    bifoldMap pure pure (EdittingZipper [2::Int,1] 3 [4,5]) === [1,2,3,4,5]

test_1 :: TestTree
test_1 =
    defaultGolden (show 'renderScore <> "blank") (renderScore $ score Metadata [])

test_2 :: TestTree
test_2 =
    let
      l = [Measure [Note "r4"]]
      x = ListZipper [] (Just $ Note "P4") []
      r = [Measure [Note "r4"]]
      thisScore =
         Score Metadata
               (EdittingZipper l x r)

    in
      defaultGolden (show 'renderScore <> "-somewhat-complicated") (renderScore thisScore)

defaultGolden :: String -> String -> TestTree
defaultGolden name output =
  goldenVsString name (".golden" </> name </> "golden") (pure $ fromString output)
