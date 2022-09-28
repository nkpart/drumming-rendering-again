{-# LANGUAGE TemplateHaskell, OverloadedStrings, OverloadedLabels, NoImplicitPrelude, MagicHash #-}
module Gui where

import qualified GI.Gtk as Gtk
import GHC.Word
import Data.GI.Base
import RIO hiding (on, set)
import Control.Lens.TH
import Score
import qualified Prelude
import Score.Render
import System.Process
import Prelude (print)
import GHC.Base (Char(C#))
import GHC.Exts (chr#, word2Int#)
import RIO.Text (pack)
import EditState (duration)
import Elem (increaseDVal, decreaseDVal, duration, toggleDotted, swapHand, hand)
import Control.Lens (_Just)
import GI.Gdk (keyvalName)
import RIO.Time
import Data.List.NonEmpty.Zipper (left, right)


-- TODO: Undo stack
-- TODO: 


data App = App {
 _logfunc :: !LogFunc,
 _scoreRef :: IORef Score
    }

makeLenses ''App

instance HasLogFunc App where logFuncL = logfunc

gui :: IO ()
gui = runSimpleApp $ do
  lf <- view logFuncL
  ref <- newIORef (score Metadata [])
  runRIO (App lf ref) $ do
    _ <- Gtk.init Nothing
    win <- new Gtk.Window [ #title := "Hi there" ]
    _ <- on win #destroy Gtk.mainQuit

    box <- Gtk.boxNew Gtk.OrientationVertical 0

    #add win box

    image <- Gtk.imageNew
    editStateLabel <- Gtk.labelNew Nothing
    debugShowLabel <- Gtk.labelNew Nothing

    Gtk.boxPackStart box debugShowLabel True True 0
    Gtk.boxPackStart box editStateLabel True True 0
    Gtk.boxPackStart box image True True 0

    -- button <- new Gtk.Button [ #label := "Click me" ]
    -- _ <- on button #clicked (set button [ 
    --   #sensitive := False,
    --   #label := "Thanks for clicking me" 
    --   ])

    let updateUI = do
          Gtk.labelSetText editStateLabel =<< (pack . show . view editState <$> readIORef ref)
          sendToLilypond ref
          Gtk.imageSetFromFile image (Just "out.png")

    liftIO updateUI

    _ <- on win #keyReleaseEvent $ \ev ->
      do v <- get ev #keyval
         keyName <- keyvalName v
         unless (elem keyName . fmap Just $ ["Meta_L", "Shift_R"]) $
          Gtk.labelSetText debugShowLabel $ fromMaybe "nothing" keyName
         case keyName of
          Just "Up" -> modifyIORef ref (notes . _Just . focus . hand %~ swapHand)
          Just "Down" -> modifyIORef ref (notes . _Just . focus . hand %~ swapHand)

          Just "Left" -> modifyIORef ref (notes %~ opOr left)
          Just "Right" -> modifyIORef ref (notes %~ opOr right)

          Just "3" -> modifyIORef ref makeTriplet
          _ -> pure ()

         case w2c v of
           'n' -> modifyIORef ref insertNote
           's' -> modifyIORef ref splitNote
           'x' -> modifyIORef ref deleteNote
           '.' -> modifyIORef ref toggleDotCut

           -- Change the edit state
           -- SHIFT changes the edit state
           '_' -> modifyIORef ref (editState . EditState.duration %~ decreaseDVal)
           '+' -> modifyIORef ref (editState . EditState.duration %~ increaseDVal)
           '>' -> modifyIORef ref (editState . EditState.duration %~ Elem.toggleDotted)

           -- Movement
           'h' -> modifyIORef ref (notes %~ opOr left)
           'l' -> modifyIORef ref (notes %~ opOr right)

           -- Change the current note
           '-' -> modifyIORef ref (notes . _Just . focus . Elem.duration %~ decreaseDVal)
           '=' -> modifyIORef ref (notes . _Just . focus . Elem.duration %~ increaseDVal)
          --  '.' -> modifyIORef ref (notes . focus . _Just . Elem.duration %~ Elem.toggleDotted)

           _ -> print ()
         updateUI
         pure False

    #showAll win

    Gtk.main

opOr :: (t -> Maybe t) -> t -> t
opOr f v = fromMaybe v (f v)

sendToLilypond :: IORef Score -> IO ()
sendToLilypond ref = do
  print =<< getCurrentTime
  input <- readIORef ref <&> renderScore
  (_ec, _stdout, _stderr) <- readProcessWithExitCode "lilypond"
    ["--png", "-o", "out", "-"] input
  Prelude.putStrLn _stderr
  print =<< getCurrentTime
  return ()

debugPrint :: RIO App ()
debugPrint = do
  sc <- view scoreRef >>= readIORef
  let r = renderScore sc
      p = show sc
  liftIO $ Prelude.putStrLn r
  liftIO $ Prelude.putStrLn p


w2c :: Word32 -> Char
{-# INLINE w2c #-}
w2c (W32# w) = C#(chr#(word2Int# w))