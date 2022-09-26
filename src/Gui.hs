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
import Elem (increaseDVal, decreaseDVal, duration, dval, toggleDotted, swapHand, hand)
import Data.ListZipper as LZ
import Control.Lens (_Just)
import GI.Gdk (keyvalName)


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
          Just "Up" -> modifyIORef ref (notes . focus . _Just . hand %~ swapHand)
          Just "Down" -> modifyIORef ref (notes . focus . _Just . hand %~ swapHand)

          Just "Left" -> modifyIORef ref (notes %~ execListZipperOpOr moveLeft)
          Just "Right" -> modifyIORef ref (notes %~ execListZipperOpOr moveRight)
          _ -> pure ()

         case w2c v of
           'n' -> modifyIORef ref insertNote
           's' -> modifyIORef ref splitNote
           'x' -> modifyIORef ref deleteNote
           '.' -> modifyIORef ref toggleDotCut

           -- Change the edit state
           -- SHIFT changes the edit state
           '_' -> modifyIORef ref (editState . EditState.duration . dval %~ decreaseDVal)
           '+' -> modifyIORef ref (editState . EditState.duration . dval %~ increaseDVal)
           '>' -> modifyIORef ref (editState . EditState.duration %~ Elem.toggleDotted)

           -- Movement
           'h' -> modifyIORef ref (notes %~ execListZipperOpOr moveLeft)
           'l' -> modifyIORef ref (notes %~ execListZipperOpOr moveRight)

           -- Change the current note
           '-' -> modifyIORef ref (notes . focus . _Just . Elem.duration . dval %~ decreaseDVal)
           '=' -> modifyIORef ref (notes . focus . _Just . Elem.duration . dval %~ increaseDVal)
          --  '.' -> modifyIORef ref (notes . focus . _Just . Elem.duration %~ Elem.toggleDotted)

           _ -> print ()
         updateUI
         pure False

    #showAll win

    Gtk.main


sendToLilypond :: IORef Score -> IO ()
sendToLilypond ref = do
  input <- readIORef ref <&> renderScore
  (_ec, _stdout, _stderr) <- readProcessWithExitCode "lilypond"
    ["--png", "-o", "out", "-"] input
  Prelude.putStrLn _stderr
  return ()

debugPrint :: RIO App ()
debugPrint = do
  sc <- view scoreRef >>= readIORef
  let r = renderScore sc
      p = show sc
  liftIO $ Prelude.putStrLn r
  liftIO $ Prelude.putStrLn p

    -- button <- new Gtk.Button [ #label := "Click me" ]
    -- _ <- on button #clicked (set button [ 
    --   #sensitive := False,
    --   #label := "Thanks for clicking me" 
    --   ])

w2c :: Word32 -> Char
{-# INLINE w2c #-}
w2c (W32# w) = C#(chr#(word2Int# w))