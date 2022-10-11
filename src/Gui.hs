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
import Note
import GI.Gdk (keyvalName)
import RIO.Time

-- TODO: Undo stack

data App = App {
 _logfunc :: !LogFunc,
 _scoreRef :: IORef Score
    }

makeLenses ''App

instance HasLogFunc App where logFuncL = logfunc

gui :: IO ()
gui = runSimpleApp $ do
  lf <- view logFuncL
  ref <- newIORef (score [])
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
         let action a = modifyIORef ref (execThis a)
         case keyName of
          Just "Up" -> action (modifyFocusNote $ hand %~ swapHand)
          Just "Down" -> action (modifyFocusNote $ hand %~ swapHand)
          Just "Left" -> action moveLeft
          Just "Right" -> action moveRight
          Just "3" -> action makeTriplet
          Just "parenright" -> action (modifyFocusNote $ mods %~ toggleMod Roll)
          _ -> pure ()

         case w2c v of
           'n' -> action insertNote
           's' -> action splitNote
           'x' -> action deleteFocus -- TODO inconsistent
           '.' -> action toggleDotCut

           -- Change the edit state
           -- SHIFT changes the edit state
           '_' -> modifyIORef ref (editState . duration %~ halveDuration)
           '+' -> modifyIORef ref (editState . duration %~ doubleDuration)
           '>' -> modifyIORef ref (editState . duration %~ toggleDotted)

           -- Movement
           'h' -> modifyIORef ref (execThis moveLeft)
           'l' -> modifyIORef ref (execThis moveRight)

           -- Change the current note
           '-' -> action (modifyFocusNote $ duration %~ halveDuration)
           '=' -> action (modifyFocusNote $ duration %~ doubleDuration)
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
  Prelude.putStrLn "Rendering"
  input <- readIORef ref <&> renderScore
  (_ec, _stdout, _stderr) <- readProcessWithExitCode "lilypond"
    ["--png", "-o", "out", "-"] input
  case _ec of 
    ExitSuccess -> pure ()
    ExitFailure _ -> Prelude.putStrLn _stderr
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
