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

    box <- new Gtk.Box []
    #add win box

    image <- Gtk.imageNew

    Gtk.boxPackStart box image True True 0

    liftIO $ sendToLilypond ref

    Gtk.imageSetFromFile image (Just "out.png")

    _ <- on win #keyReleaseEvent $ \ev ->
      do v <- get ev #keyval
         case w2c v of
           'a' -> print 'a'
           'n' -> modifyIORef ref (insertNote "P4")
           'N' -> modifyIORef ref insertMeasure
           _ -> print ()
         sendToLilypond ref
         Gtk.imageSetFromFile image (Just "out.png")
         pure False

    #showAll win

    Gtk.main


sendToLilypond :: IORef Score -> IO ()
sendToLilypond ref = do 
  input <- readIORef ref <&> renderScore
  _ <- readProcessWithExitCode "lilypond" ["--png", "-o", "out", "-"] input
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