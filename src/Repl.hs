{-# language NoImplicitPrelude, OverloadedStrings, TemplateHaskell  #-}
module Repl where

import Control.Monad.IO.Class
import Data.List (isPrefixOf)
import System.Console.Repline
import System.Process (callCommand, readProcessWithExitCode)
import RIO
import RIO.Orphans ()
import Control.Lens.TH
import qualified Prelude
import Score
import Score.Render
import Note

type Repl a = HaskelineT (RIO App) a

data App = App {
 _logfunc :: !LogFunc,
 _scoreRef :: IORef Score
    }

makeLenses ''App

instance HasLogFunc App where logFuncL = logfunc

repl :: IO ()
repl = do
 runSimpleApp $ do
    lf <- view logFuncL
    ref <- newIORef (score Metadata [])
    runRIO (App lf ref) $ do
        evalRepl  
          (const . pure $ ">>> ")
          cmd
          mempty
          (Just ':')
          (Just "paste")
          (Word completer)
          ini
          final
 pure ()

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = lift $ case words input of 
 ["show"] -> sendToLilypond
 ["print"] -> debugPrint
 ["note", x] -> do
   r <- view scoreRef
   modifyIORef r $ insertNote (Note x)
   sendToLilypond
   pure ()
 _ -> logInfo "i dunno"

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = ["kirk", "spock", "mccoy"]
  return $ filter (isPrefixOf n) names

ini :: Repl ()
ini = logInfo "Welcome!"

final :: Repl ExitDecision
final = do
  logInfo "Goodbye!"
  return Exit

sendToLilypond :: RIO App ()
sendToLilypond = do 
  input <- view scoreRef >>= readIORef <&> renderScore
  _ <- liftIO $ readProcessWithExitCode "lilypond" ["--png", "-o", "out", "-"] input
  _ <- liftIO $ callCommand "open -a \"Safari\" -g out.png"
  return ()

debugPrint :: RIO App ()
debugPrint = do
  sc <- view scoreRef >>= readIORef
  let r = renderScore sc
      p = show sc
  liftIO $ Prelude.putStrLn r
  liftIO $ Prelude.putStrLn p
