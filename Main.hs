{-# OPTIONS_GHC -Wall -Wno-missing-signatures #-}
{-# language FlexibleContexts #-}

module Main where

import Score
import System.Exit
import Score.Render
import System.Process


main :: IO ()
main = 
    sendToLilypond (phrasesToScore phrases_24)

sendToLilypond ref = do
  let input = renderScore ref
  putStrLn input
  (_ec, _stdout, _stderr) <- readProcessWithExitCode "lilypond"
    ["--png", "-o", "out", "-"] input
  case _ec of
    ExitSuccess -> pure ()
    ExitFailure _ -> Prelude.putStrLn _stderr
  return ()
