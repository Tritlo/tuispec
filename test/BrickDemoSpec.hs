{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit (exitWith)
import TuiSpec

main :: IO ()
main = do
  code <-
    runSuite
      defaultRunOptions
        { timeoutSeconds = 25
        , retries = 1
        , stepRetries = 1
        , ambiguityMode = FirstVisibleMatch
        }
      [ test "brick showcase main flow" $ \tui -> do
          launch tui (App "sh" ["-lc", "cd tests/tui && CABAL_DIR=$PWD/.cabal cabal run tui-demo"])
          waitForText tui (Exact "STATE page=Counter theme=Ocean help=off counter=0")
          expectVisible tui (Exact "STATE task0=[ ] smoke assertions")
          expectSnapshot tui "initial-screen"

          press tui (CharKey '+')
          press tui (CharKey '+')
          waitForText tui (Exact "STATE page=Counter theme=Ocean help=off counter=2")
          expectSnapshot tui "counter-updated"

          press tui (CharKey 'n')
          waitForText tui (Exact "STATE page=Checklist theme=Ocean help=off counter=2")
          expectVisible tui (Exact "STATE cursor=0")

          press tui (CharKey ' ')
          waitForText tui (Exact "STATE task0=[x] smoke assertions")
          press tui (CharKey 'j')
          press tui (CharKey ' ')
          waitForText tui (Exact "STATE cursor=1")
          waitForText tui (Exact "STATE task1=[x] keyboard flows")
          expectSnapshot tui "checklist-progress"

          press tui (CharKey 't')
          waitForText tui (Exact "STATE page=Checklist theme=Amber help=off counter=2")
          press tui (CharKey 'h')
          waitForText tui (Exact "STATE page=Checklist theme=Amber help=on counter=2")
          expectSnapshot tui "theme-and-help"
          press tui (CharKey 'q')
      , test "brick showcase test isolation reset" $ \tui -> do
          launch tui (App "sh" ["-lc", "cd tests/tui && CABAL_DIR=$PWD/.cabal cabal run tui-demo"])
          waitForText tui (Exact "STATE page=Counter theme=Ocean help=off counter=0")
          press tui (CharKey '+')
          press tui (CharKey '+')
          press tui (CharKey '+')
          waitForText tui (Exact "STATE page=Counter theme=Ocean help=off counter=3")
          press tui (CharKey 'r')
          waitForText tui (Exact "STATE page=Counter theme=Ocean help=off counter=0")
          expectSnapshot tui "reset-isolation"
          press tui (CharKey 'q')
      ]
  exitWith code
