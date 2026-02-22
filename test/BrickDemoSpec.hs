{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit (exitWith)
import TuiSpec

main :: IO ()
main = do
    code <-
        runSuite
            defaultRunOptions
                { timeoutSeconds = 12
                , retries = 1
                , stepRetries = 1
                , ambiguityMode = FirstVisibleMatch
                , artifactsDir = "artifacts/brick-showcase"
                , updateSnapshots = True
                }
            [ test "brick panes main flow" $ \tui -> do
                launch tui (App "sh" ["-lc", "cd tests/tui && CABAL_DIR=$PWD/.cabal cabal run tui-demo"])
                waitForText tui (Exact "focus=main")
                waitForText tui (Exact "split=vertical")
                waitForText tui (Exact "Current screen: dashboard")
                waitForText tui (Exact "Focused pane: main")
                expectSnapshot tui "panes-initial"

                press tui Tab
                waitForText tui (Exact "focus=inspect")
                press tui Tab
                waitForText tui (Exact "focus=nav")

                press tui (CharKey 'b')
                waitForText tui (Exact "Current screen: board")
                waitForText tui (Exact "Board Tasks (j/k move, space toggle)")
                press tui (CharKey 'j')
                press tui (CharKey ' ')
                waitForText tui (Exact "[x] Exercise split toggle")
                press tui (CharKey '+')
                waitForText tui (Exact "counter=1")
                expectSnapshot tui "board-interaction"

                press tui (CharKey 's')
                waitForText tui (Exact "split=horizontal")
                press tui (CharKey '/')
                waitForText tui (Exact ":_")
                typeText tui "theme amber"
                waitForText tui (Exact ":theme amber_")
                press tui Enter
                waitForText tui (Exact "theme=amber")
                expectSnapshot tui "split-and-command"

                press tui (CharKey 'l')
                waitForText tui (Exact "Current screen: logs")
                waitForText tui (Exact "Recent Events")
                expectSnapshot tui "logs-screen"
                press tui (CharKey '/')
                waitForText tui (Exact ":_")
                typeText tui "quit"
                waitForText tui (Exact ":quit_")
                press tui Enter
            , test "brick panes reset isolation" $ \tui -> do
                launch tui (App "sh" ["-lc", "cd tests/tui && CABAL_DIR=$PWD/.cabal cabal run tui-demo"])
                waitForText tui (Exact "counter=0")
                press tui (CharKey '+')
                press tui (CharKey '+')
                waitForText tui (Exact "counter=2")
                press tui (CharKey '/')
                typeText tui "reset"
                press tui Enter
                waitForText tui (Exact "counter=0")
                press tui (CharKey 'b')
                waitForText tui (Exact "Current screen: board")
                waitForText tui (Exact "[ ] Write selectors for panes")
                expectSnapshot tui "reset-isolation"
                press tui (CharKey '/')
                waitForText tui (Exact ":_")
                typeText tui "quit"
                waitForText tui (Exact ":quit_")
                press tui Enter
            ]
    exitWith code
