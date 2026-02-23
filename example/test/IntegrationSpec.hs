{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty (defaultMain, testGroup)
import TuiSpec

main :: IO ()
main =
    defaultMain $
        testGroup
            "example-integration"
            [ tuiTest options "brick demo main flow" $ \tui -> do
                launch tui (App "sh" ["-lc", demoLaunchCommand])
                waitForText tui (Exact "Agent Readiness")
                waitForText tui (Exact "Press / to open command mode")
                expectSnapshot tui "initial"

                press tui (CharKey 'b')
                waitForText tui (Exact "Board Tasks (j/k move, space toggle)")
                press tui (CharKey 'j')
                press tui (CharKey ' ')
                waitForText tui (Exact "[x] Exercise split toggle")
                press tui (CharKey '+')
                expectSnapshot tui "board-interaction"

                press tui (CharKey 'l')
                waitForText tui (Exact "Recent Events")
                expectSnapshot tui "logs"
                press tui (CharKey 'q')
            , tuiTest options "brick demo resets between tests" $ \tui -> do
                launch tui (App "sh" ["-lc", demoLaunchCommand])
                waitForText tui (Exact "Agent Readiness")
                press tui (CharKey 'b')
                waitForText tui (Exact "Board Tasks (j/k move, space toggle)")
                waitForText tui (Exact "[ ] Write selectors for panes")
                expectSnapshot tui "reset-isolation"
                press tui (CharKey 'q')
            ]

options :: RunOptions
options =
    defaultRunOptions
        { timeoutSeconds = 25
        , retries = 1
        , stepRetries = 1
        , ambiguityMode = FirstVisibleMatch
        , artifactsDir = "artifacts"
        }

demoLaunchCommand :: String
demoLaunchCommand = "tui-demo"
