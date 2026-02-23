{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty (defaultMain, testGroup)
import TuiSpec

main :: IO ()
main =
    defaultMain $
        testGroup
            "tuispec"
            [ tuiTest
                defaultRunOptions
                    { timeoutSeconds = 8
                    , artifactsDir = "artifacts/smoke"
                    , ambiguityMode = FirstVisibleMatch
                    }
                "smoke: text appears in viewport"
                $ \tui -> do
                    launch tui (App "sh" [])
                    typeText tui "printf 'hello from tuispec\\n'"
                    press tui Enter
                    waitForText tui (Exact "hello from tuispec")
                    expectNotVisible tui (Exact "this text should not exist")
                    expectSnapshot tui "smoke-viewport"
                    typeText tui "exit"
                    press tui Enter
            ]
