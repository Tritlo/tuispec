{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory (doesFileExist)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
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
                    sendLine tui "printf 'hello from tuispec\\n'"
                    waitForText tui (Exact "hello from tuispec")
                    expectNotVisible tui (Exact "this text should not exist")
                    expectSnapshot tui "smoke-viewport"
                    sendLine tui "exit"
            , testCase "smoke: repl session can dump view" $ do
                snapshotPath <-
                    withTuiSession
                        defaultRunOptions
                            { timeoutSeconds = 8
                            , artifactsDir = "artifacts/repl-smoke"
                            }
                        "session"
                        $ \tui -> do
                            launch tui (App "sh" [])
                            sendLine tui "printf 'hello from repl session\\n'"
                            snapshotPath <- dumpView tui "repl-view"
                            sendLine tui "exit"
                            pure snapshotPath
                exists <- doesFileExist snapshotPath
                assertBool "dumpView should write an ansi snapshot file" exists
            ]
