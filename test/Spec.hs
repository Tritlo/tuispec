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
                    launch tui (app "sh" [])
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
                            launch tui (app "sh" [])
                            sendLine tui "printf 'hello from repl session\\n'"
                            snapshotPath <- dumpView tui "repl-view"
                            sendLine tui "exit"
                            pure snapshotPath
                exists <- doesFileExist snapshotPath
                assertBool "dumpView should write an ansi snapshot file" exists
            , testCase "smoke: launch supports env overrides" $
                withTuiSession
                    defaultRunOptions
                        { timeoutSeconds = 8
                        , artifactsDir = "artifacts/repl-env-smoke"
                        }
                    "env-session"
                    ( \tui -> do
                        launch
                            tui
                            App
                                { command = "sh"
                                , args = []
                                , env = Just [("TUISPEC_TEST_ENV", "hello-env")]
                                }
                        sendLine tui "printf '%s\\n' \"$TUISPEC_TEST_ENV\""
                        waitForText tui (Exact "hello-env")
                        sendLine tui "exit"
                    )
            , testCase "smoke: waitForText ignores ANSI escape sequences" $
                withTuiSession
                    defaultRunOptions
                        { timeoutSeconds = 8
                        , artifactsDir = "artifacts/repl-ansi-smoke"
                        , ambiguityMode = FirstVisibleMatch
                        }
                    "ansi-session"
                    ( \tui -> do
                        launch tui (app "sh" [])
                        sendLine tui "printf '\\033[31mstyled text\\033[0m\\n'"
                        waitForText tui (Exact "styled text")
                        expectNotVisible tui (Exact "\ESC[31mstyled text\ESC[0m")
                        sendLine tui "exit"
                    )
            ]
