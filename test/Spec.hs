{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit (exitWith)
import TuiSpec

main :: IO ()
main = do
  code <-
    runSuite
      defaultRunOptions
      [ test "smoke: text appears in viewport" $ \tui -> do
          launch tui (App "echo" ["hello"])
          typeText tui "hello from tuispec"
          expectVisible tui (Exact "hello from tuispec")
          expectNotVisible tui (Exact "this text should not exist")
          expectSnapshot tui "smoke-viewport"
      ]
  exitWith code
