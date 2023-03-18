module Main (main) where

import Data.Foldable

import Lib

main :: IO ()
main = do
  -- runLoggingTS (withSectionTS "section" doSomething)
  (_, w) <- runLoggingTS (logTS "hello" >> withSectionTS "section" doSomething)
  traverse_ print w
