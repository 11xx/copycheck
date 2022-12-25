module Main (main) where

import CopyCheck.App (copyCheck)

initialCounter :: Int
initialCounter = 1

main :: IO ()
main = copyCheck initialCounter
