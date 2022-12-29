module Main (main) where

import CopyCheck.ByteString.App (copyCheck)

initialCounter :: Int
initialCounter = 1

main :: IO ()
main = copyCheck initialCounter
