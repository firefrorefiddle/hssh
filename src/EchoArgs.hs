module Main where

import System.Environment (getArgs, getProgName)

main = do
  p <- getProgName
  a <- getArgs
  putStrLn p
  mapM_ putStrLn a
