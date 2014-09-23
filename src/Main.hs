{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative

import System.IO
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import System.Posix.Directory.ByteString (getWorkingDirectory)
import System.Posix.User 
import System.IO.Error

import Shell
import Command
import Types
import Parser

prompt :: Shell ByteString
prompt = do
    cwd    <- liftIO getWorkingDirectory
    uentry <- liftIO (getRealUserID >>= getUserEntryForID)
    let home  = homeDirectory uentry
    let uname = userName uentry
    return $ B.concat [B.pack uname, ":", showDir (B.pack home) cwd, "$ "]
  where showDir home cwd = if home `B.isPrefixOf` cwd
                           then "~" `B.append` (B.drop (B.length home) cwd)
                           else cwd

maybeGetLine = do
 catchIOError (Just <$> B.getLine) $ \err ->
    if isEOFError err
    then return Nothing
    else print err >> return Nothing

readAndProcessCommand = do
  l <- liftIO maybeGetLine
  case l of
    Nothing -> return False
    Just l' -> 
        if B.empty == l'
        then return True
        else do
          let c = parseBS (B.append l' "\n")
          run c
{-          case parse command "stdin" l' of
            Left err -> liftIO $ print err
            Right c -> run c >> return () -}
          return True

mainLoop :: Shell ()
mainLoop = do
  prompt >>= liftIO . B.putStr
  continue <- readAndProcessCommand
  when continue mainLoop
  
main = runShell mainLoop
