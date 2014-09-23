{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module InternalCommands (getInternalCommand) where

import Control.Applicative
import Control.Monad.IO.Class

import System.Posix.Directory.ByteString
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import System.Posix.Directory.ByteString (getWorkingDirectory)
import System.Posix.User 

import Types

getInternalCommand :: ByteString -> Maybe ([ByteString] -> Shell SuccessState)
getInternalCommand c = lookup c commands

commands = [("cd", changeDir)]

changeDir args = case args of
  []    -> do home <- B.pack . homeDirectory <$>
                liftIO (getRealUserID >>= getUserEntryForID)
              changeDir [home]
  [tgt] -> do liftIO $ changeWorkingDirectory tgt
              return $ InternalResult True ""
  _ -> error "cd: Syntax error"
