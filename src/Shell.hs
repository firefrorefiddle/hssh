{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Shell where

import System.Posix.Types
import System.Posix.ByteString.FilePath
import System.Posix.Process.ByteString
import System.Posix.Directory.ByteString
import System.Posix.IO
import System.IO
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Control.Applicative
import Control.Monad.IO.Class

import ShellParser
import InternalCommands
import Command
import Types
import Run

data ShellState = ShSt
                  { shStCwd :: RawFilePath
                  } deriving (Read, Show, Eq)
  
