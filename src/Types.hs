{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}
module Types where

import Data.Text (Text)
import System.Exit
import Control.Monad.IO.Class
import Control.Applicative
import System.Posix.Process.ByteString
import Data.ByteString.Char8 (ByteString)

data Command = Command Exp [Exp]
             deriving (Read, Show, Eq)

data ExtCommand = ExtCommand ByteString [ByteString]
             deriving (Read, Show, Eq)

data Exp = StrExp Text
         | ConcatExp [Exp]
         | QuoteExp Exp
         | ParenExp Exp
         | BraceExp Exp
         | BracketExp Exp
         | BackTickExp Exp
         | DollarExp Exp
         deriving (Read, Show, Eq)

data SuccessState = CommandResult ProcessStatus
                  | InternalResult Bool String
                  deriving (Show, Eq)

newtype Shell a = Sh (IO a)
                deriving (Monad, MonadIO, Functor, Applicative)
                         
runShell :: Shell a -> IO a
runShell (Sh f) = liftIO f
