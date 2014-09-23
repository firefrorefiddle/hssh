{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}
module Types where

import Data.Text (Text)
import System.Exit
import Control.Monad.IO.Class
import Control.Applicative
import System.Posix.Process.ByteString

data Exp = StrExp Text
         | ConcatExp [Exp]
         | QuoteExp Exp
         | ParenExp Exp
         | BraceExp Exp
         | BracketExp Exp
         | DollarExp Exp
         deriving (Read, Show, Eq)

data SuccessState = CommandResult ProcessStatus
                  | InternalResult Bool String
                  deriving (Show, Eq)

newtype Shell a = Sh (IO a)
                deriving (Monad, MonadIO, Functor, Applicative)
                         
runShell :: Shell a -> IO a
runShell (Sh f) = liftIO f
