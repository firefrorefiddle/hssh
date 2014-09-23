{-# LANGUAGE
    FlexibleInstances,
    OverloadedStrings #-}
module Command where

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

import Types
import Run
import InternalCommands
import ShellParser

class Runnable r where
  run :: r -> Shell SuccessState
  
instance Runnable ExtCommand where
  run (ExtCommand cmd args) = do
    pid <- liftIO $ executeInFork cmd True args Nothing Nothing Nothing
    Just res <- liftIO $ collectProcess True pid
    return $ CommandResult res

instance Runnable Command where
  run c = do
    (cmd, args) <- commandToArgv c
    case getInternalCommand cmd of
      Just cmd' -> run ((cmd' args) `asTypeOf` (return undefined))
      Nothing   -> run (ExtCommand cmd args)

instance Runnable SuccessState where
  run = return
  
instance Runnable (Shell SuccessState) where
  run f = f

commandToArgv :: Command -> Shell (ByteString, [ByteString])
commandToArgv (Command prog args) = (,) <$> expToArgv prog <*> mapM expToArgv args

expToArgv e = case e of
  StrExp t      -> return $ T.encodeUtf8 t
  ConcatExp es  -> B.concat <$> mapM expToArgv es
  QuoteExp e    -> expToArgv e
  BackTickExp e -> do cmd <- expToArgv e
                      case parse command (B.unpack cmd) cmd of
                        Left err -> liftIO $ print err >> return ""
                        Right c -> do
                          (cmd', args) <- commandToArgv c
                          (_, out) <- liftIO $ executeInForkAndRead cmd' True args Nothing
                          return out
  ParenExp e    -> expToArgv e -- unimplemented
  BraceExp e    -> expToArgv e -- unimplemented
  BracketExp e  -> expToArgv e -- unimplemented
  DollarExp e   -> expToArgv e -- unimplemented


  
