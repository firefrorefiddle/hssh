module Run where

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


executeInFork :: RawFilePath -> Bool -> [ByteString] ->
                 Maybe [(ByteString, ByteString)] ->
                 Maybe (Fd, Fd) -> -- stdin
                 Maybe (Fd, Fd) -> -- stdout
                 IO ProcessID
executeInFork file searchPath args env stdin stdout = do  
  forkProcess $ do
    case stdin of
      Nothing -> return ()
      Just (readfd, writefd) -> do dupTo readfd stdInput
                                   closeFd writefd
    case stdout of
      Nothing -> return ()
      Just (readfd, writefd) -> do dupTo writefd stdOutput
                                   closeFd readfd      
    executeFile file searchPath args env

executeForkWithInputOutput ::
  RawFilePath -> Bool -> [ByteString] ->
  Maybe [(ByteString, ByteString)] ->
  ByteString ->
  IO (ProcessStatus, ByteString)
executeForkWithInputOutput file searchPath args env input = do
  p1@(cin, pout) <- createPipe
  p2@(pin, cout) <- createPipe
  pid <- executeInFork file searchPath args env (Just p1) (Just p2)
  closeFd cin
  closeFd cout
  hout <- fdToHandle pout
  hin  <- fdToHandle pin
  B.hPut hout input
  hClose hout
  res <- B.hGetContents hin
  Just stat <- getProcessStatus True True pid
  return (stat, res)

collectProcess wait = getProcessStatus wait True
