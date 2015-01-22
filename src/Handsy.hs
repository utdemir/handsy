{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handsy where

import           Prelude                hiding (readFile, writeFile)

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8  as B
import           System.Exit
import           System.Process

import           Control.Monad.Free
import           Control.Monad.Free.TH

-- | Base functor for our dsl
data HandsyF k = Command      String [String] B.ByteString ((ExitCode, B.ByteString, B.ByteString) -> k)
               | ReadFile     String                       (B.ByteString -> k)
               | WriteFile    String B.ByteString          (() -> k)
             deriving (Functor)

type Handsy = Free HandsyF

makeFree ''HandsyF

run :: MonadIO m => Handsy a -> m a
run = iterM go
  where
    go :: (MonadIO m) => HandsyF (m a) -> m a
    go (Command prg args stdin next) = do
      -- TODO: Don't use this function --v and use B.hGetContents
      (c, i, e) <- liftIO $ readProcessWithExitCode prg args (B.unpack stdin)
      next (c, B.pack i, B.pack e)
    go (ReadFile fp next) = (liftIO $ B.readFile fp) >>= next
    go (WriteFile fp str next) = (liftIO $ B.writeFile fp str) >>= next

copyFile :: FilePath -> FilePath -> Handsy ()
copyFile src dest = void $ command "cp" [src, dest] ""

-- Sample Program
test :: Handsy B.ByteString
test = do
  (_, uptime, _) <- command "uptime" [] ""
  writeFile "/tmp/uptime" uptime
  readFile "/tmp/uptime"

-- Sample Interpretation
main :: IO ()
main = run test >>= print


