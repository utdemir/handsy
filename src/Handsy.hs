{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handsy where

import           Prelude                  hiding (readFile, writeFile)

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8    as B
import           System.Exit
import           System.Process

-- import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Control.Monad.Trans.Free

-- | Base functor for our dsl
data HandsyF k = Command      String [String] B.ByteString ((ExitCode, B.ByteString, B.ByteString) -> k)
               | ReadFile     String                       (B.ByteString -> k)
               | WriteFile    String B.ByteString          (() -> k)
             deriving (Functor)

makeFree ''HandsyF

type Handsy = FreeT HandsyF IO

run :: Handsy a -> IO a
run h = do
  x <- runFreeT h
  case x of
    Pure r -> return r
    Free (ReadFile fp next) -> B.readFile fp >>= run . next
    Free (WriteFile fp str next) -> B.writeFile fp str >>= run . next
    Free (Command prg args stdin next) -> do
      (c, i, e) <- readProcessWithExitCode prg args (B.unpack stdin)
      run (next (c, B.pack i, B.pack e))

test :: Handsy B.ByteString
test = do
  (_, hostname, _) <- command "hostname" [] ""
  liftIO $ B.writeFile "/tmp/hostname" hostname
  liftIO $ B.readFile "/tmp/hostname"

