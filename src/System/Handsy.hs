{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module System.Handsy where

import           Prelude                        hiding (readFile, writeFile)

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy           as B
import           System.Exit

import           Control.Monad.Free.TH
import           Control.Monad.Trans.Free
import           System.Process.ByteString.Lazy

-- | Base functor for our dsl
data HandsyF k = Command      String [String] B.ByteString ((ExitCode, B.ByteString, B.ByteString) -> k)
               | ReadFile     String                       (B.ByteString -> k)
               | WriteFile    String B.ByteString          (() -> k)
             deriving (Functor)

type Handsy = FreeT HandsyF IO

makeFree ''HandsyF

run :: Handsy a -> IO a
run h = do
  x <- runFreeT h
  case x of
    Pure r -> return r
    Free (ReadFile fp next)
      -> B.readFile fp >>= run . next
    Free (WriteFile fp str next)
      -> B.writeFile fp str >>= run . next
    Free (Command prg args stdin next)
      -> readProcessWithExitCode prg args stdin >>= run . next

shell :: String -> Handsy (ExitCode, B.ByteString, B.ByteString)
shell cmd = command "/usr/bin/env" ["sh", "-c", cmd] ""
