{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module System.Handsy where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy           as B
import           System.Exit

import           Control.Monad.Free.TH
import           Control.Monad.Trans.Free
import           System.Process.ByteString.Lazy

-- * Types
data HandsyF k =
    Command      FilePath [String] B.ByteString ((ExitCode, B.ByteString, B.ByteString) -> k)
  | ReadFile     FilePath                       (B.ByteString -> k)
  | WriteFile    FilePath B.ByteString          (() -> k)
  | AppendFile   FilePath B.ByteString          (() -> k)
  deriving (Functor)

type Handsy = FreeT HandsyF IO

-- * TH generated actions

makeFree ''HandsyF

-- * Helpers

{-| Executes the given string in shell

  > shell cmd stdin = command "/usr/bin/env" ["sh", "-c", cmd] stdin
-}
shell :: String       -- ^ String to execute
      -> B.ByteString -- ^ Standart input
      -> Handsy (ExitCode, B.ByteString, B.ByteString) -- ^ (ExitCode, Stdout, Stderr)
shell cmd stdin = command "/usr/bin/env" ["sh", "-c", cmd] stdin

-- * Interpreter

-- | Executes the actions locally
run :: Handsy a -> IO a
run h = do
  x <- runFreeT h
  case x of
    Pure r -> return r
    Free (ReadFile fp next)
      -> B.readFile fp >>= run . next
    Free (WriteFile fp str next)
      -> B.writeFile fp str >>= run . next
    Free (AppendFile fp str next)
      -> B.appendFile fp str >>= run . next
    Free (Command prg args stdin next)
      -> readProcessWithExitCode prg args stdin >>= run . next

