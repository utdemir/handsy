{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module System.Handsy.Internal where

import           Control.Monad.Free
import           Control.Monad.Free.TH
import qualified Data.ByteString.Lazy  as B
import           System.Exit

data HandsyF k =
    Command      FilePath [String] B.ByteString ((ExitCode, B.ByteString, B.ByteString) -> k)
  | ReadFile     FilePath                       (B.ByteString -> k)
  | WriteFile    FilePath B.ByteString          (() -> k)
  | AppendFile   FilePath B.ByteString          (() -> k)
  deriving (Functor)

makeFree ''HandsyF
