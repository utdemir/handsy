{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module System.Handsy.Internal
  ( Handsy
  , interpret
  , interpretSimple
  , command
  ) where

import           Control.Exception        (bracket)
import           Control.Monad.Free.TH
import           Control.Monad.Trans.Free
import qualified Data.ByteString.Lazy     as B
import           System.Exit

data HandsyF k =
    Command      FilePath [String] B.ByteString ((ExitCode, B.ByteString, B.ByteString) -> k)
  deriving (Functor)

makeFree ''HandsyF

type Handsy = FreeT HandsyF IO

interpret :: IO r         -- ^ Acquire resource
          -> (r -> IO ()) -- ^ Release resource
          -> (r -> String -> [String] -> B.ByteString
              -> IO (ExitCode, B.ByteString, B.ByteString)) -- ^ 'readProcessWithExitCode' + resource
          -> Handsy a
          -> IO a
interpret acquire destroy f handsy = bracket acquire destroy (flip go handsy)
  where go res h = do
          x <- runFreeT h
          case x of
            Pure r -> return r
            Free (Command prg args stdin next)
              -> f res prg args stdin >>= (go res) . next

interpretSimple :: (String -> [String] -> B.ByteString
                -> IO (ExitCode, B.ByteString, B.ByteString)) -- ^ 'readProcessWithExitCode'
                -> Handsy a
                -> IO a
interpretSimple f = interpret (return ()) (const (return ())) (const f)
