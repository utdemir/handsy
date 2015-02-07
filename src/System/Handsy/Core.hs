{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module System.Handsy.Core
  ( Handsy
  , interpret
  , interpretSimple
  , command
  , Options
  , options
  , debug
  ) where

import           Control.Exception        (bracket)
import           Control.Monad
import           Control.Monad.Free.TH
import           Control.Monad.Trans.Free
import qualified Data.ByteString.Lazy     as B
import           System.Exit
import           System.IO

data HandsyF k =
    Command      FilePath [String] B.ByteString ((ExitCode, B.ByteString, B.ByteString) -> k)
  deriving (Functor)

makeFree ''HandsyF

type Handsy = FreeT HandsyF IO

data Options =
  Options { debug :: Bool -- ^ Log commands to stderr before running
          }

options :: Options
options = Options True

interpret :: IO r         -- ^ Acquire resource
          -> (r -> IO ()) -- ^ Release resource
          -> (r -> String -> [String] -> B.ByteString
              -> IO (ExitCode, B.ByteString, B.ByteString)) -- ^ 'readProcessWithExitCode' + resource
          -> Options
          -> Handsy a
          -> IO a
interpret acquire destroy f opts handsy = bracket acquire destroy (flip go handsy)
  where go res h = do
          x <- runFreeT h
          case x of
            Pure r -> return r
            Free (Command prg args stdin next)
              -> when (debug opts) (hPutStrLn stderr $ prg ++ ' ' : show args)
              >> f res prg args stdin >>= (go res) . next

interpretSimple :: (String -> [String] -> B.ByteString
                -> IO (ExitCode, B.ByteString, B.ByteString)) -- ^ 'readProcessWithExitCode'
                -> Options
                -> Handsy a
                -> IO a
interpretSimple f = interpret (return ()) (const (return ())) (const f)
