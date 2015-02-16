{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module System.Handsy.Internal
  ( Handsy
  , interpret
  , interpretSimple
  , shell
  , Options (..)
  ) where

import           Control.Exception        (bracket)
import           Control.Monad
import           Control.Monad.Free.TH
import           Control.Monad.Trans.Free
import qualified Data.ByteString.Lazy     as B
import           Data.Default.Class
import           System.Exit
import           System.IO                (hPutStrLn, stderr)

data HandsyF k =
    Shell FilePath B.ByteString ((ExitCode, B.ByteString, B.ByteString) -> k)
  deriving (Functor)

makeFree ''HandsyF

-- | Main monad
type Handsy = FreeT HandsyF IO

data Options =
  Options { debug :: Bool -- ^ Log commands to stderr before running
          }

instance Default Options where
  def = Options False

interpret :: IO r         -- ^ Acquire resource
          -> (r -> IO ()) -- ^ Release resource
          -> (r -> String -> B.ByteString
              -> IO (ExitCode, B.ByteString, B.ByteString))
          -> Options
          -> Handsy a
          -> IO a
interpret acquire destroy f opts handsy = bracket acquire destroy (`go` handsy)
  where go res h = do
          x <- runFreeT h
          case x of
            Pure r -> return r
            Free (Shell cmdline stdin next)
              -> when (debug opts) (hPutStrLn stderr cmdline)
              >> f res cmdline stdin >>= go res . next

interpretSimple :: (FilePath -> B.ByteString
                    -> IO (ExitCode, B.ByteString, B.ByteString)) -- ^ 'readProcessWithExitCode'
                -> Options
                -> Handsy a
                -> IO a
interpretSimple f = interpret (return ()) (const (return ())) (const f)
