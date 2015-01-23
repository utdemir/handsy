{-# LANGUAGE OverloadedStrings #-}
module System.Handsy.Remote where

import           System.Handsy            as H

import qualified Data.ByteString.Lazy     as B

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Free

data RemoteOptions =
  RemoteOptions {
    sshCommand :: (FilePath, [String])
  }

-- | Executes the actions at a remote host
runRemote :: RemoteOptions -> Handsy a -> IO a
runRemote opts h = do
  x <- runFreeT h
  case x of
    Pure r -> return r
    Free (ReadFile fp next)
      -> runSsh "cat" [fp] "" >>= \(_, stdin, _) -> runRemote opts (next stdin)
    Free (WriteFile fp str next)
      -> runSsh "dd" ["of=" ++ fp] str >> runRemote opts (next ())
    Free (AppendFile fp str next)
      -> runSsh "dd" ["of=" ++ fp, "conv=notrunc", "oflag=append"] str >> runRemote opts (next ())
    Free (Command prg args stdin next)
      -> runSsh prg args stdin >>= runRemote opts . next

  where
    (ssh, sshOpts) = sshCommand opts
    runSsh prg args stdin = run (command ssh (sshOpts ++ prg : args) stdin)

pushFile :: FilePath -- ^ Local path of source
         -> FilePath -- ^ Remote path of destination
         -> Handsy ()
pushFile local remote = liftIO (B.readFile local) >>= H.writeFile remote

pullFile :: FilePath -- ^ Remote path of source
         -> FilePath -- ^ Local path of destination
         -> Handsy ()
pullFile remote local = H.readFile remote >>= liftIO . B.writeFile local
