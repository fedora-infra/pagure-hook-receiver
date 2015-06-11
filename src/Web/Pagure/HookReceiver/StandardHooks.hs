{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Web.Pagure.HookReceiver.StandardHooks
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc
--
-- Common hooks for pagure updates
----------------------------------------------------------------------------
module Web.Pagure.HookReceiver.StandardHooks where

import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.Text as T
import Shelly (chdir, fromText, mkdir_p, run_, shelly)
import System.Posix.Files

cloneRepo :: MonadIO m => String -> m ()
cloneRepo s = shelly $ do
  mkdir_p "clones"
  run_ "/usr/bin/git" ["clone"
                      ,"https://pagure.io/" <> T.pack s
                      ,"clones/" <> T.pack s
                      ]

-- | Clones the repo if necessary, using 'cloneRepo', then adds a github remote
-- and pushes to it (using @--all@ so that tags and branches get mirrored as
-- well).
githubMirror :: MonadIO m => String -> String -> String -> m ()
githubMirror gh key s = shelly $ do
  cloneRepo s
  liftIO $ do
    writeFile ("clones/" ++ s ++ ".key") key
    setFileMode ("clones/" ++ s ++ ".key") (unionFileModes ownerReadMode ownerWriteMode)
  chdir (fromText . T.pack $ "clones/" ++ s) $ do
    run_ "/usr/bin/git" ["remote", "add", "github", "git@github.com:" <> T.pack gh]
    run_ "/usr/bin/ssh-agent" ["bash", "-c", "ssh-add ../" <> T.pack s <> ".key; git push --all github"]
