{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Web.Pagure.HookReceiver.Post
-- Copyright : (C) 2015 Ricky Elrod
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc
--
-- The POST hook receiver for Pagure projects.
----------------------------------------------------------------------------
module Web.Pagure.HookReceiver.Post where

import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Text as T
import Shelly (rm_rf, fromText, shelly)
import Web.Scotty

runPagureListener :: Int -> M.Map String [String -> IO ()] -> IO ()
runPagureListener port projectMapping =
  scotty port $
    post "/:projectname" $ do
      name <- param "projectname"
      case M.lookup name projectMapping of
       Nothing -> text "Project not found."
       Just hooks -> do
         let hooks' = fmap (\f -> f name) hooks
         liftIO (sequence_ hooks')
         shelly $ rm_rf (fromText (T.pack ("clones/" ++ name)))
         text "OK."
