{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GitLab.Project where

import Data.Conduit
import Network.HTTP.Conduit

import GitLab.Rest (restSource)
import GitLab.Types

listProjects
  :: (MonadBaseControl IO m, MonadResource m)
  => Source (GitLabT m) Project
listProjects = restSource $ \request -> request
  { path = "/projects"
  }
