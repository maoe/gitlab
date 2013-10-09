{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GitLab.Project where
import Data.Monoid ((<>), mconcat)
import qualified Data.Text.Encoding as TE

import Data.Conduit
import Network.HTTP.Conduit
import Web.PathPieces (toPathPiece)

import GitLab.Rest (rest, restSource)
import GitLab.Types

listProjects
  :: (MonadBaseControl IO m, MonadResource m)
  => Source (GitLabT m) Project
listProjects = restSource $ \request -> request
  { path = "/projects"
  }

getProject
  :: (MonadBaseControl IO m, MonadResource m)
  => ProjectId
  -> GitLabT m (Maybe Project)
getProject projectId = rest $ \request -> request
  { path = TE.encodeUtf8 $ "/projects/" <> toPathPiece projectId
  }

listProjectMembers
  :: (MonadBaseControl IO m, MonadResource m)
  => ProjectId
  -> Source (GitLabT m) ProjectMember
listProjectMembers projectId = restSource $ \request -> request
  { path = TE.encodeUtf8 $ mconcat
      [ "/projects/"
      , toPathPiece projectId
      , "/members"
      ]
  }

listProjectHooks
  :: (MonadBaseControl IO m, MonadResource m)
  => ProjectId
  -> Source (GitLabT m) ProjectHook
listProjectHooks projectId = restSource $ \request -> request
  { path = TE.encodeUtf8 $ mconcat
      [ "/projects/"
      , toPathPiece projectId
      , "/hooks"
      ]
  }
