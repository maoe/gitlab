{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GitLab.Issue where
import Data.Monoid (mconcat)
import qualified Data.Text.Encoding as TE

import Data.Conduit
import Network.HTTP.Conduit
import Web.PathPieces (toPathPiece)

import GitLab.Rest (restSource)
import GitLab.Types

listIssues
  :: (MonadBaseControl IO m, MonadResource m)
  => Source (GitLabT m) Issue
listIssues = restSource $ \request -> request
  { path = "/issues"
  }

listProjectIssues
  :: (MonadBaseControl IO m, MonadResource m)
  => ProjectId
  -> Source (GitLabT m) Issue
listProjectIssues projectId = restSource $ \request -> request
  { path = TE.encodeUtf8 $ mconcat
      [ "/projects/"
      , toPathPiece projectId
      , "/issues"
      ]
  }
