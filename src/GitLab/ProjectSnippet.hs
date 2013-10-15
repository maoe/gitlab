{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GitLab.ProjectSnippet where
import Data.Monoid ((<>), mconcat)
import qualified Data.Text.Encoding as TE

import Data.Conduit
import Network.HTTP.Conduit
import Web.PathPieces (toPathPiece)

import GitLab.Rest (rest, restSource)
import GitLab.Types

listProjectSnippets
  :: (MonadBaseControl IO m, MonadResource m)
  => ProjectId
  -> Source (GitLabT m) Snippet
listProjectSnippets projId = restSource $ \request -> request
  { path = TE.encodeUtf8 $ "/projects/" <> toPathPiece projId <> "/snippets"
  }

getProjectSnippet
  :: (MonadBaseControl IO m, MonadResource m)
  => ProjectId
  -> SnippetId
  -> GitLabT m (Maybe Snippet)
getProjectSnippet projId snipId = rest $ \request -> request
  { path = TE.encodeUtf8 $ mconcat
      [ "/projects/"
      , toPathPiece projId
      , "/snippets/"
      , toPathPiece snipId
      ]
  }
