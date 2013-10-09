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
listProjectSnippets projectId = restSource $ \request -> request
  { path = TE.encodeUtf8 $ "/projects/" <> toPathPiece projectId <> "/snippets"
  }

getProjectSnippet
  :: (MonadBaseControl IO m, MonadResource m)
  => ProjectId
  -> SnippetId
  -> GitLabT m (Maybe Snippet)
getProjectSnippet projectId snippetId = rest $ \request -> request
  { path = TE.encodeUtf8 $ mconcat
      [ "/projects/"
      , toPathPiece projectId
      , "/snippets/"
      , toPathPiece snippetId
      ]
  }
