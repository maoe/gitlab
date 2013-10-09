{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GitLab.ProjectSnippet where
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE

import Data.Conduit
import Network.HTTP.Conduit
import Web.PathPieces (toPathPiece)

import GitLab.Rest (restSource)
import GitLab.Types

listSnippets
  :: (MonadBaseControl IO m, MonadResource m)
  => ProjectId
  -> Source (GitLabT m) Snippet
listSnippets projectId = restSource $ \request -> request
  { path = TE.encodeUtf8 $ "/projects/" <> toPathPiece projectId <> "/snippets"
  }
