{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GitLab.Note where
import Data.Monoid (mconcat)
import qualified Data.Text.Encoding as TE

import Data.Conduit
import Network.HTTP.Conduit
import Web.PathPieces (toPathPiece)

import GitLab.Rest (restSource)
import GitLab.Types

listNotes
  :: (MonadBaseControl IO m, MonadResource m)
  => ProjectId
  -> Source (GitLabT m) Note
listNotes projectId = restSource $ \request -> request
  { path = TE.encodeUtf8 $ mconcat
      [ "/projects/"
      , toPathPiece projectId
      , "/notes"
      ]
  }
