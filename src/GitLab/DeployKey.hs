{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GitLab.DeployKey where
import Data.Monoid (mconcat)
import qualified Data.Text.Encoding as TE

import Data.Conduit
import Network.HTTP.Conduit
import Web.PathPieces (toPathPiece)

import GitLab.Rest (restSource)
import GitLab.Types

listDeployKeys
  :: (MonadBaseControl IO m, MonadResource m)
  => ProjectId
  -> Source (GitLabT m) DeployKey
listDeployKeys projectId = restSource $ \request -> request
  { path = TE.encodeUtf8 $ mconcat
      [ "/projects/"
      , toPathPiece projectId
      , "/keys"
      ]
  }
