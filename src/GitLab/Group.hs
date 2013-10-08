{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GitLab.Group where
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as TE

import Data.Conduit
import Network.HTTP.Conduit
import Web.PathPieces (toPathPiece)

import GitLab.Types
import GitLab.Rest (rest, restSource)

listGroups
  :: (MonadBaseControl IO m, MonadResource m)
  => Source (GitLabT m) Group
listGroups = restSource $ \request -> request
  { path = "/groups"
  }

getGroup
  :: (MonadBaseControl IO m, MonadResource m)
  => GroupId
  -> GitLabT m (Maybe Group)
getGroup groupId = rest $ \request -> request
  { method = "GET"
  , path = TE.encodeUtf8 $ "/groups/" <> toPathPiece groupId
  }
