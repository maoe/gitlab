{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GitLab.Group where
import Data.Monoid ((<>), mconcat)
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
getGroup grpId = rest $ \request -> request
  { method = "GET"
  , path = TE.encodeUtf8 $ "/groups/" <> toPathPiece grpId
  }

listGroupMembers
  :: (MonadBaseControl IO m, MonadResource m)
  => GroupId
  -> Source (GitLabT m) GroupMember
listGroupMembers grpId = restSource $ \request -> request
  { method = "GET"
  , path = TE.encodeUtf8 $ mconcat
      [ "/groups/"
      , toPathPiece grpId
      , "/members"
      ]
  }
