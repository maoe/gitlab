{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GitLab.Group where
import Data.Conduit
import Network.HTTP.Conduit

import GitLab.Types
import GitLab.Rest (restSource)

listGroups
  :: (MonadBaseControl IO m, MonadResource m)
  => Source (GitLabT m) Group
listGroups = restSource $ \request -> request
  { path = "/groups"
  }
