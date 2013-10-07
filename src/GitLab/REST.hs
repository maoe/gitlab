{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module GitLab.REST
  ( rest
  , restSource
  ) where

import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)

import Control.Monad.Base (MonadBase)
import Data.Aeson as A
import Data.Conduit
import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Data.Conduit.List as CL

import GitLab.Monad

rest
  :: (FromJSON a, MonadBaseControl IO m, MonadResource m)
  => (Request (GitLabT m) -> Request (GitLabT m))
  -> GitLabT m (Maybe a)
rest f = do
  GitLabConfig {..} <- ask
  request <- auth . modifyPath . f $ def
    { secure = gitLabSecure
    , host = gitLabHost
    , port = gitLabPort
    }
  response <- httpLbs request gitLabManager
  return $ A.decode $ responseBody response

restSource
  :: (FromJSON a, MonadBaseControl IO m, MonadResource m)
  => (Request (GitLabT m) -> Request (GitLabT m))
  -> Source (GitLabT m) a
restSource f = do
  GitLabConfig {..} <- lift ask
  request <- lift . auth . modifyPath . f $ def
    { method = "GET"
    , secure = gitLabSecure
    , host = gitLabHost
    , port = gitLabPort
    }
  response <- lift $ httpLbs request gitLabManager
  maybe CL.sourceNull CL.sourceList $ A.decode (responseBody response)

modifyPath :: Request m -> Request m
modifyPath request = request
  { path = "/api/v3" <> path request
  }

auth
  :: Monad m
  => Request (GitLabT m)
  -> GitLabT m (Request (GitLabT m))
auth request = do
  privToken <- asks (credsPrivateToken . gitLabCreds)
  let privateTokenHeader = ("PRIVATE-TOKEN", privToken)
  return request
    { requestHeaders = privateTokenHeader : requestHeaders request
    }
