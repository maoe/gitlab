{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module GitLab.Rest
  ( rest
  , restSource

  , paginate
  , paginateBy
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
import Web.PathPieces (toPathPiece)
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
restSource f = loop 1
  where
    loop !page = do
      GitLabConfig {..} <- lift ask
      request <- lift . auth . modifyPath . f $ def
        { method = "GET"
        , secure = gitLabSecure
        , host = gitLabHost
        , port = gitLabPort
        , queryString =
            renderQuery False $ paginationQuery page gitLabPagination
        }
      response <- lift $ httpLbs request gitLabManager
      case A.decode' $ responseBody response of
        Nothing -> return ()
        Just entities -> do
          CL.sourceList entities
          let numEntities = length entities
          case gitLabPagination of
            NoPagination -> return ()
            PaginateBy n -> when (numEntities == n) $ loop $ page + 1

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

paginationQuery :: Int -> Pagination -> Query
paginationQuery page pagination = toQuery $ case pagination of
   NoPagination -> [] :: [(String, Text)]
   PaginateBy perPage ->
     [ ("page", toPathPiece (page `max` 1))
     , ("per_page", toPathPiece perPage)
     ]

paginate :: Monad m => GitLabT m a -> GitLabT m a
paginate = local $ \config -> config
  { gitLabPagination = defaultPagination
  }

paginateBy :: Monad m => Int -> GitLabT m a -> GitLabT m a
paginateBy perPage = local $ \config -> config
  { gitLabPagination = userPagination perPage
  }
