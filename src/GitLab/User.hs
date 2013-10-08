{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module GitLab.User where
import Data.Monoid ((<>))
import Data.Text (Text)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as TE

import Data.Aeson.TH
import Data.Conduit
import Network.HTTP.Conduit
import Web.PathPieces (toPathPiece)
import qualified Data.Aeson as A

import GitLab.Types
import GitLab.Rest (rest, restSource)
import GitLab.Util

data UserParams = UserParams
  { userParamsEmail :: Text
  , userParamsPassword :: Text
  , userParamsUsername :: Text
  , userParamsName :: Text
  , userParamsSkype :: Maybe Text
  , userParamsLinkedin :: Maybe Text
  , userParamsTwitter :: Maybe Text
  , userParamsProjectsLimit :: Maybe Int
  , userParamsExternalUid :: Maybe Text
  , userParamsProvider :: Maybe Text
  , userParamsBio :: Maybe Text
  }

listUsers
  :: (MonadBaseControl IO m, MonadResource m)
  => Source (GitLabT m) User
listUsers = restSource $ \request -> request
  { path = "/users"
  }

getUser
  :: (MonadBaseControl IO m, MonadResource m)
  => UserId
  -> GitLabT m (Maybe User)
getUser userId = rest $ \request -> request
  { method = "GET"
  , path = TE.encodeUtf8 $ "/users/" <> toPathPiece userId
  }

createUser
  :: (MonadBaseControl IO m, MonadResource m)
  => UserParams
  -> GitLabT m (Maybe User)
createUser params = rest $ \request -> request
  { method = "POST"
  , path = "/users"
  , requestBody = RequestBodyLBS $ A.encode params
  }

modifyUser
  :: (MonadBaseControl IO m, MonadResource m)
  => UserId
  -> UserParams
  -> GitLabT m (Maybe User)
modifyUser userId params = rest $ \request -> request
  { method = "PUT"
  , path = TE.encodeUtf8 $ "/users/" <> toPathPiece userId
  , requestBody = RequestBodyLBS $ A.encode params
  }

deleteUser
  :: (MonadBaseControl IO m, MonadResource m)
  => UserId
  -> GitLabT m (Maybe User)
deleteUser userId = rest $ \request -> request
  { method = "DELETE"
  , path = TE.encodeUtf8 $ "/users/" <> toPathPiece userId
  }

getCurrentUser
  :: (MonadBaseControl IO m, MonadResource m)
  => GitLabT m (Maybe CurrentUser)
getCurrentUser = rest $ \request -> request
  { method = "GET"
  , path = "/user"
  }

------------------------------------------------------------
-- SSH Keys

data SshKeyParams = SshKeyParams
  { keyParamsTitle :: Text
  , keyParamsKey :: Text
  }

listSshKeys
  :: (MonadBaseControl IO m, MonadResource m)
  => Source (GitLabT m) SshKey
listSshKeys = restSource $ \request -> request
  { path = "/user/keys"
  }

getSshKey
  :: (MonadBaseControl IO m, MonadResource m)
  => SshKeyId
  -> GitLabT m (Maybe SshKey)
getSshKey keyId = rest $ \request -> request
  { method = "GET"
  , path = TE.encodeUtf8 $ "/user/keys/" <> toPathPiece keyId
  }

addSshKey
  :: (MonadBaseControl IO m, MonadResource m)
  => SshKeyParams
  -> GitLabT m (Maybe SshKey)
addSshKey params = rest $ \request -> request
  { method = "POST"
  , path = "/user/keys"
  , requestBody = RequestBodyLBS $ A.encode params
  }

addSshKeyForUser
  :: (MonadBaseControl IO m, MonadResource m)
  => UserId
  -> SshKeyParams
  -> GitLabT m (Maybe SshKey)
addSshKeyForUser userId params = rest $ \request -> request
  { method = "PUT"
  , path = TE.encodeUtf8 $ "/users/" <> toPathPiece userId <> "/keys"
  , requestBody = RequestBodyLBS $ A.encode params
  }

deleteSshKey
  :: (MonadBaseControl IO m, MonadResource m)
  => SshKeyId
  -> GitLabT m (Maybe SshKey)
deleteSshKey keyId = rest $ \request -> request
  { method = "DELETE"
  , path = TE.encodeUtf8 $ "/users/keys/" <> toPathPiece keyId
  }

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . drop (length ("userParams" :: String))
  }
  ''UserParams

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . drop (length ("keyParams" :: String))
  }
  ''SshKeyParams
