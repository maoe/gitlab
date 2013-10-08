{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module GitLab.Types
  ( -- * @GitLabT@ monad transformer
    module GitLab.Monad

  -- * Users
  , User(..)
  , UserId
  , UserState(..)
  , ThemeId
  , ColorSchemeId
  , CurrentUser(..)

  , SshKey(..)
  , SshKeyId

  -- * Groups
  , Group(..)
  , GroupId
  ) where

import Control.Monad (mzero)
import Data.Char (toLower)
import Data.Text (Text)
import Data.Time (UTCTime)

import Control.Lens ((&), (%~))
import Control.Lens.Aeson (_Object)
import Data.Aeson
import Data.Aeson.TH
import Web.PathPieces (PathPiece(..))
import qualified Data.HashMap.Strict as HM

import GitLab.Monad
import GitLab.Util (camelToSnake, dropPrefix)

data Group = Group
  { groupId :: GroupId
  , groupName :: Text
  , groupPath :: Text
  , groupOwnerId :: UserId
  }

newtype GroupId = GroupId Int deriving (Show, Num, PathPiece)

data User = User
  { userId :: UserId
  , userUsername :: Text
  , userEmail :: Text
  , userName :: Text
  , userState :: UserState
  , userCreatedAt :: UTCTime
  , userBio :: Maybe Text
  , userSkype :: Maybe Text
  , userLinkedin :: Maybe Text
  , userTwitter :: Maybe Text
  , userExternUid :: Maybe Text
  , userProvider :: Maybe Text
  , userThemeId :: ThemeId
  , userColorSchemeId :: ColorSchemeId
  } deriving Show

newtype UserId = UserId Int deriving (Show, Num, PathPiece)

data UserState = UserActive | UserBlocked deriving Show

newtype ThemeId = ThemeId Int deriving (Show, Num, PathPiece)

newtype ColorSchemeId = ColorSchemeId Int deriving (Show, Num, PathPiece)

data CurrentUser = CurrentUser
  { currentUser :: User
  , currentUserIsAdmin :: Bool
  , currentUserCanCreateGroup :: Bool
  , currentUserCanCreateTeam :: Bool
  , currentUserCanCreateProject :: Bool
  } deriving Show

instance ToJSON CurrentUser where
  toJSON CurrentUser {..} = toJSON currentUser & _Object
    %~ HM.insert "is_admin" (toJSON currentUserIsAdmin)
    . HM.insert "can_create_group" (toJSON currentUserCanCreateGroup)
    . HM.insert "can_create_team" (toJSON currentUserCanCreateTeam)
    . HM.insert "can_create_project" (toJSON currentUserCanCreateProject)

instance FromJSON CurrentUser where
  parseJSON obj@(Object v) = do
    currentUser <- parseJSON obj
    currentUserIsAdmin <- v .: "is_admin"
    currentUserCanCreateGroup <- v .: "can_create_group"
    currentUserCanCreateTeam <- v .: "can_create_team"
    currentUserCanCreateProject <- v .: "can_create_project"
    return CurrentUser {..}
  parseJSON _ = mzero

data SshKey = SshKey
  { sshKeyId :: SshKeyId
  , sshKeyTitle :: Text
  , sshKeyKey :: Text
  } deriving Show

newtype SshKeyId = SshKeyId Int deriving (Show, Num, PathPiece)

-------------------------------------------------
-- Aeson instances

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "group" }
  ''Group

deriveJSON defaultOptions ''GroupId

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "user" }
  ''User

deriveJSON defaultOptions
  { constructorTagModifier = camelToSnake . dropPrefix "user" }
  ''UserState

deriveJSON defaultOptions ''ColorSchemeId
deriveJSON defaultOptions ''ThemeId
deriveJSON defaultOptions ''UserId

deriveJSON defaultOptions
  { constructorTagModifier = camelToSnake . dropPrefix "sshKey" }
  ''SshKey

deriveJSON defaultOptions ''SshKeyId
