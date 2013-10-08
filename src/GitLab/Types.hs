{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module GitLab.Types
  ( -- * @GitLabT@ monad transformer
    module GitLab.Monad

  -- * Projects
  , Project(..)
  , ProjectId

  -- * Project Snippets

  , Snippet(..)
  , SnippetId

  -- * Repositories
  , Repository(..)
  , Commit(..)
  , CommitId
  , CommitUser(..)

  -- * Deploy Keys
  , DeployKey(..)
  , DeployKeyId

  -- * Users
  , User(..)
  , User'(..)
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

  -- * Issues
  , Issue(..)
  , IssueId
  , IssueState(..)

  -- * Milestones
  , Milestone(..)
  , MilestoneId
  , MilestoneState(..)

  -- * Merge Requests
  , MergeRequest(..)
  , MergeRequestId

  -- * Notes
  , Note(..)
  , NoteId
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

-----------------------------------------------------------
-- Projects

data Project = Project
  { projectId :: ProjectId
  , projectName :: Text
  , projectDescription :: Maybe Text
  , projectDefaultBranch :: Text
  , projectOwner :: User'
  , projectPublic :: Bool
  , projectPath :: Text
  , projectPathWithNamespace :: Text
  , projectIssuesEnabled :: Bool
  , projectMergeRequestsEnabled :: Bool
  , projectWallEnabled :: Bool
  , projectWikiEnabled :: Bool
  , projectCreatedAt :: UTCTime
  , projectLastActivityAt :: UTCTime
  }

newtype ProjectId = ProjectId Int deriving (Show, Num, PathPiece)


-----------------------------------------------------------
-- Project Snippets

data Snippet = Snippet
  { snippetId :: SnippetId
  , snippetTitle :: Text
  , snippetFileName :: FilePath
  , snippetAuthor :: User'
  , snippetExpiresAt :: Maybe UTCTime
  , snippetUpdatedAt :: UTCTime
  , snippetCreatedAt :: UTCTime
  }

newtype SnippetId = SnippetId Int deriving (Show, Num, PathPiece)

data SnippetAuthor = SnippetAuthor
  { snippetAuthorId :: UserId
  , snippetAuthorUsername :: Text
  , snippetAuthorEmail :: Text
  , snippetAuthorName :: Text
  , snippetAuthorBlocked :: Bool
  , snippetAuthorCreatedAt :: UTCTime
  }

-----------------------------------------------------------
-- Repositories

data Repository = Repository
  { repositoryName :: Text
  , repositoryCommit :: Commit
  , repositoryProtected :: Bool
  }

data Commit = Commit
  { commitId :: CommitId
  , commitParents :: [CommitId]
  , commitTree :: CommitId
  , commitMessage :: Text
  , commitAuthor :: CommitUser
  , commitCommitter :: CommitUser
  , commitAuthoredDate :: UTCTime
  , commitCommittedDate :: UTCTime
  }

newtype CommitId = CommitId Text deriving (Show, PathPiece)

data CommitUser = CommitUser
  { commitUserName :: Text
  , commitUserEmail :: Text
  }

-----------------------------------------------------------
-- Deploy Keys

data DeployKey = DeployKey
  { deployKeyId :: DeployKeyId
  , deployKeyTitle :: Text
  , deployKeyKey :: Text
  }

newtype DeployKeyId = DeployKeyId Int deriving (Show, Num, PathPiece)

-----------------------------------------------------------
-- Users

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

data User' = User'
  { user'Id :: UserId
  , user'Username :: Text
  , user'Email :: Text
  , user'Name :: Text
  , user'Blocked :: Bool
  , user'CreatedAt :: UTCTime
  }

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

data SshKey = SshKey
  { sshKeyId :: SshKeyId
  , sshKeyTitle :: Text
  , sshKeyKey :: Text
  } deriving Show

newtype SshKeyId = SshKeyId Int deriving (Show, Num, PathPiece)

-----------------------------------------------------------
-- Groups

data Group = Group
  { groupId :: GroupId
  , groupName :: Text
  , groupPath :: Text
  , groupOwnerId :: UserId
  }

newtype GroupId = GroupId Int deriving (Show, Num, PathPiece)

-----------------------------------------------------------
-- Issues

data Issue = Issue
  { issueId :: IssueId
  , issueProjectId :: ProjectId
  , issueTitle :: Text
  , issueDescription :: Text
  , issueLabels :: [Text]
  , issueMilestone :: Milestone
  , issueAssignee :: User'
  , issueAuthor :: User'
  , issueState :: IssueState
  , issueUpdatedAt :: UTCTime
  , issueCreatedAt :: UTCTime
  }

newtype IssueId = IssueId Int deriving (Show, Num, PathPiece)

data IssueState
  = IssueOpened
  | IssueClosed
  | IssueReopened

-----------------------------------------------------------
-- Milestones

data Milestone = Milestone
  { milestoneId :: MilestoneId
  , milestoneProjectId :: ProjectId
  , milestoneTitle :: Text
  , milestoneDescription :: Text
  , milestoneState :: MilestoneState
  , milestoneDueDate :: UTCTime
  , milestoneCreatedAt :: UTCTime
  , milestoneUpdatedAt :: UTCTime
  }

newtype MilestoneId = MilestoneId Int deriving (Show, Num, PathPiece)

data MilestoneState
  = MilestoneActive
  | MilestoneClosed

-----------------------------------------------------------
-- Merge Requests

data MergeRequest = MergeRequest
  { mergeRequestId :: MergeRequestId
  , mergeRequestTargetBranch :: Text
  , mergeRequestSourceBranch :: Text
  , mergeRequestProjectId :: ProjectId
  , mergeRequestTitle :: Text
  , mergeRequestClosed :: Bool
  , mergeRequestAuthor :: User'
  , mergeRequestAssignee :: User'
  }

newtype MergeRequestId = MergeRequestId Int deriving (Show, Num, PathPiece)

-----------------------------------------------------------
-- Notes

data Note = Note
  { noteId :: NoteId
  , noteBody :: Text
  , noteAuthor :: User'
  , noteCreatedAt :: UTCTime
  }

newtype NoteId = NoteId Int deriving (Show, Num, PathPiece)

-----------------------------------------------------------
-- Aeson manual instances

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

-----------------------------------------------------------
-- Aeson derived instances

-- Projects

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "project" }
  ''Project

deriveJSON defaultOptions ''ProjectId

-- Project Snippets

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "snippet" }
  ''Snippet

deriveJSON defaultOptions ''SnippetId

-- Repository

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "repository" }
  ''Repository

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "commit" }
  ''Commit

deriveJSON defaultOptions ''CommitId

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "commitUser" }
  ''CommitUser

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "user'" }
  ''User'

-- Deploy Key

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "deployKey" }
  ''DeployKey

deriveJSON defaultOptions ''DeployKeyId

-- Users

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

-- Groups

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "group" }
  ''Group

deriveJSON defaultOptions ''GroupId

-- Issues

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "issue" }
  ''Issue

deriveJSON defaultOptions ''IssueId

deriveJSON defaultOptions
  { constructorTagModifier = camelToSnake . dropPrefix "issue" }
  ''IssueState

-- Milestones

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "milestone" }
  ''Milestone

deriveJSON defaultOptions ''MilestoneId

deriveJSON defaultOptions
  { constructorTagModifier = camelToSnake . dropPrefix "milestone" }
  ''MilestoneState

-- Merge Requests

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "mergeRequest" }
  ''MergeRequest

deriveJSON defaultOptions ''MergeRequestId

-- Notes

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "note" }
  ''Note

deriveJSON defaultOptions ''NoteId
