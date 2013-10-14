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
  , Namespace(..)
  , NamespaceId
  , ProjectMember(..)
  , ProjectHook(..)
  , ProjectHookId

  -- * Project Snippets

  , Snippet(..)
  , SnippetId

  -- * Repositories
  , RepositoryBranch(..)
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

import Control.Applicative ((<$>), Applicative(..), Alternative(..))
import Control.Monad (mzero)
import Data.Char (toLower)
import Data.Text (Text)
import Data.Time (UTCTime, parseTime)
import System.Locale (defaultTimeLocale)
import qualified Data.Text as T

import Control.Lens ((&), (%~))
import Control.Lens.Aeson (_Object)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (typeMismatch)
import Web.PathPieces (PathPiece(..))
import qualified Data.HashMap.Strict as HM

import GitLab.Monad
import GitLab.Util (camelToSnake, dropPrefix)

-----------------------------------------------------------
-- Projects

data Project = Project
  { projectId :: ProjectId
  , projectDescription :: Maybe Text

  , projectNamespace :: Namespace
  , projectName :: Text
  , projectNameWithNamespace :: Text
  , projectPath :: Text
  , projectPathWithNamespace :: Text

  , projectDefaultBranch :: Maybe Text
  , projectOwner :: User'
  , projectPublic :: Bool

  , projectWebUrl :: Text
  , projectHttpUrlToRepo :: Text
  , projectSshUrlToRepo :: Text

  , projectIssuesEnabled :: Bool
  , projectMergeRequestsEnabled :: Bool
  , projectWallEnabled :: Bool
  , projectWikiEnabled :: Bool
  , projectSnippetsEnabled :: Bool

  , projectCreatedAt :: UTCTime
  , projectLastActivityAt :: Maybe UTCTime
  } deriving Show

newtype ProjectId = ProjectId Int deriving (Show, Num, PathPiece)

data Namespace = Namespace
  { namespaceId :: NamespaceId
  , namespaceName :: Text
  , namespacePath :: Text
  , namespaceDescription :: Text
  , namespaceOwnerId :: UserId
  , namespaceCreatedAt :: UTCTime
  , namespaceUpdatedAt :: UTCTime
  } deriving Show

newtype NamespaceId = NamespaceId Int deriving (Show, Num, PathPiece)

data ProjectMember = ProjectMember
  { projectMemberId :: UserId
  , projectMemberUsername :: Text
  , projectMemberEmail :: Text
  , projectMemberName :: Text
  , projectMemberState :: UserState
  , projectMemberCreatedAt :: UTCTime
  , projectMemberAccessLevel :: Int
  } deriving Show

data ProjectHook = ProjectHook
  { projectHookId :: ProjectHookId
  , projectHookUrl :: Text
  , projectHookCreatedAt :: UTCTime
  } deriving Show

newtype ProjectHookId = ProjectHookId Int deriving (Show, Num, PathPiece)

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
  } deriving Show

newtype SnippetId = SnippetId Int deriving (Show, Num, PathPiece)

-----------------------------------------------------------
-- Repositories

data RepositoryBranch = RepositoryBranch
  { repositoryBranchName :: Text
  , repositoryBranchCommit :: Commit
  , repositoryBranchProtected :: Bool
  } deriving Show

data Commit = Commit
  { commitId :: CommitId
  , commitParents :: [CommitParent]
  , commitTree :: CommitId
  , commitMessage :: Text
  , commitAuthor :: CommitUser
  , commitCommitter :: CommitUser
  , commitAuthoredDate :: UTCTime
  , commitCommittedDate :: UTCTime
  } deriving Show

newtype CommitParent = CommitParent
  { commitParentId :: CommitId
  } deriving Show

newtype CommitId = CommitId Text deriving (Show, PathPiece)

data CommitUser = CommitUser
  { commitUserName :: Text
  , commitUserEmail :: Text
  } deriving Show

-----------------------------------------------------------
-- Deploy Keys

data DeployKey = DeployKey
  { deployKeyId :: DeployKeyId
  , deployKeyTitle :: Text
  , deployKeyKey :: Text
  , deployKeyCreatedAt :: UTCTime
  } deriving Show

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
  , user'State :: UserState
  , user'CreatedAt :: UTCTime
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
  } deriving Show

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
  } deriving Show

newtype IssueId = IssueId Int deriving (Show, Num, PathPiece)

data IssueState
  = IssueOpened
  | IssueClosed
  | IssueReopened
  deriving Show

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
  } deriving Show

newtype MilestoneId = MilestoneId Int deriving (Show, Num, PathPiece)

data MilestoneState
  = MilestoneActive
  | MilestoneClosed
  deriving Show

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
  } deriving Show

newtype MergeRequestId = MergeRequestId Int deriving (Show, Num, PathPiece)

-----------------------------------------------------------
-- Notes

data Note = Note
  { noteId :: NoteId
  , noteBody :: Text
  , noteAuthor :: User'
  , noteCreatedAt :: UTCTime
  } deriving Show

newtype NoteId = NoteId Int deriving (Show, Num, PathPiece)

-----------------------------------------------------------

newtype Iso8601Time = Iso8601Time UTCTime deriving Show

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

instance ToJSON Iso8601Time where
  toJSON (Iso8601Time utcTime) = toJSON utcTime

instance FromJSON Iso8601Time where
  parseJSON (String t) =
    tryFormats alternateFormats <|> fail "could not parse ISO-8601 date"
    where
      tryFormat f =
        case parseTime defaultTimeLocale f (T.unpack t) of
          Just d -> pure $ Iso8601Time d
          Nothing -> empty
      tryFormats = foldr1 (<|>) . map tryFormat
      alternateFormats = ["%FT%T%QZ", "%FT%T%Q%z"]
  parseJSON v = typeMismatch "UTCTime" v

-----------------------------------------------------------
-- Aeson derived instances

-- Projects

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "project" }
  ''Project

deriveJSON defaultOptions ''ProjectId

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "namespace" }
  ''Namespace

deriveJSON defaultOptions ''NamespaceId

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "projectMember" }
  ''ProjectMember

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "projectHook" }
  ''ProjectHook

deriveJSON defaultOptions ''ProjectHookId

-- Project Snippets

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "snippet" }
  ''Snippet

deriveJSON defaultOptions ''SnippetId

-- Repository

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "repositoryBranch" }
  ''RepositoryBranch

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "commit" }
  ''Commit

deriveJSON defaultOptions ''CommitId

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "commitParent" }
  ''CommitParent

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
