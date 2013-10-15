{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
  , ProjectEvent(..)

  -- * Project Snippets

  , Snippet(..)
  , SnippetId

  -- * Repositories
  , Branch(..)
  , Commit(..)
  , CommitId
  , CommitParent(..)
  , CommitUser(..)

  -- * Deploy Keys
  , DeployKey(..)
  , DeployKeyId

  -- * Users
  , User(..)
  , SimpleUser(..)
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

import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (mzero)
import Data.Text (Text)
import Data.Time (Day, UTCTime, formatTime, parseTime)
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
  , projectOwner :: SimpleUser
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

data ProjectEvent = ProjectEvent
  { projectEventTitle :: Maybe Text
  , projectEventProjectId :: ProjectId
  , projectEventActionName :: Text -- TODO
  , projectEventTargetId :: Maybe Int -- TODO
  , projectEventTargetType :: Maybe Text
  , projectEventAuthorId :: UserId
  , projectEventData :: Value -- TODO
  , projectEventTargetTitle :: Maybe Text
  } deriving Show

-----------------------------------------------------------
-- Project Snippets

data Snippet = Snippet
  { snippetId :: SnippetId
  , snippetTitle :: Text
  , snippetFileName :: FilePath
  , snippetAuthor :: SimpleUser
  , snippetExpiresAt :: Maybe UTCTime
  , snippetUpdatedAt :: UTCTime
  , snippetCreatedAt :: UTCTime
  } deriving Show

newtype SnippetId = SnippetId Int deriving (Show, Num, PathPiece)

-----------------------------------------------------------
-- Repositories

data Branch = Branch
  { branchName :: Text
  , branchCommit :: Commit
  , branchProtected :: Bool
  } deriving Show

data Commit = Commit
  { commitId :: CommitId
  , commitParents :: [CommitParent]
  , commitTree :: CommitId
  , commitMessage :: Text
  , commitAuthor :: CommitUser
  , commitCommitter :: CommitUser
  , commitAuthoredDate :: Iso8601Time
  , commitCommittedDate :: Iso8601Time
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

data SimpleUser = SimpleUser
  { simpleUserId :: UserId
  , simpleUserUsername :: Text
  , simpleUserEmail :: Text
  , simpleUserName :: Text
  , simpleUserState :: UserState
  , simpleUserCreatedAt :: UTCTime
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
  , issueMilestone :: Maybe Milestone
  , issueAssignee :: Maybe SimpleUser
  , issueAuthor :: SimpleUser
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
  , milestoneDueDate :: Maybe Day
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
  , mergeRequestState :: MergeRequestState
  , mergeRequestAuthor :: SimpleUser
  , mergeRequestAssignee :: Maybe SimpleUser
  } deriving Show

newtype MergeRequestId = MergeRequestId Int deriving (Show, Num, PathPiece)

data MergeRequestState
  = MergeRequestOpened
  | MergeRequestMerged
  | MergeRequestClosed
  deriving Show

-----------------------------------------------------------
-- Notes

data Note = Note
  { noteId :: NoteId
  , noteBody :: Text
  , noteAuthor :: SimpleUser
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

instance ToJSON Day where
  toJSON = toJSON . formatTime defaultTimeLocale "%F"

instance FromJSON Day where
  parseJSON = withText "Day" $ \t ->
    case parseTime defaultTimeLocale "%F" (T.unpack t) of
      Just d -> pure d
      Nothing -> empty

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

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "projectEvent" }
  ''ProjectEvent

-- Project Snippets

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "snippet" }
  ''Snippet

deriveJSON defaultOptions ''SnippetId

-- Repository

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "branch" }
  ''Branch

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
  { fieldLabelModifier = camelToSnake . dropPrefix "simpleUser" }
  ''SimpleUser

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

deriveJSON defaultOptions
  { constructorTagModifier = camelToSnake . dropPrefix "mergeRequest"
  }
  ''MergeRequestState

-- Notes

deriveJSON defaultOptions
  { fieldLabelModifier = camelToSnake . dropPrefix "note" }
  ''Note

deriveJSON defaultOptions ''NoteId
