{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module GitLab.Monad
  ( GitLabT, runGitLabT

  , GitLabConfig(..)
  , Pagination(..)
  , Credentials(..)
  ) where
import Control.Applicative (Applicative)
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Text (Text)

import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control
import Data.Conduit
import qualified Network.HTTP.Conduit as HC

newtype GitLabT m a = GitLabT { unGitLabT :: ReaderT GitLabConfig m a }
  deriving
    ( Functor, Applicative, Monad, MonadIO
    , MonadReader GitLabConfig
    , MonadResource, MonadThrow
    , MonadTrans
    )

instance MonadBase b m => MonadBase b (GitLabT m) where
  liftBase = lift . liftBase

instance MonadTransControl GitLabT where
  newtype StT GitLabT a = StT { unStT :: StT (ReaderT GitLabConfig) a }
  liftWith f = GitLabT $ liftWith (\run -> f (liftM StT . run . unGitLabT))
  restoreT = GitLabT . restoreT . liftM unStT

instance MonadBaseControl b m => MonadBaseControl b (GitLabT m) where
  newtype StM (GitLabT m) a = StMT { unStMT :: ComposeSt GitLabT m a }
  liftBaseWith = defaultLiftBaseWith StMT
  restoreM = defaultRestoreM unStMT

data GitLabConfig = GitLabConfig
  { gitLabCreds :: Credentials
  , gitLabManager :: HC.Manager
  , gitLabSecure :: Bool
  , gitLabHost :: ByteString
  , gitLabPort :: Int
  , gitLabPagination :: Pagination
  }

data Pagination
  = NoPagination
  | Paginate
  | PaginateBy Int
  deriving (Eq, Ord)

data Credentials = Credentials
  { credsPrivateToken :: ByteString
  }

runGitLabT :: GitLabConfig -> GitLabT m a -> m a
runGitLabT config (GitLabT m) = runReaderT m config
