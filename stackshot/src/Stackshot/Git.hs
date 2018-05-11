{-# LANGUAGE
    PackageImports
  , DataKinds
  , DeriveGeneric
  , DerivingStrategies
  , NamedFieldPuns
  , OverloadedStrings
  , RecordWildCards
  , StrictData
  , TypeInType
  , TypeOperators
  #-}

module Stackshot.Git
  ( module Stackshot.Git
  ) where

import           "text"   Data.Text (Text)
import           "base"   GHC.Generics
import           "github" GitHub.Data
import qualified "github" GitHub.Endpoints.Repos.Contents as GC

data RepoSource
  = GitHub
  deriving stock (Show, Eq, Generic)

data RepoPackage = RepoPackage
  { rSource   :: RepoSource
  , rOwner    :: Name Owner
  , rRepo     :: Name Repo
  , rFilepath :: Text
  , rCommit   :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

-- e.g. https://api.github.com/repos/dbaynard/stackshot/contents/stackshot/stackshot.cabal?ref=develop
exampleGithubRepo :: RepoPackage
exampleGithubRepo = RepoPackage
  { rSource   = GitHub
  , rOwner    = "dbaynard"
  , rRepo     = "stackshot"
  , rFilepath = "stackshot/stackshot.cabal"
  , rCommit   = Just "develop"
  }

repoCabal :: RepoPackage -> IO (Either Error Content)
repoCabal RepoPackage{rSource = GitHub, ..} = do
  GC.contentsFor rOwner rRepo rFilepath rCommit
