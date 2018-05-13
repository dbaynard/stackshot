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

import           "base"                   Control.Monad
import           "base"                   Data.Bifunctor
import           "bytestring"             Data.ByteString (ByteString)
import qualified "base64-bytestring"      Data.ByteString.Base64 as B64
import           "base64-bytestring-type" Data.ByteString.Base64.Type
import           "text"                   Data.Text (Text)
import qualified "text"                   Data.Text as T
import           "text"                   Data.Text.Encoding
import           "base"                   GHC.Generics
import           "github"                 GitHub.Data hiding (Error)
import qualified "github"                 GitHub.Endpoints.Repos.Contents as GC
import           "this"                   Stackshot.Internal

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

repoCabal :: RepoPackage -> IO (Either GC.Error Content)
repoCabal RepoPackage{rSource = GitHub, ..} = do
  GC.contentsFor rOwner rRepo rFilepath rCommit

decodeGHContent :: Content -> Either Error Text
decodeGHContent (ContentFile (ContentFileData{contentFileEncoding = "base64", ..})) =
  first (ParserError . T.pack . show) . decodeUtf8' <=< first (ParserError . T.pack) . B64.decode . encodeUtf8 $ contentFileContent
