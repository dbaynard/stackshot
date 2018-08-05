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
import qualified "base64-bytestring"      Data.ByteString.Base64 as B64
import           "text"                   Data.Text (Text)
import qualified "text"                   Data.Text as T
import           "text"                   Data.Text.Encoding
import           "Cabal"                  Distribution.Package (pkgVersion)
import           "Cabal"                  Distribution.PackageDescription
import           "Cabal"                  Distribution.PackageDescription.Parse
import           "Cabal"                  Distribution.Version (Version)
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
  , rFilepath :: Text -- ^ Cabal filepath
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

githubVersion :: RepoPackage -> IO (Either Error Version)
githubVersion rp = do
  cbl <- parserError . first show <$> repoCabal rp
  pure $ do
    pkg <- decodeCabal =<< decodeGHContentFile =<< cbl
    pure . pkgVersion . package . packageDescription $ pkg

repoCabal :: RepoPackage -> IO (Either GC.Error Content)
repoCabal RepoPackage{rSource = GitHub, ..} = do
  GC.contentsFor rOwner rRepo rFilepath rCommit

decodeCabal :: Text -> Either Error GenericPackageDescription
decodeCabal = parsing . parseGenericPackageDescription . T.unpack

decodeGHContentFile :: Content -> Either Error Text
decodeGHContentFile (ContentFile (ContentFileData{contentFileEncoding = "base64", ..})) =
  parserError . first show . decodeUtf8' <=<
  parserError . B64.decode . encodeUtf8 . T.filter (/= '\n') $
  contentFileContent
decodeGHContentFile (ContentFile _) =
  Left (ParserError "Can only decode base64 encoded content")
decodeGHContentFile (ContentDirectory _) =
  Left ReadError

parsing :: ParseResult a -> Either Error a
parsing (ParseOk [] a) = Right a
parsing (ParseOk ws _) = Left . ParserError . T.pack . show $ ws
parsing (ParseFailed e) = Left . ParserError . T.pack . show $ e
