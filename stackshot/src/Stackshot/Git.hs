{-# LANGUAGE
    PackageImports
  , DataKinds
  , DeriveGeneric
  , DerivingStrategies
  , NamedFieldPuns
  , OverloadedStrings
  , RecordWildCards
  , StrictData
  , TypeApplications
  , TypeInType
  , TypeOperators
  #-}

module Stackshot.Git
  ( module Stackshot.Git
  ) where

import           "base"              Control.Arrow ((&&&))
import           "errors"            Control.Error
import           "lens"              Control.Lens
import           "base"              Control.Monad
import           "lens-aeson"        Data.Aeson.Lens
import           "base"              Data.Bifunctor
import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import           "text"              Data.Text (Text)
import qualified "text"              Data.Text as T
import           "text"              Data.Text.Encoding
import qualified "yaml"              Data.Yaml as Y
import           "Cabal"             Distribution.Package (pkgName, pkgVersion, mkPackageName)
import           "Cabal"             Distribution.PackageDescription
import           "Cabal"             Distribution.PackageDescription.Parse
import           "base"              GHC.Generics (Generic)
import qualified "github"            GitHub.Data as GH (Error(..))
import           "github"            GitHub.Data hiding (Error(..))
import qualified "github"            GitHub.Endpoints.Repos.Contents as GC
import           "this"              Stackshot.Internal
import           "this"              Stackshot.Parser
import           "unliftio"          UnliftIO.Exception

data RepoSource
  = GitHub
  deriving stock (Show, Eq, Generic)

data RepoPackage = RepoPackage
  { rSource   :: RepoSource
  , rOwner    :: Name Owner
  , rRepo     :: Name Repo
  , rCabal    :: Text -- ^ Cabal filepath
  , rHpack    :: Text -- ^ package.yaml filepath
  , rCommit   :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

errorFromGithubError :: GH.Error -> Error
errorFromGithubError (GH.HTTPError hx) = NetworkError . T.pack . show $ hx
errorFromGithubError (GH.ParseError t) = ParserError t
errorFromGithubError (GH.JsonError t)  = ParserError t
errorFromGithubError (GH.UserError t)  = UserError t

-- e.g. https://api.github.com/repos/dbaynard/stackshot/contents/stackshot/stackshot.cabal?ref=develop
exampleGithubRepo :: RepoPackage
exampleGithubRepo = RepoPackage
  { rSource   = GitHub
  , rOwner    = "dbaynard"
  , rRepo     = "stackshot"
  , rCabal    = "stackshot/stackshot.cabal"
  , rHpack    = "stackshot/package.yaml"
  , rCommit   = Just "develop"
  }

githubPackage :: RepoPackage -> IO (PkgName, PkgVersion)
githubPackage rp = do
  pkgConf <- repoPkgConfig rp
  fromEither $ do
    contents <- decodeGHContentFile `traverse` pkgConf
    decodePkgConfig contents

repoPkgConfig :: RepoPackage -> IO (PkgConfig Content)
repoPkgConfig RepoPackage{rSource = GitHub, ..} = do
    print rCabal
    catchAny (fromEitherIO asCabal) $ \_ -> do
      print rHpack
      fromEitherIO asHpack
  where
    asCabal = errorFromGithubError `bimap` Cabal <$>
      GC.contentsFor rOwner rRepo rCabal rCommit
    asHpack = errorFromGithubError `bimap` Hpack <$>
      GC.contentsFor rOwner rRepo rHpack rCommit

decodePkgConfig :: PkgConfig Text -> Either Error (PkgName, PkgVersion)
decodePkgConfig (Cabal a) = (PkgName . pkgName &&& PkgVersion . pkgVersion) . package . packageDescription <$> decodeCabal a
decodePkgConfig (Hpack a) = decodeHpack a

decodeHpack :: Text -> Either Error (PkgName, PkgVersion)
decodeHpack contents = do
  val <- parserError . first Y.prettyPrintParseException .
    Y.decodeEither' @Y.Value . encodeUtf8 $ contents
  name <- note (ParserError "Could not find package name") $
    val ^? key "name" . _String . to T.unpack . to (PkgName . mkPackageName)
  ver <- note (ParserError "Could not find package version") $
    val ^? key "version" . _String . to readVersion . _Right
  pure (name, ver)

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
