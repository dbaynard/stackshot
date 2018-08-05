{-# LANGUAGE
    PackageImports
  , NamedFieldPuns
  , OverloadedStrings
  , RecordWildCards
  #-}

module Stackshot.Snapshot
  ( module Stackshot.Snapshot
  ) where

import           "base"          Control.Monad
import qualified "attoparsec"    Data.Attoparsec.Text as AT
import           "base"          Data.Bifunctor
import           "text"          Data.Text (Text)
import qualified "text"          Data.Text as T
import           "yaml"          Data.Yaml hiding ((.=))
import           "Cabal"         Distribution.Package (PackageName)
import           "Cabal"         Distribution.Version (Version)
import           "github"        GitHub.Data (Name, Owner, Repo, untagName)
import           "this"          Stackshot.Git
import           "this"          Stackshot.Internal
import           "this"          Stackshot.Parser
import           "this"          Stackshot.Snapshot.Auto
import qualified "this"          Stackshot.Snapshot.Auto as A
import           "filepath"      System.FilePath
import           "unliftio"      UnliftIO (MonadUnliftIO, liftIO)

type SnapshotYaml = A.TopLevel

readSnapshotFile :: MonadUnliftIO m => FilePath -> m (Either Error StackMap)
readSnapshotFile = undefined

parseSnapshotFile :: MonadUnliftIO m => FilePath -> m (Either Error SnapshotYaml)
parseSnapshotFile
    = pure . parserError . first show
  <=< readError . liftIO . decodeFileEither

stackmapFromYaml :: SnapshotYaml -> StackMap
stackmapFromYaml = undefined

explicit :: Text -> Either String (PackageName, Version)
explicit = AT.parseOnly versionedPkg

-- github :: PackagesElt -> Either String (PackageName, Version)
-- github PackagesElt{..} =

parseGitUrl :: Text -> Either Error (Name Owner, Name Repo)
parseGitUrl = parserError . AT.parseOnly githubUrl

asRepo :: PackagesElt -> [RepoPackage]
asRepo PackagesElt{..} = do
    (rOwner, rRepo) <- either (const []) pure $ parseGitUrl packagesEltGit
    sub <- maybe [Nothing] (fmap Just) packagesEltSubdirs
    let defRFilepath = T.pack $ T.unpack (untagName rRepo) <.> "cabal"
        rFilepath = onJust sub defRFilepath $ \s -> T.pack $ T.unpack s </> T.unpack s <.> "cabal"
    pure RepoPackage{..}
  where
    rSource      = GitHub
    rCommit      = Just packagesEltCommit
