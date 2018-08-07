{-# LANGUAGE
    PackageImports
  , NamedFieldPuns
  , OverloadedStrings
  , RecordWildCards
  , TypeApplications
  #-}

module Stackshot.Snapshot
  ( module Stackshot.Snapshot
  ) where

import           "base"          Control.Monad
import           "json-autotype" Data.Aeson.AutoType.Alternative (alt)
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
import           "unliftio"      UnliftIO.Exception

type SnapshotYaml = A.TopLevel

readSnapshotFile :: MonadUnliftIO m => FilePath -> m StackMap
readSnapshotFile
    = liftIO . stackmapFromYaml
  <=< fromEitherIO . parseSnapshotFile

parseSnapshotFile :: MonadUnliftIO m => FilePath -> m (Either Error SnapshotYaml)
parseSnapshotFile
    = pure . parserError . first show
  <=< readError . liftIO . decodeFileEither

stackmapFromYaml :: SnapshotYaml -> IO StackMap
stackmapFromYaml = fmap (StackMap . buildMap . mconcat) . traverse f . topLevelPackages
  where
    f = (fmap pure . fromEither . explicit) `alt` ( github `alt` (pure @IO . const []) )

explicit :: Text -> Either Error (PackageName, Version)
explicit = parserError . AT.parseOnly versionedPkg

github :: PackagesElt -> IO [(PackageName, Version)]
github =  traverse f . asRepo
  where
    f :: Either Error RepoPackage -> IO (PackageName, Version)
    f eep = do
      p <- fromEither eep
      githubPackage p

parseGitUrl :: Text -> Either Error (Name Owner, Name Repo)
parseGitUrl = parserError . AT.parseOnly githubUrl

asRepo :: PackagesElt -> [Either Error RepoPackage]
asRepo PackagesElt{..} = do
    eep <- either (const [Left (ParserError packagesEltGit)]) (pure . Right) $ parseGitUrl packagesEltGit
    sub <- maybe [Nothing] (fmap Just) packagesEltSubdirs
    pure $ do
      (rOwner, rRepo) <- eep
      let defRCabal = T.pack $ T.unpack (untagName rRepo) <.> "cabal"
          rCabal = onJust sub defRCabal $ \s -> T.pack $ T.unpack s </> T.unpack s <.> "cabal"
          rHpack = onJust sub "package.yaml" $ \s -> T.pack $ T.unpack s </> "package.yaml"
      pure RepoPackage{..}
  where
    rSource      = GitHub
    rCommit      = Just packagesEltCommit
