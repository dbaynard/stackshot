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

import           "base"       Control.Arrow ((&&&))
import           "base"       Control.Monad
import           "json-alt"   Data.Aeson.AutoType.Alternative (alt)
import qualified "attoparsec" Data.Attoparsec.Text as AT
import           "base"       Data.Bifunctor
import qualified "text"       Data.Text as T
import           "yaml"       Data.Yaml hiding ((.=))
import           "github"     GitHub.Data (untagName)
import           "this"       Stackshot.Git
import           "this"       Stackshot.Internal
import           "this"       Stackshot.Parser
import           "this"       Stackshot.Snapshot.Auto as A
import           "this"       Stackshot.Stackage
import           "filepath"   System.FilePath
import           "unliftio"   UnliftIO (MonadUnliftIO, liftIO)
import           "unliftio"   UnliftIO.Exception

--------------------------------------------------
-- * Snapshot files
--------------------------------------------------

type SnapshotYaml = A.TopLevel

readSnapshotFile :: MonadUnliftIO m => FilePath -> m StackMap
readSnapshotFile = fmap snd . readSnapshotFile'

readSnapshotFile' :: MonadUnliftIO m => FilePath -> m (Snapshot, StackMap)
readSnapshotFile'
    = azip . (fromEither . onResolver &&& liftIO . stackmapFromYaml)
  <=< fromEitherIO . parseSnapshotFile

parseSnapshotFile :: MonadUnliftIO m => FilePath -> m (Either Error SnapshotYaml)
parseSnapshotFile
    = pure . parserError . first show
  <=< readError . liftIO . decodeFileEither

stackmapFromYaml :: SnapshotYaml -> IO StackMap
stackmapFromYaml = fmap (StackMap . buildMap . mconcat) . traverse f . topLevelPackages
  where
    f = (fmap pure . fromEither . explicit) `alt` ( github `alt` (pure @IO . const []) )

github :: PackagesElt -> IO [(PkgName, PkgVersion)]
github =  traverse f . asRepo
  where
    f :: Either Error RepoPackage -> IO (PkgName, PkgVersion)
    f eep = do
      p <- fromEither eep
      githubPackage p

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

--------------------------------------------------
-- * Resolvers
--------------------------------------------------

onResolver :: SnapshotYaml -> Either Error Snapshot
onResolver = parserError . AT.parseOnly parser . topLevelResolver
  where
    parser = parseSnapshot
