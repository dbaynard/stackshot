{-# LANGUAGE
    PackageImports
  #-}

module Stackshot.Snapshot
  ( module Stackshot.Snapshot
  ) where

import           "base"          Control.Monad
import           "json-autotype" Data.Aeson.AutoType.Alternative
import qualified "attoparsec"    Data.Attoparsec.Text as AT
import           "base"          Data.Bifunctor
import           "text"          Data.Text (Text)
import           "yaml"          Data.Yaml hiding ((.=))
import           "Cabal"         Distribution.Package (PackageName)
import           "Cabal"         Distribution.Version (Version, mkVersion)
import           "this"          Stackshot.Internal
import           "this"          Stackshot.Parser
import qualified "this"          Stackshot.Snapshot.Auto as A
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
