{-# LANGUAGE
    PackageImports
  #-}

module Stackshot.Snapshot
  ( module Stackshot.Snapshot
  ) where

import           "base"     Control.Monad
import           "base"     Data.Bifunctor
import           "yaml"     Data.Yaml hiding ((.=))
import           "this"     Stackshot.Internal
import qualified "this"     Stackshot.Snapshot.Auto as A
import           "unliftio" UnliftIO (MonadUnliftIO, liftIO)

type SnapshotYaml = A.TopLevel

parseSnapshotFile' :: MonadUnliftIO m => FilePath -> m (Either Error SnapshotYaml)
parseSnapshotFile'
    = pure . parserError . first show
  <=< readError . liftIO . decodeFileEither
