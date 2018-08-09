{-# LANGUAGE
    PackageImports
  , OverloadedStrings
  #-}

module Stackshot
  ( module Stackshot
  ) where

import           "errors"        Control.Error
import           "lens"          Control.Lens
import           "base"          Control.Monad
import qualified "bytestring"    Data.ByteString.Lazy.Char8 as B8L
import qualified "containers"    Data.Map.Strict as MapS (toAscList, union)
import           "hackage-db"    Distribution.Hackage.DB
import           "Cabal"         Distribution.Package (PackageName)
import           "Cabal"         Distribution.Version (Version)
import           "this"          Stackshot.Internal
import           "this"          Stackshot.Snapshot
import           "this"          Stackshot.Stackage
import           "this"          Stackshot.StackMap ()
import           "filepath"      System.FilePath
import           "typed-process" System.Process.Typed
import           "unliftio"      UnliftIO (MonadUnliftIO, MonadIO, liftIO)
import           "unliftio"      UnliftIO.Exception
import qualified "yaml" Data.Yaml as Y

-- | Convert a snapshot file to a list of packages (in yaml format) which
-- are updated on hackage vs the snapshot file's packages.
--
-- This can be used to update a custom snapshot to a new resolver. First,
-- update the resolver in the snapshot file. Then run this command.
runUpdated
  :: MonadUnliftIO m
  => FilePath
  -> FilePath
  -> m ()
runUpdated infile outfile = do
  _ <- updateHackage
  hack <- hackage
  sm <- resolveSnapshotFile infile
  let upds = updated hack sm
  liftIO $ Y.encodeFile outfile upds

--------------------------------------------------
-- * Comparing snapshot with hackage
--------------------------------------------------

upToDate
  :: HackageDB
  -> StackMap -- ^ A set of packages, not necessarily up to date
  -> StackMap -- ^ The most recent versions of those same packages
upToDate hack (StackMap snap) = StackMap $ imap f snap
  where
    f :: PkgName -> PkgVersion -> PkgVersion
    f (PkgName n) v = fromMaybe v $ do
      l <- PkgVersion <$> latest n hack
      guard $ l > v
      pure l

updated
  :: HackageDB
  -> StackMap -- ^ A set of packages, not necessarily up to date
  -> StackMap -- ^ The most recent versions of packages in that set which were _not_ up to date
updated hack (StackMap snap) = StackMap . buildMapMaybe . MapS.toAscList $ imap f snap
  where
    f :: PkgName -> PkgVersion -> Maybe PkgVersion
    f (PkgName n) v = do
      l <- PkgVersion <$> latest n hack
      guard $ l > v
      pure l

-- | Run 'resolveSnapshot' on the provided snapshot.yaml file.
-- That file must use a stackage resolver.
resolveSnapshotFile :: MonadUnliftIO m => FilePath -> m StackMap
resolveSnapshotFile
    = uncurry resolveSnapshot
  <=< readSnapshotFile'

-- | Combine custom packages in a 'StackMap' with the packages in
-- a 'Snapshot', preferring the original version where there is a clash.
--
-- For example, supply the set of custom packages in a snapshot file, with
-- the resolver in the file, and return the total set of packages in the
-- snapshot.
resolveSnapshot
  :: MonadIO m
  => Snapshot -- ^ A resolver for comparison
  -> StackMap -- ^ A set of packages
  -> m StackMap
resolveSnapshot s (StackMap m0) = do
  StackMap m <- fromEitherIO $ stackageReq s
  -- Note: MapS.union is left biased
  pure . StackMap $ MapS.union m0 m

--------------------------------------------------
-- * Hackage
--------------------------------------------------

-- | The hackage index as a 'HackageDB'
--
-- Do not attempt to print to terminal!
hackage :: MonadIO m => m HackageDB
hackage = do
  stackDir <- B8L.unpack <$> getStackRoot
  let hackageIdx = stackDir </> "indices" </> "Hackage" </> "01-index.tar"
  liftIO $ readTarball Nothing hackageIdx

-- | Get the latest version of a package on hackage
latest :: PackageName -> HackageDB -> Maybe Version
latest l = preview $ ix l . traverseMax . asIndex

-- | Read the first line of the stdout from the supplies process.
readLine_ :: MonadIO m => ProcessConfig stdin stdout stderrIgnored -> m B8L.ByteString
readLine_ = fmap (B8L.takeWhile (/= '\n') . fst) . readProcess_

-- | Note: requires stack to be installed.
getStackRoot :: MonadIO m => m B8L.ByteString
getStackRoot = readLine_ $ proc "stack" ["path", "--stack-root"]

updateHackage :: MonadIO m => m ()
updateHackage = runProcess_ $ proc "stack" ["update"]
