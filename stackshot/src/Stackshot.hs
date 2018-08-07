{-# LANGUAGE
    PackageImports
  , OverloadedStrings
  #-}

module Stackshot
  ( module Stackshot
  ) where

import           "lens"          Control.Lens
import           "base"          Control.Monad.IO.Class
import qualified "bytestring"    Data.ByteString.Lazy.Char8 as B8L
import           "hackage-db"    Distribution.Hackage.DB
import           "Cabal"         Distribution.Package (PackageName)
import           "Cabal"         Distribution.Version (Version)
import           "filepath"      System.FilePath
import           "typed-process" System.Process.Typed

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
