{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module StackshotSrcStackshotSnapshotAuto where

import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(decode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Monoid
import           Data.Text (Text)
import qualified GHC.Generics

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data PackagesElt = PackagesElt { 
    packagesEltSubdirs :: (Maybe ([Text])),
    packagesEltGit :: Text,
    packagesEltCommit :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON PackagesElt where
  parseJSON (Object v) = PackagesElt <$> v .:?? "subdirs" <*> v .:   "git" <*> v .:   "commit"
  parseJSON _          = mzero


instance ToJSON PackagesElt where
  toJSON     (PackagesElt {..}) = object ["subdirs" .= packagesEltSubdirs, "git" .= packagesEltGit, "commit" .= packagesEltCommit]
  toEncoding (PackagesElt {..}) = pairs  ("subdirs" .= packagesEltSubdirs<>"git" .= packagesEltGit<>"commit" .= packagesEltCommit)


data TopLevel = TopLevel { 
    topLevelPackages :: [Text:|:PackagesElt:|:[(Maybe Value)]],
    topLevelName :: Text,
    topLevelCompiler :: Text,
    topLevelResolver :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:   "packages" <*> v .:   "name" <*> v .:   "compiler" <*> v .:   "resolver"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["packages" .= topLevelPackages, "name" .= topLevelName, "compiler" .= topLevelCompiler, "resolver" .= topLevelResolver]
  toEncoding (TopLevel {..}) = pairs  ("packages" .= topLevelPackages<>"name" .= topLevelName<>"compiler" .= topLevelCompiler<>"resolver" .= topLevelResolver)




parse :: FilePath -> IO TopLevel
parse filename = do input <- BSL.readFile filename
                    case decode input of
                      Nothing -> fatal $ case (decode input :: Maybe Value) of
                                           Nothing -> "Invalid JSON file: "     ++ filename
                                           Just v  -> "Mismatched JSON value from file: " ++ filename
                      Just r  -> return (r :: TopLevel)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess


