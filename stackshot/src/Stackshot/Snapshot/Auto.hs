{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Stackshot.Snapshot.Auto where

import           Control.Monad      (mzero, join)
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(Object, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Aeson.Types (Parser)
import           Data.Monoid
import           Data.Text (Text)
import qualified GHC.Generics

-- | Workaround for https://github.com/bos/aeson/issues/287.
(.:??) :: FromJSON a => Object -> Text -> Parser (Maybe a)
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
