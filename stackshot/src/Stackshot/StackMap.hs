-- |
-- Module      : Stackshot.StackMap
-- Description : The StackMap type
-- Copyright   : David Baynard 2018

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    PackageImports
  , NoMonomorphismRestriction
  , OverloadedStrings
  , TypeApplications
  #-}

module Stackshot.StackMap
  ( -- * StackMap
    StackMap

  , packageWithVersion
  ) where

import           "base"       Control.Monad
import           "aeson"      Data.Aeson.Types
import           "base"       Data.Either
import qualified "containers" Data.Map.Strict as MapS
import           "text"       Data.Text (Text)
import qualified "text"       Data.Text as T
import           "this"       Stackshot.Internal
import           "this"       Stackshot.Parser

instance FromJSON StackMap where
  parseJSON = pure . StackMap . buildMap @[] . snd . partitionEithers . fmap explicit
    <=< parseJSON

instance ToJSON StackMap where
  toJSON (StackMap m) = toJSON . fmap (uncurry packageWithVersion) . MapS.toAscList $ m
  toEncoding (StackMap m) = toEncoding . fmap (uncurry packageWithVersion) . MapS.toAscList $ m

packageWithVersion :: PkgName -> PkgVersion -> Text
packageWithVersion n v = T.intercalate "-" $ T.pack <$>
  [ show n
  , show v
  ]
