-- |
-- Module      : Stackshot.StackMap
-- Description : The StackMap type
-- Copyright   : David Baynard 2018

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    PackageImports
  , NoMonomorphismRestriction
  , TypeApplications
  #-}

module Stackshot.StackMap
  ( -- * StackMap
    StackMap
  ) where

import           "aeson"      Data.Aeson.Types
import qualified "containers" Data.Map.Strict as MapS
import           "this"       Stackshot.Internal

instance FromJSON StackMap where
  parseJSON = fmap (StackMap . buildMap @[]) . parseJSON

instance ToJSON StackMap where
  toJSON (StackMap m) = toJSON . MapS.toAscList $ m
  toEncoding (StackMap m) = toEncoding . MapS.toAscList $ m
