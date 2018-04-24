{-# LANGUAGE
    PackageImports
  , DeriveAnyClass
  , DeriveDataTypeable
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , NoMonomorphismRestriction
  , OverloadedLists
  , OverloadedStrings
  , RankNTypes
  , TypeApplications
  , TypeOperators
  #-}

-- |
-- Module      : Stackshot.Internal
-- Description : Internal data for Stackshot
-- Copyright   : David Baynard 2018
-- 
-- License     : BSD3
-- Maintainer  : David Baynard <davidbaynard@gmail.com>
-- Stability   : unstable
-- Portability : unknown
-- 
-- Warning: API may change without warning.
module Stackshot.Internal
  (
  -- * Data types
  -- $datatypes
    StackMap(..)

  -- * Error handling
  -- $errors
  , Error(..)
  , readError
  , parserError

  -- * Helpers
  -- $helpers
  , buildMap
  , buildMapMaybe
  ) where

import           "lens"       Control.Lens hiding (noneOf)
import           "base"       Data.Bifunctor
import           "base"       Data.Data
import           "base"       Data.Foldable
import qualified "containers" Data.Map.Strict as MapS
import           "text"       Data.Text (Text)
import qualified "text"       Data.Text as T
import           "Cabal"      Distribution.Package (PackageName)
import           "Cabal"      Distribution.Version (Version)
import           "base"       GHC.Generics
import           "unliftio"   UnliftIO (MonadUnliftIO)
import           "unliftio"   UnliftIO.Exception

--------------------------------------------------
-- $datatypes

-- | A map linking each package (key) to an individual version (value).
newtype StackMap = StackMap (MapS.Map PackageName Version)
  deriving stock (Show, Eq, Generic)

--------------------------------------------------
-- $errors
--
-- Synchronous IO errors (such as failing to read a file) should be caught
-- with the 'handleIO' family of functions and then rethrown as a suitable
-- 'Error'.
--
-- Errors in pure code (such as parser errors) should be transformed into
-- pure 'Error' values and functions which can throw such errors should return
-- an 'Either Error a'.

-- | The likely error types should be enumerated here.
data Error
  = ReadError
  | ParserError Text
  deriving stock (Show, Eq, Typeable)
  deriving anyclass (Exception)

-- | Convert the 'IOException' on failed file read into an 'Error' (still
-- an exception).
readError :: MonadUnliftIO m => m a -> m a
readError = handleIO . const . throwIO $ ReadError

-- | Convert a parser 'String' error into an 'Error'.
parserError :: Bifunctor p => p String a -> p Error a
parserError = first (ParserError . T.pack)

--------------------------------------------------
-- $helpers
--
-- These functions use the 'lens' facilities to assemble maps.
-- The more efficient versions relying on ordered input will not work as
-- the input is not quite in the correct lexicographical order (mainly due
-- to capitalization differences).
-- 
-- TODO check this actually proscribes their use.

-- | Assemble a map-like structure with all the supplied key-value pairs.
buildMap :: (Foldable t, At b, Monoid b) => t (Index b, IxValue b) -> b
buildMap = buildMap' (?~)

-- | Assemble a map-like structure with all the supplied key-(Just value) pairs
-- (But not the key-Nothing pairs).
buildMapMaybe :: (Foldable t, At b, Monoid b) => t (Index b, Maybe (IxValue b)) -> b
buildMapMaybe = buildMap' (.~)

buildMap'
  :: (Foldable t, At b, Monoid b)
  => (b `Lens'` Maybe (IxValue b) -> a -> b -> b)
  -> t (Index b, a) -> b
buildMap' f = foldl' (\m (k, v) -> m & at k `f` v) mempty
{-# INLINE buildMap' #-}
