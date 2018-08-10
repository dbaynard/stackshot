{-# LANGUAGE
    PackageImports
  , DeriveAnyClass
  , DeriveDataTypeable
  , DeriveFoldable
  , DeriveFunctor
  , DeriveGeneric
  , DeriveTraversable
  , DerivingStrategies
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , NoMonomorphismRestriction
  , OverloadedLists
  , OverloadedStrings
  , RankNTypes
  , StandaloneDeriving
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
  , union
  , difference

  , Snapshot(..)
  , PkgName(..)
  , PkgVersion(..)

  -- $pkg-config
  , PkgConfig(..)
  , _Cabal
  , _Hpack

  -- * Error handling
  -- $errors
  , Error(..)
  , readError
  , parserError
  , networkError

  -- * Helpers
  -- $helpers
  , buildMap
  , buildMapMaybe
  , (<??>)
  , onJust
  , azip

  -- * Json-autotype
  -- $json-autotype
  , _AltLeft
  , _AltRight
  ) where

import           "base"             Control.Applicative
import           "lens"             Control.Lens hiding (noneOf)
import           "base"             Control.Monad
import           "json-autotype"    Data.Aeson.AutoType.Alternative
import           "aeson"            Data.Aeson.Types
import           "base"             Data.Bifunctor
import           "base"             Data.Data
import           "base"             Data.Foldable
import qualified "containers"       Data.Map.Strict as MapS
import           "base"             Data.Semigroup
import           "base"             Data.String (IsString)
import           "text"             Data.Text (Text)
import qualified "text"             Data.Text as T
import           "time"             Data.Time
import           "Cabal"            Distribution.Package (PackageName, mkPackageName, unPackageName)
import           "Cabal"            Distribution.Version (Version, mkVersion', showVersion)
import           "base"             GHC.Generics
import           "template-haskell" Language.Haskell.TH.Syntax hiding (PkgName)
import           "parsers"          Text.Parser.Combinators
import           "unliftio"         UnliftIO (MonadUnliftIO)
import           "unliftio"         UnliftIO.Exception

--------------------------------------------------
-- $datatypes

-- | A map linking each package (key) to an individual version (value).
newtype StackMap = StackMap (MapS.Map PkgName PkgVersion)
  deriving stock (Show, Eq, Generic)
  deriving newtype (Semigroup, Monoid)

union :: StackMap -> StackMap -> StackMap
union (StackMap a) (StackMap b) = StackMap $ a `MapS.union` b

-- | Take the difference of two 'StackMap's.
--
-- Return a 'StackMap' with entries from the first supplied 'StackMap' for
-- which either
--
-- -   the package is absent in the second 'StackMap', or
-- -   the version is greater in the first 'StackMap'.
difference :: StackMap -> StackMap -> StackMap
difference (StackMap a) (StackMap b) = StackMap $ MapS.differenceWith newer a b
  where
    newer a' b' = guard (a' > b') *> pure a'

-- | A stackage snapshot.
data Snapshot
  = LTS (Maybe (Int, Int)) -- ^ LTS snapshots have either a @-/Major/./Minor/@ suffix, or no suffix at all.
  | Nightly (Maybe Day)    -- ^ Nightly snapshots may have a @-/YYYY-MM-DD/@ suffix.
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (Lift)

-- | Package names with JSON instances
newtype PkgName = PkgName {unPkgName :: PackageName}
  deriving stock (Eq, Ord, Generic, Data)
  deriving newtype (IsString)
  deriving anyclass (ToJSONKey, FromJSONKey)

instance Show PkgName where
  show = unPackageName . unPkgName

instance FromJSON PkgName where
  parseJSON = fmap (PkgName . mkPackageName) . parseJSON

instance ToJSON PkgName where
  toJSON = toJSON . show
  toEncoding = toEncoding . show

-- | Package names with JSON instances
newtype PkgVersion = PkgVersion {unPkgVersion :: Version}
  deriving stock (Eq, Ord, Generic, Data)

instance Show PkgVersion where
  show = showVersion . unPkgVersion

instance FromJSON PkgVersion where
  parseJSON = fmap (PkgVersion . mkVersion') . parseJSON

instance ToJSON PkgVersion where
  toJSON = toJSON . show
  toEncoding = toEncoding . show

--------------------------------------------------
-- $pkg-config
--
--

-- | How is the package configured? For example, with package
-- 'acme-everything':
data PkgConfig a
  = Cabal a -- ^ acme-everything.cabal
  | Hpack a -- ^ package.yaml
  deriving stock (Show, Eq, Ord, Generic, Data, Functor, Foldable, Traversable)

_Cabal :: PkgConfig a `Prism'` a
_Cabal = prism' Cabal _cabal
  where
    _cabal (Cabal a) = pure a
    _cabal _ = empty
{-# INLINE _Cabal #-}

_Hpack :: PkgConfig a `Prism'` a
_Hpack = prism' Hpack _hpack
  where
    _hpack (Hpack a) = pure a
    _hpack _ = empty
{-# INLINE _Hpack #-}

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
  | GithubError Text
  | NetworkError Text
  | UserError Text
  deriving stock (Show, Eq, Typeable)
  deriving anyclass (Exception)

-- | Convert the 'IOException' on failed file read into an 'Error' (still
-- an exception).
readError :: MonadUnliftIO m => m a -> m a
readError = handleIO . const . throwIO $ ReadError

-- | Convert a parser 'String' error into an 'Error'.
parserError :: Bifunctor p => p String a -> p Error a
parserError = first (ParserError . T.pack)

-- | Convert a network into an 'Error'.
networkError :: (Show e, Bifunctor p) => p e a -> p Error a
networkError = first (NetworkError . T.pack . show)

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

-- | Flipped '<?>'
(<??>) :: Parsing m => String -> m a -> m a
(<??>) = flip (<?>)
infixr 0 <??>

-- | This is like `maybe`, but the last parameter is the continuation for Just
onJust :: Maybe a -> b -> (a -> b) -> b
onJust ma b = maybe b `flip` ma
{-# INLINE onJust #-}
infix 1 `onJust`

-- | Zip together two applicatives into one
azip :: Applicative f => (f a, f b) -> f (a, b)
azip = uncurry $ (<*>) . fmap (,)
{-# INLINE azip #-}

--------------------------------------------------
-- $json-autotype
--
-- Some helpers for interacting with automated json data structures

_AltLeft :: (a :|: b)  `Prism'` a
_AltLeft = prism' AltLeft _altLeft
  where
    _altLeft (AltLeft a) = pure a
    _altLeft _           = empty
{-# INLINE _AltLeft #-}

_AltRight :: (a :|: b)  `Prism'` b
_AltRight = prism' AltRight _altRight
  where
    _altRight (AltRight b) = pure b
    _altRight _            = empty
{-# INLINE _AltRight #-}



