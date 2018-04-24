{-# LANGUAGE
    PackageImports
  , ApplicativeDo
  , DeriveAnyClass
  , DeriveDataTypeable
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , NoMonomorphismRestriction
  , OverloadedLists
  , OverloadedStrings
  , RankNTypes
  , TypeApplications
  , TypeOperators
  #-}

-- |
-- Module      : Stackshot.Parser
-- Description : Parse stackage's cabal.config file
-- Copyright   : David Baynard 2018
-- License     : BSD3
-- Maintainer  : David Baynard <davidbaynard@gmail.com>
-- Stability   : experimental
-- Portability : unknown
-- 
-- Parse the @cabal.config@ file at e.g. <https://www.stackage.com/lts-11.6/cabal.config>
module Stackshot.Parser
  (
  -- * Parsing
  -- $parsing
    StackMap(..)
  , parseSCCFile
  , parseSCC

  -- ** Parser combinators
  , stackageCabalConfig
  , sccHeader
  , sccEntry
  , pkgVer

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

import           "base"       Control.Applicative
import           "lens"       Control.Lens hiding (noneOf)
import           "mtl"        Control.Monad.Except
import qualified "attoparsec" Data.Attoparsec.ByteString.Char8 as A8
import qualified "attoparsec" Data.Attoparsec.ByteString.Lazy as AL
import           "base"       Data.Bifunctor
import           "bytestring" Data.ByteString (ByteString)
import qualified "bytestring" Data.ByteString as BS
import           "base"       Data.Data
import           "base"       Data.Foldable
import qualified "containers" Data.Map.Strict as MapS
import           "base"       Data.String
import           "text"       Data.Text (Text)
import qualified "text"       Data.Text as T
import           "Cabal"      Distribution.Package (PackageName)
import           "Cabal"      Distribution.Version (Version, mkVersion)
import           "base"       GHC.Generics
import           "servant"    Servant.API
import           "parsers"    Text.Parser.Char
import           "parsers"    Text.Parser.Combinators
import           "parsers"    Text.Parser.Token
import           "unliftio"   UnliftIO (MonadUnliftIO)
import           "unliftio"   UnliftIO.Exception

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
-- $parsing
-- 
-- Produce a 'StackMap' from the set of packages described by stackage.

newtype StackMap = StackMap (MapS.Map PackageName Version)
  deriving stock (Show, Eq, Generic)

instance MimeUnrender PlainText StackMap where
  mimeUnrender _ = AL.eitherResult . AL.parse stackageCabalConfig

-- | Parse a stackage cabal.config file.
--
-- Use 'fromEitherM' or 'fromEitherIO' to run it all in IO.
parseSCCFile :: MonadUnliftIO m => FilePath -> m (Either Error StackMap)
parseSCCFile
    = pure . parserError . parseSCC
  <=< readError . liftIO . BS.readFile

-- | Parse the contents of a stackage cabal.config file.
parseSCC :: ByteString -> Either String StackMap
parseSCC = A8.parseOnly stackageCabalConfig

stackageCabalConfig :: TokenParsing m => m StackMap
stackageCabalConfig = do
  _ <- sccHeader
  _ <- string "constraints:"
  entries <- buildMapMaybe <$> sccEntry `sepBy1` comma
  _ <- eof
  pure $ StackMap entries

sccHeader :: CharParsing m => m String
sccHeader = fmap unlines . some $ do
  _ <- string "-- "
  comment <- noneOf "\n\r" `manyTill` some newline
  pure comment

-- | Parses @    package ==3.4.5,@ to (package, Just 3.4.5) and
-- @    package installed,@ to (package, Nothing).
sccEntry :: TokenParsing m => m (PackageName, Maybe Version)
sccEntry = do
  _ <- spaces
  pkgname <- fromString @PackageName <$> notChar ' ' `manyTill` space
  pkgver <- pure <$> pkgVer <|> string "installed" *> pure Nothing
  pure (pkgname, pkgver)

pkgVer :: TokenParsing m => m Version
pkgVer = do
  _ <- string "=="
  ver <- mkVersion . fmap fromIntegral <$> natural `sepBy1` char '.'
  pure ver

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
