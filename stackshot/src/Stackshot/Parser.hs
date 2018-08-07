{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    PackageImports
  , ApplicativeDo
  , MultiParamTypeClasses
  , NoMonomorphismRestriction
  , OverloadedStrings
  , TypeApplications
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
    StackMap
  , parseSCCFile
  , parseSCC

  -- ** Versions
  , readVersion

  -- ** Git urls
  , githubUrl

  -- ** Parser combinators
  , stackageCabalConfig
  , sccHeader
  , sccEntry
  , versionedPkg
  , pkgVer
  , version

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
import           "mtl"        Control.Monad.Except
import qualified "attoparsec" Data.Attoparsec.ByteString.Char8 as A8
import qualified "attoparsec" Data.Attoparsec.ByteString.Lazy as AL
import qualified "attoparsec" Data.Attoparsec.Text as AT
import           "bytestring" Data.ByteString (ByteString)
import qualified "bytestring" Data.ByteString as BS
import qualified "base"       Data.List as List
import           "base"       Data.String
import           "text"       Data.Text (Text)
import           "Cabal"      Distribution.Version (mkVersion)
import           "github"     GitHub.Data (Name, Owner, Repo)
import           "servant"    Servant.API
import           "this"       Stackshot.Internal
import           "parsers"    Text.Parser.Char
import           "parsers"    Text.Parser.Combinators
import           "parsers"    Text.Parser.Token
import           "unliftio"   UnliftIO (MonadUnliftIO)

instance MimeUnrender PlainText StackMap where
  mimeUnrender _ = AL.eitherResult . AL.parse stackageCabalConfig

--------------------------------------------------
-- $parsing
-- 
-- Produce a 'StackMap' from the set of packages described by stackage.

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
sccEntry :: TokenParsing m => m (PkgName, Maybe PkgVersion)
sccEntry = do
  _ <- spaces
  pkgname <- fromString @PkgName <$> notChar ' ' `manyTill` space
  pkgver <- pure <$> pkgVer <|> string "installed" *> pure Nothing
  pure (pkgname, pkgver)

readVersion :: Text -> Either Error PkgVersion
readVersion = parserError . AT.parseOnly version

-- | Parses @package-name-3.4.5@ to (package-name, 3.4.5)
versionedPkg :: TokenParsing m => m (PkgName, PkgVersion)
versionedPkg = do
  pkgname <- fromString @PkgName . List.intercalate "-" <$> some alphaNum `endBy1` char '-'
  pkgver <- version
  pure (pkgname, pkgver)

pkgVer :: TokenParsing m => m PkgVersion
pkgVer = string "==" *> version

version :: TokenParsing m => m PkgVersion
version = do
  PkgVersion . mkVersion . fmap fromIntegral <$> natural `sepBy1` char '.'

--------------------------------------------------
-- $git
--
-- Parse git repo urls

-- | For example,
--
-- @
-- - git: https://github.com/dbaynard/haskell
--   commit: ccbf50014bcb8e233b9a99604d7c2e3610611f58
--   subdirs:
--   - forestay
--   - forestay-data
--   - forestay-serial
--   - readp
-- @
--
-- @
-- - git: git@bitbucket.org:dbaynard/ucamwebauth.git
--   commit: 8b9f8f7a9ed6ad3a81131192c6ae1f51191c99f4
--   subdirs:
--   - raven-wai
--   - servant-raven
--   - servant-raven-server
--   - ucam-webauth
--   - ucam-webauth-types
-- @
githubUrl :: CharParsing m => m (Name Owner, Name Repo)
githubUrl = do
  _ <- string "https://" <|> string "git@"
  _ <- optional $ string "www."
  _ <- string "github.com"
  _ <- char ':' <|> char '/'
  owner <- fromString @(Name Owner) <$> notChar '/' `manyTill` char '/'
  repo <- fromString @(Name Repo) <$> notChar '/' `manyTill` choice [ void (string "/"), void (string ".git"), eof ]
  pure (owner, repo)
