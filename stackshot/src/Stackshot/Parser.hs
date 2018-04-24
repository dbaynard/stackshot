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
import           "mtl"        Control.Monad.Except
import qualified "attoparsec" Data.Attoparsec.ByteString.Char8 as A8
import qualified "attoparsec" Data.Attoparsec.ByteString.Lazy as AL
import           "bytestring" Data.ByteString (ByteString)
import qualified "bytestring" Data.ByteString as BS
import           "base"       Data.String
import           "Cabal"      Distribution.Package (PackageName)
import           "Cabal"      Distribution.Version (Version, mkVersion)
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
