{-# LANGUAGE
    PackageImports
  , ApplicativeDo
  , DeriveAnyClass
  , DeriveDataTypeable
  , DerivingStrategies
  , FlexibleContexts
  , NoMonomorphismRestriction
  , OverloadedLists
  , OverloadedStrings
  , TypeApplications
  #-}

module Stackshot.Parser
  ( module Stackshot.Parser
  ) where

import           "base"       Control.Applicative
import           "lens"       Control.Lens hiding (noneOf)
import           "mtl"        Control.Monad.Except
import qualified "attoparsec" Data.Attoparsec.ByteString.Char8 as A8
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
import           "parsers"    Text.Parser.Char
import           "parsers"    Text.Parser.Combinators
import           "parsers"    Text.Parser.Token
import           "unliftio"   UnliftIO (MonadUnliftIO)
import           "unliftio"   UnliftIO.Exception

type StackMap = MapS.Map PackageName Version

--------------------------------------------------
-- * Error handling
--------------------------------------------------

data Error
  = ReadError
  | ParserError Text
  deriving stock (Show, Eq, Typeable)
  deriving anyclass (Exception)

readError :: MonadUnliftIO m => m a -> m a
readError = handleIO . const . throwIO $ ReadError

parserError :: Bifunctor p => p String a -> p Error a
parserError = first (ParserError . T.pack)

--------------------------------------------------
-- * Parsing
--------------------------------------------------

parseSCCFile :: MonadUnliftIO m => FilePath -> m (Either Error StackMap)
parseSCCFile
    = pure . parserError . parseSCC
  <=< readError . liftIO . BS.readFile

parseSCC :: ByteString -> Either String StackMap
parseSCC = A8.parseOnly stackageCabalConfig

stackageCabalConfig :: TokenParsing m => m StackMap
stackageCabalConfig = do
  _ <- sccHeader
  _ <- string "constraints:"
  entries <- buildMapMaybe <$> sccEntry `sepBy1` comma
  _ <- eof
  pure entries

sccHeader :: CharParsing m => m String
sccHeader = fmap unlines . some $ do
  _ <- string "-- "
  comment <- noneOf "\n\r" `manyTill` some newline
  pure comment

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

buildMap :: (Foldable t, At b, Monoid b) => t (Index b, IxValue b) -> b
buildMap = foldl' (\m (n, v) -> m & at n ?~ v) mempty

buildMapMaybe :: (Foldable t, At b, Monoid b) => t (Index b, Maybe (IxValue b)) -> b
buildMapMaybe = foldl' (\m (n, v) -> m & at n .~ v) mempty
