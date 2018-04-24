{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    PackageImports
  , ApplicativeDo
  , DataKinds
  , NoMonomorphismRestriction
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  , TypeInType
  , TypeOperators
  #-}

-- |
-- Module      : Stackshot.Stackage
-- Description : Stackage API and client functions for accessing cabal.config
-- Copyright   : David Baynard 2018
-- 
-- License     : BSD3
-- Maintainer  : David Baynard <davidbaynard@gmail.com>
-- Stability   : experimental
-- Portability : unknown
module Stackshot.Stackage
  (
    -- * The API
    -- $api
    Stackage
  , Snapshot(..)

    -- * Client functions
    -- $client
  , getStackageCabalConfig
  , stackageBaseURL
  , stackageReq
  ) where

import           "base"            Control.Applicative
import qualified "attoparsec"      Data.Attoparsec.Text as AT
import           "base"            Data.Bifunctor
import           "base"            Data.Proxy
import           "base"            Data.String
import qualified "text"            Data.Text as T
import           "time"            Data.Time
import           "http-client-tls" Network.HTTP.Client.TLS (newTlsManager)
import           "servant"         Servant.API
import           "servant-client"  Servant.Client
import           "this"            Stackshot.Internal
import           "this"            Stackshot.Parser
import           "parsers"         Text.Parser.Char
import           "parsers"         Text.Parser.Combinators
import           "parsers"         Text.Parser.Token

--------------------------------------------------
-- $api
--
-- The 'servant' API corresponds to the @cabal.config@ endpoint only.
-- There are different endpoints for each snapshot, encoded here as
-- a 'Snapshot'.

type Stackage
    = Capture "snapshot" Snapshot :> "cabal.config" :> Get '[PlainText] StackMap

-- TODO Fix so can't make bad snapshot
instance IsString Snapshot where
  fromString = either error id . AT.parseOnly parseSnapshot . T.pack

instance ToHttpApiData Snapshot where
  toQueryParam (LTS Nothing        ) = "lts"
  toQueryParam (LTS (Just (mj, mn))) = mconcat ["lts-", toQueryParam mj, ".", toQueryParam mn]
  toQueryParam (Nightly Nothing)     = "nightly"
  toQueryParam (Nightly (Just dy))   = mconcat ["nightly-", toQueryParam dy]
instance FromHttpApiData Snapshot where
  parseQueryParam = first T.pack . AT.parseOnly parseSnapshot

-- | Parse a snapshot.
parseSnapshot :: forall m . TokenParsing m => m Snapshot
parseSnapshot = parseLTS <|> parseNightly <?> "A valid snapshot"

-- | Parse an LTS snapshot.
parseLTS :: forall m . TokenParsing m => m Snapshot
parseLTS = "A valid LTS snapshot" <??> do
  _ <- string "lts"
  ver <- optional $ "An LTS version" <??> do
    _ <- char '-'
    mj <- fromIntegral <$> natural <?> "Major version"
    _ <- char '.'
    mn <- fromIntegral <$> natural <?> "Minor version"
    pure (mj, mn)
  pure $ LTS ver

-- | Parse a Nightly snapshot.
parseNightly :: forall m . CharParsing m => m Snapshot
parseNightly = "A valid nightly snapshot" <??> do
  _ <- string "nightly"
  dy <- optional $ "A valid date suffix" <??> do
    _ <- char '-'
    Right dy <- parseQueryParam @Day . T.pack <$> count 10 anyChar <?> "YYYY-MM-DD"
    pure dy
  pure $ Nightly dy

-- | Client function for the @cabal.config@ endpoints.
--
-- Equivalent to @'Client' 'ClientM' 'Stackage'@.
getStackageCabalConfig :: Snapshot -> ClientM StackMap
getStackageCabalConfig = client (Proxy @Stackage)

-- | The base url for stackage, <https://www.stackage.org:443/>.
stackageBaseURL :: BaseUrl
stackageBaseURL = BaseUrl
  { baseUrlScheme = Https
  , baseUrlHost = "www.stackage.org"
  , baseUrlPort = 443
  , baseUrlPath = ""
  }

-- | Request the @cabal.config@ file corresponding to the supplied
-- snapshot.
stackageReq :: Snapshot -> IO (Either ServantError StackMap)
stackageReq snapshot = do
  man <- newTlsManager
  let clientEnv = mkClientEnv man stackageBaseURL
  runClientM (getStackageCabalConfig snapshot) clientEnv
