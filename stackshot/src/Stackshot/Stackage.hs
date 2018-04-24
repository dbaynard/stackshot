{-# LANGUAGE
    PackageImports
  , ApplicativeDo
  , DataKinds
  , DeriveAnyClass
  , DeriveDataTypeable
  , DeriveGeneric
  , DerivingStrategies
  , NoMonomorphismRestriction
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  , TypeInType
  , TypeOperators
  #-}

module Stackshot.Stackage
  ( module Stackshot.Stackage
  ) where

import           "base"           Control.Applicative
import qualified "attoparsec"     Data.Attoparsec.Text as AT
import           "base"           Data.Bifunctor
import           "base"           Data.Proxy
import qualified "text"           Data.Text as T
import           "time"           Data.Time
import           "base"           GHC.Generics
import           "http-client"    Network.HTTP.Client (defaultManagerSettings, newManager)
import           "servant"        Servant.API
import           "servant-client" Servant.Client
import           "this"           Stackshot.Parser
import           "parsers"        Text.Parser.Char
import           "parsers"        Text.Parser.Combinators
import           "parsers"        Text.Parser.Token

type Stackage
    = Capture "snapshot" Snapshot :> "cabal.config" :> Get '[PlainText] StackMap

data Snapshot
  = LTS Int Int
  | Nightly Day
  deriving stock (Show, Eq, Ord, Generic)

instance ToHttpApiData Snapshot where
  toQueryParam (LTS mj mn) = mconcat ["lts-", toQueryParam mj, ".", toQueryParam mn]
  toQueryParam (Nightly dy) = mconcat ["nightly-", toQueryParam dy]
instance FromHttpApiData Snapshot where
  parseQueryParam = first T.pack . AT.parseOnly parseSnapshot

parseSnapshot :: forall m . TokenParsing m => m Snapshot
parseSnapshot = lts <|> nightly
  where
    lts = do
      _ <- string "lts-"
      mj <- fromIntegral <$> natural
      _ <- char '.'
      mn <- fromIntegral <$> natural
      pure $ LTS mj mn

    nightly = do
      _ <- string "nightly-"
      Right dy <- parseQueryParam @Day . T.pack <$> count 10 anyChar
      pure $ Nightly dy

getStackageCabalConfig :: Snapshot -> ClientM StackMap
getStackageCabalConfig = client (Proxy @Stackage)

stackageBaseURL :: BaseUrl
stackageBaseURL = BaseUrl
  { baseUrlScheme = Https
  , baseUrlHost = "www.stackage.org"
  , baseUrlPort = 443
  , baseUrlPath = ""
  }

stackageReq :: Snapshot -> IO (Either ServantError StackMap)
stackageReq snapshot = do
  man <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv man stackageBaseURL
  runClientM (getStackageCabalConfig snapshot) clientEnv
