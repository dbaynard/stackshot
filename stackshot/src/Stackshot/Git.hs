{-# LANGUAGE
    PackageImports
  , DataKinds
  , TypeOperators
  , TypeInType
  #-}

module Stackshot.Git
  ( module Stackshot.Git
  ) where

import "servant" Servant.API
import "text" Data.Text (Text)

data Contents

type Github
    = "repos" :> Capture "owner" Text :> Capture "repo" Text :> "contents" :> CaptureAll "path" Text :> QueryParam "ref" Text :> Get '[JSON] Contents

-- e.g. https://api.github.com/repos/dbaynard/haskell/contents/snapshot/snapshot.cabal?ref=develop
