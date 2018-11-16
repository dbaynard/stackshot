{-# LANGUAGE
    PackageImports
  , DataKinds
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleInstances
  , OverloadedStrings
  , RecordWildCards
  , StandaloneDeriving
  , TypeInType
  , TypeOperators
  #-}

module Main where

import "optparse-generic" Options.Generic

import Stackshot (runUpToDate)

data Options w = Options
    { input :: w ::: FilePath <?> "Input snapshot.yaml (avoid github packages)"
    , output :: w ::: FilePath <?> "Output package list"
    } deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
    Options{..} <- unwrapRecord "List updates to packages in resolver"
    runUpToDate input output

