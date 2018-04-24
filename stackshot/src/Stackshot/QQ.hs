{-# LANGUAGE
    PackageImports
  , AllowAmbiguousTypes
  , Safe
  , ScopedTypeVariables
  , TemplateHaskellQuotes
  , TypeApplications
  #-}

-- |
-- Module      : Stackshot.QQ
-- Description : QuasiQuoter for making better OverloadedStrings
-- Copyright   : David Baynard 2018
-- 
-- License     : BSD3
-- Maintainer  : David Baynard <davidbaynard@gmail.com>
-- Stability   : experimental
-- Portability : unknown
-- 
-- Run 'fromString' at compile time, to ensure any failures occur then, not
-- at runtime.
--
-- To use, define a new quasiquoter @qq = os \@Type@.
module Stackshot.QQ
  ( os
  , os'
  , attoparsed
  ) where

import safe qualified "attoparsec"       Data.Attoparsec.Text as AT (Parser, parseOnly)
import safe           "base"             Data.String (IsString(..))
import safe qualified "text"             Data.Text as T (pack, strip)
import safe           "template-haskell" Language.Haskell.TH.Quote (QuasiQuoter(..))
import safe           "template-haskell" Language.Haskell.TH.Syntax (Lift)

-- | OverloadedStrings that are checked at compile time. Supply the required
-- output type and use this new variable as @[myQQ| content |]@.
os :: forall a . (Lift a, IsString a) => QuasiQuoter
os = QuasiQuoter
  { quoteExp   = \s -> let x = fromString @a s in x `seq` [| x |]
  , quotePat   = const $ error "No quotePat defined for any IsString QQ"
  , quoteType  = const $ error "No quoteType defined for any IsString QQ"
  , quoteDec   = const $ error "No quoteDec defined for any IsString QQ"
  }

-- | As 'os' but with an explicit 'proxy'.
os' :: forall proxy a . (Lift a, IsString a) => proxy a -> QuasiQuoter
os' _ = os @a

-- | Run a "AT.parser" at compile time.
attoparsed :: Lift a => AT.Parser a -> QuasiQuoter
attoparsed parser = QuasiQuoter
  { quoteExp   = \s -> let x = either error id . AT.parseOnly parser . T.strip . T.pack $ s
      in x `seq` [| x |]
  , quotePat   = const $ error "No quotePat defined for any IsString QQ"
  , quoteType  = const $ error "No quoteType defined for any IsString QQ"
  , quoteDec   = const $ error "No quoteDec defined for any IsString QQ"
  }
