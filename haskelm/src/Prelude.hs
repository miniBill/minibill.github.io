{-# LANGUAGE PackageImports #-}

module Prelude
  ( IO
  , String
  , (<|)
  ) where

import qualified "base" Prelude as P

type IO a = P.IO a

type String = P.String

infixr 0 <|

(<|) :: (a -> b) -> a -> b
f <| x = f x
