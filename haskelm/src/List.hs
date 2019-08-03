{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module List
  ( foldl
  , length
  , map
  , maximum
  , sum
  ) where

import qualified "base" Prelude as P
import           Protolude

map :: (a -> b) -> List a -> List b
map = P.map

foldl :: (e -> a -> a) -> a -> List e -> a
foldl f = P.foldl <| \a e -> f e a

length :: List a -> Int
length = P.fromIntegral << P.length

maximum :: Comparable a => List a -> Maybe a