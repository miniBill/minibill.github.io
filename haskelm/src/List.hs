{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module List
  ( foldl
  , intersperse
  , length
  , map
  , maximum
  , sum
  , zip
  ) where

import qualified Maybe
import qualified "base" Prelude as P
import           Protolude

map :: (a -> b) -> List a -> List b
map = P.map

foldl :: (e -> a -> a) -> a -> List e -> a
foldl f = P.foldl (\a e -> f e a)

length :: List a -> Int
length = P.fromIntegral << P.length

maximum :: Comparable a => List a -> Maybe a
maximum []     = Nothing
maximum [x]    = Just x
maximum (x:xs) = Maybe.map (max x) (maximum xs)

sum :: Number a => List a -> a
sum []     = fromInteger 0
sum (x:xs) = x + sum xs

intersperse :: a -> List a -> List a
intersperse _ []     = []
intersperse _ [x]    = [x]
intersperse i (x:xs) = x : i : intersperse i xs

zip :: List a -> List b -> List (a, b)
zip = P.zip
