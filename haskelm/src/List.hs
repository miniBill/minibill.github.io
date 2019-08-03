{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module List
  ( any
  , foldl
  , filterMap
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

any :: (a -> Bool) -> List a -> Bool
any _ []     = False
any f (x:xs) = f x || any f xs

map :: (a -> b) -> List a -> List b
map = P.map

filterMap :: (a -> Maybe b) -> List a -> List b
filterMap _ [] = []
filterMap f (x:xs) =
  case f x of
    Just y  -> y : filterMap f xs
    Nothing -> filterMap f xs

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
