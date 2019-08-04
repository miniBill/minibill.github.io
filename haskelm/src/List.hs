{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module List
  ( singleton
  , repeat
  , range
  , map
  , indexedMap
  , foldl
  , foldr
  , filter
  , any
  , concat
  , filterMap
  , intersperse
  , length
  , maximum
  , sum
  , zip
  ) where

import qualified Maybe
import qualified "base" Prelude as P
import           Protolude

singleton :: a -> List a
singleton x = [x]

repeat :: Int -> a -> List a
repeat = P.replicate << P.fromIntegral

range :: Int -> Int -> List Int
range from to =
  let go n acc =
        if n < from
          then acc
          else go (n - 1) (n : acc)
   in go to []

map :: (a -> b) -> List a -> List b
map = P.map

indexedMap :: (Int -> a -> b) -> List a -> List b
indexedMap f xs = map (\(i, e) -> f i e) (zip (range 1 (length xs)) xs)

foldl :: (e -> a -> a) -> a -> List e -> a
foldl f = P.foldl (\a e -> f e a)

foldr :: (e -> a -> a) -> a -> List e -> a
foldr = P.foldr

filter :: (a -> Bool) -> List a -> List a
filter _ [] = []
filter f (x:xs) =
  if f x
    then x : filter f xs
    else filter f xs

any :: (a -> Bool) -> List a -> Bool
any _ []     = False
any f (x:xs) = f x || any f xs

filterMap :: (a -> Maybe b) -> List a -> List b
filterMap _ [] = []
filterMap f (x:xs) =
  case f x of
    Just y  -> y : filterMap f xs
    Nothing -> filterMap f xs

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

concat :: List (List a) -> List a
concat []     = []
concat (x:xs) = x ++ concat xs
