{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module Protolude
  ( Appendable(..)
  , Bool(..)
  , Equatable(..)
  , Int
  , IO
  , List
  , Maybe(..)
  , Number(..)
  , Comparable(..)
  , Order(..)
  , String
  , (<|)
  , (|>)
  , (<<)
  , (>>)
  , (//)
  , max
  , not
  ) where

import           "base" Prelude (Bool, IO, Maybe, String, not)
import qualified "base" Prelude as P

type Int = P.Integer

type List a = [a]

infixr 0 <|

(<|) :: (a -> b) -> a -> b
f <| x = f x

infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x

infixr 0 <<

(<<) :: (b -> c) -> (a -> b) -> a -> c
(<<) f g x = f (g x)

infixl 0 >>

(>>) :: (a -> b) -> (b -> c) -> a -> c
(>>) g f x = f (g x)

identity :: a -> a
identity x = x

class Appendable a where
  (++) :: a -> a -> a

instance Appendable [a] where
  (++) = (P.++)

class Number a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  fromInteger :: P.Integer -> a

instance Number P.Integer where
  (+) = (P.+)
  (-) = (P.-)
  fromInteger = identity

(//) :: Int -> Int -> Int
(//) = (P.div)

class Equatable a where
  (==) :: a -> a -> Bool

class Equatable a =>
      Comparable a
  where
  compare :: a -> a -> Order

instance Equatable P.Integer where
  (==) = (P.==)

instance Comparable P.Integer where
  compare l r =
    if l == r
      then EQ
      else if (P.<) l r
             then LT
             else GT

max :: Comparable a => a -> a -> a
max l r =
  case compare l r of
    LT -> r
    _  -> l

data Order
  = LT
  | EQ
  | GT
