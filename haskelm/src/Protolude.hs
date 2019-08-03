{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module Protolude
  ( Appendable(..)
  , Bool(..)
  , Comparable(..)
  , Int
  , IO
  , List
  , Maybe(..)
  , Number(..)
  , Order(..)
  , String
  , (<|)
  , (|>)
  , (<<)
  , (>>)
  ) where

import           "base" Prelude (Bool, IO, Maybe, String)
import qualified "base" Prelude as P

type Int = P.Integer

type List a = [a]

infixr 0 <|

(<|) :: (a -> b) -> a -> b
f <| x = f x

infixr 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x

infixr 0 <<

(<<) :: (b -> c) -> (a -> b) -> a -> c
(<<) f g x = f (g x)

infixr 0 >>

(>>) :: (a -> b) -> (b -> c) -> a -> c
(>>) g f x = f (g x)

class Appendable a where
  (++) :: a -> a -> a

instance Appendable [a] where
  (++) = (P.++)

class Number a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a

instance Number P.Integer where
  (+) = (P.+)
  (-) = (P.-)

class Equatable a where
  (==) :: a -> a -> Bool
  a == b = not (a /= b)
  (/=) :: a -> a -> Bool
  a /= b = not (a == b)

data Order
  = LT
  | EQ
  | GT

instance Equatable Order

class P.Eq a =>
      Comparable a
  where
  (<) :: a -> a -> Bool
  a < b = compare a b == LT
  (>) :: a -> a -> Bool
  a > b = compare a b == GT
  (<=) :: a -> a -> Bool
  a <= b = compare a b /= GT
  (>=) :: a -> a -> Bool
  a >= b = compare a b /= LT
  compare :: a -> a -> Order
  compare a b =
    if a == b
      then EQ
      else if a < b
             then LT
             else GT
