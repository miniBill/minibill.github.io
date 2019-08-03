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
  , (||)
  , (&&)
  , (<)
  , (<=)
  , (>)
  , (>=)
  , always
  , min
  , max
  , not
  ) where

import           "base" Prelude (Bool (..), IO, Maybe, String, not, (&&), (||))
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

always :: a -> b -> a
always x _ = x

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

infix 4 <

(<) :: Comparable a => a -> a -> Bool
l < r =
  case compare l r of
    LT -> True
    _  -> False

infix 4 <=

(<=) :: Comparable a => a -> a -> Bool
l <= r =
  case compare l r of
    GT -> False
    _  -> True

infix 4 >

(>) :: Comparable a => a -> a -> Bool
l > r =
  case compare l r of
    GT -> True
    _  -> False

infix 4 >=

(>=) :: Comparable a => a -> a -> Bool
l >= r =
  case compare l r of
    LT -> False
    _  -> True

min :: Comparable a => a -> a -> a
min l r =
  case compare l r of
    GT -> r
    _  -> l

max :: Comparable a => a -> a -> a
max l r =
  case compare l r of
    LT -> r
    _  -> l

data Order
  = LT
  | EQ
  | GT
