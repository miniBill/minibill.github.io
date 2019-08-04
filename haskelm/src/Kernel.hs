{-# LANGUAGE NoRebindableSyntax   #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Kernel
  ( Appendable(..)
  , Equatable(..)
  , Number(..)
  , Comparable(..)
  , Bool(..)
  , Float
  , Int
  , Char
  , List
  , Order(..)
  , Maybe(..)
  , IO
  , fdiv
  , idiv
  , isInfinite
  , isNaN
  , sin
  , cos
  , tan
  , asin
  , acos
  , atan
  , atan2
  , sqrt
  , log
  , logBase
  , e
  , pi
  , and
  , or
  , xor
  , not
  , lt
  , gt
  , le
  , ge
  , notEqual
  , toFloat
  , round
  , floor
  , ceiling
  , truncate
  , modBy
  , remainderBy
  , ifThenElse
  ) where

import qualified Hack
import           "base" Prelude (Bool (..), Char, Maybe (..))
import qualified "base" Prelude as P

type IO a = P.IO a

type Int = P.Integer

type Float = P.Float

type List a = [a]

class Appendable a where
  append :: a -> a -> a

instance Appendable [a] where
  append = (P.++)

class Equatable a where
  equal :: a -> a -> Bool

instance Equatable Float where
  equal l r = (P.==) l r

instance Equatable Int where
  equal l r = (P.==) l r

class Equatable a =>
      Comparable a
  where
  compare :: a -> a -> Order

instance Comparable Int where
  compare l r =
    if (P.==) l r
      then EQ
      else if (P.<) l r
             then LT
             else GT

instance Comparable Float where
  compare l r =
    if (P.==) l r
      then EQ
      else if (P.<) l r
             then LT
             else GT

class Comparable a =>
      Number a
  where
  add :: a -> a -> a
  mul :: a -> a -> a
  sub :: a -> a -> a
  pow :: a -> a -> a
  fromInteger :: P.Integer -> a

instance Number Int where
  add l r = (P.+) l r
  mul l r = Hack.mul l r
  sub l r = (P.-) l r
  pow l r = (P.^) l r
  fromInteger i = i

instance Number Float where
  add l r = (P.+) l r
  mul l r = Hack.mul l r
  sub l r = (P.-) l r
  pow l r = (P.**) l r
  fromInteger = P.fromInteger

data Order
  = LT
  | EQ
  | GT

fdiv :: Float -> Float -> Float
fdiv = (P./)

idiv :: Int -> Int -> Int
idiv = P.div

isInfinite :: Float -> Bool
isInfinite = P.isInfinite

isNaN :: Float -> Bool
isNaN = P.isNaN

sin :: Float -> Float
sin = P.sin

cos :: Float -> Float
cos = P.cos

tan :: Float -> Float
tan = P.tan

asin :: Float -> Float
asin = P.asin

acos :: Float -> Float
acos = P.acos

atan :: Float -> Float
atan = P.atan

atan2 :: Float -> Float -> Float
atan2 = P.atan2

sqrt :: Float -> Float
sqrt = P.sqrt

log :: Float -> Float
log = P.log

logBase :: Float -> Float -> Float
logBase = P.logBase

e :: Float
e = P.exp 1

pi :: Float
pi = P.pi

lt :: Comparable a => a -> a -> Bool
lt l r =
  case compare l r of
    LT -> True
    _  -> False

le :: Comparable a => a -> a -> Bool
le l r =
  case compare l r of
    GT -> False
    _  -> True

notEqual :: Equatable a => a -> a -> Bool
notEqual l r = not (equal l r)

ge :: Comparable a => a -> a -> Bool
ge l r =
  case compare l r of
    LT -> False
    _  -> True

gt :: Comparable a => a -> a -> Bool
gt l r =
  case compare l r of
    GT -> True
    _  -> False

and :: Bool -> Bool -> Bool
and a b =
  case a of
    False -> False
    True  -> b

or :: Bool -> Bool -> Bool
or a b =
  case a of
    True  -> True
    False -> b

xor :: Bool -> Bool -> Bool
xor a b =
  case a of
    False -> b
    True ->
      case b of
        True  -> False
        False -> True

not :: Bool -> Bool
not a =
  case a of
    False -> True
    True  -> False

toFloat :: Int -> Float
toFloat = P.fromInteger

round :: Float -> Int
round = P.round

floor :: Float -> Int
floor = P.floor

ceiling :: Float -> Int
ceiling = P.ceiling

truncate :: Float -> Int
truncate = P.truncate

modBy :: Int -> Int -> Int
modBy by val = P.mod val by

remainderBy :: Int -> Int -> Int
remainderBy by val = P.rem val by

ifThenElse :: Bool -> a -> a -> a
ifThenElse b ~l ~r =
  case b of
    True  -> l
    False -> r