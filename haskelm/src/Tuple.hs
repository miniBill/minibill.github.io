{-# LANGUAGE NoImplicitPrelude #-}

module Tuple
  ( first
  , second
  ) where

first :: (a, b) -> a
first (x, _) = x

second :: (a, b) -> b
second (_, x) = x
