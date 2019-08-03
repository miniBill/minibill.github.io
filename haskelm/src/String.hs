{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module String
  ( fromInt
  , intersperse
  , length
  ) where

import qualified List
import qualified "base" Prelude as P
import           Protolude

fromInt :: Int -> String
fromInt = P.show

intersperse :: String -> List String -> String
intersperse _ []     = ""
intersperse s (x:xs) = List.foldl (\e a -> e ++ s ++ a) x xs

length :: String -> Int
length = P.fromIntegral << P.length
