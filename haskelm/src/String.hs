{-# LANGUAGE PackageImports #-}

module String
  ( String
  , fromInt
  , intersperse
  , length
  ) where

import qualified Data.Text       as T
import           Kernel
import qualified List
import qualified "base" Prelude  as P
import           String.Internal

fromInt :: Int -> String
fromInt i = String (T.pack (P.show i))

intersperse :: String -> List String -> String
intersperse (String i) xs = String (T.intercalate i (List.map runString xs))

length :: String -> Int
length (String s) = P.fromIntegral (T.length s)
