{-# LANGUAGE PackageImports #-}

module String
  ( String
  , concat
  , fromInt
  , fromList
  , intersperse
  , length
  ) where

import qualified Data.Text       as T
import           Kernel
import qualified List
import qualified "base" Prelude  as P
import           String.Internal

fromInt :: Int -> String
fromInt i = fromList $ P.show i

intersperse :: String -> List String -> String
intersperse (String i) xs = String (T.intercalate i (List.map runString xs))

length :: String -> Int
length (String s) = P.fromIntegral (T.length s)

concat :: List String -> String
concat xs = String (T.concat (List.map runString xs))

fromList :: List Char -> String
fromList = String . T.pack
