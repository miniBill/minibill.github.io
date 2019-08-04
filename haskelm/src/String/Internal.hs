module String.Internal
  ( String(..)
  , unpack
  ) where

import qualified Data.Text as T
import           Kernel    (Char)

newtype String =
  String
    { runString :: T.Text
    }

unpack :: String -> [Char]
unpack (String s) = T.unpack s
