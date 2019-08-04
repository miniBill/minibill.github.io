module String.Internal
  ( String(..)
  , unpack
  ) where

import qualified Data.String
import qualified Data.Text   as T
import           Kernel      (Char)

newtype String =
  String
    { runString :: T.Text
    }

instance Data.String.IsString String where
  fromString s = String (T.pack s)

unpack :: String -> [Char]
unpack (String s) = T.unpack s
