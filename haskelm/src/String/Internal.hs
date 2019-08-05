module String.Internal
  ( String(..)
  , unpack
  ) where

import qualified Data.String
import qualified Data.Text   as T
import           Kernel      (Appendable (..), Char, List)

newtype String =
  String
    { runString :: T.Text
    }

instance Data.String.IsString String where
  fromString s = String (T.pack s)

instance Appendable String where
  append (String l) (String r) = String (T.append l r)

unpack :: String -> List Char
unpack (String s) = T.unpack s
