module Maybe
  ( Maybe(..)
  , map
  , withDefault
  ) where

import           Kernel (Maybe (..))

map :: (a -> b) -> Maybe a -> Maybe b
map _ Nothing  = Nothing
map f (Just x) = Just (f x)

withDefault :: a -> Maybe a -> a
withDefault d Nothing  = d
withDefault _ (Just x) = x
