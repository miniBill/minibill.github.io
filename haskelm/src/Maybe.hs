module Maybe
  ( map
  , withDefault
  ) where

map :: (a -> b) -> Maybe a -> Maybe b
map _ Nothing  = Nothing
map f (Just x) = Just (f x)

withDefault :: a -> Maybe a -> a
withDefault d Nothing  = d
withDefault _ (Just x) = x
