module CLI.Attributes
  ( Attribute
  , onClick
  ) where

data Attribute msg =
  OnClick msg

onClick :: msg -> Attribute msg
onClick = OnClick
