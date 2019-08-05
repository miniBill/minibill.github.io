module CLI.Attributes
  ( Attribute(..)
  , backgroundColor
  , foregroundColor
  , onClick
  ) where

import           Color (Color)

data Attribute msg
  = OnClick msg
  | Foreground Color
  | Background Color

onClick :: msg -> Attribute msg
onClick = OnClick

foregroundColor :: Color -> Attribute msg
foregroundColor = Foreground

backgroundColor :: Color -> Attribute msg
backgroundColor = Background
