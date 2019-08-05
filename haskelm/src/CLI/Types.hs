{-# LANGUAGE ImplicitPrelude #-}

module CLI.Types
  ( Attribute(..)
  , CLI(..)
  , InputType(..)
  , Program(..)
  , attributes
  , border
  , button
  , column
  , input
  , leftAlignedColumn
  , row
  , text
  ) where

import           Color (Color)

data Attribute msg
  = OnClick msg
  | Foreground Color
  | Background Color

data CLI msg
  = Text String
  | Row (List (CLI msg))
  | LeftAlignedColumn (List (CLI msg))
  | Column (List (CLI msg))
  | Attributes (List (Attribute msg)) (CLI msg)
  | Border (CLI msg)
  | Input InputType String (String -> msg)

data InputType
  = TypeText
  | TypePassword

data Program flags model msg =
  Program
    (flags -> model)
    (model -> CLI msg)
    (msg -> model -> (model, IO (List msg)))

attributes :: List (Attribute msg) -> CLI msg -> CLI msg
attributes = Attributes

button :: List (Attribute msg) -> CLI msg -> CLI msg
button attrs = Attributes attrs . Border

border :: CLI msg -> CLI msg
border = Border

leftAlignedColumn :: List (CLI msg) -> CLI msg
leftAlignedColumn = LeftAlignedColumn

column :: List (CLI msg) -> CLI msg
column = Column

input :: InputType -> String -> (String -> msg) -> CLI msg
input = Input

row :: List (CLI msg) -> CLI msg
row = Row

text :: String -> CLI msg
text = Text
