module Main
  ( main
  ) where

import           CLI            (CLI, button, row, text)
import qualified CLI
import           CLI.Attributes (onClick)

-- unfortunately Haskell does not support qualified module exports
import qualified String

main :: IO ()
main = CLI.run_ $ CLI.sandbox 0 view update

data Msg
  = Increment
  | Decrement

update :: Msg -> Int -> Int
update msg model =
  case msg of
    Increment -> model + 1
    Decrement -> model - 1

view :: Int -> CLI Msg
view model =
  row
    [ button [onClick Decrement] $ text "-"
    , text $ String.fromInt model
    , button [onClick Increment] $ text "+"
    ]
