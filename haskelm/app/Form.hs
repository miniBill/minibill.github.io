module Main
  ( main
  ) where

import           CLI            (CLI, attributes, button, column, text)
import qualified CLI
import           CLI.Attributes (onClick)

-- unfortunately Haskell does not support qualified module exports
import qualified String

data Model =
  Model
    { name          :: String
    , password      :: String
    , passwordAgain :: String
    }

main :: IO ()
main = CLI.run_ $ CLI.sandbox init view update

init :: Model
init = Model "" "" ""

data Msg
  = Name String
  | Password String
  | PasswordAgain String

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Name name' -> Model name (password model) (passwordAgain model)
    Password password' -> Model (name model) password' (passwordAgain model)
    PasswordAgain passwordAgain' ->
      Model (name model) (password model) passwordAgain'

view :: Model -> CLI Msg
view model =
  column
    [ viewInput "text" "Name" (name model) Name
    , viewInput "password" "Password" (password model) Password
    , viewInput
        "password"
        "Re-enter Password"
        (passwordAgain model)
        PasswordAgain
    , viewValidation model
    ]

viewInput :: String -> String -> String -> (String -> msg) -> CLI msg
viewInput t p v toMsg =
  input [type_ t, placeholder p, value v, onInput toMsg] []

viewValidation :: Model -> CLI msg
viewValidation model =
  if password model == passwordAgain model
    then div [style "color" "green"] [text "OK"]
    else div [style "color" "red"] [text "Passwords do not match!"]
