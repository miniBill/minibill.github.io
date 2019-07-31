module Main
  ( main
  ) where

import           CLI

type Flags = ()

type Model = ()

type Msg = ()

main :: IO ()
main = error "tmp" {-
  CLI.sandbox
    { init = \flags -> ()
    , view = \model -> CLI.text "Hello world!"
    , update = \msg model -> model
    } -}
