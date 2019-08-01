{-# LANGUAGE DataKinds #-}

module Main
  ( main
  ) where

import           CLI

type Flags = ()

type Model = ()

type Msg = ()

main :: IO ()
main =
  let init = Label :: Label "init"
      view = Label :: Label "view"
      update = Label :: Label "update"
   in CLI.sandbox <|
      init .=. () .*. view .=. \model ->
        CLI.text "Hello world!" .*. update .=. \msg model ->
          model .*. emptyRecord
