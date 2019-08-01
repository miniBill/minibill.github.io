module Main
  ( main
  ) where

import           CLI

type Flags = ()

type Model = ()

type Msg = ()

main :: IO ()
main =
  let init = ()
      view _ = CLI.text "Hello world!"
      update _ model = model
   in CLI.run_ <| CLI.sandbox init view update
