{-# LANGUAGE PackageImports #-}

module CLI
  ( CLI
  , Program
  , button
  , row
  , run
  , run_
  , sandbox
  , text
  ) where

import           CLI.Attributes (Attribute)

--import qualified "base" Control.Concurrent as Concurrent
--import qualified "base" Control.Monad      as Monad
import qualified List
import           "base" Prelude (return)

--import qualified "base" Prelude
import qualified String
import           UI.NCurses     (Update)
import qualified UI.NCurses     as Curses

data CLI msg
  = Text String
  | Row (List (CLI msg))
  | Button (List (Attribute msg)) (List (CLI msg))

data Program flags model msg =
  Program
    (flags -> model)
    (model -> CLI msg)
    (msg -> model -> (model, IO [msg]))

run_ :: Program () model msg -> IO ()
run_ = run ()

run :: flags -> Program flags model msg -> IO ()
run flags (Program init view _) =
  let model = init flags
   in Curses.runCurses <| do
        w <- Curses.defaultWindow
        Curses.updateWindow w <| do
          Curses.moveCursor 0 0
          display <| view model
          Curses.moveCursor 0 0
        Curses.render
        _ <- Curses.getEvent w Nothing
        return ()

display :: CLI msg -> Update ()
display widget =
  case widget of
    Text s            -> Curses.drawString s
    Row xs            -> String.intersperse " " <| List.map unpack xs
    Button _ children -> "[ " ++ unpack (Row children) ++ " ]"

getSize :: CLI msg -> (Int, Int)
getSize widget =
  case widget of
    Text s -> (String.length s, 1)
    Row [] -> (0, 0)
    Row xs ->
      let sizes = List.map getSize xs
          gapsWidth = List.length xs - 1
          widgetsWidth = List.sum <| List.map (Tuple.first) sizes
          width = gapsWidth + widgetsWidth
          height =
            sizes |> List.map Tuple.second |> List.maximum |>
            Maybe.withDefault 0
       in (width, 1)

sandbox ::
     model
  -> (model -> CLI msg)
  -> (msg -> model -> model)
  -> Program () model msg
sandbox init view update =
  let init' _ = init
      update' msg model = (update msg model, return [])
   in Program init' view update'

button :: List (Attribute msg) -> List (CLI msg) -> CLI msg
button = Button

row :: List (CLI msg) -> CLI msg
row = Row

text :: String -> CLI msg
text = Text
