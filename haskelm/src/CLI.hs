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
import qualified List
import qualified Maybe
import           "base" Prelude (mapM_, return)
import qualified String
import qualified Tuple
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
    Text s -> Curses.drawString s
    Row children -> do
      let sizes = List.map getSize children
      let maxHeight =
            sizes |> List.map Tuple.second |> List.maximum |>
            Maybe.withDefault 0
      mapM_
        (\(child, (width, height)) -> do
           (r, c) <- Curses.cursorPosition
           let vpad = (maxHeight - height) // 2
           Curses.moveCursor (r + vpad) (c)
           display child
           Curses.moveCursor r (c + width + 1))
        (List.zip children sizes)
    Button _ children -> do
      let (width, height) = getRowSize children
      (r, c) <- Curses.cursorPosition
      Curses.drawGlyph Curses.glyphCornerUL
      Curses.moveCursor r (c + 1)
      Curses.drawLineH (Just Curses.glyphLineH) (width)
      Curses.moveCursor r (c + width + 1)
      Curses.drawGlyph Curses.glyphCornerUR
      Curses.moveCursor (r + 1) c
      Curses.drawLineV (Just Curses.glyphLineV) (height)
      Curses.moveCursor (r + height + 1) c
      Curses.drawGlyph Curses.glyphCornerLL
      Curses.drawLineH (Just Curses.glyphLineH) (width)
      Curses.moveCursor (r + height + 1) (c + width + 1)
      Curses.drawGlyph Curses.glyphCornerLR
      Curses.moveCursor (r + 1) (c + width + 1)
      Curses.drawLineV (Just Curses.glyphLineV) (height)
      Curses.moveCursor (r + 1) (c + 1)
      mapM_ display children

getRowSize :: List (CLI msg) -> (Int, Int)
getRowSize [] = (0, 0)
getRowSize children =
  let sizes = List.map getSize children
      gapsWidth = List.length children - 1
      widgetsWidth = List.sum <| List.map (Tuple.first) sizes
      width = gapsWidth + widgetsWidth
      height =
        sizes |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 0
   in (width, height)

getSize :: CLI msg -> (Int, Int)
getSize widget =
  case widget of
    Text s -> (String.length s, 1)
    Row [] -> (0, 0)
    Row children -> getRowSize children
    Button _ children ->
      let (width, height) = getRowSize children
       in (width + 2, height + 2)

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
