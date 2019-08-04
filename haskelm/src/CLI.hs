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

import           CLI.Attributes (Attribute (..))
import qualified List
import qualified Maybe
import           "base" Prelude (mapM_, return)
import qualified String
import qualified Tuple
import           UI.NCurses     (Curses, Update)
import qualified UI.NCurses     as Curses

data CLI msg
  = Text String
  | Row (List (CLI msg))
  | Button (List (Attribute msg)) (List (CLI msg))

data Program flags model msg =
  Program
    (flags -> model)
    (model -> CLI msg)
    (msg -> model -> (model, IO (List msg)))

run_ :: Program () model msg -> IO ()
run_ = run ()

run :: flags -> Program flags model msg -> IO ()
run flags (Program init view update) =
  let model = init flags
  -- runCurses initializes the ncurses library
   in Curses.runCurses <| mainLoop view update model

mainLoop ::
     (model -> CLI msg)
  -> (msg -> model -> (model, IO (List msg)))
  -> model
  -> Curses ()
mainLoop view update =
  let go model = do
        let root = view model
        maybeEvent <- displayAndWait root
        case maybeEvent of
          Nothing -> go model -- Something went wrong, just keep going
          Just event -> do
            let maybeMsgs = eventToMsgs root event
            case maybeMsgs of
              Nothing -> return () -- Exit
              Just msgs -> do
                let (model', _) = List.foldl step (model, []) msgs
                go model'
      step msg (mod, cmds) =
        let (mod', cmd) = update msg mod
         in (mod', cmd : cmds)
   in go

-- Returns Nothing to exit, Just msgs for messages
eventToMsgs :: CLI msg -> Curses.Event -> Maybe (List msg)
eventToMsgs root event =
  case event of
    Curses.EventMouse _ mouseState ->
      let (x, y, _) = Curses.mouseCoordinates mouseState
          clicks =
            List.sum <|
            List.map
              (\(_, b) ->
                 case b of
                   Curses.ButtonReleased      -> 1
                   Curses.ButtonClicked       -> 1
                   Curses.ButtonDoubleClicked -> 2
                   Curses.ButtonTripleClicked -> 3
                   _                          -> 0)
              (Curses.mouseButtons mouseState)
       in Just <| List.concat <| List.repeat clicks (onClick x y root)
    Curses.EventCharacter 'q' -> Nothing
    _ -> Just []

onClick :: Int -> Int -> CLI msg -> List msg
onClick x y root =
  case root of
    Text _ -> []
    Row [] -> []
    Row (child:children) ->
      let (width, height) = getSize child
       in if x < width
            then if y < height
                   then onClick x y child
                   else []
            else if x == width
                   then []
                   else onClick (x - width - 1) y (Row children)
    Button attrs children ->
      let (w, h) = getRowSize children
          msgs =
            List.filterMap
              (\attr ->
                 case attr of
                   OnClick msg -> Just msg)
              attrs
       in if x < (w + 2) && y < (h + 2)
            then msgs
            else []

-- Displays a widget in the top left corner of the screen
-- and waits for an event
displayAndWait :: CLI msg -> Curses (Maybe Curses.Event)
displayAndWait root = do
  w <- Curses.defaultWindow
  -- updateWindow prepares the drawing
  Curses.updateWindow w <| do
    Curses.moveCursor 0 0 -- Move the cursor in the top left corner
    Curses.clear
    displayWidget root
    Curses.moveCursor 0 0 -- Move the cursor in the top left corner, again
  -- Actually do the drawing on screen
  Curses.render
  -- Wait for an event. "Nothing" means it should wait forever
  Curses.catchCurses (Curses.getEvent w Nothing) (always <| return Nothing)

displayWidget :: CLI msg -> Update ()
displayWidget widget =
  case widget of
    Text s -> Curses.drawString s -- A piece of text is simply written
    Row children -> do
      let sizes = List.map getSize children
      let maxHeight =
            sizes |> List.map Tuple.second |> List.maximum |>
            Maybe.withDefault 0
      -- mapM_ is like List.map, but it's used for functions whose
      -- results are in a monad. It uses map to transform a List (Update a) into
      -- a Update (List a). The underscore is a convention meaning "ignore the result",
      -- so it becomes a Update ()
      mapM_
        (\(child, (width, height)) -> do
           (r, c) <- Curses.cursorPosition
           -- This is used to center vertically
           let vpad = (maxHeight - height) // 2
           Curses.moveCursor (r + vpad) (c)
           displayWidget child
           Curses.moveCursor r (c + width + 1))
        (List.zip children sizes)
    Button _ children -> do
      let (width, height) = getRowSize children
      displayBox width height
      displayWidget <| Row children -- Just reuse the logic from Row

-- Draws a box, and moves the cursor inside it
displayBox :: Int -> Int -> Update ()
displayBox width height = do
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

-- Get the size of a row of widgets
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

-- Get the size of a widget
getSize :: CLI msg -> (Int, Int)
getSize widget =
  case widget of
    Text s -> (String.length s, 1) -- Text is shown with no wrapping
    Row [] -> (0, 0)
    Row children -> getRowSize children
    Button _ children ->
      let (width, height) = getRowSize children
       in (width + 2, height + 2) -- +2 is for the border

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
