{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE PackageImports  #-}

module CLI
  ( CLI
  , Program
  , attributes
  , border
  , button
  , column
  , row
  , run
  , run_
  , sandbox
  , text
  ) where

import           CLI.Attributes (Attribute (..))
import           Color          (Color)
import qualified Color
import           Curses         (Curses, Update)
import qualified Curses
import qualified List
import qualified Maybe
import           "base" Prelude (Monad (..), mapM_)
import qualified String
import qualified Tuple

--import           Dict            (Dict)
--import qualified Dict
--import           Result          (Result)
--import qualified Result
data CLI msg
  = Text String
  | Row (List (CLI msg))
  | Column (List (CLI msg))
  | Attributes (List (Attribute msg)) (CLI msg)
  | Border (CLI msg)

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
   in Curses.runCurses $ mainLoop view update model

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
            List.sum $
            List.map
              (\(_, b) ->
                 case b of
                   Curses.ButtonReleased      -> 1
                   Curses.ButtonClicked       -> 1
                   Curses.ButtonDoubleClicked -> 2
                   Curses.ButtonTripleClicked -> 3
                   _                          -> 0)
              (Curses.mouseButtons mouseState)
       in Just $ List.concat $ List.repeat clicks (onClick x y root)
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
    Column [] -> []
    Column (child:children) ->
      let (width, height) = getSize child
       in if y < height
            then if x < width
                   then onClick x y child
                   else []
            else if y == height
                   then []
                   else onClick x (y - height - 1) (Column children)
    Border child ->
      let (w, h) = getSize child
       in if x < (w + 2) && y < (h + 2)
            then onClick (x - 1) (y - 1) child
            else []
    Attributes as child ->
      let (w, h) = getSize child
       in if x < (w) && y < (h)
            then List.filterMap
                   (\attr ->
                      case attr of
                        OnClick msg -> Just msg
                        _           -> Nothing)
                   as
            else []

-- Displays a widget in the top left corner of the screen
-- and waits for an event
displayAndWait :: CLI msg -> Curses (Maybe Curses.Event)
displayAndWait root = do
  w <- Curses.defaultWindow
  defineColors Color.white Color.transparent root
  -- updateWindow prepares the drawing
  Curses.updateWindow w Color.white Color.transparent $ do
    Curses.moveCursor 0 0 -- Move the cursor in the top left corner
    Curses.clear
    displayWidget root
    Curses.moveCursor 0 0 -- Move the cursor in the top left corner, again
  -- Actually do the drawing on screen
  Curses.render
  -- Wait for an event. "Nothing" means it should wait forever
  Curses.catchCurses (Curses.getEvent w Nothing) (always $ return Nothing)

defineColors :: Color -> Color -> CLI msg -> Curses ()
defineColors fore back widget =
  case widget of
    Text _ -> Curses.defineColors (fore, back)
    Row children -> mapM_ (defineColors fore back) children
    Column children -> mapM_ (defineColors fore back) children
    Border child -> defineColors fore back child
    Attributes attrs child -> do
      let (fore', back') = getColorsFromAttributes fore back attrs
      Curses.defineColors (fore', back')
      defineColors fore' back' child

displayWidget :: CLI msg -> Update ()
displayWidget widget =
  case widget of
    Text s                 -> Curses.drawString s -- A piece of text is simply written
    Row children           -> displayRow children
    Column children        -> displayColumn children
    Border child           -> displayBorder child
    Attributes attrs child -> displayAttrs attrs child

getColorsFromAttributes ::
     Color -> Color -> List (Attribute msg) -> (Color, Color)
getColorsFromAttributes fore back attrs =
  let apply attr (f, b) =
        case attr of
          OnClick _     -> (f, b)
          Foreground f' -> (f', b)
          Background b' -> (f, b')
   in List.foldl apply (fore, back) attrs

displayAttrs :: List (Attribute msg) -> CLI msg -> Update ()
displayAttrs attrs child = do
  (fore, back) <- Curses.getColors
  let (fore', back') = getColorsFromAttributes fore back attrs
  Curses.setColors (fore', back')
  displayWidget child
  Curses.setColors (fore, back)

displayRow :: List (CLI msg) -> Update ()
displayRow children = do
  let sizes = List.map getSize children
  let maxHeight =
        sizes & List.map Tuple.second & List.maximum & Maybe.withDefault 0
  -- mapM_ is like List.map, but it's used for functions whose
  -- results are in a monad. It uses map to transform a List (Update a) into
  -- a Update (List a). The underscore is a convention meaning "ignore the result",
  -- so it becomes a Update ()
  mapM_
    (\(child, (width, height)) -> do
       (r, c) <- Curses.cursorPosition
       -- This is used to center vertically
       let vpad = (maxHeight - height) // 2
       Curses.moveCursor (r + vpad) c
       displayWidget child
       Curses.moveCursor r (c + width + 1))
    (List.map2 (,) children sizes)

displayColumn :: List (CLI msg) -> Update ()
displayColumn children = do
  let sizes = List.map getSize children
  let maxWidth =
        sizes & List.map Tuple.first & List.maximum & Maybe.withDefault 0
  -- mapM_ is like List.map, but it's used for functions whose
  -- results are in a monad. It uses map to transform a List (Update a) into
  -- a Update (List a). The underscore is a convention meaning "ignore the result",
  -- so it becomes a Update ()
  mapM_
    (\(child, (width, height)) -> do
       (r, c) <- Curses.cursorPosition
        -- This is used to center horizontally
       let hpad = (maxWidth - width) // 2
       Curses.moveCursor r (c + hpad)
       displayWidget child
       Curses.moveCursor (r + height + 1) c)
    (List.map2 (,) children sizes)

displayBorder :: CLI msg -> Update ()
displayBorder child = do
  let (width, height) = getSize child
  (r, c) <- Curses.cursorPosition
  displayBox width height
  Curses.moveCursor (r + 1) (c + 1)
  displayWidget child

-- Draws a box
displayBox :: Int -> Int -> Update ()
displayBox width height = do
  (r, c) <- Curses.cursorPosition
  Curses.drawGlyph Curses.glyphCornerUL
  Curses.moveCursor r (c + 1)
  Curses.drawLineH Curses.glyphLineH width
  Curses.moveCursor r (c + width + 1)
  Curses.drawGlyph Curses.glyphCornerUR
  Curses.moveCursor (r + 1) c
  Curses.drawLineV Curses.glyphLineV height
  Curses.moveCursor (r + height + 1) c
  Curses.drawGlyph Curses.glyphCornerLL
  Curses.drawLineH Curses.glyphLineH width
  Curses.moveCursor (r + height + 1) (c + width + 1)
  Curses.drawGlyph Curses.glyphCornerLR
  Curses.moveCursor (r + 1) (c + width + 1)
  Curses.drawLineV Curses.glyphLineV height

-- Get the size of a row of widgets
getRowSize :: List (CLI msg) -> (Int, Int)
getRowSize [] = (0, 0)
getRowSize children =
  let sizes = List.map getSize children
      gapsWidth = List.length children - 1
      widgetsWidth = List.sum $ List.map (Tuple.first) sizes
      width = gapsWidth + widgetsWidth
      height =
        sizes & List.map Tuple.second & List.maximum & Maybe.withDefault 0
   in (width, height)

-- Get the size of a column of widgets
getColumnSize :: List (CLI msg) -> (Int, Int)
getColumnSize [] = (0, 0)
getColumnSize children =
  let sizes = List.map getSize children
      gapsHeight = List.length children - 1
      widgetsHeight = List.sum $ List.map (Tuple.second) sizes
      height = gapsHeight + widgetsHeight
      width = sizes & List.map Tuple.first & List.maximum & Maybe.withDefault 0
   in (width, height)

-- Get the size of a widget
getSize :: CLI msg -> (Int, Int)
getSize widget =
  case widget of
    Text s -> (String.length s, 1) -- Text is shown with no wrapping
    Row children -> getRowSize children
    Column children -> getColumnSize children
    Border child ->
      let (width, height) = getSize child
       in (width + 2, height + 2) -- +2 is for the border
    Attributes _ child -> getSize child

sandbox ::
     model
  -> (model -> CLI msg)
  -> (msg -> model -> model)
  -> Program () model msg
sandbox init view update =
  let init' _ = init
      update' msg model = (update msg model, return [])
   in Program init' view update'

attributes :: List (Attribute msg) -> CLI msg -> CLI msg
attributes = Attributes

button :: List (Attribute msg) -> CLI msg -> CLI msg
button attrs = Attributes attrs . Border

border :: CLI msg -> CLI msg
border = Border

column :: List (CLI msg) -> CLI msg
column = Column

row :: List (CLI msg) -> CLI msg
row = Row

text :: String -> CLI msg
text = Text
