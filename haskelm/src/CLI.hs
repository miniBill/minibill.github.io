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

import           CLI.Attributes  (Attribute (..))
import           Color           (Color)
import qualified Color
import           Graphics.Vty    (Attr, Image, Picture, Vty)
import qualified Graphics.Vty    as Vty
import qualified List
import           "base" Prelude  (Monad (..))
import qualified "base" Prelude  as P
import qualified String.Internal

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
   in do cfg <- Vty.standardIOConfig
         vty <- Vty.mkVty $ cfg {Vty.mouseMode = Just True}
         mainLoop vty view update model
         Vty.shutdown vty

mainLoop ::
     Vty
  -> (model -> CLI msg)
  -> (msg -> model -> (model, IO (List msg)))
  -> model
  -> IO ()
mainLoop vty view update =
  let go model = do
        let root = view model
        Vty.update vty $ display root
        event <- Vty.nextEvent vty
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
eventToMsgs :: CLI msg -> Vty.Event -> Maybe (List msg)
eventToMsgs root event =
  case event of
    Vty.EvMouseUp x y _ ->
      Just $ onClick (P.fromIntegral x) (P.fromIntegral y) root
    Vty.EvKey Vty.KEsc [] -> Nothing
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

imageSize :: Image -> (Int, Int)
imageSize image =
  ( P.fromIntegral $ Vty.imageWidth image
  , P.fromIntegral $ Vty.imageHeight image)

getSize :: CLI msg -> (Int, Int)
getSize widget = imageSize $ displayWidget Vty.defAttr widget

-- Displays a widget in the top left corner of the screen
-- and waits for an event
display :: CLI msg -> Picture
display = Vty.picForImage . displayWidget Vty.defAttr

displayWidget :: Attr -> CLI msg -> Image
displayWidget attr widget =
  case widget of
    Text s                 -> Vty.text' attr $ String.Internal.raw s -- A piece of text is simply written
    Row children           -> displayRow attr children
    Column children        -> displayColumn attr children
    Border child           -> displayBorder attr child
    Attributes attrs child -> displayAttrs attr attrs child

displayRow :: Attr -> List (CLI msg) -> Image
displayRow attr children =
  let raw = Vty.horizCat $ List.map (displayWidget attr) children
      (_, height) = imageSize raw
      displayPadded child =
        let childPicture = displayWidget attr child
            (cwidth, cheight) = imageSize childPicture
            missing = (height - cheight) // 2
            tpad = Vty.charFill attr ' ' cwidth missing
            bpad = Vty.charFill attr ' ' cwidth (height - cheight - missing)
         in Vty.vertCat [tpad, childPicture, bpad]
   in Vty.horizCat $
      List.map displayPadded $ List.intersperse (Text " ") children

displayColumn :: Attr -> List (CLI msg) -> Image
displayColumn attr children =
  let raw = Vty.vertCat $ List.map (displayWidget attr) children
      (width, _) = imageSize raw
      displayPadded child =
        let childPicture = displayWidget attr child
            (cwidth, cheight) = imageSize childPicture
            missing = (width - cwidth) // 2
            lpad = Vty.charFill attr ' ' missing cheight
            rpad = Vty.charFill attr ' ' (width - cwidth - missing) cheight
         in Vty.horizCat [lpad, childPicture, rpad]
   in Vty.vertCat $
      List.map displayPadded $ List.intersperse (Text " ") children

toVtyColor :: Color -> Vty.Color
toVtyColor c =
  let (r, g, b, _) = Color.toRgba c
   in Vty.rgbColor (round $ r * 255) (round $ g * 255) (round $ b * 255)

displayAttrs :: Attr -> List (Attribute msg) -> CLI msg -> Image
displayAttrs attr attrs child =
  let attr' = List.foldr step attr attrs
      step :: Attribute msg -> Attr -> Attr
      step (OnClick _) a    = a
      step (Foreground f) a = Vty.withForeColor a $ toVtyColor f
      step (Background b) a = Vty.withBackColor a $ toVtyColor b
   in displayWidget attr' child

displayBorder :: Attr -> CLI msg -> Image
displayBorder attr child =
  let (width, height) = getSize child
      char = Vty.char attr
      hline c w = Vty.charFill attr c w 1
      vline c h = Vty.charFill attr c 1 h
   in grid
        [ [char '┌', hline '─' width, char '┐']
        , [vline '│' height, displayWidget attr child, vline '│' height]
        , [char '└', hline '─' width, char '┘']
        ]

grid :: List (List Image) -> Image
grid = Vty.vertCat . List.map Vty.horizCat

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
