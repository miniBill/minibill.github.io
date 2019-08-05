{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitPrelude   #-}
{-# LANGUAGE PackageImports    #-}

module CLI
  ( CLI
  , InputType(..)
  , Program
  , attributes
  , border
  , button
  , column
  , input
  , leftAlignedColumn
  , row
  , run
  , run_
  , sandbox
  , text
  ) where

import           CLI.Attributes  (Attribute (..))
import           Color           (Color)
import qualified Color
import qualified Debug.Trace     as Debug
import           Graphics.Vty    (Attr, Image, Picture, Vty)
import qualified Graphics.Vty    as Vty
import qualified List
import qualified Maybe
import           "base" Prelude  (Monad (..))
import qualified "base" Prelude  as P
import qualified String
import qualified String.Internal
import qualified Tuple

trace :: String -> a -> a
trace msg x =
  if False
    then Debug.trace (String.Internal.unpack msg) x
    else x

data InputType
  = TypeText
  | TypePassword
  deriving (P.Show)

data CLI msg
  = Text String
  | Row (List (CLI msg))
  | LeftAlignedColumn (List (CLI msg))
  | Column (List (CLI msg))
  | Attributes (List (Attribute msg)) (CLI msg)
  | Border (CLI msg)
  | Input InputType String (String -> msg)
  deriving (P.Show)

instance P.Show (String -> msg) where
  show _ = ['[', '-', '>', ']']

instance P.Show (Attribute msg) where
  show _ = ['[', '?', ']']

data Focus
  = RowChild Int Focus
  | ColumnChild Int Focus
  | This
  deriving (P.Show)

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
mainLoop vty view update initialModel =
  let go focus model = do
        let root = view model
        Vty.update vty $ display root
        case Maybe.andThen (getFocusPosition root) focus of
          Just (r, c) -> do
            Vty.showCursor $ Vty.outputIface vty
            Vty.setCursorPos
              (Vty.outputIface vty)
              (P.fromIntegral r)
              (P.fromIntegral c)
          Nothing -> Vty.hideCursor $ Vty.outputIface vty
        event <- Vty.nextEvent vty
        let maybeMsgs = eventToMsgs root focus event
        case maybeMsgs of
          Nothing -> return () -- Exit
          Just (msgs, focus') -> do
            let (model', _) = List.foldl step (model, []) msgs
            go focus' model'
      step msg (mod, cmds) =
        let (mod', cmd) = update msg mod
         in (mod', cmd : cmds)
      initialFocus widget =
        case widget of
          Attributes _ child -> initialFocus child
          Text _ -> Nothing
          Border child -> initialFocus child
          Input _ _ _ -> Just This
          Row children ->
            List.indexedMap
              (\i child -> initialFocus child & Maybe.map (RowChild i))
              children &
            List.filterMap (\a -> a) &
            List.head
          Column children ->
            List.indexedMap
              (\i child -> initialFocus child & Maybe.map (ColumnChild i))
              children &
            List.filterMap (\a -> a) &
            List.head
          LeftAlignedColumn children ->
            List.indexedMap
              (\i child -> initialFocus child & Maybe.map (ColumnChild i))
              children &
            List.filterMap (\a -> a) &
            List.head
   in go (initialFocus $view initialModel) initialModel

-- Returns Nothing to exit, Just msgs for messages
eventToMsgs ::
     CLI msg -> Maybe Focus -> Vty.Event -> Maybe (List msg, Maybe Focus)
eventToMsgs root focus event =
  case event of
    Vty.EvMouseUp x y _ ->
      Just $ onClick (P.fromIntegral x) (P.fromIntegral y) root
    Vty.EvKey Vty.KEsc [] -> Nothing
    _ -> Just ([], focus)

getFocusPosition :: CLI msg -> Focus -> Maybe (Int, Int)
getFocusPosition widget focus =
  let containerFocus position children i cfocus =
        position children & List.drop i & List.head &
        Maybe.andThen
          (\((cx, cy), child) ->
             getFocusPosition child cfocus &
             Maybe.map (\(x, y) -> (x + cx, y + cy)))
   in case widget of
        Border child ->
          Maybe.map (\(x, y) -> (x + 1, y + 1)) $ getFocusPosition child focus
        Text _ -> Nothing
        Attributes _ child -> getFocusPosition child focus
        Input _ v _ ->
          case focus of
            This -> Just (String.length v + 1, 1)
            _    -> Nothing
        Row children ->
          case focus of
            RowChild i cfocus -> containerFocus rowPositions children i cfocus
            _ -> Nothing
        LeftAlignedColumn children ->
          case focus of
            ColumnChild i cfocus ->
              containerFocus leftAlignedColumnPositions children i cfocus
            _ -> Nothing
        Column children ->
          case focus of
            ColumnChild i cfocus ->
              containerFocus columnPositions children i cfocus
            _ -> Nothing

onClick :: Int -> Int -> CLI msg -> (List msg, Maybe Focus)
onClick relx rely root =
  let containerClick ::
           (Int -> Focus -> Focus)
        -> List ((Int, Int), CLI msg)
        -> (List msg, Maybe Focus)
      containerClick mapper positions =
        let found =
              positions & List.indexedMap (\i (pos, child) -> (i, pos, child)) &
              List.filter
                (\(_, (cx, cy), child) ->
                   let (cwidth, cheight) = getSize child
                    in cx <= relx &&
                       relx < cx + cwidth && cy <= rely && rely < cy + cheight) &
              List.head
            (msgs, focus) =
              case found of
                Nothing -> ([], Nothing)
                Just (i, (cx, cy), child) ->
                  onClick (relx - cx) (rely - cy) child &
                  Tuple.mapSecond (Maybe.map $ mapper i)
         in trace
              ("containerClick [->] " ++
               String.fromList (P.show positions) ++
               " -> " ++ String.fromList (P.show focus))
              (msgs, focus)
      (msgs, focus) =
        case root of
          Text _ -> ([], Nothing)
          Row children -> containerClick RowChild $ rowPositions children
          LeftAlignedColumn children ->
            containerClick ColumnChild $ leftAlignedColumnPositions children
          Column children ->
            containerClick ColumnChild $ columnPositions children
          Border child ->
            let (w, h) = getSize child
             in if relx < (w + 2) && rely < (h + 2)
                  then onClick (relx - 1) (rely - 1) child
                  else ([], Nothing)
          Input _ v _ ->
            let (w, h) = (max 10 $ String.length v + 1, 1)
             in if relx < (w + 2) && rely < (h + 2)
                  then ([], Just This)
                  else ([], Nothing)
          Attributes as child ->
            let (w, h) = getSize child
             in if relx < w && rely < h
                  then ( List.filterMap
                           (\attr ->
                              case attr of
                                OnClick msg -> Just msg
                                _           -> Nothing)
                           as
                       , Just This)
                  else ([], Nothing)
   in trace
        ("onClick " ++
         String.fromInt relx ++
         " " ++
         String.fromInt rely ++
         " " ++
         String.fromList (P.show root) ++
         " -> " ++ String.fromList (P.show focus))
        (msgs, focus)

leftAlignedColumnPositions :: List (CLI msg) -> List ((Int, Int), CLI msg)
leftAlignedColumnPositions children =
  children &
  List.foldl
    (\child (y, acc) ->
       let (_, ch) = getSize child
        in (y + ch + 1, ((0, y), child) : acc))
    (0, []) &
  Tuple.second &
  List.reverse

columnPositions :: List (CLI msg) -> List ((Int, Int), CLI msg)
columnPositions children =
  let maxWidth =
        children & List.map getSize & List.map Tuple.first & List.maximum &
        Maybe.withDefault 0
   in children &
      List.foldl
        (\child (y, acc) ->
           let (cw, ch) = getSize child
            in (y + ch + 1, (((maxWidth - cw) // 2, y), child) : acc))
        (0, []) &
      Tuple.second &
      List.reverse

rowPositions :: List (CLI msg) -> List ((Int, Int), CLI msg)
rowPositions children =
  let maxHeight =
        children & List.map getSize & List.map Tuple.second & List.maximum &
        Maybe.withDefault 0
   in children &
      List.foldl
        (\child (x, acc) ->
           let (cw, ch) = getSize child
            in (x + cw + 1, ((x, (maxHeight - ch) // 2), child) : acc))
        (0, []) &
      Tuple.second &
      List.reverse

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
    Text s -> Vty.text' attr $ String.Internal.raw s -- A piece of text is simply written
    Row children -> displayRow attr children
    Column children -> displayColumn attr children
    LeftAlignedColumn children -> displayLeftAlignedColumn attr children
    Border child -> displayBorder attr child
    Attributes attrs child -> displayAttrs attr attrs child
    Input t v _ ->
      let displayString =
            case t of
              TypeText -> v
              TypePassword ->
                String.fromList $ List.repeat (String.length v) '*'
       in displayBorder attr $ Text $ String.padRight 10 ' ' displayString ++ " "

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

displayLeftAlignedColumn :: Attr -> List (CLI msg) -> Image
displayLeftAlignedColumn attr children =
  Vty.vertCat $
  List.map (displayWidget attr) $ List.intersperse (Text " ") children

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

leftAlignedColumn :: List (CLI msg) -> CLI msg
leftAlignedColumn = LeftAlignedColumn

column :: List (CLI msg) -> CLI msg
column = Column

input :: InputType -> String -> (String -> msg) -> CLI msg
input = Input

row :: List (CLI msg) -> CLI msg
row = Row

text :: String -> CLI msg
text = Text
