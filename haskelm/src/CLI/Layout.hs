{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE PackageImports  #-}

module CLI.Layout
  ( columnPositions
  , display
  , getSize
  , leftAlignedColumnPositions
  , rowPositions
  ) where

import           CLI.Types       (Attribute (..), CLI (..), InputType (..))
import           Color           (Color)
import qualified Color
import           Graphics.Vty    (Attr, Image, Picture, Vty)
import qualified Graphics.Vty    as Vty
import qualified List
import qualified Maybe
import qualified "base" Prelude  as P
import qualified String
import qualified String.Internal
import qualified Tuple

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
       in displayBorder attr $
          Text $ String.padRight 10 ' ' displayString ++ " "

getSize :: CLI msg -> (Int, Int)
getSize widget = imageSize $ displayWidget Vty.defAttr widget

imageSize :: Image -> (Int, Int)
imageSize image =
  ( P.fromIntegral $ Vty.imageWidth image
  , P.fromIntegral $ Vty.imageHeight image)

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