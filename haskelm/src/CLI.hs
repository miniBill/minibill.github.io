{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitPrelude   #-}
{-# LANGUAGE PackageImports    #-}

module CLI
  ( CLI
  , InputType(..)
  , Program(..)
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

import           CLI.Attributes     (Attribute (..))
import qualified CLI.Layout         as Layout
import           CLI.Types          (Attribute (..), CLI (..), InputType (..),
                                     Program (..), attributes, border, button,
                                     column, input, leftAlignedColumn, row,
                                     text)
import           CLI.Types.Internal (Focus (..))
import           Color              (Color)
import qualified Color
import qualified Debug.Trace        as Debug
import           Graphics.Vty       (Attr, Image, Picture, Vty)
import qualified Graphics.Vty       as Vty
import qualified List
import qualified Maybe
import           "base" Prelude     (Monad (..))
import qualified "base" Prelude     as P
import qualified String
import qualified String.Internal
import qualified Tuple

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
        Vty.update vty $ Layout.display root
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
            RowChild i cfocus ->
              containerFocus Layout.rowPositions children i cfocus
            _ -> Nothing
        LeftAlignedColumn children ->
          case focus of
            ColumnChild i cfocus ->
              containerFocus Layout.leftAlignedColumnPositions children i cfocus
            _ -> Nothing
        Column children ->
          case focus of
            ColumnChild i cfocus ->
              containerFocus Layout.columnPositions children i cfocus
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
                   let (cwidth, cheight) = Layout.getSize child
                    in cx <= relx &&
                       relx < cx + cwidth && cy <= rely && rely < cy + cheight) &
              List.head
            (msgs, focus) =
              case found of
                Nothing -> ([], Nothing)
                Just (i, (cx, cy), child) ->
                  onClick (relx - cx) (rely - cy) child &
                  Tuple.mapSecond (Maybe.map $ mapper i)
         in (msgs, focus)
      (msgs, focus) =
        case root of
          Text _ -> ([], Nothing)
          Row children -> containerClick RowChild $ Layout.rowPositions children
          LeftAlignedColumn children ->
            containerClick ColumnChild $
            Layout.leftAlignedColumnPositions children
          Column children ->
            containerClick ColumnChild $ Layout.columnPositions children
          Border child ->
            let (w, h) = Layout.getSize child
             in if relx < (w + 2) && rely < (h + 2)
                  then onClick (relx - 1) (rely - 1) child
                  else ([], Nothing)
          Input _ v _ ->
            let (w, h) = (max 10 $ String.length v + 1, 1)
             in if relx < (w + 2) && rely < (h + 2)
                  then ([], Just This)
                  else ([], Nothing)
          Attributes as child ->
            let (w, h) = Layout.getSize child
             in if relx < w && rely < h
                  then ( List.filterMap
                           (\attr ->
                              case attr of
                                OnClick msg -> Just msg
                                _           -> Nothing)
                           as
                       , Just This)
                  else ([], Nothing)
   in (msgs, focus)

sandbox ::
     model
  -> (model -> CLI msg)
  -> (msg -> model -> model)
  -> Program () model msg
sandbox init view update =
  let init' _ = init
      update' msg model = (update msg model, return [])
   in Program init' view update'
