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

import qualified Char
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
   in go (initialFocus $view initialModel) initialModel

initialFocus :: CLI msg -> Maybe Focus
initialFocus widget =
  let childrenFocus children =
        List.indexedMap
          (\i child -> initialFocus child & Maybe.map (ChildFocus i))
          children &
        List.filterMap identity &
        List.head
   in case widget of
        Attributes _ child         -> initialFocus child
        Text _                     -> Nothing
        Border child               -> initialFocus child
        Input _ _ _                -> Just This
        Row children               -> childrenFocus children
        Column children            -> childrenFocus children
        LeftAlignedColumn children -> childrenFocus children

finalFocus :: CLI msg -> Maybe Focus
finalFocus widget =
  let childrenFocus children =
        List.indexedMap
          (\i child -> finalFocus child & Maybe.map (ChildFocus i))
          children &
        List.filterMap identity &
        List.reverse &
        List.head
   in case widget of
        Attributes _ child         -> finalFocus child
        Text _                     -> Nothing
        Border child               -> finalFocus child
        Input _ _ _                -> Just This
        Row children               -> childrenFocus children
        Column children            -> childrenFocus children
        LeftAlignedColumn children -> childrenFocus children

-- Returns Nothing to exit, Just msgs for messages
eventToMsgs ::
     CLI msg -> Maybe Focus -> Vty.Event -> Maybe (List msg, Maybe Focus)
eventToMsgs root focus (Vty.EvMouseUp x y _) =
  Just $ onClick (P.fromIntegral x) (P.fromIntegral y) root
eventToMsgs _ _ (Vty.EvKey Vty.KEsc []) = Nothing
eventToMsgs root (Just focus) (Vty.EvKey (Vty.KChar '\t') []) =
  Just
    ( []
    , case nextFocus root focus of
        Just f  -> Just f
        Nothing -> initialFocus root)
eventToMsgs root Nothing (Vty.EvKey (Vty.KChar '\t') []) =
  Just ([], initialFocus root)
eventToMsgs root (Just focus) (Vty.EvKey (Vty.KBackTab) []) =
  Just
    ( []
    , case previousFocus root focus of
        Just f  -> Just f
        Nothing -> finalFocus root)
eventToMsgs root Nothing (Vty.EvKey (Vty.KBackTab) []) =
  Just ([], finalFocus root)
eventToMsgs root (Just focus) (Vty.EvKey (Vty.KChar char) modifiers) =
  let char' =
        if List.any
             (\m ->
                case m of
                  Vty.MShift -> True
                  _          -> False)
             modifiers
          then Char.toUpper char
          else char
   in Just (onKeyUp char' root focus, Just focus)
eventToMsgs _ focus _ = Just ([], focus)

nextFocus :: CLI msg -> Focus -> Maybe Focus
nextFocus =
  let containerNextFocus children i focus =
        case children & List.drop i of
          [] -> Nothing
          (x:xs) ->
            case go x focus of
              Just focus' -> Just $ ChildFocus i $ focus'
              Nothing ->
                List.indexedMap
                  (\j e -> Maybe.map (ChildFocus $i + j + 1) $ initialFocus e)
                  xs &
                List.filterMap identity &
                List.head
      go (Border child) focus = go child focus
      go (Attributes _ child) focus = go child focus
      go (Input _ v onInput) This = Nothing
      go (Row children) (ChildFocus i cfocus) =
        containerNextFocus children i cfocus
      go (LeftAlignedColumn children) (ChildFocus i cfocus) =
        containerNextFocus children i cfocus
      go (Column children) (ChildFocus i cfocus) =
        containerNextFocus children i cfocus
      go (Text _) _ = Nothing
      go _ _ = Nothing
   in go

previousFocus :: CLI msg -> Focus -> Maybe Focus
previousFocus =
  let containerPreviousFocus children i focus =
        case List.take (i + 1) children & List.reverse of
          [] -> Nothing
          (x:xs) ->
            case go x focus of
              Just focus' -> Just $ ChildFocus i $ focus'
              Nothing ->
                List.indexedMap
                  (\j e -> Maybe.map (ChildFocus (i - j - 1)) $ finalFocus e)
                  xs &
                List.filterMap identity &
                List.head
      go (Border child) focus = go child focus
      go (Attributes _ child) focus = go child focus
      go (Input _ v onInput) This = Nothing
      go (Row children) (ChildFocus i cfocus) =
        containerPreviousFocus children i cfocus
      go (LeftAlignedColumn children) (ChildFocus i cfocus) =
        containerPreviousFocus children i cfocus
      go (Column children) (ChildFocus i cfocus) =
        containerPreviousFocus children i cfocus
      go (Text _) _ = Nothing
      go _ _ = Nothing
   in go

onKeyUp :: Char -> CLI msg -> Focus -> List msg
onKeyUp char =
  let containerKeyUp :: List (CLI msg) -> Int -> Focus -> List msg
      containerKeyUp children i cfocus =
        children & List.drop i & List.head &
        (\h ->
           case h of
             Just x  -> go x cfocus
             Nothing -> [])
      go (Border child) focus = go child focus
      go (Attributes _ child) focus = go child focus
      go (Input _ v onInput) This = [onInput $ v ++ String.fromList [char]]
      go (Row children) (ChildFocus i cfocus) = containerKeyUp children i cfocus
      go (LeftAlignedColumn children) (ChildFocus i cfocus) =
        containerKeyUp children i cfocus
      go (Column children) (ChildFocus i cfocus) =
        containerKeyUp children i cfocus
      go (Text _) _ = []
      go _ _ = []
   in go

getFocusPosition :: CLI msg -> Focus -> Maybe (Int, Int)
getFocusPosition =
  let containerFocus position children i cfocus =
        position children & List.drop i & List.head &
        Maybe.andThen
          (\((cx, cy), child) ->
             getFocusPosition child cfocus &
             Maybe.map (\(x, y) -> (x + cx, y + cy)))
      go (Border child) focus =
        Maybe.map (\(x, y) -> (x + 1, y + 1)) $ getFocusPosition child focus
      go (Text _) _ = Nothing
      go (Attributes _ child) focus = getFocusPosition child focus
      go (Input _ v _) This = Just (String.length v + 1, 1)
      go (Row children) (ChildFocus i cfocus) =
        containerFocus Layout.rowPositions children i cfocus
      go (LeftAlignedColumn children) (ChildFocus i cfocus) =
        containerFocus Layout.leftAlignedColumnPositions children i cfocus
      go (Column children) (ChildFocus i cfocus) =
        containerFocus Layout.columnPositions children i cfocus
      go _ _ = Nothing
   in go

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
          Row children ->
            containerClick ChildFocus $ Layout.rowPositions children
          LeftAlignedColumn children ->
            containerClick ChildFocus $
            Layout.leftAlignedColumnPositions children
          Column children ->
            containerClick ChildFocus $ Layout.columnPositions children
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
