{-# LANGUAGE PackageImports #-}

module Curses
  ( ButtonState(..)
  , ColorID
  , Curses
  , CursesException
  , Event(..)
  , Glyph
  , MouseState(..)
  , Update
  , Window
  , catchCurses
  , clear
  , cursorPosition
  , defaultWindow
  , drawGlyph
  , drawLineH
  , drawLineV
  , drawString
  , getColors
  , getEvent
  , glyphCornerLL
  , glyphCornerLR
  , glyphCornerUL
  , glyphCornerUR
  , glyphLineH
  , glyphLineV
  , moveCursor
  , render
  , runCurses
  , setColors
  , updateWindow
  ) where

import           Color           (Color)
import qualified Color
import qualified Control.Monad
import           Dict            (Dict)
import qualified Dict
import qualified Maybe
import           Prelude
import           "base" Prelude  (return)
import qualified "base" Prelude  as P
import           Result          (Result (..))
import qualified String.Internal
import           UI.NCurses      (ButtonState (..), ColorID, Curses,
                                  CursesException, Event (..), Glyph,
                                  MouseState (..), Window, catchCurses,
                                  defaultWindow, getEvent, glyphCornerLL,
                                  glyphCornerLR, glyphCornerUL, glyphCornerUR,
                                  glyphLineH, glyphLineV, render, runCurses)
import qualified UI.NCurses      as NCurses

--import qualified Result
data UpdateState =
  UpdateState
    (BiDict Int Color) -- Defined colors
    (BiDict Int (NCurses.Color, NCurses.Color)) -- Defined ColorIDs
    Int -- Current ColorID

data BiDict k v =
  BiDict (Dict k v) (Dict v k)

emptyBiDict :: BiDict k v
emptyBiDict = BiDict Dict.empty Dict.empty

newtype Update a =
  Update
    { runUpdate :: UpdateState -> NCurses.Update (Result String (UpdateState, a))
    }

instance P.Functor Update where
  fmap = Control.Monad.liftM

instance P.Applicative Update where
  pure = lift . P.pure
  (<*>) = Control.Monad.ap

instance P.Monad Update where
  x >>= f =
    Update $ \state -> do
      y <- runUpdate x state
      case y of
        Ok (state', x') -> runUpdate (f x') state'
        Err err         -> return $ Err err

lift :: NCurses.Update a -> Update a
lift op = Update $ \state -> P.fmap (\v -> Ok (state, v)) op

lift1 :: (a -> NCurses.Update b) -> a -> Update b
lift1 f x = lift (f x)

lift2 :: (a -> b -> NCurses.Update c) -> a -> b -> Update c
lift2 f x y = lift (f x y)

get :: Update UpdateState
get = Update $ \state -> return (Ok (state, state))

moveCursor :: Int -> Int -> Update ()
moveCursor = lift2 NCurses.moveCursor

drawLineV :: Glyph -> Int -> Update ()
drawLineV g = lift1 (NCurses.drawLineV $ Just g)

drawLineH :: Glyph -> Int -> Update ()
drawLineH g = lift1 (NCurses.drawLineH $ Just g)

drawGlyph :: Glyph -> Update ()
drawGlyph = lift1 NCurses.drawGlyph

cursorPosition :: Update (Int, Int)
cursorPosition = lift NCurses.cursorPosition

clear :: Update ()
clear = lift NCurses.clear

drawString :: String -> Update ()
drawString = lift . NCurses.drawString . String.Internal.unpack

updateWindow :: Window -> Update () -> Curses ()
updateWindow window update = do
  ci <- NCurses.newColorID NCurses.ColorWhite NCurses.ColorDefault 1
  NCurses.updateWindow window $ do
    let initialState = UpdateState emptyBiDict emptyBiDict 1
    NCurses.setColor ci
    r <- runUpdate update initialState
    case r of
      Ok (_, x) -> return x
      Err msg -> do
        NCurses.clear
        NCurses.moveCursor 0 0
        NCurses.drawString $ String.Internal.unpack msg
        return ()

getColors :: Update (Color, Color)
getColors = do
  UpdateState (BiDict colors _) (BiDict colorIDs _) currentColorId <- get
  let getColor r =
        case r of
          NCurses.Color id   -> Dict.get (P.fromIntegral id) colors
          NCurses.ColorWhite -> Just Color.white
          NCurses.ColorBlack -> Just Color.black
          _                  -> Nothing -- TODO
  Dict.get currentColorId colorIDs &
    Maybe.andThen (\(f, b) -> Maybe.map2 (,) (getColor f) (getColor b)) &
    Maybe.withDefault (Color.white, Color.black) &
    return

setColors :: (Color, Color) -> Update ()
setColors _ = return () -- TODO
