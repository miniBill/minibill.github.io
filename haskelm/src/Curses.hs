{-# LANGUAGE PackageImports #-}

module Curses
  ( ButtonState(..)
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
  , defineColors
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
import qualified GHC.Int
import qualified List
import           Prelude
import           "base" Prelude  (return)
import qualified "base" Prelude  as P
import           Result          (Result (..))
import qualified String
import qualified String.Internal
import           UI.NCurses      (ButtonState (..), CursesException, Event (..),
                                  Glyph, MouseState (..), Window, glyphCornerLL,
                                  glyphCornerLR, glyphCornerUL, glyphCornerUR,
                                  glyphLineH, glyphLineV)
import qualified UI.NCurses      as NCurses

data ColorState =
  ColorState
    (List ((Int, Int, Int), GHC.Int.Int16)) -- Defined colors
    (List ((NCurses.Color, NCurses.Color), NCurses.ColorID)) -- Defined ColorIDs

newtype Curses a =
  Curses
    { unwrapCurses :: ColorState -> NCurses.Curses (ColorState, a)
    }

liftCurses :: NCurses.Curses a -> Curses a
liftCurses op = Curses $ \state -> P.fmap (\r -> (state, r)) op

defaultWindow :: Curses Window
defaultWindow = liftCurses NCurses.defaultWindow

catchCurses :: Curses a -> (CursesException -> Curses a) -> Curses a
catchCurses op handler =
  Curses $ \state -> do
    let raw = unwrapCurses op state
    let rawHandler ex = unwrapCurses (handler ex) state
    NCurses.catchCurses raw rawHandler

getEvent :: Window -> Maybe Int -> Curses (Maybe Event)
getEvent window timeout = liftCurses $ NCurses.getEvent window timeout

render :: Curses ()
render = liftCurses NCurses.render

runCurses :: Curses a -> IO a
runCurses op = do
  (_, r) <- NCurses.runCurses $ unwrapCurses op $ ColorState [] []
  return r

newtype Update a =
  Update
    { unwrapUpdate :: (ColorState, Color, Color) -- This is the initial state
                       -> NCurses.Update --This is the wrapped monad
                           (Result String -- This will allow to manage failures
                             ( Color -- This is the final foreground color
                             , Color -- This is the final background color
                             , a -- This is the result
                              ))
    }

instance P.Functor Update where
  fmap = Control.Monad.liftM

instance P.Applicative Update where
  pure = liftUpdate . P.pure
  (<*>) = Control.Monad.ap

instance P.Monad Update where
  x >>= g =
    Update $ \(colorState, f, b) -> do
      u <- unwrapUpdate x (colorState, f, b)
      case u of
        Ok (f', b', x') -> unwrapUpdate (g x') (colorState, f', b')
        Err err         -> return $ Err err

fail :: String -> Update a
fail msg = Update $ \_ -> return $ Err msg

instance P.Functor Curses where
  fmap = Control.Monad.liftM

instance P.Applicative Curses where
  pure = liftCurses . P.pure
  (<*>) = Control.Monad.ap

instance P.Monad Curses where
  x >>= f =
    Curses $ \state -> do
      (state', x') <- unwrapCurses x state
      unwrapCurses (f x') state'

liftUpdate :: NCurses.Update a -> Update a
liftUpdate op = Update $ \(_, f, b) -> P.fmap (\v -> Ok (f, b, v)) op

moveCursor :: Int -> Int -> Update ()
moveCursor r c = liftUpdate $ NCurses.moveCursor r c

drawLineV :: Glyph -> Int -> Update ()
drawLineV g w = liftUpdate $ NCurses.drawLineV (Just g) w

drawLineH :: Glyph -> Int -> Update ()
drawLineH g h = liftUpdate $ NCurses.drawLineH (Just g) h

drawGlyph :: Glyph -> Update ()
drawGlyph g = liftUpdate $ NCurses.drawGlyph g

cursorPosition :: Update (Int, Int)
cursorPosition = liftUpdate NCurses.cursorPosition

clear :: Update ()
clear = liftUpdate NCurses.clear

drawString :: String -> Update ()
drawString = liftUpdate . NCurses.drawString . String.Internal.unpack

updateWindow :: Window -> Color -> Color -> Update () -> Curses ()
updateWindow window f b update = do
  defineColors (f, b)
  Curses $ \state -> do
    NCurses.updateWindow window $ do
      r <- unwrapUpdate update (state, f, b)
      case r of
        Ok _ -> return ()
        Err msg -> do
          NCurses.clear
          NCurses.moveCursor 0 0
          NCurses.drawString $ String.Internal.unpack msg
          return ()
    return (state, ())

getColors :: Update (Color, Color)
getColors = Update $ \(_, f, b) -> return $ Ok (f, b, (f, b))

setColors :: (Color, Color) -> Update ()
setColors (f, b) =
  let findColor :: ColorState -> Color -> Update NCurses.Color
      findColor (ColorState colors _) c =
        case normalizeColor c of
          Nothing -> return NCurses.ColorDefault
          Just (r1k, g1k, b1k) ->
            case find (\(p, _) -> p == (r1k, g1k, b1k)) colors of
              Nothing ->
                fail $
                String.concat
                  [ "Color not defined: "
                  , String.fromInt r1k
                  , " "
                  , String.fromInt g1k
                  , " "
                  , String.fromInt b1k
                  ]
              Just (_, id) -> return $ NCurses.Color id
      findColorID ::
           ColorState
        -> NCurses.Color
        -> NCurses.Color
        -> Update NCurses.ColorID
      findColorID (ColorState colors colorIDs) fore back =
        case find (\(p, _) -> (P.==) p (fore, back)) colorIDs of
          Nothing ->
            fail $
            String.concat
              [ "Color pair not defined: "
              , String.fromList $ P.show fore
              , " ("
              , Color.toHex f
              , ") "
              , String.fromList $ P.show back
              , " ("
              , Color.toHex b
              , "); defined colors: "
              , String.fromList $ P.show colors
              , "; defined IDs: "
              , String.fromList $ P.show colorIDs
              ]
          Just (_, id) -> return id
      get :: Update ColorState
      get = Update $ \(state, fore, back) -> return $ Ok (fore, back, state)
   in do state <- get
         f' <- findColor state f
         b' <- findColor state b
         id <- findColorID state f' b'
         liftUpdate $ NCurses.setColor id
         Update $ \_ -> return $ Ok (f, b, ())

normalizeColor :: Color -> Maybe (Int, Int, Int)
normalizeColor color =
  let (r, g, b, a) = Color.toRgba color
      r1k = round $ r * 1000
      g1k = round $ g * 1000
      b1k = round $ b * 1000
   in if a == 0
        then Nothing
        else Just (r1k, g1k, b1k)

find :: (a -> Bool) -> List a -> Maybe a
find f = List.head . List.filter f

defineColors :: (Color, Color) -> Curses ()
defineColors (foreground, background) =
  let getOrInsertColor :: Color -> Curses NCurses.Color
      getOrInsertColor color =
        Curses $ \(ColorState colors colorIDs) -> do
          ncolor <-
            case normalizeColor color of
              Just (r, g, b) ->
                case find (\(p, _) -> p == (r, g, b)) colors of
                  Nothing -> do
                    maxColor <- NCurses.maxColor
                    let id = P.fromIntegral $ maxColor - List.length colors
                    NCurses.defineColor (NCurses.Color id) r g b
                    let colors' = colors ++ [((r, g, b), id)]
                    return (ColorState colors' colorIDs, NCurses.Color id)
                  Just (_, id) ->
                    return (ColorState colors colorIDs, NCurses.Color id)
              Nothing ->
                return (ColorState colors colorIDs, NCurses.ColorDefault)
          return ncolor
      getOrInsertColorID ::
           (NCurses.Color, NCurses.Color) -> Curses NCurses.ColorID
      getOrInsertColorID (f, b) =
        Curses $ \(ColorState colors colorIDs) -> do
          case find (\(p, _) -> (P.==) p (f, b)) colorIDs of
            Nothing -> do
              maxColorID <- NCurses.maxColor
              let newID = maxColorID - List.length colorIDs
              colorID <- NCurses.newColorID f b newID
              let colorIDs' = colorIDs ++ [((f, b), colorID)]
              return (ColorState colors colorIDs', colorID)
            Just (_, colorID) -> return (ColorState colors colorIDs, colorID)
   in do f <- getOrInsertColor foreground
         b <- getOrInsertColor background
         _ <- getOrInsertColorID (f, b)
         return ()
