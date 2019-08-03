---
title: "HaskElm #3: egyptian (n)curses"
tags: elm haskell
---

## Introduction ##
HaskElm is a project in which I try and rewrite a Prelude for Haskell that feels like Elm's Prelude.

This set of posts is aimed at people with some experience in Elm (having read the guide should be enough) and no experience in Haskell.
It is structured as a log of my explorations, rather than a tutorial or an howto, so it will have warts, false ends and errors (although I'll cut some of the uninteresting ones).

In [part II](/2019-08-01-haskelm-2-don-t-put-that-on-record) we actually managed to put something on the screen!

In this episode we'll start handling events.

## Messages ##
To handle messages we need two things:
1. detecting mouse clicks,
2. transforming clicks into `msg`s and feeding them to `update`.

For working with the screen and mouse we'll use the `ncurses` package.

This will also let us block on events instead of looping and consuming CPU cycles.

### ncurses ###
```bash
$ sudo apt install c2hs
$ cabal install ncurses
```

`src/Protolude.hs`:
```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module Protolude
  ( Appendable(..)
  , Bool(..)
  , Equatable(..)
  , Int
  , IO
  , List
  , Maybe(..)
  , Number(..)
  , Comparable(..)
  , Order(..)
  , String
  , (<|)
  , (|>)
  , (<<)
  , (>>)
  , (//)
  , max
  , not
  ) where

import           "base" Prelude (Bool, IO, Maybe, String, not)
import qualified "base" Prelude as P

type Int = P.Integer

type List a = [a]

infixr 0 <|

(<|) :: (a -> b) -> a -> b
f <| x = f x

infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x

infixr 0 <<

(<<) :: (b -> c) -> (a -> b) -> a -> c
(<<) f g x = f (g x)

infixl 0 >>

(>>) :: (a -> b) -> (b -> c) -> a -> c
(>>) g f x = f (g x)

identity :: a -> a
identity x = x

class Appendable a where
  (++) :: a -> a -> a

instance Appendable [a] where
  (++) = (P.++)

class Number a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  fromInteger :: P.Integer -> a

instance Number P.Integer where
  (+) = (P.+)
  (-) = (P.-)
  fromInteger = identity

(//) :: Int -> Int -> Int
(//) = (P.div)

class Equatable a where
  (==) :: a -> a -> Bool

class Equatable a =>
      Comparable a
  where
  compare :: a -> a -> Order

instance Equatable P.Integer where
  (==) = (P.==)

instance Comparable P.Integer where
  compare l r =
    if l == r
      then EQ
      else if (P.<) l r
             then LT
             else GT

max :: Comparable a => a -> a -> a
max l r =
  case compare l r of
    LT -> r
    _  -> l

data Order
  = LT
  | EQ
  | GT
```

Not much to say here. We are implementing some more arithmetic. We lifted `not` from Haskell's Prelude because it's identical, but chose to reimplement comparisons because the names are different, and in the end I don't want to expose typeclasses from HaskElm's Prelude.

`src/Prelude.hs`:
```haskell
module Prelude
  ( Bool(..)
  , Int
  , IO
  , List
  , Maybe(..)
  , Order(..)
  , String
  , (<|)
  , (|>)
  , (<<)
  , (>>)
  , (==)
  , (++)
  , (+)
  , (-)
  , (//)
  , compare
  , max
  , not
  ) where

import           Protolude
```

Just re-exporting from `Protolude`. Notice how we don't expose typeclasses.

Then we have some obvious code:

`src/Tuple.hs`:
```haskell
{-# LANGUAGE NoImplicitPrelude #-}

module Tuple
  ( first
  , second
  ) where

first :: (a, b) -> a
first (x, _) = x

second :: (a, b) -> b
second (_, x) = x
```

`src/List.hs`:
```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module List
  ( foldl
  , intersperse
  , length
  , map
  , maximum
  , sum
  , zip
  ) where

import qualified Maybe
import qualified "base" Prelude as P
import           Protolude

map :: (a -> b) -> List a -> List b
map = P.map

foldl :: (e -> a -> a) -> a -> List e -> a
foldl f = P.foldl (\a e -> f e a)

length :: List a -> Int
length = P.fromIntegral << P.length

maximum :: Comparable a => List a -> Maybe a
maximum []     = Nothing
maximum [x]    = Just x
maximum (x:xs) = Maybe.map (max x) (maximum xs)

sum :: Number a => List a -> a
sum []     = fromInteger 0
sum (x:xs) = x + sum xs

intersperse :: a -> List a -> List a
intersperse _ []     = []
intersperse _ [x]    = [x]
intersperse i (x:xs) = x : i : intersperse i xs

zip :: List a -> List b -> List (a, b)
zip = P.zip
```

`src/String.hs`:
```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module String
  ( fromInt
  , intersperse
  , length
  ) where

import qualified List
import qualified "base" Prelude as P
import           Protolude

fromInt :: Int -> String
fromInt = P.show

intersperse :: String -> List String -> String
intersperse _ []     = ""
intersperse s (x:xs) = List.foldl (\e a -> e ++ s ++ a) x xs

length :: String -> Int
length = P.fromIntegral << P.length
```

`src/Maybe.hs`:
```haskell
module Maybe
  ( map
  , withDefault
  ) where

map :: (a -> b) -> Maybe a -> Maybe b
map _ Nothing  = Nothing
map f (Just x) = Just (f x)

withDefault :: a -> Maybe a -> a
withDefault d Nothing  = d
withDefault _ (Just x) = x
```

Everything is quite obvious. We recycled some functions from Haskell's `Prelude`, but we could have reimplemented them, if needed.

`haskelm.cabal`
```cabal
name:                haskelm
version:             0.1.0.0
-- synopsis:
-- description:
license:             BSD3
license-file:        LICENSE
author:              Leonardo Taglialegne
maintainer:          cmt.miniBill@gmail.com
-- copyright:
-- category:
build-type:          Simple
extra-source-files:  ChangeLog.md
cabal-version:       >=1.10

library
  exposed-modules:   Prelude, CLI, CLI.Attributes, List, Maybe, String, Tuple
  other-modules:     Protolude
  build-depends:     base >=4.11 && <4.12, ncurses >=0.2 && <0.3
  hs-source-dirs:    src
  default-language:  Haskell2010
  ghc-options:       -Wall -Werror

executable haskelm
  main-is:           Main.hs
  build-depends:     haskelm
  hs-source-dirs:    app
  default-language:  Haskell2010
```

Now `src/CLI.hs` becomes:
```haskell
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
    (msg -> model -> (model, IO [msg]))

run_ :: Program () model msg -> IO ()
run_ = run ()

run :: flags -> Program flags model msg -> IO ()
run flags (Program init view _) =
  let model = init flags
  -- runCurses initializes the ncurses library
   in Curses.runCurses <| do
        _ <- displayAndWait <| view model
        return ()

-- Displays a widget in the top left corner of the screen
-- and waits for an event
displayAndWait :: CLI msg -> Curses (Maybe Curses.Event)
displayAndWait root = do
  w <- Curses.defaultWindow
  -- updateWindow prepares the drawing
  Curses.updateWindow w <| do
    Curses.moveCursor 0 0 -- Move the cursor in the top left corner
    displayWidget root
    Curses.moveCursor 0 0 -- Move the cursor in the top left corner, again
  -- Actually do the drawing on screen
  Curses.render
  -- Wait for an event. "Nothing" means it should wait forever
  Curses.getEvent w Nothing

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
      -- results are in a monad. It uses `map` to transform a List (Update a) into
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
```

So, we've got `displayAndWait` that returns a `Maybe Curses.Event` (wrapped in a monad, but we'll use `andThen`/`do`-notation for that), and we want to implement the main loop.

We will implement a `eventToMsgs :: Curses.Event -> CLI msg -> List msg` (the `List` makes code simpler), we already have a `displayAndWait :: CLI msg -> Curses (Maybe Curses.Event)` and we want to create a loop that will have type `Curses ()`.

The function will take the initial model:

```haskell
mainLoop model view update = do
  let root = view model
  event <- displayAndWait 
```