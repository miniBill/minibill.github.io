---
title: "HaskElm #2: don't put that on record"
tags: elm haskell
---

## Introduction ##
HaskElm is a project in which I try and rewrite a Prelude for Haskell that feels like Elm's Prelude.

This set of posts is aimed at people with some experience in Elm (having read the guide should be enough) and no experience in Haskell.
It is structured as a log of my explorations, rather than a tutorial or an howto, so it will have warts, false ends and errors (although I'll cut some of the uninteresting ones).

In [part I](/2019-07-31-haskelm-1-getting-the-caball-rolling) we tried to work with records in Haskell.

It was painful but we were finally able to compile something using `HList` and some fairly recent GHC extensions.

In the end I don't think the pain is worth the result (right now), so we'll drop records and Keep It Simple.

Everything is formatted with `hfmt`, which is an opinionated formatted, à la `gofmt` or `elm-format`, so you may not like its style (I don't love it), but it eliminates bikeshedding and lost time on formatting decisions, so I like it for that.

`src/Prelude.hs`
```haskell
{-# LANGUAGE PackageImports #-} -- This is needed so we can import the "official" Prelude

module Prelude
  ( IO
  , String
  , (<|)
  ) where

import qualified "base" Prelude as P

type IO a = P.IO a -- We have to expose it, because `main` must have type `IO ()`

type String = P.String -- Strings in Haskell are actually List Char, but for now we don't care.

infixr 0 <|

(<|) :: (a -> b) -> a -> b
f <| x = f x
```

This is the absolute minimum we'll need for out first example.

`src/CLI.hs`
```haskell
{-# LANGUAGE PackageImports #-}

module CLI
  ( CLI
  , Program
  , run
  , run_
  , sandbox
  , text
  ) where

import qualified "base" Prelude

data CLI msg =
  Text String -- We only have a single kind of view for now: a text string

data Program flags model msg =
  Program
    (flags -> model) -- init
    (model -> CLI msg) -- view
    (msg -> model -> (model, IO [msg])) -- update

run_ :: Program () model msg -> IO ()
run_ = run ()

run :: flags -> Program flags model msg -> IO ()
run flags (Program init view update) =
  let model = init flags
      unpack w =
        case w of
          Text s -> s
   -- let's keep it ultra-simple for now, and just echo the initial view to screen
   in do Prelude.putStrLn <| unpack <| view model

sandbox ::
     model
  -> (model -> CLI msg)
  -> (msg -> model -> model)
  -> Program () model msg
sandbox init view update =
  let init' _ = init
      update' msg model = (update msg model, Prelude.return [])
   in Program init' view update'

text :: String -> CLI msg
text = Text
```

We just want to put something on the screen, we will be adding actual message handling later.

`app/Main.hs`
```haskell
module Main
  ( main
  ) where

import           CLI

type Flags = ()

type Model = ()

type Msg = ()

main :: IO ()
main =
  let init = ()
      view _ = CLI.text "Hello world!"
      update _ model = model
   in CLI.run_ <| CLI.sandbox init view update
```

This is not as clean as a record would be in Elm, but still pretty readable IMO.

`haskelm.cabal`:
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
  exposed-modules:   Prelude, CLI
  build-depends:     base >=4.11 && <4.12
  hs-source-dirs:    src
  default-language:  Haskell2010
  ghc-options:       -Wall -Werror

executable haskelm
  main-is:           Main.hs
  build-depends:     haskelm
  hs-source-dirs:    app
  default-language:  Haskell2010
```

This is much simpler than what we reached in Part I.

```bash
$ cabal run
Preprocessing library for haskelm-0.1.0.0..
Building library for haskelm-0.1.0.0..
[1 of 2] Compiling Prelude          ( src/Prelude.hs, dist/build/Prelude.o )
[2 of 2] Compiling CLI              ( src/CLI.hs, dist/build/CLI.o )
Preprocessing executable 'haskelm' for haskelm-0.1.0.0..
Building executable 'haskelm' for haskelm-0.1.0.0..
[1 of 1] Compiling Main             ( app/Main.hs, dist/build/haskelm/haskelm-tmp/Main.o )
Linking dist/build/haskelm/haskelm ...
Running haskelm...
Hello world!
```

Yay! Simple wins again!

## A simple loop ##
Let's swap the implementation of `run` in `CLI.hs` with:

```haskell
import qualified "base" Control.Monad      as Monad -- Monads!!!

run :: flags -> Program flags model msg -> IO ()
run flags (Program init view _) =
  let model = init flags
      unpack w =
        case w of
          Text s -> s
   in do Prelude.putStrLn <| unpack <| view model
         Monad.forever <| return ()
```

Ok, let's talk about the M word. I *will* be using monads but I'll try to keep it simple, and use them only in the implementation, what in Elm would be kernel code.

Monads are simply types `M x` with two functions:
* `return :: x -> M x`, this is usually trivial, `\x -> [x]` for `List`, `\x -> Just x` for `Maybe`, `\x -> Ok x` for `Result`,... It takes a value and puts it inside the type;
* `(>>=) :: M x -> (x -> M y) -> M y` this is Elm's `andThen` with arguments flipped (`List.andThen`, `Maybe.andThen`, `Result.andThen`, `Decoder.andThen`, ...).

`do` is just a compact notation, without it we would have to write:

```haskell
(Prelude.putStrLn <| unpack <| view model) >>= {- |> andThen -} (\_ ->
  Monad.forever <| return ())
```

`Monad.forever` just does what it says on the tin: it executes something forever.

This works but consumes a lot of CPU while idling. The simplest way to not do that is to suspend the thread a bit:

```haskell
import qualified "base" Control.Concurrent as Concurrent

run :: flags -> Program flags model msg -> IO ()
run flags (Program init view _) =
  let model = init flags
   in do Prelude.print <| view model
         Monad.forever <| Concurrent.threadDelay 100
```

Much better, the CPU usage stays under 1%.

## Count von Count ##

Ok, let's try to implement the simplest example: the simple counter.

```elm
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = 0, update = update, view = view }

type Msg = Increment | Decrement

update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]
```

This becomes:

`app/Main.hs`:
```haskell
module Main
  ( main
  ) where

import           CLI            (button, row, text)
import qualified CLI
import           CLI.Attributes (onClick)

-- unfortunately Haskell does not support qualified module exports, so we have to explicitly import modules instead of just exposing them from Prelude
import qualified String

main :: IO ()
main = CLI.run_ <| CLI.sandbox 0 view update

data Msg
  = Increment
  | Decrement

update msg model =
  case msg of
    Increment -> model + 1
    Decrement -> model - 1

view model =
  row
    [ button [onClick Decrement] [text "-"]
    , text <| String.fromInt model
    , button [onClick Increment] [text "+"]
    ]
```

Ok, let's try and implement this API!

In `src/CLI/Attributes.hs` (a new file):
```haskell
module CLI.Attributes
  ( Attribute
  , onClick
  ) where

data Attribute msg =
  OnClick msg

onClick :: msg -> Attribute msg
onClick = OnClick
```

Add `CLI.Attributes` to the `other-modules` in the cabal file.

In `CLI.elm` we overwrite/add the following:
```haskell
import           CLI.Attributes            (Attribute)

data CLI msg
  = Text String
  | Row (List (CLI msg))
  | Button (List (Attribute msg)) (List (CLI msg))

run :: flags -> Program flags model msg -> IO ()
run flags (Program init view _) =
  let model = init flags
      unpack w =
        case w of
          Text s            -> s
          Row xs            -> String.intersperse " " <| List.map unpack xs
          Button _ children -> "[ " ++ unpack (Row children) ++ " ]"
   in do Prelude.putStrLn <| unpack <| view model
         Monad.forever <| Concurrent.threadDelay 100

button :: List (Attribute msg) -> List (CLI msg) -> CLI msg
button = Button

row :: List (CLI msg) -> CLI msg
row = Row
```

We should split `Prelude.hs`: we'll keep a `Prelude` (exposed to `CLI`, applications), and a `Protolude` (used to build the `Prelude` itself).

`src/Prelude.hs`
```haskell
module Prelude
  ( Appendable(..)
  , Int
  , IO
  , List
  , Maybe(..)
  , Number(..)
  , String
  , (<|)
  ) where

import           Protolude
```

`src/Protolude.hs`
```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module Protolude
  ( Appendable(..)
  , Int
  , IO
  , List
  , Maybe(..)
  , Number(..)
  , String
  , (<|)
  ) where

import           "base" Prelude (IO, Maybe, String)
import qualified "base" Prelude as P

type Int = P.Integer

type List a = [a]

infixr 0 <|

(<|) :: (a -> b) -> a -> b
f <| x = f x

class Appendable a where
  (++) :: a -> a -> a

instance Appendable [a] where
  (++) = (P.++)

class Number a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a

instance Number P.Integer where
  (+) = (P.+)
  (-) = (P.-)
```

We need `class` and `instance` to define the equivalent of Elm's "magic" `number` and `appendable`. In Haskell we use typeclasses.
Typeclasses are sets of generic functions, and instances are how those functions are defined for types. Again, we'll use this in "kernel" code, but clients need not worry.

Add `Protolude` to the `other-modules` in the cabal file.

In `src/String.hs` (a new file):
```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module String
  ( fromInt
  , intersperse
  ) where

import qualified List
import qualified "base" Prelude as P
import           Protolude

fromInt :: Int -> String
fromInt = P.show

intersperse :: String -> List String -> String
intersperse _ []     = ""
intersperse s (x:xs) = List.foldl (\e a -> e ++ s ++ a) x xs
```

Add `String` to the `exposed-modules` in the cabal file.

In `src/List.hs` (a new file):
```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

module List
  ( foldl
  , map
  ) where

import qualified "base" Prelude as P
import           Protolude

map :: (a -> b) -> List a -> List b
map = P.map

foldl :: (e -> a -> a) -> a -> List e -> a
foldl f = P.foldl <| \a e -> f e a
```

Add `List` to the `exposed-modules` in the cabal file.

```bash
$ cabal run
Preprocessing library for haskelm-0.1.0.0..
Building library for haskelm-0.1.0.0..
[1 of 6] Compiling Protolude        ( src/Protolude.hs, dist/build/Protolude.o )
[2 of 6] Compiling Prelude          ( src/Prelude.hs, dist/build/Prelude.o )
[4 of 6] Compiling List             ( src/List.hs, dist/build/List.o )
[5 of 6] Compiling String           ( src/String.hs, dist/build/String.o )
[6 of 6] Compiling CLI              ( src/CLI.hs, dist/build/CLI.o )
Preprocessing executable 'haskelm' for haskelm-0.1.0.0..
Building executable 'haskelm' for haskelm-0.1.0.0..
[1 of 1] Compiling Main             ( app/Main.hs, dist/build/haskelm/haskelm-tmp/Main.o )
Linking dist/build/haskelm/haskelm ...
Running haskelm...
[ + ] 0 [ - ]
```

It works!

## In the next episode ##
We'll start implementing user interactions!

Continue to [Part III](/2019-08-03-haskelm-3-egyptian-ncurses)!
