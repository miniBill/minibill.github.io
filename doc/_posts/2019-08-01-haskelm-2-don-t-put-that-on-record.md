---
title: "HaskElm #2: don't put that on record"
tags: elm haskell
---

## Introduction ##
In [part 1](/2019-07-31-haskelm-1-getting-the-caball-rolling) we tried to work with records in Haskell.

It was painful but we were finally able to compile something using `HList` and some fairly recent GHC extensions.

In the end I don't think the pain is worth the result (right now), so we'll drop records and Keep It Simple.

Everything is formatted with `hfmt`, which is an opinionated formatted, à la `gofmt` or `elm-format`, so you may not like its style (I don't), but it eliminates bikeshedding and lost time on formatting decisions, so I like it.

`src/Prelude.hs`
```haskell
{-# LANGUAGE PackageImports #-}

module Prelude
  ( IO
  , String
  , (<|)
  ) where

import qualified "base" Prelude as P

type IO a = P.IO a

type String = P.String

infixr 0 <|

(<|) :: (a -> b) -> a -> b
f <| x = f x
```

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
  Text String

instance Prelude.Show (CLI msg) where
  show (Text t) = t

data Program flags model msg =
  Program
    (flags -> model)
    (model -> CLI msg)
    (msg -> model -> (model, IO [msg]))

run_ :: Program () model msg -> IO ()
run_ = run ()

run :: flags -> Program flags model msg -> IO ()
run flags (Program init view update) =
  let model = init flags
   in Prelude.print <| view model

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
import qualified "base" Control.Monad      as Monad

run :: flags -> Program flags model msg -> IO ()
run flags (Program init view update) =
  let model = init flags
   in do Prelude.print <| view model
         Monad.forever <| return ()
```

This works but consumes a lot of CPU while idling. One thing I remember is that the simplest way to not do that is to suspend the thread a bit:

```haskell
import qualified "base" Control.Concurrent as Concurrent

run :: flags -> Program flags model msg -> IO ()
run flags (Program init view update) =
  let model = init flags
   in do Prelude.print <| view model
         Monad.forever <| Concurrent.threadDelay 20
```

Much better, the CPU usage stays under 1%.
