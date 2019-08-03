---
title: "HaskElm #1: getting the (ca)ball rolling"
tags: elm haskell
---

## Introduction ##
HaskElm is a project in which I try and rewrite a Prelude for Haskell that feels like Elm's Prelude.

This set of posts is aimed at people with some experience in Elm (having read the guide should be enough) and no experience in Haskell.
It is structured as a log of my explorations, rather than a tutorial or an howto, so it will have warts, false ends and errors (although I'll cut some of the uninteresting ones).

Let's start with a minimal viable example: the target will be to compile the following program:

```haskell
module Main
  ( main
  ) where

import qualified CLI

type Flags = ()

type Model = ()

type Msg = ()

main :: IO ()
main =
  CLI.sandbox
    { init = \flags -> ()
    , view = \model -> CLI.text "Hello world!"
    , update = \msg model -> model
    }
```

Some observations for people unfamiliar with Haskell:
* `type` in Haskell has the same meaning as `type alias` in Elm, Haskell uses `data` for what Elm simply calls `type`;
* `import qualified` is similar to the behaviour you have in Elm: the default in Haskell (without `qualified`) is exposing everything (`expose (..)` in Elm);
* `module X (ys) where` is Elm's `module X exposing (ys)`;
* `main` in Haskell must have the type `IO ()`, which is the type of impure computations, as a first approximation you can think of `IO` as `Task`;
* the meaning of `::` and `:` is swapped in Elm and Haskell.

Seems simple, right? We'll see...

Warning: in the end I chose a slightly different approach so, unless you want to read a rambling on trying to use records in Haskell in a way that is similar to Elm, just skip to [Part II](/2019-08-01-haskelm-2-don-t-put-that-on-record).

## Cabal init ##
Let's keep it simple: we'll start with `cabal`, then switch to `stack` later.

```bash
$ sudo apt install cabal-install
$ mkdir haskelm
$ cd haskelm
$ cabal init
... Answer some questions, mostly with defaults ...
```

The only significant choice is building a library and an executable (we'll write the Prelude inside the library, the minimal code from above in the executable).

Let's go ahead and edit the generated `haskelm.cabal` file, by removing the `build-depends: base >= x.yy && < x.zz` part, and setting the hs-source-dirs for the executable to `app`. Then we can then create `app/Main.hs` with the code from above.

## Trying to build ##
Let's try and do it Elm-style: we'll follow the compiler errors and try to make things work from here!

```bash
$ cabal build
```

First error is GHC warning us that the `Main` module for the executable cannot find the `Prelude` and `CLI` modules, so let's stub them.

We'll only need `IO` in `Prelude`, and `sandbox` and `text` in `CLI`, so let's do that.

`src/Prelude.hs`:
```haskell
module Prelude
  ( IO
  ) where

import qualified "base" Prelude as P -- Import the standard Prelude

type IO a = P.IO a
```

`src/CLI.hs`:
```haskell
module CLI
  ( CLI
  , sandbox
  , text
  ) where

data CLI msg =
  Todo

sandbox :: { init :: flags -> model, update :: msg -> model -> model, view :: model -> CLI msg } -> IO ()
sandbox = error "Todo"

text :: String -> CLI msg
text msg = error "Todo"
```

We create those two files, try to `cabal build` again and... nothing changes. We need to change `haskelm.cabal` to add `Prelude` and `CLI` to the library's `exposed-modules`! Let's try again... with the same result. A little Google later (thanks Stack Overflow!), we add `build-depends: haskelm` to the executable section in `haskelm.cabal` and the error finally changes!

    Package-qualified imports are not enabled; use PackageImports

Makes sense: `default-extensions: PackageImports` in the library section of `haskelm.cabal` and off we go!

Let's see what does GHC have to say about our code:

1. We cannot write a record in the type signature.
2. We are missing a `String` definition in our `Prelude`.

The `String` error is trivial to fix: `type String = P.String` (and the corresponding `String` in the exposing section of `Prelude`).

## Modern Records - The contenders ##
A search on the record syntax brings us to <a href="https://wiki.haskell.org/Extensible_record">this page</a> in Haskell's wiki, which talks about some unfinished tickets and links to some existing libraries. Let's have a look at the libraries!

### vinyl ###
Url: https://hackage.haskell.org/package/vinyl

The hackage page is really bare.
Let's try and find some docs: there is a [README](https://github.com/VinylRecords/Vinyl/) that points to a [tutorial](https://github.com/VinylRecords/Vinyl/blob/master/tests/Intro.lhs).

After having a look at the tutorial: nope, way too complex; remember, our main objective is to keep everything simple!

### grapefruit-records ###
I can't seem to find an example.

### named-records ###
Url: https://hackage.haskell.org/package/named-records

The hackage page looks encouraging (there is a mention of extending records).

The link to the source repo is broken. Actual link is https://hub.darcs.net/scravy/named-records (probably).
The [example](https://hub.darcs.net/scravy/named-records/browse/examples/Sample.hs) doesn't look bad, although it uses Template Haskell it's reasonably readable.

### has ###
Url: https://hackage.haskell.org/package/has

**It has a link to an example in the hackage page!** Cautiosly optimistic!

Ouch, a field can only has a single type per module...
_Might_ work but I'm much less optimistic.

Syntax for type is okay-ish, for record creation okay-ish too, read write and modify is flipped but that can be solved with some strategic redefinition. Syntax for "has record" is good (classical Haskell typeclass constraint).

In summary, a reasonable but limited choice.

It also has some more libraries at the end.

### hlist ###
Url: https://hackage.haskell.org/package/HList

Syntax is nice, labels don't fix a single type, access syntax is nice, order is different from Elm but not a biggie. It's a widely used library too.
Polymorphic "has fields" constraint is simple.

Not clear how to write the type of a record.

This one is good.

### records ###
Url: https://hackage.haskell.org/package/records

[The repo url link](http://darcs.wolfgang.jeltsch.info/haskell/records) is broken.

The syntax for record building is ok, writing types is ok, except for the `style` parameter which is probably way overkill for us.
Syntax for modify and access are ok I guess.

This one looks like a viable alternative.

### others ###
`lenses`: it's the most used and battle tested option, but it doesn't solve the type problem and the types become quite gnarly quite fast.
`data-accessor`, `fclabels`: look more complicated (and based on `lenses`). Will keep for later if `hlist` and `records` fail to work for us.

## Modern Records - trying hlist ##
Ok, let's try adding `HList >= 0.5 && <0.6` to the `build-depends` in the library section of `haskelm.cabal`.

```bash
$ cabal build
Resolving dependencies...
Warning: solver failed to find a solution:
Could not resolve dependencies:
[__0] trying: haskelm-0.1.0.0 (user goal)
[__1] unknown package: HList (dependency of haskelm)
[__1] fail (backjumping, conflict set: haskelm, HList)
After searching the rest of the dependency tree exhaustively, these were the
goals I've had most trouble fulfilling: haskelm, HList
Trying configure anyway.
Configuring haskelm-0.1.0.0...
cabal: Encountered missing dependencies:
HList ==0.5.*
$ cabal install HList
... bla bla bla ...
$ cabal build
... we get the same error we had before (record syntax) ...
```

Ok, let's find out what the syntax is for hlist records.

Let's comment `sandbox` type signature in `CLI.hs`. A `cabal build` later we need to define `error` in `Prelude.hs`.

```haskell
error :: String -> a
error msg = P.error msg
```

Still have some errors in `Main.hs`. Let's comment it out and leave just an `error`.

```bash
$ cabal repl exe:haskelm
```

A ton of errors with missing functions. Just reexport them all.
Even if reeported it keeps saying `Not in scope: data constructor ‘System.IO.NoBuffering’` which brings us to [bug #16563](https://gitlab.haskell.org/ghc/ghc/issues/16563) which isn't fixed at the time of writing.

Yay.

SO.

```bash
$ mkdir tmp
$ cd tmp
$ cabal repl
GHCi, version 8.4.4: http://www.haskell.org/ghc/  :? for help
Prelude> import Data.HList.Record
Prelude Data.HList.Record> let x = Label :: Label "x" ; y = Label :: Label "y" in x .==. "v1" .*. y .==. 2 .*. emptyRecord

<interactive>:2:18: error:
    Not in scope: type constructor or class ‘Label’
    Perhaps you meant ‘Labels’ (imported from Data.HList.Record)

<interactive>:2:24: error:
    Illegal type: ‘"x"’ Perhaps you intended to use DataKinds

<interactive>:2:43: error:
    Not in scope: type constructor or class ‘Label’
    Perhaps you meant ‘Labels’ (imported from Data.HList.Record)

<interactive>:2:49: error:
    Illegal type: ‘"y"’ Perhaps you intended to use DataKinds
```
Perusing the docs it looks like we need `:set -XDataKinds`
```bash
Prelude Data.HList.Record> :set -XDataKinds
Prelude Data.HList.Record> let x = Label :: Label "x" ; y = Label :: Label "y" in x .==. "v1" .*. y .==. 2 .*. emptyRecord

<interactive>:3:18: error:
    Not in scope: type constructor or class ‘Label’
    Perhaps you meant ‘Labels’ (imported from Data.HList.Record)

<interactive>:3:43: error:
    Not in scope: type constructor or class ‘Label’
    Perhaps you meant ‘Labels’ (imported from Data.HList.Record)
```
Also `import Data.HList.FakePrelude`
```bash
Prelude Data.HList.Record> import Data.HList.FakePrelude
Prelude Data.HList.Record Data.HList.FakePrelude> let x = Label :: Label "x" ; y = Label :: Label "y" in x .=. "v1" .*. y .=. '2' .*. emptyRecord
Record{x="v1",y='2'}
Prelude Data.HList.Record Data.HList.FakePrelude> :t (let x = Label :: Label "x" ; y = Label :: Label "y" in x .=. "v1" .*. y .=. '2' .*. emptyRecord)
(let x = Label :: Label "x" ; y = Label :: Label "y" in x .=. "v1" .*. y .=. '2' .*. emptyRecord)
  :: Num v => Record '[Tagged "x" [Char], Tagged "y" Char]
```
Looks good!

SO. Let's add `DataKinds` to the `default-extensions` in the `library` section of `haskelm.cabal`.

`src/Prelude.hs`:
```haskell
import Data.HList.Record
import Data.HList.FakePrelude
```

`src/CLI.hs`:
```haskell
sandbox :: Record '[Tagged "init" (flags -> model), Tagged "update" (msg -> model -> model), Tagged "view" (model -> CLI msg)] -> IO ()
```

```bash
$ cabal build
Preprocessing library for haskelm-0.1.0.0..
Building library for haskelm-0.1.0.0..
[2 of 2] Compiling CLI              ( src/CLI.hs, dist/build/CLI.o )

src/CLI.hs:10:28: error:
    • Expected a type, but
      ‘"init"’ has kind
      ‘ghc-prim-0.5.2.0:GHC.Types.Symbol’
    • In the first argument of ‘Tagged’, namely ‘"init"’
      In the first argument of ‘Record’, namely
        ‘'[Tagged "init" (flags -> model),
           Tagged "update" (msg -> model -> model),
           Tagged "view" (model -> CLI msg)]’
      In the type signature:
        sandbox :: Record '[Tagged "init" (flags -> model),
                            Tagged "update" (msg -> model -> model),
                            Tagged "view" (model -> CLI msg)]
                   -> IO ()
   |
10 | sandbox :: Record '[Tagged "init" (flags -> model), Tagged "update" (msg -> model -> model), Tagged "view" (model -> CLI msg)] -> IO ()
   |                            ^^^^^^

src/CLI.hs:10:60: error:
    • Expected a type, but
      ‘"update"’ has kind
      ‘ghc-prim-0.5.2.0:GHC.Types.Symbol’
    • In the first argument of ‘Tagged’, namely ‘"update"’
      In the first argument of ‘Record’, namely
        ‘'[Tagged "init" (flags -> model),
           Tagged "update" (msg -> model -> model),
           Tagged "view" (model -> CLI msg)]’
      In the type signature:
        sandbox :: Record '[Tagged "init" (flags -> model),
                            Tagged "update" (msg -> model -> model),
                            Tagged "view" (model -> CLI msg)]
                   -> IO ()
   |
10 | sandbox :: Record '[Tagged "init" (flags -> model), Tagged "update" (msg -> model -> model), Tagged "view" (model -> CLI msg)] -> IO ()
   |                                                            ^^^^^^^^

src/CLI.hs:10:101: error:
    • Expected a type, but
      ‘"view"’ has kind
      ‘ghc-prim-0.5.2.0:GHC.Types.Symbol’
    • In the first argument of ‘Tagged’, namely ‘"view"’
      In the first argument of ‘Record’, namely
        ‘'[Tagged "init" (flags -> model),
           Tagged "update" (msg -> model -> model),
           Tagged "view" (model -> CLI msg)]’
      In the type signature:
        sandbox :: Record '[Tagged "init" (flags -> model),
                            Tagged "update" (msg -> model -> model),
                            Tagged "view" (model -> CLI msg)]
                   -> IO ()
   |
10 | sandbox :: Record '[Tagged "init" (flags -> model), Tagged "update" (msg -> model -> model), Tagged "view" (model -> CLI msg)] -> IO ()
   |                                                                                                     ^^^^^^
```
ARGH.

A Google search later, we add `PolyKinds` to `default-extensions` in the library section in `haskelm.cabal`.

Aaaaand:

```bash
$ cabal build
Resolving dependencies...
Configuring haskelm-0.1.0.0...
Preprocessing library for haskelm-0.1.0.0..
Building library for haskelm-0.1.0.0..
[1 of 2] Compiling Prelude          ( src/Prelude.hs, dist/build/Prelude.o ) [flags changed]
[2 of 2] Compiling CLI              ( src/CLI.hs, dist/build/CLI.o ) [flags changed]
Preprocessing executable 'haskelm' for haskelm-0.1.0.0..
Building executable 'haskelm' for haskelm-0.1.0.0..
Linking dist/build/haskelm/haskelm ...
```

![IT'S ALIVE](https://i.imgur.com/B2h7KK9.gif)

## In the next episode ##
We'll ditch records and just try to build what we wanted to build in the first place!

[Part II](/2019-08-01-haskelm-2-don-t-put-that-on-record)
