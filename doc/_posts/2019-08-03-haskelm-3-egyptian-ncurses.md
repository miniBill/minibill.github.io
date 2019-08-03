---
title: "HaskElm #3: egyptian (n)curses"
tags: elm haskell
---

## Introduction ##
HaskElm is a project in which I try and rewrite a Prelude for Haskell that feels like Elm's Prelude.

This set of posts is aimed at people with some experience in Elm (having read the guide should be enough) and no experience in Haskell.
It is structured as a log of my explorations, rather than a tutorial or an howto, so it will have warts, false ends and errors (although I'll cut some of the uninteresting ones).

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

Add `ncurses >=0.2 && <0.3` in `build-depends` for the library.


