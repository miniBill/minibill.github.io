---
title: "HaskElm #4: Prelude (to the foundation)"
tags: elm haskell
---

## Introduction ##
HaskElm is a project in which I try and rewrite a Prelude for Haskell that feels like Elm's Prelude.

This set of posts is aimed at people with some experience in Elm (having read the guide should be enough) and no experience in Haskell.
It is structured as a log of my explorations, rather than a tutorial or an howto, so it will have warts, false ends and errors (although I'll cut some of the uninteresting ones).

In [part III](/2019-08-03-haskelm-3-egyptian-ncurses) we wrote the (rudimental) mouse handling code.

In this episode we'll take a pause from implementing the UI and events and actually finish implementing the Prelude.

## Choices ##
Up until now all the implementation choices were dictated by simplicity, and keeping it simple. While implementing the Prelude we do need to keep in mind that performance and memory usage are actually concerns to be taken into account.

We have also (up until) ignored the fact that Haskell is a lazy language which means, simplifying, that values are not computed until it is necessary and so functions actually accept expressions rather than values. Elm on the other hand is a strict language, which means that expressions are fully transformed into values before being passed around.

We'll try enabling the `Strict` and `StrictData` extensions globally in the cabal file and see what happens.

Everything seems to work correctly, let's go on!

## List ##
W.I.P.

## In the next episode ##
We'll implement subscriptions, which will require us to go deeper down the rabbit hole of multithreading in Haskell.

Continue to [Part V](/)!
