---
title: "Advanced Elm #2: Modular arithmetic"
tags: elm advanced_elm math
---

## Introduction ##
*Advanced Elm is a project to explore some of the more advanced corners of the Elm language.*

I have some objectives for Advanced Elm:
1. showing that Elm, while simple and perfect for beginners, is powerful enough for a lot of advanced scenarios,
2. trying to unravel how to use advanced language features like phantom types and extensible records,
3. trying to shed some light on how I come up with complex types and simple APIs.

In this post I'll try to answer a seemingly simple question: why do we have to add a default branch when we use `case ... of` when using `modBy n` with a fixed `n`?

The simple, and sane, answer is that the in Elm we have no way to tell the compiler that the only possible results are `1`, `2`, `3`, ... `n-1`.

The [How To style](https://www.amazon.com/How-Absurd-Scientific-Real-World-Problems/dp/0525537090) answer is that it's actually possible to write a `safeModBy` function where we have exactly `n` branches when we do `safeModBy n`.

As with `elm-codec`, rather than explaining how the current code works I'll reimplement (parts of) it from scratch to emphasize the design process.

A word of caution: this is mostly an excuse to play with complex types, the code is not optimized, the API is insane and the idea is bad. It's still interesting though.

### Notation ###
`[a, b]` = `[a, a+1, a+2, ..., b]`.

## APIs ##

To have `n` branches we must convince the compiler that our type has exactly `n` possible values, "make impossible states impossible" style.

This immediately rules out `Int`, because from the compiler point of view it always has "infinite" possible values, we'll probably need a custom type.

When the road ahead is unclear it's often useful to write some examples.

What are the possible results of `modBy n`? Let's reason about small values for `n`:

* `x` (< 10): a single digit in the `[0, x-1]` range,
* `xy` (< 100): the first digit of the result is in the `[0, x]`/`[0, x-1]` range (`x-1` if `y` is `0`, `x` otherwise), the second digit is in the `[0,y-1]` range if the first digit is `x`, in the `[0,9]` range otherwise.
* `xzy` (< 1000): the first digit of the result is in the `[0, x]`/`[0, x-1]` range (see above), the second digit is in the `[0,y-1]` range if the first digit is `x`, in the `[0,9]` range otherwise, the third digit is in the `[0,z-1]` range if the first digit is `x` and the second digit is `y`, in the `[0,9]` range otherwise.
* ...

From this we can understand that:

1. the shape is regular, but a bit different from what we're used to,
2. considering that the shape (hence, the type) of the result depends on the value we pass to `modBy`, we will not be able to use an `Int` as argument.

### Digits ###

We will need at the very least a different type for every single digit, because every possible `modBy` with a single digit number will need a different type (unless we create an API that uses Peano-style numbers, but it would be unwieldy and horrible).

Let's try and write some (hypotetical) code.

```Elm
type D0 = D0
type D1 = D0 | D1
type D2 = D0 | D1 | D2
...
type D9 = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
```

This is not legal code! Every variant needs a different name. I don't have any ideas that don't produce decent names, the least-bad I've found is:

```Elm
type D0 = D0_0
type D1 = D0_1 | D1_1
type D2 = D0_2 | D1_2 | D2_2
...
type D9 = D0_9 | D1_9 | D2_9 | D3_9 | D4_9 | D5_9 | D6_9 | D7_9 | D8_9 | D9_9
```

For two digits numbers we cannot create a type like `type TwoDigits a b = TD a b`, because we need the second digit to depend on the value of the first one. This seems to point toward dependent types, which Elm doesn't have, but it's actually doable within Elm's typesystem.

To keep the code a bit more compact, from now on I'll use base 4 instead of base 10. Mutatis mutandis everything will work for base 10.

Let's write some pseudo-Elm for the type of two digit numbers up to `22` (in base 4, it would be `10` in base 10).

```Elm
type UpTo22
    =   0 |   1 |   2 |   3
    | 1 0 | 1 1 | 1 2 | 1 3
    | 2 0 | 2 1 | 2 2
```

With some grouping, informed by the analysis we did above for multi-digit numbers, we reach (`D` stands for "digit"):

```Elm
type D2 = D0_2 | D1_2 | D2_2
type D3 = D0_3 | D1_3 | D2_3 | D3_3

type UpTo22
    = Zero D3
    | One D3
    | Three D2
```

So if we want to create the `UpTo2X` type we can imagine it would be:

```Elm
type UpTo2X x
    = Zero D3
    | One D3
    | Two x
```

Analyzing `UpTo2X` we can see that:

1. if we replace all the `D3` with `()`s, `UpTo2X ()` gets the same shape as `D3`,
2. if we replace all the `D3` with `UpTo3X D3`, `UpTo2X (UpToyX z)` is type type of numbers up to `2yz`.

This suggest a further generalization:

```Elm
type D0 x y
    = D0_0 y

type D1 x y
    = D0_1 x
    | D1_1 y

type D2 x y
    = D0_2 x
    | D1_2 x
    | D2_2 y

type D3 x y
    | D0_3 x
    | D1_3 x
    | D2_3 x
    | D3_3 y
```

This is finally enough to represent the numbers up to `n`, for every `n`. So if we have a number of the form `abc...pqr`, we'll choose `Da x y`, filling `x` with the type of numbers up to `333...3` (with the correct length), and `y` with the type of numbers up to `bc...pqr`.

Let's enumerate the values of the type `D1 (D3 (D3 () ()) (D3 () ())) (D2 (D3 () ()) (D1 () ()))` (this type alone should convince you pretty quickly that what we're doing here, while interesting, is completely impractical).

Let's write some intermediate definitions to avoid getting lost:

```Elm
type alias UpTo1 = D1 () ()
type alias UpTo3 = D3 () ()
type alias UpTo21 = D2 UpTo3 UpTo1
type alias UpTo33 = D3 UpTo3 UpTo3
type alias UpTo121 = D1 UpTo33 UpTo21
```

* `UpTo1` is `D1 () ()`, so `D0_1 () | D1_1 ()`, representing `[0, 1]`;
* `UpTo3` is `D3 () ()`, so `D0_3 () | D1_3 () | D2_3 () | D3_3 ()`, representing `[0, 1]`;
* `UpTo21` is `D3 UpTo3 UpTo1`, so `D0_2 UpTo3 | D1_2 UpTo3 | D2_2 UpTo1`, representing `0 [0, 3] | 1 [0, 3] | 2 [0, 1]` which is exactly `[00, 21]`;
* `UpTo33` is `D3 UpTo3 UpTo3`, so `D0_3 UpTo3 | D1_3 UpTo3 | D2_3 UpTo3 | D3_3 UpTo3`, representing `[00, 33]`;
* `UpTo121` is `D1 UpTo33 UpTo21`, so `D0_1 UpTo33 | D1_1 UpTo21`, representing `0 [00, 33] | 1 [00, 21]` which is exactly `[000, 121]`.

We now need two things:
* a nice way to build those awful types,
* a way to perform the calculations and return our custom type.

Before we go on: the most common variants will be the `D3` ones, so let's simplify that definition:

```Elm
type D x y
    = D0 x
    | D1 x
    | D2 x
    | D3 y
```

### `modBy` ###
So what will the API for `modBy` be? We have found out that it will need to return a value of type `Dx y z`, and the type of the result will depend on the modulus, so the modulus itself needs to encode something about the type. We'll try and keep the input number an `Int`.

```Elm
modBy : Modulus r -> Int -> r
```

### Building moduli ###
Looking at the types above we can distinguish between the last digit (which is `Dx () ()`), and the other digits (which are `Dx 333....333 partial`).

Thinking about the types it's obvious that we can't use lists or `Int`s to build our modulus, the type changes with every digit.

For the last digits' we can expose simple functions:

```Elm
f0 : Modulus (D0 () ())
f1 : Modulus (D1 () ())
f2 : Modulus (D2 () ())
f3 : Modulus (D () ())
```

For the other digits, we'll have a function `dx` that gets an input of type `Modulus (Dy z w)` and we want to produce `Modulus (Dx (D z z) (Dy z w))`. The problem is that Elm's functions cannot be generic like that (generic in `Dx`), so we need to either write a `dx` for every `y` (this would mean 100 functions for base 10) or we need to keep track of `z` in some additional way (and just put a type parameter in place of `Dy z w`).

How do we keep track of another type? We add a type parameter! So instead of having `Modulus x` we'll have `Modulus x y` where `x` is the "full" part (which will be of the form `D (D (D ...) (D ...)) (D (D ...) (D ...))`) and `y` the `Dy z w` part.

```Elm
f0 : Modulus () (D0 () ())
f1 : Modulus () (D1 () ())
f2 : Modulus () (D2 () ())
f3 : Modulus () (D () ())

d0 : Modulus f p -> Modulus (D f f) (D0 f p)
d1 : Modulus f p -> Modulus (D f f) (D1 f p)
d2 : Modulus f p -> Modulus (D f f) (D2 f p)
d3 : Modulus f p -> Modulus (D f f) (D f p)
```

## Implementation ##
To be completed...

## Epilogue ##

I hope that this post helped you understand how bend <s>spoons</s> the Elm typesystem to achieve seemingly-impossible compile time safety. I intend to continue publishing posts in the "Advanced Elm" spirit, and I think I'll talk about crazy compile-time checking of tree/graph structure. Let me know what would you like to see!

Please send me feedback via Slack, Discourse or e-mail!
