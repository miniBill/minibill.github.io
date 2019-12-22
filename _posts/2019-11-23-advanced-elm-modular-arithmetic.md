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

The [How To style](https://www.amazon.com/How-Absurd-Scientific-Real-World-Problems/dp/0525537090) answer is that it's actually possible to write a `modBySafe` function where we have exactly `n` branches when we do `modBySafe n`.

As with `elm-codec`, rather than explaining how the current code works I'll reimplement (parts of) it from scratch to emphasize the design process.

A word of caution: this is mostly an excuse to play with complex types, the code is not optimized, the API uses insane types and the idea is bad. It's still interesting though.

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
2. considering that the shape (hence, the type) of the result depends on the value we pass to `modBySafe`, we will not be able to use an `Int` as argument.

### Digits ###

We will need at the very least a different type for every single digit, because every possible `modBySafe` with a single digit number will need a different type (unless we create an API that uses Peano-style numbers, but it would be unwieldy and even more horrible than what we're already concocting).

Let's try and write some (hypotetical) code.

```Elm
type D0 = D0
type D1 = D0 | D1
type D2 = D0 | D1 | D2
...
type D9 = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
```

This is not legal Elm code! Every variant needs a different name. I don't have any ideas for decent names, the least-bad I've found is:

```Elm
type D0 = D0_0
type D1 = D0_1 | D1_1
type D2 = D0_2 | D1_2 | D2_2
...
type D9 = D0_9 | D1_9 | D2_9 | D3_9 | D4_9 | D5_9 | D6_9 | D7_9 | D8_9 | D9_9
```

For two digits numbers we cannot create a type like `type TwoDigits a b = TD a b`, because we need the second digit to depend on the value of the first one. This seems to point toward dependent types, which Elm doesn't have, but it's actually doable within Elm's typesystem.

To keep the code a bit more compact, from now on I'll use base four instead of base ten. Mutatis mutandis everything will work for base ten. We'll also pretend that Elm works in base four, so `10` will be four.

Let's write some pseudo-Elm for the type of two digit numbers up to `22` (in base four, that is, ten in base four).

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
    | Two D2
```

So if we want to create the `UpTo2X` type we can imagine it would be:

```Elm
type UpTo2X x
    = Zero D3
    | One D3
    | Two x
```

Analyzing `UpTo2X` we can see that:

1. if we replace all the `D3` with `()`s, `UpTo2X ()` has the same shape as `D2`,
2. if we replace all the `D3` with `UpTo3X D3`, `UpTo2X (UpTo1X z)` is type type of numbers up to `21z`.

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

Let's enumerate the values of the type `UpTo121`, that is `D1 (D3 (D3 () ()) (D3 () ())) (D2 (D3 () ()) (D1 () ()))` (the length alone of this type should convince you that what we're doing here, while interesting, is completely impractical).

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
* `UpTo21` is `D2 UpTo3 UpTo1`, so `D0_2 UpTo3 | D1_2 UpTo3 | D2_2 UpTo1`, representing `0 [0, 3] | 1 [0, 3] | 2 [0, 1]` which is exactly `[00, 21]`;
* `UpTo33` is `D3 UpTo3 UpTo3`, so `D0_3 UpTo3 | D1_3 UpTo3 | D2_3 UpTo3 | D3_3 UpTo3`, representing `[00, 33]`;
* `UpTo121` is `D1 UpTo33 UpTo21`, so `D0_1 UpTo33 | D1_1 UpTo21`, representing `0 [00, 33] | 1 [00, 21]` which is exactly `[000, 121]`.

We now need two things:
* a nice way to build those awful types,
* a way to perform the calculations and return our custom type.

Before we go on: the most common variants will be the `D3` ones (`D9` in base ten), so let's simplify that definition:

```Elm
type D x y
    = D0 x
    | D1 x
    | D2 x
    | D3 y
```

### `modBy` ###
So what will the API for `modBySafe` be? We have found out that it will need to return a value of type `Dx y z`, and the type of the result will depend on the modulus, so the modulus itself needs to encode something about the type. We'll try and keep the input number an `Int`.

```Elm
modBySafe : Modulus r -> Int -> r
```

Considering that the result can be any of the `Dx` we cannot write a more explicit type than `r` in the result, an we must rely completely on the modulus to inform Elm's compiler.

### Building moduli ###
Looking at the types above we can distinguish between the last digit (which is `Dx () ()`), and the other digits (which are `Dx 333....333 partial`).

Thinking about the types it's obvious that we can't use lists or `Int`s to build our modulus, the type changes with every digit.

For the last digits' we can expose simple functions:

```Elm
f0 : Modulus something
f1 : Modulus (D0 () ())
f2 : Modulus (D1 () ())
f3 : Modulus (D2 () ())
```

What is `something`? A modulus by zero doesn't make sense. In fact `modBy 0` is one of the few ways to crash an Elm program. We could simply use `()` and be happy or we can go fancy and use `Never`. Considering that we're interested in type contortionism rather than production-readyness, we'll use `Never`.

For adding digits, we would like to have functions `d1`, `d2`, `d3`, `d4` that get an input of type `Modulus (Dy z w)` and produce `Modulus (D1 (D z z) (Dy z w))` for `d1`, `(D2 ...)` for `d2`, ... The problem is that Elm's functions cannot be generic like that (generic in what `Dy` is), so we need to either write a `dx` for every `y` (this would mean one hundred functions for base ten) or we need to keep track of `z` in some additional way (and just put a type parameter in place of `Dy z w`).

How do we keep track of another type? We add a type parameter! So instead of having `Modulus x` we'll have `Modulus x y` where `x` is the "full" part (which will be of the form `D (D ...) (D ...)`) and `y` the "partial" part, which will be of the form `Dy (D ...) (Dz ...)`.

```Elm
f0 : Modulus () Never
f1 : Modulus () (D0 () ())
f2 : Modulus () (D1 () ())
f3 : Modulus () (D2 () ())

d0 : Modulus f p -> Modulus (D f p) (D0 f p)
d1 : Modulus f p -> Modulus (D f p) (D1 f p)
d2 : Modulus f p -> Modulus (D f p) (D2 f p)
d3 : Modulus f p -> Modulus (D f p) (D3 f p)
```

We can notice that `D` is always used with the same argument

For compactness sake let's also define:
```Elm
type alias OneDigit = D () ()
type alias TwoDigits = D OneDigit OneDigit
type alias ThreeDigits = D TwoDigits TwoDigits
```

## Implementation ##
We now need to implement the `modBySafe`, `fX` and `dX` functions. Our current API builds the modulus digit by digit, but (to my knowledge, correct me if I'm wrong), there are no algorithms for calculating the result the same way. On the other hand we'll have the input as an `Int`. To avoid reimplementing the wheel, let's see if we can use `elm/core`s `modBy` in the implementation.

To do this we need two ingredients:
1. the modulus as an Int (this looks easy),
2. a way to turn the result of `modBy modulus input` into a `Dx (...) (...)` value (this looks hard).

### Calculating the `Int` modulus ###
We can try simply storing the modulus inside `Modulus x y`:

```Elm
type alias Modulus x y =
    { modulus : Int
    , ...
    }
```

(In this article I'll write `Modulus` as a type alias, in a library we would use an opaque type).

But this quickly hits a wall: we are building `Modulus` values from the least significant digit to the most significant one, so when we add a zero the `Int` cannot track it. We could memorize the number "in reverse" and then invert the digits inside `modBySafe`, but this would just trade the problem of leading zeroes with a problem with trailing zeroes. We could store the number as a `String`, but this is dirty and could hide some bugs. Even better, we can try "reading" the construction from the other side.

If we write `d1 <| d2 <| f3`, which means a modulus of 123, when we read it from left to right we can interpret it as "write a 1, then multiply by 10 (four) and add 2, then multiply by 10 and add 3", so how can we express the "then multiply by 10 and add X" in a way that allows us to invert the order?

"given the previous intermediate value, multiply by 10 and add X" sounds a lot like `\v -> v * 10 + x`, and indeed representing the `Int` modulus as a function allows us to invert the flow!

```Elm
f3 =
    { modulus = \v -> v * 10 + 3
    }

d1 p =
    { modulus = \v -> (v * 10 + 1) |> p.modulus
    }

d2 p =
    { modulus = \v -> (v * 10 + 2) |> p.modulus
    }
```

So in the `dX` case we first take what's "arriving from the left" (more significant digits), multiply and add, and then pass it on "to the right" for the next (less significant) digits.

So now let's check what `(d1 <| d2 <| f3).modulus` is.

```Elm
f3 = { modulus = \v -> v * 10 + 3 }
d2 <| f3 =
    (\p -> { modulus = \v -> (v * 10 + 2) |> p.modulus })
    { modulus = \v -> v * 10 + 3 }
```
let's [change the name](https://wiki.haskell.org/Alpha_conversion) of the variable `v` in the expression for `f3` to avoid confusion
```Elm
d2 <| f3 =
    (\p -> { modulus = \v -> (v * 10 + 2) |> p.modulus })
    = { modulus = \v -> (v * 10 + 2) |> ({ modulus = \w -> w * 10 + 3 }).modulus }
    = { modulus = \v -> (v * 10 + 2) |> \w -> w * 10 + 3 }
```

And now (with another renaming):
```Elm
d1 <| d2 <| d3 =
    (\p -> { modulus = \z -> (z * 10 + 1) |> p.modulus })
    { modulus = \v -> (v * 10 + 2) |> \w -> w * 10 + 3 }
    = { modulus = \z -> (z * 10 + 1) |> \v -> (v * 10 + 2) |> \w -> w * 10 + 3 }
```
So we can see how using a function allowed us to "invert the `<|`s": the result applies the multiplications and additions in the order we need.

Let's simplify a little
```Elm
    = { modulus = \z -> ((z * 10 + 1) * 10 + 2) |> \w -> w * 10 + 3 }
    = { modulus = \z -> ((z * 10 + 1) * 10 + 2) * 10 + 3 }
    = { modulus = \z -> z * 1000 + 123 }
```
This also let us see how to recover the `Int` modulus when we'll need it for implementing `modBySafe`: we pass a simple `0` to `.modulus`.

## Converting an `Int` into the custom type ##
So we have the `Int` we need for `modBySafe` and the `Int` version of the modulus. If we call `modBy` from `elm/core` we get our result, but we now need to convert it to our custom `Dx (...) (...)` type. How can we do that?

Well, here we can actually go digit-by-digit. We shall build the result from the least significant digit, for the exact same reasons we outlined above while building the modulus.

Let's start small.

Let's say that `modulus` is `2`, so we need a function `Int -> D2 () ()`. `f2` has type `Modulus () (D2 () ())` so this suggests that a `Modulus x y` could contain a function `Int -> y`. In fact, considering that `modBySafe` must be fully generic, it seems that the function _must_ be contained inside the `Modulus`.

So, how would a function with signature `Int -> D2 () ()` look like?

We know that the input will be either `0` or `1`, because it's the result of `modBy`. Incidentally, this is why we want an opaque type and not a simple type alias: we don't want users to call it directly with some out of range value! We'll call the filed `partial` because it turns a `Int` into the "partial" part `p` of a `Modulus f p`.

```Elm
f2 =
    { modulus = \v -> v * 10 + 2
    , partial = partialF3
    }

partialF3 : Int -> D2 () ()
partialF3 i =
    case i of
        0 ->
            D0_2 ()

        1 ->
            D1_2 ()
```

Let's now take `123` for `modulus` and let the result of `x |> modBy modulus` be `101`, we want to build `D1_1 (D0_2 (D1 ()))`.

`123` as a modulus is `d1 <| d2 <| f3` which has type:
```Elm
Modulus
    ThreeDigits
    (D1 TwoDigits (D2 OneDigit (D1 () ())))
```
We need a function of type `Int -> D1 TwoDigits (D2 OneDigit (D1 () ()))`.

## Epilogue ##

I hope that this post helped you understand how bend <s>spoons</s> the Elm typesystem to achieve seemingly-impossible compile time safety. I intend to continue publishing posts in the "Advanced Elm" spirit, and I think I'll talk about crazy compile-time checking of tree/graph structure. Let me know what would you like to see!

Please send me feedback via Slack, Discourse or e-mail!
